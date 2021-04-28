---
layout: default
parent: Z-IO
title: BIO 流
nav_order: 3
---

## Table of contents
{: .no_toc .text-delta }

1. TOC
{:toc}

<!-- # BIO: push and pull -->

# BIO: 推和拉

<!-- In previous sections, we have introduced the `Z.IO.Buffered` module. And it provides APIs for buffered reading and writing. When combined with [Builder and Parser]() facility, it is easy to handle some simple streaming tasks, for example, read/write packets from TCP wire. But sometimes, things could get complicated. Let's say you want to use the [zlib]() library to decompress a bytes stream from some file. The interface provided by zlib is like this: -->

在前面的部分中，我们介绍了 `Z.IO.Buffered` 模块，并提供了用于缓冲读写的API。与 [Builder and Parser]() 工具结合使用时，很容易处理一些简单的流式任务，例如，从TCP线读取/写入数据包。但有时情况可能会变得复杂。 假设您要使用 [zlib]() 库从某个文件中解压缩字节流， zlib 提供的接口是这样的：

```c
int inflateInit (z_streamp strm, int level);
int inflate (z_streamp strm, int flush);
int inflateEnd (z_streamp strm);
```

<!-- It's OK to draw a chunk from `BufferedInput`, feed it to `z_streamp`, check the status and do some computation if a decompressed chunk is produced. But how to read a line from decompressed streams? We can't reuse `readLine` from `Z.IO.Buffered` since decompressed chunks are not drawn directly from `BufferedInput`. -->

可以从 `BufferedInput` 中提取一个 chunk ，将其提供给 `z_streamp` ，检查状态并在产生了一个解压缩的 chunk 时进行一些计算。但是如何从解压缩的流中读取一行呢？我们不能重用来自 `Z.IO.Buffered` 中的 `readLine` ，因为解压缩的 chunk 不是直接从 `BufferedInput` 拉取的。

<!-- Ideally, we should have a composable `BufferedInput` type, which can accept some transformations and yield another `BufferedInput`. But `BufferedInput` is all about managing reading from buffer so that raw byte chunks can be drawn from the device. In Z-IO the `BIO` type is introduced to solve the composable streaming problem: -->

理想情况下，我们应该有一个可组合的 `BufferedInput` 类型，该类型可以接受一些转换并产生另一个 `BufferedInput` 。但是，`BufferedInput` 仅用于管理从缓冲区的读取，以便可以从设备中提取原始字节块。 在Z-IO中，引入了 `BIO` 类型来解决可组合的流数据问题：

```haskell
data BIO inp out = BIO
    { push :: inp -> IO (Maybe out)
    , pull :: IO (Maybe out)
    }
```

<!-- Conceptually a `BIO` is a box doing data transformation: -->

理论上来说，`BIO` 可以看成是执行数据转换的盒子：

<!-- ```
           +-------------+ 
           |       +-----+------ Maybe out | Just x : push directly produced an output chunk x
      push |      /      | push            | Nothing: push is not enough to produce an output
 inp ------+-----+       | 
           | BIO inp out | pull (called after input reached EOF)
           |             +------ Maybe out | Just x : there's a chunk x left inside BIO's state
           +-------------+                 | Nothing: the BIO reached its EOF after input ended
``` -->

```
           +-------------+ 
           |       +-----+-----> Maybe out | Just x : push 直接产生了一个输出块 x
      push |      /      | push            | Nothing: push 不足以产生输出
 inp ----->+-----+       | 
           | BIO inp out | pull (当Input遇到EOF时调用)
           |             +-----> Maybe out | Just x : BIO 的状态里还有块 x
           +-------------+                 | Nothing: BIO 也遇到了EOF在输入停止之后
```


<!-- Let's take zlib's `z_streamp` as an example:

+ A `z_streamp` struct could be `push`ed with an input chunk using `inflate`, possibly producing an output chunk. 
+ If input reached EOF, use `inflateEnd` to `pull` the trailing compressed bytes buffered inside `z_streamp` struct.


Another `BIO` example is a rechunk node, which would divide input chunks into chunks with fixed granularity(or multipliers of the fixed granularity). This is often useful in data encryption/decryption. Following code is an implementation of such a node: -->

让我们用 zlib's `z_streamp` 举一个例子:

+ 一个 z_streamp 结构可以使用 inflate 推入一个输入块，可能会产生一个输出块。
+ 如果输入达到EOF，可以使用 inflateEnd 来拉出 z_streamp 结构中缓冲的尾部压缩块。


另一个 `BIO` 示例是一个 rechunk 节点，它将输入块分为具有固定大小（或固定粒度若干倍）的块。这通常在数据加密/解密中很有用。以下代码是此类节点的一种实现：

```haskell
import Data.IORef
import qualified Z.Data.Vector as V

newReChunk :: Int   -- ^ chunk granularity
           -> IO (BIO V.Bytes V.Bytes)
{-# INLINABLE newReChunk #-}
newReChunk n = do
    -- create a storage for trailing bytes
    trailingRef <- newIORef V.empty
    return (BIO (push_ trailingRef) (pull_ trailingRef))
  where
    -- implement push operation, take input chunk, return chunk in multiplier of granularity
    push_ trailingRef bs = do
        trailing <- readIORef trailingRef
        -- concat trailing bytes from last chunk first
        let chunk =  trailing `V.append` bs
            l = V.length chunk
        if l >= n
        then do     -- if we have enough bytes, then make a cut
            let l' = l - (l `rem` n)
                (chunk', rest) = V.splitAt l' chunk
            writeIORef trailingRef rest
            return (Just chunk')
        else do     -- otherwise we continue waiting for new chunks
            writeIORef trailingRef chunk
            return Nothing

    -- implement pull operation, which is called after input ended
    -- here we choose to return trailing bytes directly
    -- depending on usage, you may throw it away or add some padding 
    pull_ trailingRef = do
        trailing <- readIORef trailingRef
        if V.null trailing
        then return Nothing
        else do
            writeIORef trailingRef V.empty
            return (Just trailing)
```

<!-- Look at `newReChunk`'s implementation, which uses `IORef` a.k.a. mutable reference in IO. It's clear that this `BIO` carries its state inside IO monad, in a way similar to `IORef` or `z_streamp` above. So it can't be used like immutable data structures: -->

看一下 `newReChunk` 的实现，它使用了 `IORef` 也就是 IO 中的可变引用。显然，`BIO` 在 IO monad 中保持状态的方式，和上面提到的 `IORef` 或 `z_streamp` 很详细。因此不能像不可变的数据结构那样使用它：

<!-- ```haskell
...
rechunk <- newReChunk
-- used in one place will mutate rechunk's state(trailing bytes in this case)
... rechunk ...
-- it's unsafe to be used in another place 
... rechunk ...
``` -->

```haskell
...
rechunk <- newReChunk
-- 在一个地方使用将改变rechunk的状态（在这种情况下为尾部的结余字节）
... rechunk ...
-- 用在另一个地方就导致了不安全性 
... rechunk ...
```

<!-- The `Z.IO.BIO` module provides various `BIO` node types, from UTF-8 decoder to counter node. Most of them are stateful, and you should create a new node each time. Some nodes are not stateful though: -->

`Z.IO.BIO` 模块提供了从 UTF-8 解码器到计数器等各种 `BIO` 节点类型。它们中的大多数都是有状态的，您应该每次都创建一个新节点去使用他们。但是，某些节点不是有状态的：

<!-- ```haskell
-- A `hexEncoder` is a pure bytes transform node, thus can be used without initialization.
hexEncoder :: Bool  -- ^ uppercase?
           -> BIO Bytes Bytes
hexEncoder upper = pureBIO (hexEncode upper)

-- | BIO node from a pure function.
pureBIO :: (a -> b) -> BIO a b
pureBIO f = BIO (\ x -> let !r = f x in return (Just r)) (return Nothing)
``` -->

```haskell
-- `hexEncoder` 是一个纯的字节转换节点, 所以不用初始化也可以使用。
hexEncoder :: Bool  -- ^ 大写?
           -> BIO Bytes Bytes
hexEncoder upper = pureBIO (hexEncode upper)

-- | 用于 pure function 的BIO节点.
pureBIO :: (a -> b) -> BIO a b
pureBIO f = BIO (\ x -> let !r = f x in return (Just r)) (return Nothing)
```

<!-- # Source and Sink types

Now let's consider the following devices:

+ A data source which doesn't take any input but can be read until EOF.
+ A data sink which only performs writing without producing any meaningful result.

We can have the definitions for data `Source` and `Sink` by using `Void` from `Data.Void`: -->

# 源和汇聚 类型

现在让我们考虑以下设备：

+ 一个源类型，不接受任何输入，但是可以在遇到 EOF 之前读取。
+ 一个汇聚类型，仅执行写入操作而不会产生任何有意义的结果。

我们可以使用 `Data.Void` 中的 `Void` 来定义 `Source` 和 `Sink` 的类型：

<!-- 
```haskell
-- Source type doesn't need input
type Source a = BIO Void a
-- Sink type doesn't produce output
type Sink a = BIO a Void
``` -->

```haskell
-- 源类型不需要输入
type Source a = BIO Void a
-- 汇聚类型不需要输出
type Sink a = BIO a Void
```

<!-- Because `Void` type doesn't have constructors, thus `push` `Source` is impossible, one can only define `pull` field when defining a `Source`: -->

由于 `Void` 类型没有构造函数，因此 `push` `Source` 是不可能的，因此在定义 `Source` 时只能定义 `pull` 字段：

<!-- ```
           +--------------+ 
 Void --X->|              | 
           | BIO Void out | pull 
           |              +------ Maybe out | Just x : there's a chunk x left inside `Source`
           +--------------+                 | Nothing: the `Source` reached its EOF
``` -->

```
           +--------------+ 
 Void --X->|              | 
           | BIO Void out | pull 
           |              +-----> Maybe out | Just x : 在 `Source` 中仍有块x
           +--------------+                 | Nothing: `Source` 已经遇到 EOF
```

<!-- For example, a `BIO` node sourcing elements from `[a]` can be implemented like this: -->

例如，可以像这样实现读取 `[a]` 的 `BIO` 源节点：

<!-- ```haskell
{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | Source a list from memory.
sourceFromList :: [a] -> IO (Source a)
sourceFromList xs0 = do
    xsRef <- newIORef xs0
    -- there's no need to set the push field, since no one could possibly call it
    return BIO{ pull = popper xsRef }
  where
    popper xsRef = do
        xs <- readIORef xsRef
        case xs of
            (x:xs') -> do
                writeIORef xsRef xs'
                return (Just x)
            _ -> return Nothing
``` -->

```haskell
{-# OPTIONS_GHC -Wno-missing-fields #-}

-- | 从内存里将数组转化成流.
sourceFromList :: [a] -> IO (Source a)
sourceFromList xs0 = do
    xsRef <- newIORef xs0
    -- 没有必要实现push方法 因为不会有机会调用它
    return BIO{ pull = popper xsRef }
  where
    popper xsRef = do
        xs <- readIORef xsRef
        case xs of
            (x:xs') -> do
                writeIORef xsRef xs'
                return (Just x)
            _ -> return Nothing
```

<!-- For `type Sink a = BIO a Void`, both `push` and `pull`'s field type is `a -> Maybe Void`, which means both `push` and `pull` can only return `Nothing`. We deliberately use `pull` for flushing output device in Z: -->

对于 `type Sink a = BIO a Void` ，`push` 和 `pull` 的字段类型均为 `a -> Maybe Void` ，这意味着 `push` 和 `pull` 都只能返回 `Nothing` 。我们特意使用 `pull` 来 `flush` Z中的输出设备：

```
           +--------------+ 
           |       +------+-----> Nothing :: Maybe Void
      push |      /       | push (push input chunk into `Sink`)
 inp ----->+-----+        | 
           | BIO inp Void | pull (flush the `Sink`)
           |              +-----> Nothing :: Maybe Void
           +--------------+              
```

<!-- # Composing BIO -->

# BIO的组合

<!-- Now we have the abstract stream transformation type `BIO`, let's start to consider how to compose transformations together instead of `pull` from one and `push` to another manually. The composition of two `BIO` node should:

+ Take the first node's input type, yield second node's output.
+ If an input chunk is `push`ed, produced an output chunk if both node could produce output.
+ After input reached EOF, `pull` should consume buffered trailing bytes from both node. -->

现在我们有了抽象的流转换类型 `BIO`，让我们开始考虑如何将转换组合在一起，而不是通过手动调用 `pull` 和 `push` 来组合流的转换。两个 `BIO` 节点的组合应该可以保证：

+ 获取第一个节点的输入，产生第二个节点的输出。
+ 如果输入块被 `push` ， 如果两个节点都可以产生输出，则产生输出块。
+ 输入达到EOF后， `pull` 应该消耗两个节点的缓冲尾部字节。

<!-- `>|>` from `Z.IO.BIO` is the implementation of such a composition: -->

`Z.IO.BIO` 中的 `>|>` 实现了下面的组合逻辑:

<!-- ```haskell
                                   Nothing
                                   +--------------------------------+
           +--------------+       /             +--------------+     \
           |       +------+------+--------------+--------------+------+----- Maybe outB
      push |      /       | push   Just x  push | BIO inB outB |       push
 inA ------+-----+        |      +--------------+------+       |
           | BIO inA outA | pull | Just x  push | loop |       |       pull
           |              +------+--------------+------+-------+------+----- Maybe outB
           +--------------+       \             +--------------+     /
                                   +--------------------------------+
                                   Nothing

-- | Connect two 'BIO' nodes, feed the output from left to right.
(>|>) :: BIO a b -> BIO b c -> BIO a c
{-# INLINE (>|>) #-}
BIO pushA pullA >|> BIO pushB pullB = BIO push_ pull_
  where
    push_ inp = do
        -- push to node A first
        x <- pushA inp
        -- if node A produces output, push to node B, otherwise hold
        case x of Just x' -> pushB x'
                  _       -> return Nothing
    pull_ = do
        -- pull node A first
        x <- pullA
        case x of
            -- if node A produces output, push to node B
            Just x' -> do
                y <- pushB x'
                -- draw input from A in a loop until there's an output from B
                case y of Nothing -> pull_  
                          _       -> return y
            -- node A reached EOF, pull node B
            _       -> pullB

``` -->

```haskell
                                   Nothing
                                   +--------------------------------+
           +--------------+       /             +--------------+     \
           |       +------+----->+------------->+--------------+------+----> Maybe outB
      push |      /       | push   Just x  push | BIO inB outB |       push
 inA ----->+-----+        |      +--------------+------+       |
           | BIO inA outA | pull | Just x  push | loop |       |       pull
           |              +----->+------------->+------+-------+------+----> Maybe outB
           +--------------+       \             +--------------+     /
                                   +--------------------------------+
                                   Nothing

-- | 连接两个 `BIO` 节点，从左到右输入
(>|>) :: BIO a b -> BIO b c -> BIO a c
{-# INLINE (>|>) #-}
BIO pushA pullA >|> BIO pushB pullB = BIO push_ pull_
  where
    push_ inp = do
        -- 先 push 到 A节点
        x <- pushA inp
        -- 如果A节点产生了输出 push 到 B节点； 否则保存在状态里
        case x of Just x' -> pushB x'
                  _       -> return Nothing
    pull_ = do
        -- 先从 A节点 pull
        x <- pullA
        case x of
            -- 如果A节点产生了输出 push 到 B节点
            Just x' -> do
                y <- pushB x'
                -- 循环从A提取输入，直到B产生输出
                case y of Nothing -> pull_  
                          _       -> return y
            -- A节点 到达 EOF ，pull B节点
            _       -> pullB

```

<!-- This composition's type has some interesting results:

+ If you compose a `Source a` to `BIO a b`, you will get a `Source b`.
+ If you compose a `BIO a b` to `Sink b`, you will get a `Sink a`.

So let's say you want to count the line number of a file, you could use `BIO`: -->

这种组合的类型产生了一些有趣的结果：

+ 如果您将 `Source a` 和 `BIO a b` 组合， 则将获得 `Source b` 类型
+ 如果您将 `BIO a b` 和 `Sink b` 组合， 则将获得 `Sink a` 类型

假设您要计算文件的行号，可以像这样使用 `BIO` ：

```haskell
import Z.IO
import Z.Data.PrimRef (readPrimIORef)

main :: IO ()
main = do
    _:path:_ <- getArgs
    withResource (initSourceFromFile path) $ \ fileSource -> do
        (counterRef, counterNode) <- newCounterNode
        splitNode <- newLineSplitter
        _ <- runSource_ $ fileSource >|> splitNode >|> counterNode
        printStd =<< readPrimIORef counterRef
```

`runSource_ :: Source a-> IO()` 是一个函数，它不断地从`Source`节点获取输入，直到到达EOF，其定义为：

```
-- | Drain a source without collecting result.
runSource_ :: Source x -> IO ()
runSource_ BIO{..} = loop pull
  where
    loop f = do
        r <- f
        case r of Just _ -> loop f
                  _      -> return ()
```

<!-- As long as `pull`ing from `Source` returns chunks, `runSource_` will not stop. In the example above, we use this function to drive the whole `BIO` chain, draw chunks from the file and feed into the line splitter and then the counter. The counter state is a primitive reference that can be read using functions from `Z.Data.PrimRef`.

If you have a complete `BIO` from `Source` to `Sink`, you will get a composition node with type `BIO Void Void`, which doesn't have any input or output. You can directly `pull` it to run the whole chain because when you `pull` the composition node, `>|>` will continuously draw chunks from the left node if the right node outputs `Nothing`. And it is the only possible output if the right node is a `Sink`. Thus we have the following function: -->

只要从 `Source` 中能拉取数据，`runSource_` 就不会停止。在上面的示例中，我们使用此函数来驱动整个 `BIO` 链，从文件中提取大块，并传入行拆分器，然后传入计数器。计数器状态是一个原语引用，可以使用 `Z.Data.PrimRef` 中的函数来读取。

如果您具有从 `Source` 到 `Sink` 的完整的 `BIO`，则将得到一个类型为 `BIO Void Void` 的合成节点，该节点没有任何输入或输出。您可以直接 `pull` 它来驱动整个链，因为当您 `pull` 合成节点时，如果右节点输出 `Nothing`，则 `>|>` 将连续从左节点拉取数据。 并且如果右节点是一个 `Sink` ，则这是唯一可能的输出。因此，我们具有以下函数：

```haskell
runBIO :: BIO Void Void -> IO ()
runBIO BIO{..} = pull >> return ()
```

<!-- # BIO Cheatsheet -->

# BIO 备忘录

<!-- `Z.IO.BIO` provides many functions to construct `Source`, `Sink` and `BIO`s. Here's a cheatsheet: -->

`Z.IO.BIO` 提供许多函数来构造 `Source`, `Sink` 和 `BIO` 。这是一份备忘单：

<!-- + `Source` and `Sink` -->

+ `Source` 和 `Sink`

<!-- ```haskell
    -- source from list
    sourceFromList                 :: [a] -> IO (Source a)
    -- source from file
    initSourceFromFile             :: CBytes -> Resource (Source Bytes)
    -- source from IO
    sourceFromIO                   :: IO (Maybe a) -> Source a
    -- source from `BufferedInput`
    sourceFromBuffered             :: BufferedInput -> Source Bytes
    sourceTextFromBuffered         :: BufferedInput -> Source Text
    sourceJSONFromBuffered         :: JSON a => BufferedInput -> Source a
    sourceParserFromBuffered       :: Parser a -> BufferedInput -> Source a
    sourceParseChunksFromBuffered  :: Print e
                                   => ParseChunks IO Bytes e a -> BufferedInput -> Source a
    -- sink to list
    sinkToList                     :: IO (IORef [a], Sink a)
    -- sink to file
    initSinkToFile                 :: CBytes -> Resource (Sink Bytes)
    -- sink to perform IO
    sinkToIO                       :: (a -> IO ()) -> Sink a
    -- sink to `BufferedOutput`
    sinkToBuffered                 :: BufferedOutput -> Sink Bytes
    sinkBuilderToBuffered          :: BufferedOutput -> Sink (Builder a)
    ``` 
-->

```haskell
    -- 将列表变成流
    sourceFromList                 :: [a] -> IO (Source a)
    -- 将文件变成流
    initSourceFromFile             :: CBytes -> Resource (Source Bytes)
    -- 从 IO 中获取流
    sourceFromIO                   :: IO (Maybe a) -> Source a
    -- 从 `BufferedInput` 中获取流
    sourceFromBuffered             :: BufferedInput -> Source Bytes
    sourceTextFromBuffered         :: BufferedInput -> Source Text
    sourceJSONFromBuffered         :: JSON a => BufferedInput -> Source a
    sourceParserFromBuffered       :: Parser a -> BufferedInput -> Source a
    sourceParseChunksFromBuffered  :: Print e
                                   => ParseChunks IO Bytes e a -> BufferedInput -> Source a
    -- 将流转化成列表
    sinkToList                     :: IO (IORef [a], Sink a)
    -- 将流写入文件
    initSinkToFile                 :: CBytes -> Resource (Sink Bytes)
    -- 将流输出到 IO
    sinkToIO                       :: (a -> IO ()) -> Sink a
    -- 将流输出到 `BufferedOutput`
    sinkToBuffered                 :: BufferedOutput -> Sink Bytes
    sinkBuilderToBuffered          :: BufferedOutput -> Sink (Builder a)
```

<!-- + Built-in `BIO`s -->
+ 内置 `BIO` 节点

    <!-- ```haskell
    -- | Parse bytes to produce result 
    newParserNode :: Parser a -> IO (BIO Bytes a)
    -- | Rechunk chunks to size in multipler of a fixed granularity
    newReChunk :: Int -> IO (BIO V.Bytes V.Bytes)
    -- | UTF-8 decoder
    newUTF8Decoder :: IO (BIO V.Bytes T.Text)
    -- | Split chunk by magic byte(keep the byte)
    newMagicSplitter :: Word8 -> IO (BIO V.Bytes V.Bytes)
    -- | Split chunk by linefeed(drop linefeeds)
    newLineSplitter :: IO (BIO V.Bytes V.Bytes)
    -- | Base64 encoder
    newBase64Encoder :: IO (BIO V.Bytes V.Bytes)
    -- | Base64 decoder
    newBase64Decoder :: IO (BIO V.Bytes V.Bytes)
    -- | Stateless hex encoder
    hexEncoder :: Bool -> BIO V.Bytes V.Bytes
    -- | Hex decoder
    newHexDecoder :: IO (BIO V.Bytes V.Bytes)
    -- | Count input elements number
    newCounterNode :: IO (Counter, BIO a a)
    -- | Label input elements with a sequence number
    newSeqNumNode :: IO (Counter, BIO a (Int, a))
    -- | Grouping input elements into fixed size arrays
    newGroupingNode :: Int -> IO (BIO a (A.SmallArray a))
    ``` -->


```haskell
    -- | 解析字节以产生结果
    newParserNode :: Parser a -> IO (BIO Bytes a)
    -- | 将块重新调整大小到为固定粒度的倍数
    newReChunk :: Int -> IO (BIO V.Bytes V.Bytes)
    -- | UTF-8 解码器
    newUTF8Decoder :: IO (BIO V.Bytes T.Text)
    -- | Split chunk by magic byte(keep the byte)
    newMagicSplitter :: Word8 -> IO (BIO V.Bytes V.Bytes)
    -- | Split chunk by linefeed(drop linefeeds)
    newLineSplitter :: IO (BIO V.Bytes V.Bytes)
    -- | Base64 编码器
    newBase64Encoder :: IO (BIO V.Bytes V.Bytes)
    -- | Base64 解码器
    newBase64Decoder :: IO (BIO V.Bytes V.Bytes)
    -- | 无状态 hex 编码器
    hexEncoder :: Bool -> BIO V.Bytes V.Bytes
    -- | Hex 解码器
    newHexDecoder :: IO (BIO V.Bytes V.Bytes)
    -- | 输入元素计数器
    newCounterNode :: IO (Counter, BIO a a)
    -- | 用序列号标记输入元素
    newSeqNumNode :: IO (Counter, BIO a (Int, a))
    -- | 将输入元素分组为固定大小的数组
    newGroupingNode :: Int -> IO (BIO a (A.SmallArray a))
```

<!-- + Composition -->
+ 组合

    <!-- ```haskell
    -- | Connect two 'BIO' nodes, feed left one's output to right one's input.
    (>|>) :: BIO a b -> BIO b c -> BIO a c
    -- | Map a function to BIO's output elements.
    (>~>) :: BIO a b -> (b -> c) -> BIO a c
    -- | Connect BIO to an effectful function.
    (>!>) :: BIO a b -> (b -> IO c) -> BIO a c
    -- | Connect two 'BIO' source, after first reach EOF, draw element from second.
    appendSource :: Source a -> Source a  -> IO (Source a)
    -- | Connect list of 'BIO' sources, after one reach EOF, draw element from next.
    concatSource :: [Source a] -> IO (Source a)
    -- | Zip two 'BIO' source into one, reach EOF when either one reached EOF.
    zipSource :: Source a -> Source b -> IO (Source (a,b))
    -- | Zip two 'BIO' nodes into one, reach EOF when either one reached EOF.
    zipBIO :: BIO a b -> BIO a c -> IO (BIO a (b, c))
    -- | Write to both left and right sink.
    joinSink :: Sink out -> Sink out -> Sink out
    -- | Write to a list of sinks.
    fuseSink :: [Sink out] -> Sink out
    ``` -->

```haskell
    -- | 连接两个 `BIO` 节点，将左一个的输出传递到右一个的输入。
    (>|>) :: BIO a b -> BIO b c -> BIO a c
    -- | 将函数映射到 BIO 的输出元素。
    (>~>) :: BIO a b -> (b -> c) -> BIO a c
    -- | 连接 BIO 到一个有副作用的函数.
    (>!>) :: BIO a b -> (b -> IO c) -> BIO a c
    -- | 连接两个 `BIO` 源，第一个遇到EOF时，从第二个节点拉取数据。
    appendSource :: Source a -> Source a  -> IO (Source a)
    -- | 连接 `BIO` 源列表，一个到达EOF后，从下一个继续拉取数据。
    concatSource :: [Source a] -> IO (Source a)
    -- | 将两个 `BIO` 源压缩成一个，当其中任意一个到达EOF时到达EOF。
    zipSource :: Source a -> Source b -> IO (Source (a,b))
    -- | 将两个 `BIO` 节点压缩成一个，当其中任意一个到达EOF时到达EOF。
    zipBIO :: BIO a b -> BIO a c -> IO (BIO a (b, c))
    -- | 写到两个汇聚节点。
    joinSink :: Sink out -> Sink out -> Sink out
    -- | 写到列表中所有的汇聚节点。
    fuseSink :: [Sink out] -> Sink out
```

<!-- + Execution -->
+ 执行

```haskell
    -- | Run a 'BIO' chain from source to sink.
    runBIO :: BIO Void Void -> IO ()
    -- | Drain a 'BIO' source into a List in memory.
    runSource :: Source x -> IO [x]
    -- | Drain a source without collecting result.
    runSource_ :: Source x -> IO ()
    -- | Supply a single block of input, then run BIO node until EOF.
    runBlock :: BIO inp out -> inp -> IO [out]
    -- | Supply a single block of input, then run BIO node until EOF with collecting result.
    runBlock_ :: BIO inp out -> inp -> IO ()
    -- | Wrap 'runBlocks' into a pure interface.
    unsafeRunBlock :: IO (BIO inp out) -> inp -> [out]
    -- | Supply blocks of input, then run BIO node until EOF.
    runBlocks :: BIO inp out -> [inp] -> IO [out]
    -- | Supply blocks of input, then run BIO node until EOF with collecting result.
    runBlocks_ :: BIO inp out -> [inp] -> IO ()
    -- | Wrap 'runBlocks' into a pure interface.
    unsafeRunBlocks :: IO (BIO inp out) -> [inp] -> [out]
```
