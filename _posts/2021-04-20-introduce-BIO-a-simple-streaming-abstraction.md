---
layout: post
title:  "Introduce BIO: A Simple Streaming Abstraction"
date:   2021-04-20 14:43:14 CST
author: Dong
categories: design
---

<!-- Streaming IO is an old idea: the data is read in chunks, each chunk gets processed and written to output so that the whole memory a program used is kept under a relatively low level. e.g. -->

流式 IO 是一个古老的想法：按块读取数据，对每个块进行处理并将其写入输出，以使程序使用的内存大小保持在较低的水平。例如：

```base
cat foo.txt | gzip | base64 | tee foo.gz
```

<!-- Above UNIX commands read a file `foo.txt` in chunks, perform gzip and base64 transformation, and get piped to both `foo.gz` and stdout. We'd like to get similar syntax when using Haskell to work with chunked data, and that's the starting point of streaming abstraction. -->

在上面的UNIX命令中，按块读取文件foo.txt，执行 gzip 和 base64 转换，并通过管道传递到 `foo.gz` 和 `stdout` 。使用 Haskell 处理分块数据时，我们希望获得类似的语法，这就是流抽象的起点。

<!--more-->

<!-- ## A Stream ADT -->
## 流式代数数据类型

<!-- ### Partial closure -->
### 部分闭包

<!-- In [Z-Data's parser section](https://z.haskell.world/Z-Data/Parser-and-Builder.html), we described a resumable parser, which can consume input in chunks: -->
在 [Z-Data 的解析器部分](https://z.haskell.world/Z-Data/Parser-and-Builder.html) 中，我们描述了一个可恢复的解析器，该解析器可以分块消费输入：

```haskell
> P.parse' dateParser "2020-12-12"
Date 2020 12 12
> P.parseChunk dateParser "2020-"
Partial _
> let (P.Partial f) = P.parseChunk dateParser "2020-"
> let (P.Partial f') = f "05-05"    -- incrementally provide input
> f' ""                             -- push empty chunk to signal EOF
Success Date {year = 2020, month = 5, day = 5}
```

<!-- The core type to achieve resumable parsing is `Result`: -->
实现可恢复解析的核心类是 `Result` ：

```haskell
data Result e r
    = Success r !Bytes
    | Failure e !Bytes
    | Partial (V.Bytes -> Result e r)
```

<!-- The `Partial` constructor contains a closure capturing the last chunk's parsing state, which could be applied to the next chunk to produce a new `Result`. Now let's consider if we could apply this construction to IO(or an arbitrary monad), following definition is from the [streaming](https://hackage.haskell.org/package/streaming) package: -->
`Partial` 构造函数包含一个捕获了上一个块的解析状态的闭包，可以将其应用于下一个块以产生新的结果。现在让我们考虑是否可以将这种构造应用于 IO （或任意 monad ），以下定义来自 [streaming](https://hackage.haskell.org/package/streaming) 包：

```haskell
data Stream f m r = Step !(f (Stream f m r))
                  | Effect (m (Stream f m r))
                  | Return r

data Of a b = !a :> b
```

<!-- ### Stream Monad -->
### 流单子

<!-- In streaming, `Stream (Of a) IO ()` are used to represent `IO` streams, with some monad primitives you can construct an `IO` stream like this: -->
streaming 中，`Stream (Of a) IO ()` 用于表示 `IO` 流，通过一些 monad 原语，您可以像这样构造 `IO` 流：

```haskell
-- Stream monad will provide some primitives to create monadic value, e.g.
-- yield :: Monad m => a -> Stream (Of a) m ()
-- yield a = Step (a :> Return ())
-- instance (MonadIO m, Functor f) => MonadIO (Stream f m) where
--   liftIO = Effect . fmap Return . liftIO

foo :: Stream (Of a) IO ()
foo = do
    yield 1
    yield 2
    lift readLn >>= yield
```

<!-- With the `Stream`'s `Monad` instance, the value of foo now becomes a chain of Stream ADTs: -->
在 `Stream` 的 `Monad` 实例中，foo 的值现在变成了一条 Stream ADT 的链：

```haskell
Step (1 :> Step (2 :>  Effect (\ x -> Step x :> Return ()) <$> readLn))
```

<!-- Now if we provide a function to iterate through this ADT, the stream could be processed. Such a function is often called an interpreter, a term from [the free monad design pattern](https://softwareengineering.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern). For example streaming provides its own `foldrM` interpreter to fold over a `Stream` structure: -->

现在，如果我们提供迭代此ADT的函数，则可以处理该流。这种函数通常称为解释器，是[free monad 设计模式](https://softwareengineering.stackexchange.com/questions/242795/what-is-the-free-monad-interpreter-pattern) 中的术语。例如， streaming 提供了自己的 `foldrM` 解释器来处理 `Stream` 结构：

```haskell
foldrM :: Monad m => (a -> m r -> m r) -> Stream (Of a) m r -> m r
foldrM step = loop where
  loop stream = case stream of
    Return r       -> return r
    Effect m       -> m >>= loop        -- This is where IO effects happened!
    Step (a :> as) -> step a (loop as)
```

<!-- ### The Magic Pipes -->
### The Magic Pipes

<!-- There're some packages on hackage pushing the free monad technique to its limit, e.g. the [pipes](http://hackage.haskell.org/package/pipes) provide a rather incomprehensible core ADT type: -->
在 hackage 里有一些包将 free monad 技术推向了极限，例如[pipes](http://hackage.haskell.org/package/pipes) 提供了一种相当难以理解的核心ADT类型：

```haskell
data Proxy a' a b' b m r
    = Request a' (a  -> Proxy a' a b' b m r )
    | Respond b  (b' -> Proxy a' a b' b m r )
    | M          (m    (Proxy a' a b' b m r))
    | Pure    r
```

<!-- With this beast at hand, pipes could provide more interesting primitives like `await`, or `>->`. e.g `do x <- await; y <- await; return (x, y)` becomes: -->
有了这个复杂的类型，管道可以提供更多有趣的原语，例如 `await` 或 `>->` 。 例如 `do x <- await; y <- await; return (x, y)` 变为：

```haskell
Request () (\ x -> Request () (\ y -> Pure (x, y)))
```

<!-- One technique pipes used is to use type `Void` to eliminate some constructors under certain types while still keep composability: -->
一种 pipes 所使用的技术是使用 `Void` 类型来消除某些类型下的某些构造函数，同时仍保持可组合性：

```haskell
-- | type with no constructors
type X = Void

-- | 'Effect's neither 'Pipes.await' nor 'Pipes.yield'
type Effect = Proxy X () () X
-- | 'Producer's can only 'Pipes.yield'
type Producer b = Proxy X () () b
-- | 'Pipe's can both 'Pipes.await' and 'Pipes.yield'
type Pipe a b = Proxy () a () b
-- | 'Consumer's can only 'Pipes.await'
type Consumer a = Proxy () a () X
```

<!-- ## A Retrospective -->
## 回顾

<!-- ### Free monad is powerful, but hard to use -->
### Free monad is powerful, but hard to use
### Free monad 很强大，但是难以使用

<!-- The free monad approach could give you as many primitives as you want, and you could choose different interpreter to run, but it's hard to use in several ways:

+ It's hard to comprehend, you have to read the monad instance very carefully, to understand how those primitives work.
+ It has the same problem with monad transformers, i.e. now every base monad operations need to be lifted.
+ It's hard to be optimized by the compiler, because now every operation becomes an ADT constructor, and often leads to higher allocations.

A free monad construction for streaming may also need to provide a different set of combinators, such as `mapM` or `foldM`, which is incompatible with `Control.Monad`. -->

Free monad 可以为您提供尽可能多的原语，并且您可以选择不同的解释器运行他，但是因为以下几个层面的原因导致它很难被使用:

+ Free monad 很难被理解，您必须非常仔细地阅读 monad 实例，才能理解这些原语是如何工作的
+ Monad transformers 也有同样的问题，因为现在每个基础的 monad 操作都需要被提升
+ 很难通过编译器进行优化，因为现在每个操作都成为 ADT 的构造函数，并经常导致更多的内存分配

用于流的 free monad 构造器还需要提供一组不同的 combinators ，例如 `mapM` 或 `foldM` ，它们与 `Control.Monad` 不兼容。

<!-- ### How other languages do streaming -->

### 其他语言是如何处理流的

<!-- It's interesting to find out that most of the OO languages solve this problem in a much simpler way, for example in javascript. -->
有趣的是，发现大多数面向对象语言都以一种非常简单的方式解决了此问题，例如javascript。

```javascript
// from node.js example
const fs = require('fs');
const zlib = require('zlib');
const r = fs.createReadStream('file.txt');

const z = zlib.createGzip();
const w = fs.createWriteStream('file.txt.gz');
r.pipe(z).pipe(w);

// or you can manually connect streams like this:
r.on('data', (chunk) => { z.write(chunk); });
z.on('data', (chunk) => { w.write(chunk); });
```

<!-- In OO's viewpoint, a stream node is an object, with a method to receive chunks, and write to downstream inside callbacks, and that's it. This pattern has some drawbacks:

+ Stream node somehow lost its control, e.g. you can't stop the stream processing in a middle node without touching the source. This is the *Inversion of Control* problem of all callback-based APIs.
+ Stream node now became a mutable stateful object, which is unnatural in Haskell. -->

在面向对象编程的视角中，流节点是一个对象，具有一种接收块并写入下游内部回调方法，仅此而已。这种模式有一些缺点：

+ Stream 节点失去了控制，例如您没有办法在不接触 source 的情况下停止中间节点中的流处理。这是所有基于回调的API的 *控制反转* 问题。
+ Stream 节点现在变成了可变的有状态的对象，这在 Haskell 中是不自然的。

<!-- ## Introduce the BIO -->
## BIO 介绍

<!-- In [Z-IO](https://hackage.haskell.org/package/Z-IO) v0.8, we introduce a new `BIO` type to simplified streaming processing with three design goals:

+ Simple composable types.
+ No transformer, no lift.
+ Easier to be used for writing both processors and applications.

The result is a type focusing on *callback transformation*: -->


在 [Z-IO](https://hackage.haskell.org/package/Z-IO) v0.8中，我们为简化流处理引入了一种新的 `BIO` 类型，其具有三个设计目标：

+ 提供一种简单的可组合的类型
+ 没有 transformer ; 没有提升。
+ 使得应用和 BIO 库的开发都更容易

结果就是我们提供了一种专注于 *callback transformation* 的类型：

```haskell
-- A bio node receives a callback, returns a new callback to be called from upstream.
type BIO inp out = (Maybe out -> IO ()) -> Maybe inp -> IO ()

-- A Source doesn't consume any meaningful input
type Source a = BIO Void a
-- A Sink doesn't produce any meaningful output
type Sink a = BIO a Void

-- | A pattern for more meaningful matching.
pattern EOF :: Maybe a
pattern EOF = Nothing
```

<!-- For example to implemented a [zlib](https://zlib.net/) node with BIO: -->
例如，使用 BIO 实现 [zlib](https://zlib.net/) 节点：

```haskell
compressBIO :: ZStream -> BIO V.Bytes V.Bytes
compressBIO zs = \ callback mbs ->
    case mbs of
        Just bs -> do
            -- feed input chunk to ZStream
            set_avail_in zs bs (V.length bs)
            let loop = do
                    oavail :: CUInt <- withCPtr zs $ \ ps -> do
                        -- perform deflate and peek output buffer remaining
                        throwZlibIfMinus_ (deflate ps (#const Z_NO_FLUSH))
                        (#peek struct z_stream_s, avail_out) ps
                    when (oavail == 0) $ do
                        -- when output buffer is full,
                        -- freeze chunk and call the callback
                        oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                        callback (Just (V.PrimVector oarr 0 bufSiz))
                        newOutBuffer
                        loop
            loop
        _ -> ... similar to above, with no input chunk and Z_FINISH flag
```

<!-- When implemented a `Source`, you just ignore the `EOF` param, and call the callback once a new chunk is ready. -->
当实现 `Source` 时，您只需忽略 `EOF` 参数，并在准备好新块后调用回调。

```haskell
-- | Turn a `IO` action into 'Source'
sourceFromIO :: HasCallStack => IO (Maybe a) -> Source a
sourceFromIO io = \ k _ ->
    let loop = io >>= \ x ->
            case x of
                Just _ -> k x >> loop   -- you should loop inside a Source
                _      -> k EOF
    in loop
```

<!-- You should assume the `EOF` param is only given once, so a loop is often needed. Similar to `Source`, a `Sink` doesn't need to write any output until the final `EOF`: -->

您应该假设 `EOF` 参数仅给出一次，因此经常需要循环。 类似于 `Source` ， `Sink` 不需要写任何输出，直到最后的 `EOF` ：

```haskell
sinkToIO :: HasCallStack => (a -> IO ()) -> Sink a
sinkToIO f = \ k ma ->
    case ma of
        Just a -> f a
        _ -> k EOF
```

<!-- ### Composing BIO and running -->
### 组合 BIO 并运行

<!-- Composing BIO is simple: you can use `(.)` the function composition operator to connect BIOs, since it's just a callback transformation: -->

组合 BIO 很简单：您可以使用函数组合运算符 `(.)` 来连接 BIO ，因为它只是一个回调的转换：

```haskell
import Z.Data.CBytes    (CBytes)
import Z.IO
import Z.IO.BIO
import Z.IO.BIO.Zlib

base64AndCompressFile :: HasCallStack => CBytes -> CBytes -> IO ()
base64AndCompressFile origin target = do
    base64Enc <- newBase64Encoder
    (_, zlibCompressor) <- newCompress defaultCompressConfig{compressWindowBits = 31}

    withResource (initSourceFromFile origin) $ \ src ->
        withResource (initSinkToFile target) $ \ sink ->
            runBIO_ $ src . base64Enc . zlibCompressor . sink
```

<!-- Above code is similar to command line `cat origin | base | gzip > target`, and `runBIO_` is defined simply as: -->

上面的代码类似于命令行 `cat origin | base | gzip > target` ，且 `runBIO_` 的定义很简单：

```haskell
-- | Discards a value, used as the callback to `Sink`.
discard :: a -> IO ()
discard _ = return ()

runBIO_ :: HasCallStack => BIO inp out -> IO ()
runBIO_ bio = bio discard EOF
```

<!-- ### Conclusion -->
### 结论

<!-- There're many streaming libraries on hackage, and most of them are designed around the free monad pattern. In `Z-IO` we introduced a new simpler design around callback transformation, which is much easier to use for writing both stream processors and applications. Of course, nothing is silver bullets. The `BIO` type in `Z-IO` also has limitations, for example, the source can not be paused by a downstream processor without using some IO state, and the whole state management now relies on IO, rather than user-supplied state monads. -->

Hackage 上的流处理库很多，其中大多数都是围绕 free monad 模式设计的。在 `Z-IO` 中，我们基于回调转换引入了一种新的更简单的设计，该设计使得编写流处理器和应用程序都变得更简单。当然，没有什么是银弹。 `Z-IO` 中的 `BIO` 类型也有局限性，例如，下游节点不使用 IO 状态的时候是没有办法停止 source 唤起整个回调链路的，并且整个状态管理现在会依赖于 IO ，而不是用户提供的 state monad。