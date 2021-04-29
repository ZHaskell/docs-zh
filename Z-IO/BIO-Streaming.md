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

<!-- # BIO: composable callbacks -->
# BIO: 可组合的回调

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
type BIO inp out = (Maybe out -> IO ()) -> Maybe inp -> IO ()
```

<!-- Conceptually a `BIO` is a box doing transformation on data callbacks: -->

理论上来说，`BIO` 可以看成是在数据回调时执行数据转换的盒子：

<!-- ```haskell
-- A pattern synonym for more meaningful pattern match
pattern EOF :: Maybe a
pattern EOF = Nothing

fooBIO :: BIO foo bar
fooBIO callback maybeFoo = do
    ... use callback to pass output data
    case maybeFoo of
        Just foo ->
            ... you can send result to downstream by pass Just values
            ... to callback, and you can call callback multiple time.
            callback (Just ...)
            ...
            callback (Just ...)
            ...
        EOF ->
            ... Nothing input indicate EOF
            .. you should pass EOF to callback to indicate current
            .. node also reaches its EOF
            callback EOF
``` -->
```haskell
-- 通过 pattern synonym 使模式匹配变得更可读
pattern EOF :: Maybe a
pattern EOF = Nothing

fooBIO :: BIO foo bar
fooBIO callback maybeFoo = do
    ... use callback to pass output data
    case maybeFoo of
        Just foo ->
            ... you can send result to downstream by pass Just values
            ... to callback, and you can call callback multiple time.
            callback (Just ...)
            ...
            callback (Just ...)
            ...
        EOF ->
            ... Nothing input indicate EOF
            .. you should pass EOF to callback to indicate current
            .. node also reaches its EOF
            callback EOF
```

<!-- Let's take zlib's `z_streamp` as an example:

+ A `z_streamp` struct could be `push`ed with an input chunk using `inflate`, possibly producing an output chunk.
+ If input reached EOF, use `inflateEnd` to `pull` the trailing compressed bytes buffered inside `z_streamp` struct.

The `Z.IO.BIO` module provides various `BIO` node types, from UTF-8 decoder to counter node. Most of them are stateful, so you should create a new node each time. -->

让我们用 zlib's `z_streamp` 举一个例子:

+ 一个 z_streamp 结构可以使用 inflate 推入一个输入块，可能会产生一个输出块。
+ 如果输入达到EOF，可以使用 inflateEnd 来拉出 z_streamp 结构中缓冲的尾部压缩块。

`Z.IO.BIO` 模块提供了从 UTF-8 解码器到计数器等各种 `BIO` 节点类型。它们中的大多数都是有状态的，您应该每次都创建一个新节点去使用他们。

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


Because `Void` type doesn't have constructors, thus `push` values other than `Nothing` to `Source` is impossible, one should ignore the `Maybe Void` param when defining a `Source`. For example, a `BIO` node sourcing chunks from `BufferedInput` can be implemented like this:

由于 `Void` 类型没有构造函数，因此不可能往 `Source` 中 `push` 除了 `Nothing` 之外的值，因此在定义 `Source` 时应该忽略 `Maybe Void` 参数。 比如，一个从 `BufferedInput` 中读取数据流的 `BIO` 节点可以这样实现：


```haskell
sourceFromBuffered :: BufferedInput -> Source V.Bytes
sourceFromBuffered i = \ k _ ->
    let loop = readBuffer i >>= \ x ->
            if V.null x then k EOF else k (Just x) >> loop
    in loop
```

<!-- For `type Sink a = BIO a Void`, the callback type is `Maybe Void -> IO ()`, which means you can only pass `Nothing` to the callback, the convention here is to only call callback once with `Nothing` on EOF: -->

对于 `type Sink a = BIO a Void` ，回调类型为 `Maybe Void -> IO ()` ，这意味着你只能将 `Nothing` 传递给回调。这里的约定是只有在遇到 EOF 的时候用 `Nothing` 调用一次回调函数：

```haskell
-- | The `BufferedOutput` device will get flushed only on EOF.
sinkToBuffered :: BufferedOutput -> Sink V.Bytes
sinkToBuffered bo = \ k mbs ->
    case mbs of
        Just bs -> writeBuffer bo bs
        _       -> flushBuffer bo >> k EOF
```

<!-- # Composing BIO -->
# BIO的组合

<!-- The `BIO` type could be composed via `(.)`, i.e. the function composition. The composition's result has some interesting facts: -->
BIO类型可以通过 `(.)` 组合，即函数组合。这种组合的类型产生了一些有趣的结果：

<!-- + If you compose a `Source a` to `BIO a b`, you will get a `Source b`.
+ If you compose a `BIO a b` to `Sink b`, you will get a `Sink a`. -->
+ 如果您将 `Source a` 和 `BIO a b` 组合， 则将获得 `Source b` 类型
+ 如果您将 `BIO a b` 和 `Sink b` 组合， 则将获得 `Sink a` 类型

<!-- So let's say you want to count the line number of a file, you could use `BIO`: -->
假设您要计算文件的行数，可以使用 `BIO`

```haskell
import Z.IO
import Z.Data.PrimRef 

main :: IO ()
main = do
    _:path:_ <- getArgs
    withResource (initSourceFromFile path) $ \ fileSource -> do
        counterRef <- newCounter 0
        let counter = counterNode counterRef
        splitter <- newLineSplitter
        runBIO_ $ fileSource . splitter . counter
        printStd =<< readPrimIORef counterRef
```

<!-- `runBIO_ :: Source a -> IO ()` simply supply a `EOF` to the BIO chain, and fileSource will drive the whole chain running until EOF, it's defined as: -->

`runBIO_ :: Source a -> IO ()` 仅向 BIO 链提供一个 `EOF` ，fileSource 将驱动整个链运行直到 EOF，其定义为：

```haskell
discard :: a -> IO ()
{-# INLINABLE discard #-}
discard _ = return ()

runBIO_ :: BIO inp out -> IO ()
{-# INLINABLE runBIO_ #-}
runBIO_ bio = bio discard EOF
```

<!-- Another example from the [introduce BIO blog post](https://z.haskell.world/design/2021/04/20/introduce-BIO-a-simple-streaming-abstraction.html): -->
[介绍BIO](https://z.haskell.world/design/2021/04/20/introduce-BIO-a-simple-streaming-abstraction.html) 的博客中有另一个示例：

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

<!-- Above code is similar to command line `cat origin | base | gzip > target`. -->
上面的代码类似于命令行 `cat origin | base | gzip > target` 。
