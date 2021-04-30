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

概念上来说，`BIO` 可以看成是针对回调进行变换的盒子：

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
            ... you should pass EOF to callback to indicate current
            ... node also reaches its EOF
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
            ... 你可以通过给 callback 传递 Just 值向下游写入
            ... 你可以调用 callback 多次
            callback (Just ...)
            ...
            callback (Just ...)
            ...
        EOF ->
            ... 你应该通过给 callback 传递 EOF 来告知下游 EOF
            callback EOF
```

<!-- `BIO` type have two params:

+ A `callback :: Maybe out -> IO ()`(often written as `k`) which get called when to write downstream:
    + A `Just out` value is an item passed to downstream.
    + A `EOF` notified downstream EOF.
+ A `Maybe inp` value which comes from upstream:
    + A `Just inp` value is an item from upstream.
    + A `EOF` notified upstream EOF.

Let's take zlib's `z_streamp` as an example to implement a compressing BIO node -->

`BIO` 是一个接受两个参数的函数：

+ 一个 `callback :: Maybe out -> IO ()`(也经常被记作 `k`)，会被再向下游写入的时候被调用：
    + `Just out` 向下游写入 out。
    + `EOF` 告诉下游 EOF。
+ 一个从上游传过来的 `Maybe inp`：
    + `Just inp` 是一个来自上游的 `inp` 值。
    + `EOF` 意味着上游结束了。

让我们用 zlib's `z_streamp` 举一个实现 BIO 节点的例子:

<!-- ```haskell
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
``` -->
```haskell
compressBIO :: ZStream -> BIO V.Bytes V.Bytes
compressBIO zs = \ callback mbs ->
    case mbs of
        Just bs -> do
            -- 给 ZStream 结构体设置输入
            set_avail_in zs bs (V.length bs)
            let loop = do
                    oavail :: CUInt <- withCPtr zs $ \ ps -> do
                        -- 执行压缩操作 deflate，并查询输出 buffer 的剩余空间
                        throwZlibIfMinus_ (deflate ps (#const Z_NO_FLUSH))
                        (#peek struct z_stream_s, avail_out) ps
                    when (oavail == 0) $ do
                        -- 当输出 buffer 满时，冻结 chunk 并传递给下游 callback
                        oarr <- A.unsafeFreezeArr =<< readIORef bufRef
                        callback (Just (V.PrimVector oarr 0 bufSiz))
                        newOutBuffer           
                        loop
            loop
        _ -> ... 和上面类似，只是没有了输入，所以要给 deflate 操作传递 Z_FINISH 标志
```

<!-- # Source and Sink types

Now let's consider the following devices:

+ A data source which doesn't take any input but can be read until EOF.
+ A data sink which only performs writing without producing any meaningful result.

We can have the definitions for data `Source` and `Sink` by using `Void` from `Data.Void`: -->

# Source 和 Sink 类型

现在让我们考虑以下设备：

+ 一个 Source 类型，不接受任何输入，但是可以在遇到 EOF 之前读取。
+ 一个 Sink 类型，仅执行写入操作而不会产生任何有意义的结果。

我们可以使用 `Data.Void` 中的 `Void` 来定义 `Source` 和 `Sink` 的类型：

<!-- 
```haskell
-- Source type doesn't need input
type Source a = BIO Void a
-- Sink type doesn't produce output
type Sink a = BIO a Void
``` -->

```haskell
-- Source 类型不需要输入
type Source a = BIO Void a
-- Sink 类型不需要输出
type Sink a = BIO a Void
```

<!-- Because `Void` type doesn't have constructors, one should ignore the `Maybe Void` param when defining a `Source`. For example, a `BIO` node sourcing chunks from `BufferedInput` can be implemented like this: -->

由于 `Void` 类型没有构造函数，因此在定义 `Source` 时应该忽略 `Maybe Void` 参数。 比如，一个从 `BufferedInput` 中读取数据流的 `BIO` 节点可以这样实现：


```haskell
sourceFromBuffered :: BufferedInput -> Source V.Bytes
sourceFromBuffered i = \ k _ ->
    let loop = readBuffer i >>= \ x ->
            if V.null x then k EOF else k (Just x) >> loop
    in loop
```

<!-- For `type Sink a = BIO a Void`, the callback type is `Maybe Void -> IO ()`, which means you can only pass `EOF` to the callback, the convention here is to only call callback when EOF: -->

对于 `type Sink a = BIO a Void` ，回调类型为 `Maybe Void -> IO ()` ，这意味着你只能将 `EOF` 传递给回调。这里的约定是只有在遇到 EOF 的时候调用一次回调函数：

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
[介绍BIO](https://z.haskell.world/design/2021/04/20/introduce-BIO-a-simple-streaming-abstraction.html) 的博客中的另一个示例：

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
