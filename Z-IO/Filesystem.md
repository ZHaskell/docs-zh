---
layout: default
parent: Z-IO
title: Filesystem：文件系统
nav_order: 1
---

{::comment}
## Table of contents
{:/}

## 目录

{: .no_toc .text-delta }

1. TOC
{:toc}

{::comment}
# Hello File
{:/}

# 文件操作

{::comment}
It's easy to use Z-IO package's filesystem module, first please import `Z.IO.Filesystem`:
{:/}

导入 `Z.IO.Filesystem` 以使用 Z-IO 的文件系统模块：

```haskell
import qualified Z.IO.FileSystem as FS
```

{::comment}
If it's OK to load a file into memory at once, you can use following:
{:/}

在需要将文件直接导入内存时，可以使用以下函数：

```haskell
readFile :: HasCallStack => CBytes -> IO Bytes
readTextFile :: HasCallStack => CBytes -> IO Text
writeFile :: HasCallStack => CBytes -> Bytes -> IO ()
writeTextFile :: HasCallStack => CBytes -> Text -> IO ()
```

{::comment}
`CBytes` is Z's file path type. `Bytes`, and `Text` are types for binary and textual content, respectively. These types are documented in [Z-Data section](https://z.haskell.world/Z-Data/). `readTextFile` and `writeTextFile` assumes UTF-8 encoding:
{:/}

此处的 `CBytes` 是 Z.Haskell 所使用的路径类型，`Bytes` 和 `Text` 则分别代表内容的二进制和文本编码。在 [Z-Data 一节](https://z.haskell.world/Z-Data/)可以查阅它们的相关文档。函数 `readTextFile` 和 `writeTextFile` 都假设读写的内容为 UTF-8 编码。

{::comment}
```haskell
> FS.writeTextFile "./test_file" "hello world!"
> FS.readFile "./test_file"
[104,101,108,108,111,32,119,111,114,108,100,33]
> FS.readTextFile "./test_file"
"hello world!"
```
{:/}

```haskell
> FS.writeTextFile "./test_file" "hello world!"
> FS.readFile "./test_file"
[104,101,108,108,111,32,119,111,114,108,100,33]
> FS.readTextFile "./test_file"
"hello world!" -- “你好，世界！”
```

{::comment}
# Resource Handling
{:/}

# 资源管理

{::comment}
Now let's see a more complicated function:
{:/}

现在让我们看一个更复杂的函数：

{::comment}
```haskell
initFile :: CBytes
         -> FileFlag        -- ^ Opening flags, e.g. 'O_CREAT' @.|.@ 'O_RDWR'
         -> FileMode        -- ^ Sets the file mode (permission and sticky bits),
                            -- but only if the file was created, see 'DEFAULT_FILE_MODE'.
         -> Resource File
```
{:/}

```haskell
initFile :: CBytes
         -> FileFlag        -- ^开启文件时传入的标志参数，例如：'O_CREAT' 和 'O_RDWR'
         -> FileMode        -- ^ 设置文件模式（权限和粘滞位）
                            -- 但只有在文件被已被创建的情况下作用，参考 'DEFAULT_FILE_MODE'
         -> Resource File
```

{::comment}
`FileFlag` and `FileMode` are bit constants controlling the file opening behavior, such as if we have read or write access or if a new file will be created when there's none. You can find more constants on [hackage docs](https://hackage.haskell.org/package/Z-IO-0.7.1.0/docs/Z-IO-FileSystem-Base.html#g:5). The interesting thing here is that `initFile` function returns a `Resource File` type instead of `IO File`. `Resource` is defined in `Z.IO.Resource` module, with following functions to use it:
{:/}

类型 `FileFlag` 和 `FileMode` 是控制打开文件时行为的位常数，如我们是否有读或写的权限、或者当没有新文件时是否会创建新文件。要查阅更多这样的常量可以参考 [Hackage 文档的本节](https://hackage.haskell.org/package/Z-IO-0.7.1.0/docs/Z-IO-FileSystem-Base.html#g:5)。特别地，函数 `initFile` 返回类型为 `Resource File` 而不是 `IO File`。类型 `Resource` 在模块 `Z.IO.Resource` 被定义，连同一些使用它的函数：

{::comment}
```haskell
withResource :: HasCallStack
             => Resource a      -- ^ resource management record
             -> (a -> IO b)     -- ^ function working on a resource
             -> IO b

withResource' :: HasCallStack
              => Resource a      -- ^ resource management record
              -> (a -> IO () -> IO b)
                    -- ^ second param is the close function for early closing
              -> IO b
```
{:/}

```haskell
withResource :: HasCallStack
             => Resource a      -- ^ 用于管理资源的记录（Record）类型
             -> (a -> IO b)     -- ^ 使用给定资源的工作函数
             -> IO b

withResource' :: HasCallStack
              => Resource a      -- ^ 用于管理资源的记录类型
              -> (a -> IO () -> IO b)
                    -- ^ 第二个参数用于在过早的关闭行为中关闭资源
              -> IO b
```

{::comment}
We simplified those two functions' type a little bit, and here is the idea: `withResource` will take care of resource opening and cleanup automatically, after you finish using it, or when exceptions happen. You only need to pass a function working on that resource. Now let's read the file created above again:
{:/}

函数 `withResource` 会自动处理好资源的开启与使用完毕或抛出异常后的清理。因此你只需要传递需要作用在资源上的工作函数。现在利用它们再读一次先前创建的文件：

{::comment}
```haskell
import           Z.IO       -- this module re-export Z.IO.Resource and other common stuff
import qualified Z.IO.FileSystem as FS

withResource (FS.initFile "./test_file" FS.O_RDWR FS.DEFAULT_FILE_MODE) $ \ file -> do
    bi <- newBufferedInput file
    printStd =<< readLine bi
```
{:/}

```haskell
import           Z.IO       -- 本模块重导出了模块 Z.IO.Resource 和其它的常用定义
import qualified Z.IO.FileSystem as FS

withResource (FS.initFile "./test_file" FS.O_RDWR FS.DEFAULT_FILE_MODE) $ \ file -> do
    bi <- newBufferedInput file
    printStd =<< readLine bi
```

{::comment}
`initFile` function doesn't open the file, and it just records how to open and close the file. Every time you want to do something with the file, use `withResource` to open(and close) it, that's all about resource handling in Z.
{:/}

函数 `initFile` 本身不会开启目标文件，它只记录下了如何去开启和关闭目标文件。要处理文件或使用 Z 中的其它资源（`Resource a`），使用 `withResource` 函数来开启并关闭它们。这是利用 Z 进行资源管理的一贯风格。

{::comment}
`Resource` has a `Monad` instance, which is useful for safely combining resources, e.g. instead of writing following code:
{:/}

类型 `Resource` 是 `Monad` 的一个实例，因此安全地组合资源的操作得到了简化。例如，比起写下面的样版代码：

```haskell
withResource initRes1 $ \ res1 ->
    withResource initRes2 $ \ res2 ->
        withResource initRes3 $ \ res3 ->
            ... res1 ... res2 ... res3
```

{::comment}
You could define a combined `Resource`:
{:/}

复用和组合性更强的写法是可以定义一个组合的 `Resource`：

```haskell
initRes123 :: Resource (Res1, Res2, Res3)
initRes123 = do
    res1 <- initRes1
    res2 <- initRes2
    res3 <- initRes3
    return (res1, res2, res3)
```

Now `withResource initRes123 $ \ (res1, res2, res3) -> ...` will first open `res1`, `res2`, `res3` in order, then close them in reverse order. You could even interleave `IO` action within `Resource` using its `MonadIO` instance:

```haskell
initRes123 :: Resource (Res1, Res2)
initRes123 = do
    res1 <- initRes1
    res2Param <- liftIO $ ... res1 ...
    res2 <- initRes2 res2Param
    return (res1, res2)
```

The lifted `IO` action will become a part of the resource opening process.

# Buffered I/O

`newBufferedInput` and `readLine` functions in the code above are from `Z.IO.Buffered` module(also re-exported from `Z.IO`). In Z-IO, many IO devices(including `File` above) are instances of `Input/Output` class:

```haskell
class Input i where
    readInput :: HasCallStack => i -> Ptr Word8 -> Int -> IO Int
class Output o where
    writeOutput :: HasCallStack => o -> Ptr Word8 -> Int -> IO ()
```

`readInput` and `writeOutput` work on pointers, which is not very convenient for direct usage. Open a `BufferedInput` or `BufferedOutput` to get auto-managed buffered I/O:

```haskell
newBufferedInput :: Input i => i -> IO BufferedInput
newBufferedOutput :: Output o => o -> IO BufferedOutput
```

There's a set of functions working on `BufferedInput/BufferedOutput` in `Z.IO.Buffered`, for example, to implement a word counter for files:

```haskell
import           Z.IO
import qualified Z.IO.FileSystem    as FS
import qualified Z.Data.Vector      as V

main :: IO ()
main = do
    -- get file path from command line
    (_:path:_) <- getArgs
    withResource (FS.initFile path FS.O_RDWR FS.DEFAULT_FILE_MODE) $ \ file -> do
        bi <- newBufferedInput file
        printStd =<< loop bi 0
  where
    loop :: BufferedInput -> Int -> IO Int
    loop input !wc = do
        -- read a single line with linefeed dropped
        line <- readLine input
        case line of
            Just line' ->
                loop input (wc + length (V.words line'))
            _ -> return wc
```

Here's a quick cheatsheet on buffered IO, `BufferedInput` first:

```haskell
-- | Request a chunk from the input device.
readBuffer :: HasCallStack => BufferedInput -> IO Bytes

-- | Push back an unconsumed chunk
unReadBuffer :: HasCallStack => Bytes -> BufferedInput -> IO ()

-- | Read exactly N bytes, throw exception if EOF reached before N bytes.
readExactly :: HasCallStack => Int -> BufferedInput -> IO Bytes

--  /----- readToMagic ----- \ /----- readToMagic -----\ ...
-- +------------------+-------+-----------------+-------+
-- |       ...        | magic |       ...       | magic | ...
-- +------------------+-------+-----------------+-------+
readToMagic :: HasCallStack => Word8 -> BufferedInput -> IO Bytes

--  /--- readLine ---\ discarded /--- readLine ---\ discarded / ...
-- +------------------+---------+------------------+---------+
-- |      ...         | \r\n/\n |       ...        | \r\n/\n | ...
-- +------------------+---------+------------------+---------+
readLine :: HasCallStack => BufferedInput -> IO (Maybe Bytes)

-- | Read all chunks from input.
readAll :: HasCallStack => BufferedInput -> IO [Bytes]
readAll' :: HasCallStack => BufferedInput -> IO Bytes

-- | See Parser & Builder under Z-Data section for the following functions.
-- | Request input using Parser
readParser :: HasCallStack => Parser a -> BufferedInput -> IO a

-- | Request input using ParseChunks, see Parser & Builder under Z-Data section.
readParseChunks :: (Print e, HasCallStack) => ParseChunks IO Bytes e a -> BufferedInput -> IO a
```

`BufferedOutput` is relatively simple:

```haskell
-- | Write a chunk into buffer.
writeBuffer :: HasCallStack => BufferedOutput -> Bytes -> IO ()
-- | Directly write Builder into output device.
writeBuilder :: HasCallStack => BufferedOutput -> Builder a -> IO ()
-- | Flush the buffer into output device.
flushBuffer :: HasCallStack => BufferedOutput -> IO ()
```

# A note on filepath

Other operations from `Z.IO.FileSystem` module, e.g., `seek`, `mkdtemp`, `rmdir`, etc., are basically mirroring the Unix system call, which should be familiar to people who come from C/C++. The type for file path in Z is `CBytes`, which is a `\NUL` terminated byte array managed on GHC heap.

We assumed that `CBytes`'s content is UTF-8 encoded though it may not always be the case, and there're some platform differences on file path handling, e.g., the separator on windows is different from Unix. To proper handle file path, use `Z.IO.FileSystem.FilePath` (which is re-exported from `Z.IO.FileSystem`), for example, instead of manually connecting file path like:

```haskell
let p = "foo" <> "/" <> "bar"
```
You should always use functions from the library

```haskell
import qualified Z.IO.FileSystem as FS

let p = "foo" `FS.join` "bar"
-- "foo" `FS.join` "../bar" will yield "bar" instead of "foo/../bar"
```
