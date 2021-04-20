---
layout: default
parent: Z-IO
title: Logger：日志系统
nav_order: 4
---

{::comment}
## Table of contents
{:/}
{: .no_toc .text-delta }

## 目录

1. TOC
{:toc}

{::comment}
# Logging functions
{:/}

# 日志函数

{::comment}
High-performance logging is important to all kinds of applications. In Z-IO, all you have to do is to import `Z.IO` and use the following functions:
{:/}

高性能的日志系统在各类应用程序中都举足轻重。在 Z-IO 中，它们开箱即用，只需要导入 `Z.IO` 模块就可以使用以下函数：

{::comment}
```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Z.Data.Builder as B
import Z.IO

-- logging functions all work directly in IO monad
debug, info , warning, fatal, critical :: B.Builder () -> IO ()

-- you can use B.Builder's IsString instance
debug "..."
-- together with B.Builder's Monad instance
info $ "..." >> B.int 666 >> "..."
warning $ do
    "..."
    B.int 666
    "..."
fatal "this is an important message"
critical "OMG, system is on fire"
```
{:/}

```haskell
{-# LANGUAGE OverloadedStrings #-}
import qualified Z.Data.Builder as B
import Z.IO

-- 日志函数工作在 `IO` 单子中，可以在 do 语句块内进行组合
debug, info , warning, fatal, critical :: B.Builder () -> IO ()

-- 可以使用 B.Builder 的 IsString 实例
debug "..."
-- 再与 B.Builder 的 Monad 实例加以组合
info $ "..." >> B.int 666 >> "..."
warning $ do
    "..."
    B.int 666
    "..."
fatal "this is an important message" -- 重要消息
critical "OMG, system is on fire"    -- 天吶， 系统着火了
```

{::comment}
Note that `debug/info/warning` does not trigger a log flushing, while fatal/critical always triggers a log flushing. If `debug/info/warning` logs matter to you, use `withDefaultLogger` like this:
{:/}

注意 `debug / info / warning` 操作不会触发日志刷新，而 `fatal / critical` 总是触发一次日志刷新。

```
main :: IO
main = withDefaultLogger $ do
    ...
```

{::comment}
It will add a flush after the application finishes to ensure that `debug/info/warning` logs are flushed.
{:/}

它将在应用结束后增加一次刷新，以确保 `debug / info / warning` 操作的日志被刷新。

{::comment}
# Setup Logger
{:/}

# 设置日志器

{::comment}
Z-IO's `Logger` have the following concurrent characteristics:
{:/}

Z-IO 的 `Logger` 模块有以下有助并发的特点：

{::comment}
* Logging functions are lock-free and can be used across threads.
* Logs are atomic, and the order is preserved.
* Flushing is protected by the lock, and there'll be no concurrent writing to the buffered device.
{:/}

* 日志功能是无锁（lock-free）的，可以被跨线程使用。
* 日志是原子态的，且保留顺序。
* 日志刷新受到锁的保护，不会有并发的写入缓冲设备的情况发生。

{::comment
So there is no need to worry about anything since most of the things are taken care of, just import and start to log. Functions like `debugTo/infoTo/warningTo...` that explicitly write logs to given `Logger` are provided. However, most of the time, use the default `Logger`. And, use `setDefaultLogger` to change it when the application starts. Z-IO supports writing logs to different devices with different formats:
{:/}

无需担心大部分已被 Z-IO 内部处理好的细节问题，只需要导入模块然后开始用这些日志机能构建应用程序。像 `debugTo / infoTo / warningTo...` 一类的函数可以显式地将日志写进给定的 `Logger`，但在大部分情况下会使用默认的 `Logger`。使用 `setDefaultLogger` 函数可以在程序启动时更改默认的 `Logger`。Z-IO 支持在向不同设备写入日志时使用不同的格式：

{::comment}
```haskell
-- logs can be written to any `BufferedOutput`s with `MVar` protected from concurrent access
newLogger :: LoggerConfig -> MVar BufferedOutput -> IO Logger
-- create a logger connected to stderr
newStdLogger :: LoggerConfig -> IO Logger
-- create a file based logger
newFileLogger :: LoggerConfig -> CBytes -> IO Logger

-- Change LoggerConfig's loggerFormatter field to change logging format:
-- [FATAL][2021-02-01T15:03:30+0800][interactive:31:1][thread#669]...\n
defaultFmt :: LogFormatter
-- Same with defaultFmt, but level is colored: cyan DEBUG, yellow WARNING, red FATAL and CRITICAL
defaultColoredFmt :: LogFormatter
-- {"level":"FATAL","time":"2021-02-01T15:02:19+0800","loc":"<interactive>:27:1","theadId":606,"content":"..."}\n
defaultJSONFmt :: LogFormatter
```
{:/}

```haskell
-- 日志可以被写入任意的 `BufferedOutput` 并使用 `MVar` 来维护并行访问
newLogger :: LoggerConfig -> MVar BufferedOutput -> IO Logger
-- 创建一个连接到标准错误输出设备（stderr）的日志器
newStdLogger :: LoggerConfig -> IO Logger
-- 创建一个基于文件的日志器
newFileLogger :: LoggerConfig -> CBytes -> IO Logger

-- 用于改变 `LoggerConfig` 的 `loggerFormatter` 字段以改变记录格式：
-- [FATAL][2021-02-01T15:03:30+0800][interactive:31:1][thread#669]...\n
defaultFmt :: LogFormatter
-- 用法与函数 `defaultFmt` 类似，但是日志等级被着色了：DEBUG 为青色、WARNING 黄色、 FATAL 和 CRITICAL 为红色
defaultColoredFmt :: LogFormatter
-- {"level":"FATAL","time":"2021-02-01T15:02:19+0800","loc":"<interactive>:27:1","theadId":606,"content":"..."}\n
defaultJSONFmt :: LogFormatter
```

{::comment}
Initial default loggers are connected to stderr. Use `defaultColoredFmt` if stderr is connected to a TTY device, and use `defaultFmt` otherwise. An example about setting up logger:
{:/}

默认日志器最初被连接到标准错误输出。如果标准错误输出被连接到 TTY 设备，则使用 `defaultColoredFmt` 作为配置，在其它情况使用 `defaultFmt`。下面是一个有关设置日志器的例子：

{::comment}
```haskell
main :: IO ()
main = do
    -- setup filter level to WARNING, info/debug logs will be ignored.
    -- use file based logger, and write to "app.log"
    setDefaultLogger =<< newFileLogger defaultJSONLoggerConfig
        { loggerConfigLevel = WARNING } "app.log"
    withDefaultLogger $ do
        ...
```
{:/}

```haskell
main :: IO ()
main = do
    -- 设置筛选等级为 WARNING，info 和 debug 日志由此将被忽略
    -- 使用基于文件的日志器并写入文件 `app.log`
    setDefaultLogger =<< newFileLogger defaultJSONLoggerConfig
        { loggerConfigLevel = WARNING } "app.log"
    withDefaultLogger $ do
        ...
```
