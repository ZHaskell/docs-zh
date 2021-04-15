---
layout: default
parent: Z-IO
title: Network：网络
nav_order: 2
---

{::comment}
## Table of contents
{:/}

## 目录
{: .no_toc .text-delta }

1. TOC
{:toc}

{::comment}
# Client and server
{:/}

# 客户机（Client）与服务器（Server）

{::comment}
The Network is all about sending and receiving data. Using Z-IO's Network is straightforward:
{:/}

网络（Network）模块围绕发送与接收数据构建。下面的例子展示了一些使得 Z-IO 简明易用的特性：

{::comment}
```haskell
{-# LANGUAGE OverloadedStrings #-}
import Z.IO
import Z.IO.Network
import Z.Data.Text as T

main :: IO ()
main = do
    -- use getAddrInfo to perform DNS resolution
    addr:_ <- getAddrInfo Nothing "www.bing.com" "http"
    -- use initTCPClient to initialize a TCP client
    withResource (initTCPClient defaultTCPClientConfig{
            tcpRemoteAddr = addrAddress addr}) $ \ tcp -> do
        -- use BufferedInput/BufferedOutput facility to read from/write to tcp socket
        i <- newBufferedInput tcp
        o <- newBufferedOutput tcp
        writeBuffer o "GET http://www.bing.com HTTP/1.1\r\nHost: www.bing.com\r\n\r\n"
        flushBuffer o
        readBuffer i >>= pure . T.validate

    -- use startTCPServer to start serving in TCP protocol
    startTCPServer defaultTCPServerConfig{
        tcpListenAddr = SocketAddrIPv4 ipv4Loopback 8080} $ \ tcp -> do
            o <- newBufferedOutput tcp
            writeBuffer o "hello world" >> flushBuffer o
```
{:/}

```haskell
{-# LANGUAGE OverloadedStrings #-}
import Z.IO
import Z.IO.Network
import Z.Data.Text as T

main :: IO ()
main = do
    -- 使用 getAddrInfo 函数解析 DNS。
    addr:_ <- getAddrInfo Nothing "www.bing.com" "http"
    -- 使用 initTCPClient 函数初始化一个 TCP 客户机。
    withResource (initTCPClient defaultTCPClientConfig{
            tcpRemoteAddr = addrAddress addr}) $ \ tcp -> do
        -- 使用 BufferedInput / BufferedOutput 及其相关函数以从 TCP 套接字（Socket）中读取或写入内容。
        i <- newBufferedInput tcp
        o <- newBufferedOutput tcp
        writeBuffer o "GET http://www.bing.com HTTP/1.1\r\nHost: www.bing.com\r\n\r\n"
        flushBuffer o
        readBuffer i >>= pure . T.validate

    -- 使用 startTCPServer 函数启动 TCP 服务。
    startTCPServer defaultTCPServerConfig{
        tcpListenAddr = SocketAddrIPv4 ipv4Loopback 8080} $ \ tcp -> do
            o <- newBufferedOutput tcp
            writeBuffer o "hello world" >> flushBuffer o
```

{::comment}
Z.Haskell provide several network capabilities:
{:/}

Z.Haskell 提供了一些网络工具：

{::comment}
+ `Z.IO.Network.IPC` provides the stream channel for inter-process communication based on domain socket(Unix) or named pipe(Windows).
+ `Z.IO.Network.TCP` provides the stream channel for remote communication based on TCP socket.
+ `Z.IO.Network.UDP` provides the message channel on top of the UDP socket.
+ A TLS implementation based on [botan](https://botan.randombit.net/) is under development.
{:/}

+ `Z.IO.Network.IPC` 为基于域套接字（Unix）或命名管道（Windows）的进程间通信提供流通道（Stream Channel）。
+ `Z.IO.Network.TCP` 提供基于 TCP 套接字的远程通信流通道。
+ `Z.IO.Network.UDP` 在 UDP 套接字之上提供消息通道。
+ 一个基于 [botan](https://botan.randombit.net/) 的 TLS 实现正处于开发阶段。

{::comment}
Let's take TCP module as an example. Lots of low-level socket details(`bind`, `listen`, `accept`, etc.) are hidden, with two high-level operations left:
{:/}

以 TCP 模块为例。不少于套接字相关的底层细节都被隐藏起来，如 `bind`、`listen`、`accept` 等，而是提供两个高阶操作：

{::comment}
```haskell
-- | Connect to a TCP target
initTCPClient :: HasCallStack => TCPClientConfig -> Resource UVStream
-- | Start a TCP server
startTCPServer :: HasCallStack
               => TCPServerConfig
               -> (UVStream -> IO ())
               -- ^ worker which will get an accepted TCP stream
               -- and run in a seperated Haskell thread
               -> IO
```
{:/}

```haskell
-- | 连接至 TCP 目标。
initTCPClient :: HasCallStack => TCPClientConfig -> Resource UVStream
-- | 启动 TCP 服务器。
startTCPServer :: HasCallStack
               => TCPServerConfig
               -> (UVStream -> IO ())
               -- ^ 工作函数将接受一个 TCP 流，
               -- 并运行于一个独立的 Haskell 线程。
               -> IO
```

{::comment}
# Send/receive packet
{:/}

# 发送和接收数据包

{::comment}
The `UVStream` type implements the `Input/Output` class from `Z.IO.Buffered`, so that you can reuse all the buffered read/write API. For example, let's say you have designed a simple framed message protocol:
{:/}

由于 `UVStream` 类型是 `Z.IO.Buffered` 模块中 `Input` 和 `Output` 类的实例，因此可以复用所有与带缓冲读写（Buffered read / write）相关的 API。假设已经设计了一个简单的帧消息协议（Framed Message Protocol）：

{::comment}
```haskell
import Data.Word
import qualified Z.Data.Vector as V

--   uint8 message type  uint16 payload length  message payload
--  +------------------+----------------------+------------------
--  |       0xXX       |   0xXXXX(big endian) |       ...
--  +------------------+----------------------+------------------

data Message = Message { msgTyp :: Word8, msgPayload :: V.Bytes }
```
{:/}


```haskell
import Data.Word
import qualified Z.Data.Vector as V

--   uint8 消息类型     uint16 有效载荷长度    信息有效载荷
--  +------------------+----------------------+------------------
--  |       0xXX       |   0xXXXX（大端）     |       ...
--  +------------------+----------------------+------------------

data Message = Message { msgTyp :: Word8, msgPayload :: V.Bytes }
```


{::comment}
You can manually decode message frames like this:
{:/}

既可以以如下方式手动解码消息帧：

{::comment}
```haskell
-- import bit operations
import Data.Bits    (unsafeShiftL, (.|.))
import Z.IO

readMessage :: HasCallStack => BufferedInput -> IO Message
readMessage bi = do
    msg_typ <- readExactly buffered_i 1
    payload_len_h <- readExactly buffered_i 1
    payload_len_l <- readExactly buffered_i 1
    let payload_len =
        (fromIntegral payload_len_h) `unsafeShiftL` 8
            .|. (fromIntegral payload_len_l)
    payload <- readExactly payload_len
    return (Message msg_typ payload)
```
{:/}

```haskell
-- 导入位运算相关操作。
import Data.Bits    (unsafeShiftL, (.|.))
import Z.IO

readMessage :: HasCallStack => BufferedInput -> IO Message
readMessage bi = do
    msg_typ <- readExactly buffered_i 1
    payload_len_h <- readExactly buffered_i 1
    payload_len_l <- readExactly buffered_i 1
    let payload_len =
        (fromIntegral payload_len_h) `unsafeShiftL` 8
            .|. (fromIntegral payload_len_l)
    payload <- readExactly payload_len
    return (Message msg_typ payload)
```

{::comment}
Or you can use `Parser` from `Z.Data.Parser` module:
{:/}

也可以使用 `Z.Data.Parser` 模块中的 `Parser` 相关机能：

```haskell
{-# LANGUAGE TypeApplications #-}
import qualified Z.Data.Parser as P
import Data.Word
import Z.IO

parseMessage :: P.Parser Message
parseMessage = do
    msg_type <- P.decodePrim @Word8
    payload_len <- P.decodePrimBE @Word16
    payload <- P.take (fromIntegral payload_len)
    return (Message msg_typ payload)

readMessage :: HasCallStack => BufferedInput -> IO Message
readMessage = readParser parseMessage
```

{::comment}
`readParser` will run `Parser` once a time, parse `Message` out of the buffer, and waiting for input automatically. To write a `Message` to the TCP socket is similar:
{:/}

函数 `readParser` 将恰好运行一次给定的 `Parser`，自动解析流出缓冲区的消息 `Message` 并等待输入。将消息 `Message` 写入到给定的 TCP 套接字做法类似：

{::comment}
```haskell
import qualified Z.Data.Builder as B
import qualified Z.Data.Vector as V
import Z.IO

writeMessage :: HasCallStack => BufferedOutput -> Message -> IO ()
writeMessage bo (Message msg_typ payload) = do
    -- use Builder monad to compose buffer writing functions
    writeBuilder bo $ do
        B.encodePrim msg_typ
        B.encodePrimBE (V.length payload)
        B.bytes payload
    -- you may want to add a flush after each message has been written
    -- or leave flush to the caller
    -- flushBuffer bo
```
{:/}

```haskell
import qualified Z.Data.Builder as B
import qualified Z.Data.Vector as V
import Z.IO

writeMessage :: HasCallStack => BufferedOutput -> Message -> IO ()
writeMessage bo (Message msg_typ payload) = do
    -- 使用 Builder 单子组合写入函数。
    writeBuilder bo $ do
        B.encodePrim msg_typ
        B.encodePrimBE (V.length payload)
        B.bytes payload
    -- 可以在每次写入消息后刷新缓冲区，或留给调用者来做。
    -- 这需要使用 flushBuffer bo。
```

{::comment}
Z.Haskell provides many tools to deal with the streaming nature of TCP protocol (and many other streaming devices such as IPC and Files). In the next section, we will introduce the `BIO`, a more high-level streaming API.
{:/}

Z.Haskell 提供了许多工具以处理 TCP 的流性质，连同许多其它流设备例如 IPC 和文件等。在下一节中我们将介绍一个更高阶的流 API，`BIO`。

# UDP

{::comment}
UDP is different from IPC or TCP in that it's a message protocol rather than a streaming one. There are no `Input/Output` instances for the `UDP` type. Instead, Z-IO provides message reading & writing functions for UDP directly:
{:/}

与 IPC 或 TCP 不同的是，UDP 是一个消息协议，而不是一个流协议。类型 `UDP` 不是 `Input` 或 `Output` 类的实例，因此 Z-IO 为读写 UDP 消息提供了一些函数：

{::comment}
```haskell
-- | Initialize a UDP socket.
initUDP :: UDPConfig -> Resource UDP
-- | Send a UDP message to target address.
sendUDP :: HasCallStack => UDP -> SocketAddr -> V.Bytes -> IO ()
-- | Receive messages from UDP socket, return source address if available, and a `Bool`
-- to indicate if the message is partial (larger than receive buffer size).
recvUDP :: HasCallStack => UDPRecvConfig -> UDP -> IO [(Maybe SocketAddr, Bool, V.Bytes)]
-- | Receive UDP messages within a loop
recvUDPLoop :: HasCallStack
            => UDPRecvConfig
            -> UDP
            -> ((Maybe SocketAddr, Bool, V.Bytes) -> IO a)
            -> IO ()
```
{:/}

```haskell
-- | 初始化一个 UDP 套接字。
initUDP :: UDPConfig -> Resource UDP
-- | 向目标地址发送一条 UDP 消息。
sendUDP :: HasCallStack => UDP -> SocketAddr -> V.Bytes -> IO ()
-- | 从 UDP 套接字中获取消息，在可用时返回消息源地址。
-- 返回值中的 `Bool` 用于指明接收的消息是否是不完全的，即消息大小大于接收缓冲区的大小。
recvUDP :: HasCallStack => UDPRecvConfig -> UDP -> IO [(Maybe SocketAddr, Bool, V.Bytes)]
-- | 循环接收 UDP 消息。
recvUDPLoop :: HasCallStack
            => UDPRecvConfig
            -> UDP
            -> ((Maybe SocketAddr, Bool, V.Bytes) -> IO a)
            -> IO ()
```

{::comment}
Loop receiving(`recvUDPLoop`) can be faster since it can reuse the receiving buffer internally. Unlike the TCP server above, the UDP worker function is called on the current Haskell thread instead of a forked one. If you have heavy computations within the worker function, consider using `forkBa` from `Z.IO.UV.Manager` (a function similar to `forkIO`, but with active thread balancing).
{:/}

由于 `recvUDPLoop` 函数在其内部复用接收缓冲区，使用它循环接收消息比手动写循环快。与上述的 TCP 服务器不同的是，UDP 的工作函数在当前的 Haskell 线程被调用，而不是分支出的一个。因此如果在一个工作函数中有大量的计算任务，可以使用 `Z.IO.UV.Manager` 模块中的 `forkBa` 函数。它类似于 `forkIO`，但有着积极的线程平衡策略。
