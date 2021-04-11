---
layout: page
title: 教程
nav_order: 1
permalink: /guide
---

{::comment}
## Table of contents
{:/}

## 目录

{: .no_toc .text-delta }

1. TOC
{:toc}

{::comment}
## Requirements
{:/}

## 开始使用 Haskell

{::comment}
You need a working Haskell compiler system: GHC(>={{site.data.version.ghc_version}}), cabal-install(>={{site.data.version.cabal_version}}). There are several choices:
{:/}

Z.Haskell 是基于 GHC 与 Cabal 构建的 Haskell 项目，为了使用 Z.Haskell，需要如下工具：
+ GHC (>={{site.data.version.ghc_version}})
+ Cabal (>={{site.data.version.cabal_version}})

{::comment}
+ Use the package manager on your operating system if available:

    * Mac users can get them via [homebew](//brew.sh/): `brew install ghc cabal-install`.
    * Windows users can get them via [chocolatey](//chocolatey.org): `choco install ghc cabal`.
    * Ubuntu users are recommended to use this [ppa](//launchpad.net/~hvr/+archive/ubuntu/ghc).

+ Setup via [ghcup](https://www.haskell.org/ghcup/).

+ Download pre-built binaries([GHC](https://www.haskell.org/ghc/download.html), [cabal-install](https://www.haskell.org/cabal/download.html)) and install manually.
{:/}

虽然可以使用系统包管理器来安装这些工具（请参考你的操作系统 / 发行版教程），使用 GHCup 和针对中国地区的优化配置能获得不错的体验。GHCup 是安装 GHC、Cabal 和 Haskell 语言服务器的跨平台、多版本共存解决方案。

+ 安装 GHCup

[官方](https://www.haskell.org/ghcup/)安装脚本：

Linux、macOS（搭载 Intel 芯片）、FreeBSD 或适用于 Linux 的 Windows 子系统

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

macOS（搭载 Apple Silicon 芯片）

```sh
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | arch -x86_64 /bin/bash
```

中国用户可以使用[上海交通大学 SJTUG](https://mirror.sjtu.edu.cn/docs/ghcup)的 GHCup 安装镜像，见如下：

Linux、macOS（搭载 Intel 芯片）、FreeBSD 或适用于 Linux 的 Windows 子系统

```sh
curl --proto '=https' --tlsv1.2 -LsSf https://mirror.sjtu.edu.cn/ghcup/script/install.sh | sh
```

macOS（搭载 Apple Silicon 芯片）

```sh
curl --proto '=https' --tlsv1.2 -LsSf https://mirror.sjtu.edu.cn/ghcup/script/install.sh | arch -x86_64 /bin/bash
```

Windows 用户请参考在 Windows 上安装 Haskell 平台的[说明](https://www.haskell.org/platform/#windows)。

ArchLinux 用户可以使用 [ArchLinux CN 软件仓库](https://mirrors.bfsu.edu.cn/help/archlinuxcn/)中的 `archlinuxcn/ghcup-hs-bin`。

```sh
sudo pacman -Syu ghcup-hs-bin
```

+ 杂项

也可以获取预构建的二进制包后手动安装 [GHC](https://www.haskell.org/ghc/download.html) 和 [Cabal](https://www.haskell.org/cabal/download.html)。

{::comment}
## Installation
{:/}

## 安装 Z.Haskell

中国地区用户可能需要配置 Hackage / Stackage 来获取更好的使用体验。

+ [如果你使用 Cabal](https://mirrors.bfsu.edu.cn/help/hackage/)
+ [如果你使用 Stack](https://mirrors.bfsu.edu.cn/help/stackage/)

Stack 在运行过程中可能会需要访问一些不便于连接的地址。

{::comment}
To use [Z-Data](https://hackage.haskell.org/package/Z-Data) package as an example. Add the following lines to your project's cabal file:
{:/}

以使用 [Z-Data](https://hackage.haskell.org/package/Z-Data) 包为例，只需要将下面的内容添加到项目的 Cabal 配置文件：

```
...
    build-depends:          Z-Data == {{site.data.version.z_version}}.*
```

{::comment}
Now run `cabal build` within your project directory, cabal should be able to download [Z-Data](https://hackage.haskell.org/package/Z-Data) dependency automatically. Let's write a simple TCP echo server just for teaching purpose:
{:/}

接下来在项目路径下运行 `cabal build`，Cabal 将会自动下载构建 Z-Data 的依赖。出于教学目的，接下来利用 Z-Data 与 Z-IO 创建一个简单的 TCP 回音服务器：

{::comment}
1. Initialize a project with `cabal`.

    ```
    mkdir tcp-echo
    cd tcp-echo
    cabal init -i
    ```

    `cabal` will ask you some questions about your project and create a `tcp-echo.cabal` file.
{:/}

1. 使用 `cabal` 命令初始化项目

    ```
    mkdir tcp-echo
    cd tcp-echo
    cabal init -i
    ```

    `cabal` 将需要获取一些与项目有关的信息来初始化 `tcp-echo.cabal` 文件。

{::comment}
2. Add dependencies.

    Now open the `tcp-echo.cabal` file with a text editor, and add the following lines under the `executable` section:

    ```
    ...
        build-depends:          Z-IO  == {{site.data.version.z_version}}.*
    ```
{:/}

2. 编辑 Cabal 项目配置文件以添加依赖

    用一个文本编辑器打开 `tcp-echo.cabal` 文件，并将以下内容添加到 `executable` 一节。

    ```
    ...
        build-depends:          Z-IO  == {{site.data.version.z_version}}.*
    ```

{::comment}
3. Edit code.

    Open `src/Main.hs` and add a simple echo TCP server:

    ```haskell
    import Control.Monad
    import Z.IO
    import Z.IO.Network

    main :: IO ()
    main = do
        let addr = SocketAddrIPv4 ipv4Loopback 8080
        startTCPServer defaultTCPServerConfig{ tcpListenAddr = addr } $ \ tcp -> do
            i <- newBufferedInput tcp
            o <- newBufferedOutput tcp
            forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
    ```
{:/}

3. 编辑代码

    编辑 `src/Main.hs` 文件来创建一个简单的 TCP 回音服务器：

    ```haskell
    import Control.Monad
    import Z.IO
    import Z.IO.Network

    main :: IO ()
    main = do
        let addr = SocketAddrIPv4 ipv4Loopback 8080
        startTCPServer defaultTCPServerConfig{ tcpListenAddr = addr } $ \ tcp -> do
            i <- newBufferedInput tcp
            o <- newBufferedOutput tcp
            forever $ readBuffer i >>= writeBuffer o >> flushBuffer o
    ```

{::comment}
4. Build!

    Ensure that you have run `cabal update` to get the latest package list. `cabal build` will start to download dependencies and build your project. You may see output like this:

    ```
    Resolving dependencies...
    Build profile: -w ghc-{{site.data.version.ghc_version}} -O1
    In order, the following will be built (use -v for more details):
     - Z-IO-{{site.data.version.z_version}}.0.0 (lib) (requires download & build)
     - tcp-echo-0.1.0.0 (exe:tcp-echo) (first run)
    Downloaded   Z-IO-{{site.data.version.z_version}}.0.0
    Starting     Z-IO-{{site.data.version.z_version}}.0.0 (lib)
    Building     Z-IO-{{site.data.version.z_version}}.0.0 (lib)
    ...
    ```

    It may take a while to build for the first time because cabal needs to download and build all the dependencies. Build afterward will be faster since dependencies are cached. For reference, on an intel 4th gen core, it takes around 10mins to compile Z-Data and Z-IO. So sit back and relax, or go for a coffee.
{:/}

4. 构建运行

    确保在这之前运行过 `cabal update` 命令以获取最新的软件包列表。运行 `cabal build` 命令将会在自动下载构建依赖后构建这个项目。过程中可能看到如下输出：

    ```
    Resolving dependencies...
    Build profile: -w ghc-{{site.data.version.ghc_version}} -O1
    In order, the following will be built (use -v for more details):
     - Z-IO-{{site.data.version.z_version}}.0.0 (lib) (requires download & build)
     - tcp-echo-0.1.0.0 (exe:tcp-echo) (first run)
    Downloaded   Z-IO-{{site.data.version.z_version}}.0.0
    Starting     Z-IO-{{site.data.version.z_version}}.0.0 (lib)
    Building     Z-IO-{{site.data.version.z_version}}.0.0 (lib)
    ...
    ```

    第一次使用时 Cabal 需要一些时间来下载并编译所有的依赖，本次构建的缓存将会提高之后构建的速度。作为参考，在一台搭载 Intel 四代处理器的笔记本上构建 Z-Data 和 Z-IO 需要十分钟。放轻松，喝杯茶。

{::comment}
After building complete, you can use `cabal run` to run your echo server and `nc 0.0.0.0 8080` to test it. That's it, happy hacking!
{:/}

构建完成后，可以使用 `cabal run` 来运行这个回音服务器，并用 `nc 0.0.0.0 8080` 来测试。教程结束，玩得愉快！
