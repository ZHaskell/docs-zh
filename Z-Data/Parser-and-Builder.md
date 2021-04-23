---
layout: default
parent: Z-Data
title: Parser 与 Builder
nav_order: 3
---

{::comment}
## Table of contents
{:/}
## 目录
{: .no_toc .text-delta }

1. TOC
{:toc}

# Parser 单子（Parser Monad）

{::comment}
The `Parser` from `Z.Data.Parser` is designed for high performance resumable binary parsing and simple textual parsing, such as network protocols, JSON, etc. Write a parser by using basic parsers from `Z.Data.Parser` such as `takeWhile`, `int`, etc.
{:/}
`Z.Data.Parser` 中的 `Parser` 用于高性能、可恢复的二进制解析和简单的文本解析，例如网络协议、JSON 等。可以使用 `Z.Data.Parser` 中基本的解析函数例如 `takeWhile`、`int` 等编写解析器。


```haskell
import qualified Z.Data.Parser as P
import Z.Data.ASCII 

data Date = Date { year :: Int, month :: Int, day :: Int } deriving Show

dateParser :: P.Parser Date
dateParser = do
    y <- P.int
    P.word8 HYPHEN 
    m <- P.int
    P.word8 HYPHEN 
    d <- P.int
    return $ Date y m d
```
{::comment}
`Parser` in Z works directly on `Bytes`:
{:/}
`Z.Data.Parser` 中的 `Parser` 直接作用于 `Bytes`:



```haskell
> P.parse' dateParser "2020-12-12"
Date 2020 12 12
> P.parse' dateParser "2020-JAN-12"
Left ["Z.Data.Parser.Numeric.int","Z.Data.Parser.Base.takeWhile1: no satisfied byte at [74,65,78,45,49,50]"]
> P.parse dateParser "2020-12-12, 08:00"
([44,32,48,56,58,48,48], Right (Date {year = 2020, month = 12, day = 12}))
> P.parseChunk dateParser "2020-"
Partial _
> let (P.Partial f) = P.parseChunk dateParser "2020-"
> let (P.Partial f') = f "05-05"    -- incrementally provide input
> f' ""                             -- push empty chunk to signal EOF
Success Date {year = 2020, month = 5, day = 5}
```

{::comment}
Binary protocol can use `decodePrim/decodePrimLE/decodePrimBE` with `TypeApplications` extension, let's say you want to implement a [MessagePack str format](https://github.com/msgpack/msgpack/blob/master/spec.md#str-format-family) parser:
{:/}
二进制协议可以使用带有 `TypeApplications` 扩展的 `decodePrim / decodePrimLE / decodePrimBE`，假设您要实现一个 [MessagePack str format](https://github.com/msgpack/msgpack/blob/master/spec.md#str-format-family) 解析器:

```haskell
import           Data.Bits
import           Data.Word
import qualified Z.Data.Parser as P
import qualified Z.Data.Text   as T

msgStr :: P.Parser T.Text
msgStr = do
    tag <- P.anyWord8
    case tag of
        t | t .&. 0xE0 == 0xA0 -> str (t .&. 0x1F)
        0xD9 -> str =<< P.anyWord8
        0xDA -> str =<< P.decodePrimBE @Word16
        0xDB -> str =<< P.decodePrimBE @Word32
        _    -> P.fail' "unknown tag"
  where
    str !l = do
        bs <- P.take (fromIntegral l)
        case T.validateMaybe bs of
            Just t -> return (Str t)
            _  -> P.fail' "illegal UTF8 Bytes"
```
{::comment}
Comparing to `parsec` or `megaparsec`, `Parser` in Z provides limited error reporting, and do not support using as a monad transformer. But provides an instance of `PrimMonad`, which allows some limited effects, such as mutable variables and array operations. 
{:/}

与 `parsec` 或 `megaparsec` 不同，Z中的 `Parser` 提供有限的错误报告且不支持将其用作单子转换器（Monad Transformer）。 但是提供了 `PrimMonad` 的实例，该实例允许一些有限的副作用，例如可变量和数组的操作。

{::comment}
## Auto Backtracked Alternative
{:/}
## 使用 `Alternative` 自动回溯


{::comment}
Similar to `attoparsec`, `Parser` in Z always backtrack when used with `<|>` (`Alternative` instance), that means the failed branch will not consume any input without doing anything special:
{:/}


与 `attoparsec` 相似，Z中的 Parser 与 `<|>` （ `Alternative` 实例的函数）一起使用时总是回溯，这意味着失败的分支在不做任何特殊操作的情况下不会消费任何输入：

```haskell
import Control.Applicative
...
p = fooParser <|> barParser <|> quxParser
```

{::comment}
In above code, if any parser failed, the next parser is retried from the beginning of the input. Backtracking is not always needed though, it recommended to use `peek` 
or `peekMaybe` if the syntax or protocol can be parsed as LL(1) grammer since it's faster than backtracking.
{:/}

在上面的代码中，如果任何 parser 失败，则从输入的开头重试下一个 parser。虽然并非总是需要回溯，还是建议使用 `peek`。或者在语法或协议可以被解析为 LL(1) 语法的时候使用`peekMaybe`， 因为它比回溯更快。

# Builder 单子（Builder Monad）

{::comment}
The `Builder` from `Z.Data.Builder` is the reverse process of parsing, i.e. writing Haskell data types to `Bytes`, aka *Writer* monad. The usage is very similiar to `Parser`:
{:/}

`Z.Data.Builder` 中的 `Builder` 是解析过程的逆过程，即将 Haskell 数据类型写入 `Bytes` ，也就是 *Writer* 单子。用法非常类似于 `Parser`：

```haskell
import qualified Z.Data.Builder as B
import Z.Data.ASCII 

data Date = Date { year :: Int, month :: Int, day :: Int } deriving Show

dataBuilder :: Date -> B.Builder ()
dataBuilder (Date y m d) = do
    int' y
    B.word8 HYPHEN 
    int' m
    B.word8 HYPHEN 
    int' d
  where
    int' x | x > 10    = B.int x
           | otherwise = B.word8 DIGIT_0 >> B.int x
```

{::comment}
Underhood a `Builder` records a buffer writing function, thus can be composed quickly. Use `build/buildText` to run a `Builder`, which produces `Bytes` and `Text` respectively:
{:/}

类型 `Builder` 内部记录了一个 buffer writing 函数，因此可以快速写出。使用 `build/buildText` 运行一个 `Builder` ，分别产生 `Bytes` 和 `Text` ：

```haskell
> B.build (dataBuilder $ Date 2020 11 1)
[50,48,50,48,45,49,49,45,48,49]
> B.buildText (dataBuilder $ Date 2020 11 1)
"2020-11-01"
```

{::comment}
Binary `Builder` can be constructed with `encodePrim/encodePrimLE/encodePrimBE`, let's still take [MessagePack str format](https://github.com/msgpack/msgpack/blob/master/spec.md#str-format-family) as an example:
{:/}

可以使用 `encodePrim/encodePrimLE/encodePrimBE` 来构建二进制 `Builder` ，让我们仍然采用 [MessagePack str format](https://github.com/msgpack/msgpack/blob/master/spec.md#str-format-family) 举个例子：


```haskell
import           Data.Bits
import           Data.Word
import qualified Z.Data.Builder as B
import qualified Z.Data.Text    as T
import qualified Z.Data.Vector  as V

msgStr :: T.Text -> B.Builder ()
msgStr t = do
    let bs = T.getUTF8Bytes t
    case V.length bs of
        len | len <= 31      ->  B.word8 (0xA0 .|. fromIntegral len)
            | len < 0x100    ->  B.encodePrim (0xD9 :: Word8, fromIntegral len :: Word8)
            | len < 0x10000  ->  B.encodePrim (0xDA :: Word8, BE (fromIntegral len :: Word16))
            | otherwise      ->  B.encodePrim (0xDB :: Word8, BE (fromIntegral len :: Word32))
    B.bytes bs
```

{::comment}
Note that we directly use `Unalign a, Unalign b => Unalign (a, b)` instance to write serveral primitive types in a row, The `Unalign` class provide basic reading and writing facilities to read primitive types from and to raw bytes(with unaligned offset).
{:/}


请注意，我们直接使用 `Unalign a, Unalign b => Unalign (a, b)` 实例在一行内写若干个 primitive types。 `Unalign` 类提供了基本的读写接口，可以从 raw bytes 读取 primitive types，也可以将 primitive types 写入 raw bytes。（raw bytes 的偏移量未对齐）

{::comment}
## Text formatting with `Builder`
{:/}
## 利用 `Builder` 处理文本格式化


{::comment}
Different from other standard libraries which usually provide `printf` or similar, in Z directly using `Builder` to format text is recommended:
{:/}

与通常提供 `printf` 或类似方法的其他标准库不同，建议在Z中直接使用 `Builder` 格式化文本格式：

{::comment}
```haskell
-- Similar to print("The result are %d, %d", x, y)
-- If you can ensure all Builders will write UTF-8 encoded bytes,
-- you can use unsafeBuildText to save a validation

B.unsafeBuildText $ do
    "The result are " >> B.double x >> ", " >> B.double y

-- Or use do syntax

B.unsafeBuildText $ do
    "The result are " 
    B.double x 
    ", " 
    B.double y
...

```
{:/}

```haskell
-- 与 print("The result are %d, %d", x, y) 相似的
-- 如果你确认所有Builder都将写utf-8格式的字符串
-- 你可以直接使用unsafeBuildText以减少一次校验

B.unsafeBuildText $ do
    "The result are " >> B.double x >> ", " >> B.double y

-- Or use do syntax

B.unsafeBuildText $ do
    "The result are " 
    B.double x 
    ", " 
    B.double y
...

```

{::comment}
The strength of monadic `Builder` is that you can reuse all control structure from `Control.Monad`, such as conditions, loops, etc.  `Builder ()` has an `IsString` instance which can wrap writing literals in UTF-8 encoding, with some modifications:

+ `\NUL` will be written as `\xC0\x80`.
+ `\xD800` ~ `\xDFFF` will be encoded in three bytes as normal UTF-8 codepoints.

It's safe to put an string literal inside a `unsafeBuildText` as long as you don't write `\0` or `\55296` ~ `\57343`. 
{:/}

Monadic `Builder` 的优势在于，您可以重用 `Control.Monad` 中的所有控制结构，例如条件，循环等。`Builder ()` 具有 `IsString` 实例，可以将字符串字面值写成稍作调整的UTF-8格式：

+ `\NUL` 将被写为 `\xC0\x80`。
+ `\xD800` ~ `\xDFFF` 将被编码为三个字节，作为普通的UTF-8的替代。

只要您不写 `\0` or `\55296` ~ `\57343`, 就可以在 `unsafeBuildText` 中放心的使用字符串字面值。
