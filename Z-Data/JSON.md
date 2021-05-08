---
layout: default
parent: Z-Data
title: JSON
nav_order: 4
---

{::comment}
## Table of contents
{:/}

## 目录

{: .no_toc .text-delta }

1. TOC
{:toc}

{::comment}
Using `Z.Data.JSON` module to get human readable serialization/deserialization. The easiest way to use the library is to define target data type, deriving
`Generic` and `JSON` instances, which provides:
{:/}

可以使用 `Z.Data.JSON` 模块以获得具有可读性的序列化 / 反序列化结果。要使用函数库中的相关机能，最简单的方法是定义好目标数据类型，派生出 `Generic` 与 `JSON` 实例，可以利用它们来处理如下：

{::comment}
* `fromValue` to convert `Value` to Haskell values.
* `toValue` to convert Haskell values to `Value`.
* `encodeJSON` to directly write Haskell value into JSON bytes.
{:/}

* 使用 `fromValue` 函数将类型为 `Value` 的元素转换为对应的 Haskell 值
* 使用 `toValue` 函数将 Haskell 值转换为对应的类型为 `Value` 的元素
* 使用 `encodeJSON` 将 Haskell 值转换为对应的 JSON 字节表示
{::comment}
```haskell
class JSON a where
  ...
  toValue :: a -> Value
  fromValue :: Value -> Converter a
  encodeJSON :: a -> B.Builder () -- `Z.Data.Builder` as `B`
  ...
```
{:/}
```haskell
class JSON a where
  ...
  toValue :: a -> Value
  fromValue :: Value -> Converter a
  encodeJSON :: a -> B.Builder () -- 将 `Z.Data.Builder` 以 `B` 导入
  ...
```

{::comment}
For example,
{:/}

一个使用用例是：

```haskell
{-# LANGUAGE DeriveGeneric, DeriveAnyClass, DerivingStrategies #-}

import GHC.Generics (Generic)
import qualified Z.Data.Builder as Builder
import qualified Z.Data.JSON as JSON
import qualified Z.Data.Text as T

data Person = Person {name :: T.Text, age :: Int}
    deriving (Show, Generic)
    deriving anyclass (JSON.JSON)
```

{::comment}
We can now encode & decode JSON like this:
{:/}

可以以如下方式进行 JSON 的编码与解码：

```haskell
> JSON.toValue (Person{ name="Alice", age=16 })
Object [("name",String "Alice"),("age",Number 16.0)]
> JSON.encode (Person{ name="Alice", age=16 })
[123,34,110,97,109,101,34,58,34,65,108,105,99,101,34,44,34,97,103,101,34,58,49,54,125]
> JSON.encodeText (Person{ name="Alice", age=16 })
"{\"age\":16,\"name\":\"Alice\"}"
> JSON.decodeText' "{\"age\":16,\"name\":\"Alice\"}" :: Either JSON.DecodeError Person
Right (Person {age = 16, name = "Alice"})
```

{::comment}
The `Generic` based instances convert Haskell data with following rules:
{:/}

与 `Z.Data.JSON` 一同使用时，作为 `Generic` 实例的 Haskell 数据类型满足如下规则：

{::comment}
* Constructors without payloads are encoded as JSON String, `data T = A | B` are encoded as `"A"` or `"B"`.
* Single constructor are ingored if there're payloads, `data T = T ...`,  `T` is ingored:
```haskell
data Singl = Singl Int String
  deriving (Generic)
  deriving anyclass (JSON)
-- Singl 42 "42"
-- [42,\"42\"]
```
* Records are encoded as JSON object. `data T = T{k1 :: .., k2 :: ..}` are encoded as `{"k1":...,"k2":...}`.
* Plain product are encoded as JSON array. `data T = T t1 t2` are encoded as "[x1,x2]".
* Single field plain product are encoded as it is, i.e. `data T = T t` are encoded as "t" just like its payload.
* Multiple constructors are convert to single key JSON object if there're payloads:
* Records are encoded as JSON object like above. `data T = A | B {k1 :: .., k2 :: ..}` are encoded as
    `{"B":{"k1":...,"k2":...}}` in `B .. ..` case, or `"A"` in `A` case.
* Plain product are similar to above, wrappered by an outer single-key object layer marking which constructor.
{:/}

* 没有载荷（零元）的构造器被直接编码为 JSON 字符串，例如 `data T = A | B` 被编码为 `"A"` or `"B"`。
* 单构造器的类型在编码时会省略构造器本身（构造器有载荷）：
```haskell
data Singl = Singl Int String
  deriving (Generic)
  deriving anyclass (JSON)
-- Singl 42 "42"
-- [42,\"42\"]
```
* 记录（Record）被编码为一个 JSON 对象，例如 `data T = T{k1 :: .., k2 :: ..}` 被编码为 `{"k1":...,"k2":...}`。
* 普通的积类型被编码成 JSON 数组，如 `data T = T t1 t2` 被编码成 `"[x1,x2]"`。
* 单字段的积类型将直接编码载荷，如  `data T = T t` 直接被编码为 `"x"`。
* 如果有有效载荷，多个构造器将转换为单键 JSON 对象。
* 如上情况可以组合，如 `data T = A | B {k1 :: .., k2 :: ..}` 会被编码成 `"A"` 或 `{"B":{"k1":...,"k2":...}}`，取决于元素本身的构造。
* 普通的积类型也适用于如上，在外层以一个单键对象包装以表明元素本身的构造。

These rules apply to user defined ADTs, but some built-in instances have different behaviours, namely:

* `Maybe a` are encoded as JSON `null` in `Nothing` case, or directly encoded to its payload in `Just` case.
* `[a]` are encoded to JSON array, `[Char]` are encoded into JSON string.
* `NonEmpty`, `Vector`, `PrimVector`, `HashSet`, `FlatSet`, `FlatIntSet` are also encoded to JSON array.
* `Bytes` are encoded into JSON text using base64 encoding.
* `HashMap`, `FlatMap`, `FlatIntMap` are encoded to JSON object.

## Custom Settings

There're some modifying options if you providing a custom `Settings`, which
allow you to modify field name or constructor name, but please *DO NOT*
produce control characters during your modification, since we assume field
labels and constructor name won't contain them, thus we can save an extra
escaping pass. To use custom `Settings` just write:

```haskell
data T = T {fooT :: Int, barT :: [Int]} deriving Generic
instance JSON.JSON T where
    -- You can omit following definitions if you don't need to change settings
    toValue = JSON.gToValue JSON.defaultSettings{ JSON.fieldFmt = JSON.snakeCase } . from
    encodeJSON = JSON.gEncodeJSON JSON.defaultSettings{ JSON.fieldFmt = JSON.snakeCase } . from
```

```haskell
> JSON.toValue (T 0 [1,2,3])
Object [("foo_t",Number 0.0),("bar_t",Array [Number 1.0,Number 2.0,Number 3.0])]
```

## Manually Writing Instances

You can write `JSON` instances by hand if the `Generic` based one doesn't suit you.
Here is an example similar to aeson's.

```haskell
import qualified Z.Data.Text          as T
import qualified Z.Data.Vector        as V
import qualified Z.Data.Builder       as B
import qualified Z.Data.JSON          as JSON
import           Z.Data.JSON          ((.:), (.=), (.!), JSON(..))

data Person = Person { name :: T.Text , age  :: Int } deriving Show

instance JSON Person where
    fromValue = JSON.withFlatMapR "Person" $ \ v -> Person
                    <$> v .: "name"
                    <*> v .: "age"

    toValue (Person n a) = JSON.object ["name" .= n, "age" .= a]

    encodeJSON (Person n a) = JSON.object' $ ("name" .! n <> "age" .! a)
```

```haskell
> toValue (Person "Joe" 12)
Object [("name",String "Joe"),("age",Number 12.0)]
> JSON.convert' `Person . JSON.Object $ V.pack [("name",JSON.String "Joe"),("age",JSON.Number 12.0)]
Right (Person {name = "Joe", age = 12})
> JSON.encodeText (Person "Joe" 12)
"{"name":"Joe","age":12}"
```

The `Value` type is different from aeson's one in that we use `Vector (Text, Value)` to represent JSON objects, thus
we can choose different strategies on key duplication, the lookup map type, etc. so instead of a single `withObject`,
we provide `withHashMap`, `withHashMapR`, `withFlatMap` and `withFlatMapR` which use different lookup map type, and different key order piority. Most of time `FlatMap` is faster than `HashMap` since we only use the lookup map once, the cost of constructing a `HashMap` is higher. If you want to directly working on key-values, `withKeyValues` provide key-values vector access.

There're some useful tools to help write encoding code in `Z.Data.JSON.Builder` module, such as JSON string escaping tool, etc. If you don't particularly care for fast encoding, you can also use `toValue` together with value builder, the overhead is usually very small.
