---
layout: default
parent: Z-Data
title: Array：数组
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
# Array in Haskell
{:/}

# Haskell中的数组

{::comment}
Unlike the ubiquitous linked list type `[a]`. In Haskell arrays doesn't have any built-in syntax support, or any other special compiler support expects some built-in primitive functions, which can be found in [ghc-prim](http://hackage.haskell.org/package/ghc-prim/docs/GHC-Prim.html):
{:/}
与普遍存在的链表类型`[a]`不同。在Haskell中，没有任何内置语法支持，或者任何其他特殊的编译器支持数组,除了一些内置的原生函数，可以在[ghc-prim](http://hackage.haskell.org/package /ghc-prim/docs/GHC-Prim.html)中找到

```haskell
newArray# :: Int# -> a -> State# s -> (# State# s, MutableArray# s a #)
readArray# :: MutableArray# s a -> Int# -> State# s -> (# State# s, a #)
writeArray# :: MutableArray# s a -> Int# -> a -> State# s -> State# s
newByteArray# :: Int# -> State# s -> (# State# s, MutableByteArray# s #)
indexInt8Array# :: ByteArray# -> Int# -> Int#
indexInt16Array# :: ByteArray# -> Int# -> Int#
...
```
{::comment}
It's hard to directly use those functions because they directly manipulate `State#` token, and they distinguish different array types: boxed `Array#`, `ByteArray#`, etc. The `#` after those types imply they are special primitive types, which will be discussed later.
{:/}
我们很难直接使用这些函数，因为它们直接操作`State#`令牌，并且它们区分不同的数组类型：如盒装的(boxed)`Array#`，`ByteArray#`等。这些类型后的`#`表示它们是特殊的原始类型，我们将稍后将进行讨论。

{::comment}
In [Z-Data](https://hackage.haskell.org/package/Z-Data)，we provide type wrappers and typeclass to unified array operations:
{:/}
在[Z-Data](https://hackage.haskell.org/package/Z-Data)中，我们为统一数组操作提供了类型包装器和类型类：


{::comment}
```haskell
class Arr (arr :: * -> * ) a where
    -- | Mutable version of this array type.
    type MArr arr = (mar :: * -> * -> *) | mar -> arr
    -- | Make a new array with given size.
    newArr :: (PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)
    -- | Make a new array and fill it with an initial value.
    newArrWith :: (PrimMonad m, PrimState m ~ s) => Int -> a -> m (marr s a)
    -- | Index mutable array in a primitive monad.
    readArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m a
    -- | Write mutable array in a primitive monad.
    writeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> a -> m ()
    -- | Fill mutable array with a given value.
    setArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> a -> m ()
    -- | Index immutable array, which is a pure operation,
    indexArr :: arr a -> Int -> a
    -- | Index immutable array in a primitive monad, this helps in situations that
    -- you want your indexing result is not a thunk referencing whole array.
    indexArrM :: (Monad m) => arr a -> Int -> m a
    -- | Safely freeze mutable array by make a immutable copy of its slice.
    freezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (arr a)
    -- | Safely thaw immutable array by make a mutable copy of its slice.
    thawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> Int -> Int -> m (marr s a)
    -- | In place freeze a mutable array, the original mutable array can not be used
    -- anymore.
    unsafeFreezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m (arr a)
    -- | In place thaw a immutable array, the original immutable array can not be used
    -- anymore.
    unsafeThawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> m (marr s a)
    -- | Copy a slice of immutable array to mutable array at given offset.
    copyArr ::  (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> arr a -> Int -> Int -> m ()
    -- | Copy a slice of mutable array to mutable array at given offset.
    -- The two mutable arrays shall no be the same one.
    copyMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
    -- | Copy a slice of mutable array to mutable array at given offset.
    -- The two mutable arrays may be the same one.
    moveArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
    -- | Create immutable copy.
    cloneArr :: arr a -> Int -> Int -> arr a
    -- | Create mutable copy.
    cloneMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (marr s a)
    -- | Resize mutable array to given size.
    resizeMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m (marr s a)
    -- | Shrink mutable array to given size. This operation only works on primitive arrays.
    -- For boxed array, this is a no-op, e.g. 'sizeOfMutableArr' will not change.
    shrinkMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m ()
    -- | Is two mutable array are reference equal.
    sameMutableArr :: marr s a -> marr s a -> Bool
    -- | Size of immutable array.
    sizeofArr :: arr a -> Int
    -- | Size of mutable array.
    sizeofMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m Int
    -- | Is two immutable array are referencing the same one.
    sameArr :: arr a -> arr a -> Bool
```
{:/}
```haskell
class Arr (arr :: * -> * ) a where
    -- | 可变版本的数组类型.
    type MArr arr = (mar :: * -> * -> *) | mar -> arr
    -- | 创建指定大小的数组.
    newArr :: (PrimMonad m, PrimState m ~ s) => Int -> m (marr s a)
    -- | 创建数组并为其中的元素填补指定值.
    newArrWith :: (PrimMonad m, PrimState m ~ s) => Int -> a -> m (marr s a)
    -- | 访问在原生monad中的数组.
    readArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m a
    -- | 写入原生monad中的数组.
    writeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> a -> m ()
    -- | 用指定值填充可变数组值.
    setArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> a -> m ()
    -- | 访问不可变数组 这是一个纯函数
    indexArr :: arr a -> Int -> a
    -- | 访问原生monad中的不可变数组,当你想要访问你的索引元素不是一个引用整个数组的thunk时很有用。
    indexArrM :: (Monad m) => arr a -> Int -> m a
    -- | 通过创建一个可变数组切片的不可变数组拷贝，以安全地冻结可变数组.
    freezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (arr a)
    -- | 通过创建一个不可变数组切片的可变数组拷贝，以安全地解冻不可变数组.
    thawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> Int -> Int -> m (marr s a)
    -- | 原地冻结数组，原来的可变数组将无法继续使用.
    unsafeFreezeArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m (arr a)
    -- | 原地解冻数组，原来的不可变数组将无法继续使用.
    unsafeThawArr :: (PrimMonad m, PrimState m ~ s) => arr a -> m (marr s a)
    -- | 拷贝一个不可变数组的切片到一个可变数组的指定偏移位置.
    copyArr ::  (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> arr a -> Int -> Int -> m ()
    -- | 拷贝一个可变数组的切片到一个可变数组的指定偏移位置.
    -- 这两个数组不能是同一个.
    copyMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
    -- | | 拷贝一个可变数组的切片到一个可变数组的指定偏移位置.
    -- 这两个数组可以是同一个.
    moveArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> marr s a -> Int -> Int -> m ()
    -- | 创建一个不可变拷贝.
    cloneArr :: arr a -> Int -> Int -> arr a
    -- | 创建一个可变拷贝.
    cloneMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> Int -> m (marr s a)
    -- | Resize mutable array to given size.
    resizeMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m (marr s a)
    -- | 收缩数组的大小至指定值. 该操作只能对原生数组生效.
    -- 对于盒装的数组, 这个操作是一个无用操作, 即 'sizeOfMutableArr' 不会改变.
    shrinkMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> Int -> m ()
    -- | 两个可变数组是否指向同一个引用.
    sameMutableArr :: marr s a -> marr s a -> Bool
    -- | 不可变数组的长度.
    sizeofArr :: arr a -> Int
    -- | 可变数组的长度.
    sizeofMutableArr :: (PrimMonad m, PrimState m ~ s) => marr s a -> m Int
    -- | 判断两个不可变数组是否指向同一个引用.
    sameArr :: arr a -> arr a -> Bool
```

{::comment}
And we have following instances:
{:/}
我们有以下的实例：

{::comment}
```haskell
-- | Boxed array type, for holding haskell ADTs.
instance Arr Array a where
    type MArr Array = MutableArray
    ...
-- | Boxed array type, for holding haskell ADTs, but doesn't carry a card table.
instance Arr SmallArray a where
    type MArr SmallArray = SmallMutableArray
    ...
-- | Unboxed array type, for holding primitive types like Int, Word8, etc.
instance Prim a => Arr PrimArray a where
    type MArr PrimArray = MutablePrimArray
    ...
-- | Boxed array type, for holding boxed unlifted types, see following section.
instance PrimUnlifted a => Arr UnliftedArray a where
    type MArr UnliftedArray = MutableUnliftedArray
    ...
```
{:/}

```haskell
-- | 盒装的数组类型, 持有haskell抽象数据类型.
instance Arr Array a where
    type MArr Array = MutableArray
    ...
-- | 盒装的数组类型, 持有haskell抽象数据类型, 但没有卡片表.
instance Arr SmallArray a where
    type MArr SmallArray = SmallMutableArray
    ...
-- | 非盒装的数组类型, 持有原生数据类型如Int, Word8等.
instance Prim a => Arr PrimArray a where
    type MArr PrimArray = MutablePrimArray
    ...
-- | 盒装的数组类型, 持有盒装非提升的类型, 详见后续章节.
instance PrimUnlifted a => Arr UnliftedArray a where
    type MArr UnliftedArray = MutableUnliftedArray
    ...
```


{::comment}
If you know how `IO` works in Haskell, `PrimMonad` simply means `ST` or `IO`. But if you get confused by the `PrimMonad` constraint, please get [more details here](https://wiki.haskell.org/IO_inside).
{:/}

如果你知道 `IO` 在Haskell中是如何工作的，可以将`PrimMonad`简单地等同于`ST`或`IO`。如果你对`PrimMonad`约束感到困惑，请在[更多详细信息](https://wiki.haskell.org/IO_inside)中获取。

{::comment}
# Boxed, Unboxed
{:/}
# 盒装的，非盒装的

{::comment}
For many haskellers, using arrays may be the first time one wants to know what's the difference between boxed, unboxed types. It's important to spend some time explaining these buzzwords.
{:/}

对于许多Haskell的使用者而言，使用数组可能是你第一次知道`boxed`和`unboxed`类型之间的区别是什么。花一些时间了解这些术语的意思是很重要的。

{::comment}
In other languages, you often have to distinguish *reference* and *value*. For example, in C pointers are references to other objects. It's a memory location in hardware sense: you can use machine code to follow a reference to the memory it pointing to. While the other non-pointer types value are not memory locations, their 1-0 arrangements stands for a certain value of that type.
{:/}

在其他语言中，你通常必须区分 *引用* 和 *值*。例如，在C中，指针是对其他对象的引用。从硬件的角度来看，指针是一个内存地址：你可以使用机器代码来表示它所指向的内存的引用。而其他非指针类型的值就不是一个内存地址了，它们的1-0排列表示了该类型的某个值。

{::comment}
In haskell almost every value you see is a pointer from C's perspective, i.e. a memory location point to a heap object, for example a data type like:
{:/}

在haskell中，从C语言的角度来看几乎每个你所见到的的值都是一个指针，即指向堆对象的内存位置，例如，像这样的数据类型：

```haskell
data Foo = Foo Int Char
foo = Foo 3 'a'
```

{::comment}
Are represented as:
{:/}

上述代码内存布局表现为: 

{::comment}
```
    foo(from registers or other boxes)
     |
     V
+----+--------+---+---+    +-------------+------+
| info-table* | * | * +----+ info-table* | 'a'# |
+-------------+-+-+---+    +-------------+------+
  Foo           |           C# (Char's constructor)
                V
            +---+---------+----+
            | info-table* | 3# |
            +-------------+----+
             I# (Int's constructor)
```
{:/}

```
    foo(由寄存器或者其他盒子指向的)
     |
     V
+----+--------+---+---+    +-------------+------+
| info-table* | * | * +--->+ info-table* | 'a'# |
+-------------+-+-+---+    +-------------+------+
  Foo           |           C# (Char构造器)
                V
            +---+---------+----+
            | info-table* | 3# |
            +-------------+----+
             I# (Int构造器)
```

{::comment}
During runtime the value `foo` is a reference, and all the operations, e.g. pattern match, go through dereferencing. Values like this are called *boxed* because it's a reference to a box, i.e. heap objects with [info-table](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#info-tables). The info-table contains many useful infomation about the box, such as how many words the boxed occupied, which constructor the box stand for, etc.
{:/}

在运行时，值`foo`是引用，且所有的操作，例如模式匹配，都需要进行引用求值(dereferencing)。这样的值称为*盒装的(boxed)*，因为它是对盒子的引用，即持有[info-table](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/heap-objects#info-tables)的堆上对象。信息表(info-table)包含有关该盒子的许多有用信息，例如，盒子所占用的字节数，盒子所对应的构造函数等。

{::comment}
The `3#` and `'a'#` above are haskell's non-pointer value, we call values like this *unboxed* values. Unboxed values don't have info-tables, so we really can't have them directly on heap: otherwise the GC would get confused when it scans them: without infomation from info-table, it can't decide how many bytes to copy. These values are usually belong to registers or other boxes: we generate machine code to manipulate them directly.
{:/}

上面用到的 `3#` 和 `'a'#` 是haskell的非指针值，我们称这些值为 *非盒装的((unboxed)* 值。非盒装的值没有信息表，因此我们不能将它们直接放在堆上：否则，GC在扫描它们时会感到困惑：如果没有来自信息表的信息，则无法确定要复制多少字节。这些值通常属于寄存器或其他盒子：我们生成机器码以直接对其进行操作。

{::comment}
## Boxed array
{:/}
## 盒装的（Boxed）数组

{::comment}
Now let's consider GHC arrays, they're special heap objects provided by RTS. We have boxed arrays `MutableArray#` and `Array#` that store references to boxes:
{:/}

现在让我们考虑GHC的数组，它们是RTS提供的特殊堆对象。我们有装箱的数组 `MutableArray#` 和 `Array#`，他们存储着对盒子的引用：

{::comment}
```
+-------------+--------------+---------------------------+---+-...-+---+---+------------+
| info-table* | payload size | payload + card-table size | * | ... | * | * | card table |
+-------------+--------------+---------------------------+-+-+-...-+---+---+------------+
 MutableArray#                                             |
 Array#                                                    V
                                                    +------+------+-----+
                                                    | info-table* | ... |
                                                    +-------------+-----+
                                                      Boxes, maybe a thunk
                                                      Most of the operations on boxed array
                                                      are lazy on its element
```
{:/}

```
+-------------+--------------+---------------------------+---+-...-+---+---+------------+
| info-table* | payload size | payload + card-table size | * | ... | * | * | card table |
+-------------+--------------+---------------------------+-+-+-...-+---+---+------------+
 MutableArray#                                             |
 Array#                                                    V
                                                    +------+------+-----+
                                                    | info-table* | ... |
                                                    +-------------+-----+
                                                      盒子, 可能是一个 thunk
                                                      盒装的数组大部分操作作用于数组元素时都是懒加载的
```

{::comment}
It looks quite complicated, especially the card-table part, which is used to [optimize the GC for arrays](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/gc/remembered-sets). `MutableArray#`s are always kept in a generation's mutable list once it's promoted to that generation, so this optimization is important if you keep a large mutable array on heap for a long time. For small arrays, it's unnecessary to use a card-table, and GHC provides `MutableSmallArray#/SmallArray#` for that purpose.
{:/}

它看起来非常复杂，尤其是卡片表（card-table）的部分，该部分用于[数组GC优化](https://gitlab.haskell.org/ghc/ghc/-/wikis/commentary/rts/storage/gc/remembered-sets)。在分代式GC中，一旦将 `MutableArray#` 提升到一个世代后，它们始终会保留在该世代的可变列表中，因此，卡片表的优化很重要，如果你将大型可变数组长时间保留在堆。对于小数组其实没有必要使用卡片表，GHC为此提供了 `MutableSmallArray#/SmallArray#`。

```
+-------------+--------------+---+-...-+---+---+
| info-table* | payload size | * | ... | * | * | 
+-------------+--------------+---+-...-+---+---+
 MutableSmallArray#
 SmallArray#
```

{::comment}
There're ADT wrappers for these types to make it easier to work with:
{:/}

为了可以更轻松地使用这些类型，我们提供了ADT包装器：

```haskell
data MutableArray s a = MutableArray (MutableArray# s a)
data Array a = Array (Array# a)

data SmallMutableArray s a = SmallMutableArray (SmallMutableArray# s a)
data SmallArray a = SmallArray (SmallArray# a)
```

{::comment}
A common pattern in Haskell is to turn `MutableArray` into an `Array` with freeze operations after creation complete, but the card-table's space is still there in case we thaw the array in place again. Generally speaking, under creation-freeze pattern, `MutableSmallArray` and `SmallArray` are more recommended since you won't keep mutable array on heap for too long.
{:/}

Haskell中的一种常见模式是在完成创建后通过冻结(freeze)操作将 `MutableArray` 转换为 `Array`，为了方便我们再次解冻(thaw)数组，card-table的空间仍然会被保留。一般来说，在创建冻结模式下，更建议使用`MutableSmallArray` 和 `SmallArray`，因为可变数组往往不会在堆上保留太长时间。

{::comment}
## Unboxed array
{:/}
## 非盒装的数组

{::comment}
`MutableByteArray#`, `ByteArray#` are GHC's unboxed array. They don't contain pointers, and their payload do not need to be traced during GC:
{:/}
`MutableByteArray#`, `ByteArray#` 是GHC的非盒装的数组(unboxed array)。它们不包含指针，并且在GC期间无需关注他们的负载(payload)：

```
+-------------+--------------+-------------+---+-...-+---+---+
| info-table* | payload size | 0xXXXXXXXX# | # | ... | # | # |
+-------------+--------------+-------------+---+-...-+---+---+
 MutableByteArray#
 ByteArray#   
```

{::comment}
`ByteArray#`s can be used to encode different size non-pointer data, such as `Int` and `Word8`, `ghc-prim` provide seperated functions to work with different data types: `indexIntArray#`, `indexWord8Array#`, etc, So there're `Prim` class and `PrimArray` type to make working with different types easier:
{:/}

`ByteArray#` 可以用来编码不同大小的非指针数据，例如`Int`和`Word8`，`ghc-prim` 提供了独立的函数来处理不同的数据类型：如 `indexIntArray#`，`indexWord8Array#` 等，因此我们提供了 `Prim` 类型类和 `PrimArray` 类型，这让使用不同的类型更加容易：

```haskell
-- types which can be stored in ByteArray# 
class Prim a where
    indexByteArray# :: ByteArray# -> Int# -> a
    ...

-- | type indexed ByteArray#
data PrimArray a = PrimArray ByteArray#

indexPrimArray :: Prim a => PrimArray a -> Int -> a
...
```

# Lifted, Unlifted

Another difference between types: unlifted and lifted, exists because in haskell we have non-strict evaluation mechanism, e.g. a value `1 + 2` may have a representation like:

```
+-------------+----------+---+    +-------------+----+
| info-table* | reserved | * +--->+ info-table* | 2# |
+------+------+----------+---+    +-------------+----+
       |                           This is I#
       V
 The info-table points to (+1) code.
```

In Haskell `1 + 2` and `3` are both references, they can be used interchangeably: a function expecting an `Int` argument can accept both pointers. This is done by *entering* the heap objects. i.e. execute the entry code following the info-table. The entry code for constructors are simply returns. For thunks the code will do evaluation and the `reserved` word above is reserved exactly for evaluation result, by writing a forward pointer and change the thunk box into an indirection box.

The evaluation may fail(diverged recursion, stackoverflow, etc.), so the pointer could potentially point to an undefined value, this kind of things are called *bottom* in haskell, written as `_|_`. The intuition for this name is that all the other evaluated values have certain meaning, but bottom doesn't, it sits lower in the spectrum of determinism, concreteness, usefulness ... whatever suits your mind. Hence comes the concept of `lifted` type, i.e. types which contain `bottom` values, or more formly, inhabited by `_|_`.

As you expected, most of the boxed type can be inhabited by `_|_`, the thunk may explode and terminate your program, or call `error` or `undefined` in base. And most of the unboxed types are unlifted types. e.g. It's impossible that an `Int#` would stand for an undefined value, because all 1-0 arrangements would represent a `Int#`, or put it another way: there's no way we get a bottom from `Int#`, because it doesn't have an info-table, and we can't enter it.

But some boxed unlifted types do exist, e.g. `MutableArray#/Array#` are such types, their representation on heap have an info-table pointer, but they were never entered. All the primitive operations manipulating them won't enter them, and the only way to create them is via `newArray#`, `cloneArray#`, etc. 

To efficiently store boxed unlifted types, `Unlifted` class and `UnliftedArray` type are introduced similar to `Prim` and `PrimArray`, `UnliftedArray` store unlifted references instead of normal haskell ADTs. Comparing `Array Array`, `UnliftedArray Array` could remove a level of redirection, i.e. remove item's `Array` box and store `Array#` directly.

# More on arrays

There're more details on Haskell arrays, such as pinned vs unpinned `ByteArray`s, etc. Interested readers could find all these details on [GHC wiki](https://gitlab.haskell.org/ghc/ghc/-/wikis/home), especially on RTS section.
To use array properly, all you need to do is choose the proper storage type and import `Z.Data.Array`. In next section we will introduce vectors, which is simply slices of arrays.
