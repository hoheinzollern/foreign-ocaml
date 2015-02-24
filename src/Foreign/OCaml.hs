{- |
   Module      : Foreign.OCaml
   Description : Foreign Function Interface for OCaml
   Copyright   : (c) Alessandro Bruni 2014
   License     : GPL-2
   Maintainer  : albr@dtu.dk


Haskell Foreign Function Interface (FFI) for OCaml.
Allows integrating OCaml code into Haskell programs: right now the interface is still pretty rough, and there are probably terrible memory holes that we need to deal with, consider it as a proof of concept.

It offers:

* calling OCaml functions from Haskell;
* serialization and deserialization of OCaml datatypes, including `unit`, `bool`, `int`, `double`, `string`, `list`s, `tuple`s and `option`s;
* limited automatic suppport for serializing algebraic data types, works when there are no type variables (helper functions are provided for converting between the two representations, so custom serializations can be built)
* strict and lazy evaluation, support for side effects

Current limitations:

* does not support passing higher order functions to OCaml (Haskell function serialization is not supported)
* tuple serialization limited to tuples of 2 to 5 arguments
* no handling of garbage collection, so your program might explode

 -}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable, ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.OCaml (caml_startup,
                      Marshal(..),
                      GMarshal(..),
                      Value,Tag,
                      register_closure,
                      block_constructor,
                      constant_constructor,
                      is_block,is_value,
                      get_const,
                      get_field,
                      get_tag,
                      module Data.Data,
                      module Data.Typeable) where


import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Control.Applicative
import Control.Monad
import qualified Control.Monad.State as CMS

import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.Data
import Data.Generics.Aliases
import Data.Typeable
import Data.List
import Data.Int
import Data.Word

-- | Opaque datatype that represents an OCaml object.
type Value = CLong
type ValueHandle = Ptr Value

type CStringHandle = Ptr CString

type CAMLRootsBlock = Ptr CLong

type MLSize = CULong
-- | Represents tags used in block constructors
type Tag = CUChar

-- Function calls
foreign import ccall "caml_named_value" caml_named_value :: CString -> IO ValueHandle
foreign import ccall "caml_callback" caml_callback :: Value -> Value -> Value
foreign import ccall "caml_callback2" caml_callback2 :: Value -> Value -> Value -> Value
foreign import ccall "caml_callback3" caml_callback3 :: Value -> Value -> Value -> Value -> Value
foreign import ccall "caml_callbackN" _caml_callbackN :: Value -> CULong -> ValueHandle -> Value
foreign import ccall "caml_startup" _caml_startup :: CStringHandle -> IO ()
foreign import ccall "caml_alloc" caml_alloc :: MLSize -> Tag -> IO Value
foreign import ccall "caml_modify" caml_modify :: ValueHandle -> Value -> IO ()
foreign import ccall "caml_copy_string" caml_copy_string :: CString -> IO Value
foreign import ccall "caml_copy_double" caml_copy_double :: CDouble -> IO Value

foreign import ccall "caml/memory.h &caml_local_roots" caml_local_roots :: CAMLRootsBlock

crbGetNext :: CAMLRootsBlock -> IO CAMLRootsBlock
crbGetNext p = do
  x <- peekElemOff p 0
  return $ wordPtrToPtr $ fromIntegral x

-- | Startup for the OCaml runtime: call this function before using
-- any other function in this module. The list of arguments is fed to
-- the OCaml runtime. For example:
-- 
-- @
-- main = do
--   args <- getArgs
--   caml_startup args
--   -- ..rest of the program..
-- @
caml_startup :: [String] -> IO ()
caml_startup args = startup args []
	where startup :: [String] -> [CString] -> IO ()
	      startup (x:xs) args = withCString x (\x->startup xs (x:args))
	      startup [] args = withArray (reverse (nullPtr:args)) _caml_startup

caml_callbackN :: Value -> [Value] -> Value
caml_callbackN cl args = unsafePerformIO $ withArray (reverse args)
                         (return . _caml_callbackN cl (fromIntegral $ length args)) 

get_closure name = unsafePerformIO $
                   do ref <- withCString name caml_named_value
                      if ref == nullPtr then
                          error $ "Unregistered OCaml callback: " ++ name
                          else peek ref

store_field :: Value -> Int -> Value -> IO ()
store_field block offset val =
    caml_modify (plusPtr (handle_val block) (offset * sizeOf val)) val

val_uchar :: CUChar -> Value
val_uchar x = fromIntegral ((shiftL x 1) + 1)

uchar_val :: Value -> CUChar
uchar_val x = fromIntegral (shiftR x 1)

val_int :: Int -> Value
val_int x = fromIntegral ((shiftL x 1) + 1)

int_val :: Value -> Int
int_val x = fromIntegral (shiftR x 1)

val_cdouble :: CDouble -> Value
val_cdouble x = unsafePerformIO $ caml_copy_double $ x

cdouble_val :: Value -> CDouble
cdouble_val x = unsafePerformIO $ peek $ castPtr $ handle_val x

unit_val :: Value -> ()
unit_val x = ()

val_handle :: ValueHandle -> Value
val_handle x = fromIntegral (ptrToWordPtr x)

handle_val :: Value -> ValueHandle
handle_val x = wordPtrToPtr (fromIntegral x)

string_val :: Value -> String
string_val x = unsafePerformIO $ peekCString $ wordPtrToPtr $ fromIntegral x

string_tag = 252

val_string :: String -> Value
val_string x = unsafePerformIO $ withCString x caml_copy_string

closure_tag = 247

-- | True if `v` is an atomic value
is_value :: Value -> Bool
is_value v = testBit v 0

-- | True if `v` is a block constructor
is_block :: Value -> Bool
is_block v = not $ is_value v

littleEndian =  (decode $ runPut $ putWord16host 42 :: Word8) == 42

-- | Produces a constant constructor, that is, a constructor that has
-- no parameters. The integer parameter is the index of the constant
-- constructor, in order of appearance on the declaration of the
-- datatype, counting from 0 and not considering block constructors.
constant_constructor :: Tag -> Value
constant_constructor = val_uchar

-- | Produces a block constructor, that is, a constructor with
-- arguments. The integer pararmeter is the index of the block
-- constructor, in order of appearance on the declaration of the
-- datatype, counting from 0 and not considering constant
-- constructors.
block_constructor :: Tag -> [Value] -> Value
block_constructor x args = unsafePerformIO $ do
  r <- caml_alloc (fromIntegral $ length args) x
  store_args r 0 args
  return r
    where store_args r i (x:xs) = do store_field r i x
                                     store_args r (i+1) xs
          store_args r i [] = return ()

-- | Returns the index for a constant constructor, inverse of
-- `constant_constructor`.
get_const :: Value -> Tag
get_const = uchar_val

-- | Returns a field in position `offset` from a block constructor.
get_field :: Value -> Int -> Value
get_field block offset =
    unsafePerformIO $ peekElemOff (handle_val block) offset

-- | Returns the idnex for a block constructor, more or less an
-- inverse of `block_constructor`, to be used together with `get_field`
get_tag :: Value -> Tag
get_tag v = if littleEndian then
                unsafePerformIO $ peekByteOff (castPtr $ handle_val v) (-sizeOf v)
            else
                unsafePerformIO $ peekByteOff (castPtr $ handle_val v) (-1)

-- | "Marshal" provides functions for transforming a Haskell
-- representation of data into OCaml and viceversa.  Default
-- implementations are given for all basic types: "Unit", "Bool",
-- "Int", "Float", "Double", "String"; and dependent declarations
-- allow to construct "Marshal" implementations for lists, tuples and
-- applicative types.  Note that for applicative types the `marshal`
-- function is not implemented, so it is not (yet) possible to pass
-- higher order values.
class Marshal t where
    -- | `marshal` takes a Haskell value of type `t` and produces an
    -- opaque OCaml value.
    marshal :: t -> Value
    -- | `unmarshal` takes an OCaml value and returns its
    -- deserialization into a Haskell data type
    unmarshal :: Value -> t

-- | Converts @()@ into `unit`
instance Marshal () where
    marshal () = val_int 0
    unmarshal 0x1 = ()
    unmarshal _ = error "Unit value should be 1"

-- | Converts "Bool" into `bool`
instance Marshal Bool where
    marshal False = val_int 0
    marshal True = val_int 1
    unmarshal 0x1 = False
    unmarshal 0x3 = True
    unmarshal _ = error "Booleans should be either true or false"

-- | Represents the `int` datatype in OCaml. Be aware of the
-- difference in representation between the OCaml and the Haskell
-- datatypes: OCaml's `int` datatype is a 31 bit signed integer, while
-- Haskell's "Int" is a 32 bit signed integer, hence the representable
-- ranges vary.
instance Marshal Int where
    marshal = val_int
    unmarshal = int_val

-- | Converts an Haskell "Float" into an OCaml `float`, which is a
-- double precision floating point value.
instance Marshal Float where
    marshal = val_cdouble . realToFrac
    unmarshal = realToFrac . cdouble_val

-- | Converts an Haskell "Double" into an OCaml `float`, which is a
-- double precision floating point value.
instance Marshal Double where
    marshal = val_cdouble . realToFrac
    unmarshal = realToFrac . cdouble_val

-- | Converts an Haskell "String" into an OCaml `string`. Note that
-- while the first is just an alias for ["Char"], `string` is a native
-- datatype. Needs the extensions @-XFlexibleInstances@ and
-- @-XOverlappingInstances@ in order to work properly.
instance Marshal String where
    marshal = val_string
    unmarshal = string_val

-- | Converts the Maybe monad to the corresponding OCaml datatype.
instance (Marshal a) => Marshal (Maybe a) where
    marshal (Just x)  = block_constructor 0 [marshal x]
    marshal (Nothing) = constant_constructor 0

    unmarshal v =
        if is_block v then Just (unmarshal $ get_field v 0)
        else Nothing

-- | Converts an Haskell list into an OCaml list.
instance (Marshal a) => Marshal [a] where
    marshal (x:xs) = block_constructor 0 [marshal x, marshal xs]
    marshal [] = val_int 0

    unmarshal v =
        if is_block v then (unmarshal $ get_field v 0) : (unmarshal $ get_field v 1)
        else []

-- | Allows calling the C function @caml_callbackN@, by passing
-- marshalled arguments in reverse order. It is supposed to be more
-- efficient than the other forms, but is not general, so use it with
-- care.
instance (Marshal a) => Marshal ([Value]->a) where
    marshal f = error "Unable to marshal functions"
    unmarshal cl args = unmarshal $ caml_callbackN cl args

-- | Applicative types: allows calling OCaml functions of type @a -> b
-- -> c -> d@. Please note that marshalling is not supported, hence no
-- functions can be passed as values to OCaml.
instance (Marshal a, Marshal b, Marshal c, Marshal d) => Marshal (a->b->c->d) where
    marshal f = error "Unable to marshal functions"
    unmarshal cl x1 x2 x3 = unmarshal $ caml_callback3 cl (marshal x1) (marshal x2) (marshal x3)

-- | Applicative types: allows calling OCaml functions of type @a -> b
-- -> c@. Please note that marshalling is not supported, hence no
-- functions can be passed as values to OCaml.
instance (Marshal a, Marshal b, Marshal c) => Marshal (a->b->c) where
    marshal f = error "Unable to marshal functions"
    unmarshal cl x1 x2 = unmarshal $ caml_callback2 cl (marshal x1) (marshal x2)

-- | Applicative types: allows calling OCaml functions of type @a ->
-- b@. Please note that marshalling is not supported, hence no
-- functions can be passed as values to OCaml.
instance (Marshal a, Marshal b) => Marshal (a -> b) where
    marshal f = error "Unable to marshal functions"
    unmarshal cl x = unmarshal $ caml_callback cl $ marshal x

-- | Use the "IO" monad to mark those functions that produce side
-- effects to the system. The "IO" monad also enforces strict
-- evaluation, hence the call is executed at the point where the
-- function is called.
instance (Marshal a) => Marshal (IO a) where
    marshal x = marshal $ unsafePerformIO x
    unmarshal v = let !r = unmarshal v in
                  return r

-- TODO: consider using the lens package to marshal arbitrary tuples
-- | Converts an Haskell 2-tuple into an OCaml 2-tuple and viceversa
instance (Marshal a, Marshal b) => Marshal (a, b) where
    marshal (a, b) = block_constructor 0 [marshal a, marshal b]
    unmarshal v = case get_tag v of
                    0 -> (unmarshal $ get_field v 0, unmarshal $ get_field v 1)
                    _ -> error "OCaml tuples have should tag 0"

-- | Converts an Haskell 3-tuple into an OCaml 3-tuple and viceversa
instance (Marshal a, Marshal b, Marshal c) => Marshal (a, b, c) where
    marshal (a, b, c) = block_constructor 0 [marshal a, marshal b, marshal c]
    unmarshal v = case get_tag v of
                    0 -> (unmarshal $ get_field v 0, unmarshal $ get_field v 1, unmarshal $ get_field v 2)
                    _ -> error "OCaml tuples have should tag 0"

-- | Converts an Haskell 4-tuple into an OCaml 4-tuple and viceversa
instance (Marshal a, Marshal b, Marshal c, Marshal d) => Marshal (a, b, c, d) where
    marshal (a, b, c, d) = block_constructor 0 [marshal a, marshal b, marshal c, marshal d]
    unmarshal v = case get_tag v of
                    0 -> (unmarshal $ get_field v 0, unmarshal $ get_field v 1, unmarshal $ get_field v 2, unmarshal $ get_field v 3)
                    _ -> error "OCaml tuples have should tag 0"

-- | Converts an Haskell 5-tuple into an OCaml 5-tuple and viceversa
instance (Marshal a, Marshal b, Marshal c, Marshal d, Marshal e) => Marshal (a, b, c, d, e) where
    marshal (a, b, c, d, e) = block_constructor 0 [marshal a, marshal b, marshal c, marshal d, marshal e]
    unmarshal v = case get_tag v of
                    0 -> (unmarshal $ get_field v 0, unmarshal $ get_field v 1, unmarshal $ get_field v 2, unmarshal $ get_field v 3, unmarshal $ get_field v 4)
                    _ -> error "OCaml tuples have should tag 0"

-- | `register_closure` returns a closure of type a for any OCaml name
-- that has been registered with:
--
-- @
-- let _ = Callback.register "f" f
-- @
--
-- If `f` is a function of OCaml type @int -> int@ then it can be
-- registered with:
--
-- @
-- f = register_closure "f" :: Int -> Int
-- @
--
-- `register_closure` works with type variables as well, as long as
-- all its instantiations are also instance of "Marshal". For example
-- the function `linearize` that takes a tree and returns a list, with
-- parameter type `a`:
--
-- @
-- type 'a tree = Leaf of 'a | Node of 'a tree * 'a tree;;
-- let rec linearize t = match t with
--   | Leaf(x)   -> [x]
--   | Node(l,r) -> let ll = linearize l in
--                  let lr = linearize r in
--                  ll @ lr;;
-- let _ = Callback.register "linearize" linearize
-- @
--
-- can be registered as such:
--
-- @
-- linearize = register_closure "linearize" :: Tree a -> [a]
-- @
--
-- provided that an implementation of @Tree a@ and "Marshal" for both @Tree a@ and @a@ exist.
register_closure :: Marshal a => String -> a
register_closure name = unmarshal (get_closure name)

data CT = EmptyC Tag | BlockC Tag
          deriving (Eq, Show)


class GMarshal a where
    gmarshal :: a -> Value
    gunmarshal :: Value -> a

instance (GMarshal a) => Marshal a where
    marshal = gmarshal
    unmarshal = gunmarshal

isBlockC :: forall d. Data d => Proxy d -> Constr -> Bool
isBlockC _ c = getConst $ gfoldl (\_ _ -> Const True) (\_ -> Const False) (fromConstr c :: d)

instance (Data a) => GMarshal a where
    gmarshal = gmarshal'
               `extQ` (marshal :: () -> Value)
               `extQ` (marshal :: Bool -> Value)
               `extQ` (marshal :: Int -> Value)
               `extQ` (marshal :: Float -> Value)
               `extQ` (marshal :: Double -> Value)
               `extQ` (marshal :: String -> Value)
               `extQ` (marshal :: Maybe a -> Value)
               `extQ` (marshal :: [a] -> Value)
        where gmarshal' x =
                  case idx of
                    EmptyC i -> constant_constructor i
                    BlockC i -> block_constructor i margs
                  where d = dataTypeOf x
                        cs = dataTypeConstrs d
                        c = toConstr x
                        idx = find cs c (0, 0)
                        pr = undefined :: Proxy a
                        find (c:cs) c' (i, j) =
                            if isBlockC pr c then
                                if c == c' then BlockC j
                                else find cs c' (i, j+1)
                            else
                                if c == c' then EmptyC i
                                else find cs c' (i+1, j)
                        find [] c idx = error $ "Constructor not found, this should never happen" ++ show c ++ show idx
                        margs = gmapQ gmarshal x

    gunmarshal = gunmarshal'
                 `extB` (unmarshal :: Value -> ())
                 `extB` (unmarshal :: Value -> Bool)
                 `extB` (unmarshal :: Value -> Int)
                 `extB` (unmarshal :: Value -> Float)
                 `extB` (unmarshal :: Value -> Double)
                 `extB` (unmarshal :: Value -> String)
                 `extB` (unmarshal :: Value -> Maybe a)
                 `extB` (unmarshal :: Value -> [a])
        where gunmarshal' v =
                  blockV
                  where -- Determine result type
                    d = dataTypeOf (getArg blockV)
                        where
                          getArg :: GMarshal a' => a' -> a'
                          getArg = undefined
                    cs = dataTypeConstrs d
                    cid = if is_block v then BlockC $ get_tag v
                          else EmptyC $ get_const v
                    con = find cs cid
                    blockV = CMS.evalState (fromConstrM unmarshalArg con) 0 :: a
                    unmarshalArg :: forall a. Data a => CMS.State Int a
                    unmarshalArg = do
                      x <- CMS.get
                      CMS.put (x+1)
                      return (gunmarshal (get_field v x))
                    pr = undefined :: Proxy a
                    find (c:cs) (EmptyC i) =
                        if isBlockC pr c then
                            find cs (EmptyC i)
                        else if i == 0 then c
                             else find cs (EmptyC $ i-1)
                    find (c:cs) (BlockC i) =
                        if isBlockC pr c then
                            if i == 0 then c
                            else find cs (BlockC $ i-1)
                        else find cs (BlockC i)
                    find [] x = error $ "Constructor not found, this should never happen" ++ show x
