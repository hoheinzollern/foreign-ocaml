{- |
   Module      : Foreign.OCaml
   Description : Foreign Function Interface for OCaml
   Copyright   : (c) Alessandro Bruni 2014
   License     : GPL-2
   Maintainer  : albr@dtu.dk
 -}
{-# LANGUAGE FlexibleInstances, OverlappingInstances #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module Foreign.OCaml (caml_startup,
                      Marshal(..),
                      deriveMarshalInstance,
                      Value,
                      register_closure,
                      block_constructor,
                      constant_constructor,
                      is_block,is_value,
                      get_const,
                      get_field,
                      get_tag) where


import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe
import Language.Haskell.TH
import Control.Monad

import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.List
import Data.Int
import Data.Word

-- | Opaque datatype that represents an OCaml object.
type Value = CLong
type ValueHandle = Ptr Value

type CStringHandle = Ptr CString

type CAMLRootsBlock = Ptr CLong

type MLSize = CULong
type Tag = CUInt

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

-- | Startup for the OCaml runtime: call this function before using any
-- other function in this module.
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
constant_constructor :: Int -> Value
constant_constructor = val_int

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
get_const :: Value -> Int
get_const = int_val

-- | Returns a field in position `offset` from a block constructor.
get_field :: Value -> Int -> Value
get_field block offset =
    unsafePerformIO $ peekElemOff (handle_val block) offset

-- | Returns the idnex for a block constructor, more or less an
-- inverse of `block_constructor`, to be used together with `get_field`
get_tag :: Value -> CUChar
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
                  
instance Marshal () where
    marshal () = val_int 0
    unmarshal 0x1 = ()
    unmarshal _ = error "Unit value should be 1"

instance Marshal Bool where
    marshal False = val_int 0
    marshal True = val_int 1
    unmarshal 0x1 = False
    unmarshal 0x3 = True
    unmarshal _ = error "Booleans should be either true or false"

instance Marshal Int where
    marshal = val_int
    unmarshal = int_val

instance Marshal Float where
    marshal = val_cdouble . realToFrac
    unmarshal = realToFrac . cdouble_val

instance Marshal Double where
    marshal = val_cdouble . realToFrac
    unmarshal = realToFrac . cdouble_val

instance Marshal String where
    marshal = val_string
    unmarshal = string_val

instance (Marshal a) => Marshal [a] where
    marshal (x:xs) = block_constructor 0 [marshal x, marshal xs]
    marshal [] = val_int 0

    unmarshal v = if is_block v then
                      (unmarshal $ get_field v 0) : (unmarshal $ get_field v 1)
                  else []

instance (Marshal a) => Marshal ([Value]->a) where
    marshal f = error "Unable to marshal functions"
    unmarshal cl args = unmarshal $ caml_callbackN cl args

instance (Marshal a, Marshal b, Marshal c, Marshal d) => Marshal (a->b->c->d) where
    marshal f = error "Unable to marshal functions"
    unmarshal cl x1 x2 x3 = unmarshal $ caml_callback3 cl (marshal x1) (marshal x2) (marshal x3)

instance (Marshal a, Marshal b, Marshal c) => Marshal (a->b->c) where
    marshal f = error "Unable to marshal functions"
    unmarshal cl x1 x2 = unmarshal $ caml_callback2 cl (marshal x1) (marshal x2)

instance (Marshal a, Marshal b) => Marshal (a -> b) where
    marshal f = error "Unable to marshal functions"
    unmarshal cl x = unmarshal $ caml_callback cl $ marshal x

instance (Marshal a) => Marshal (IO a) where
    marshal x = marshal $ unsafePerformIO x
    unmarshal v = let !r = unmarshal v in
                  return r

-- TODO: consider using the lens package to marshal arbitrary tuples
instance (Marshal a, Marshal b) => Marshal (a, b) where
    marshal (a, b) = block_constructor 0 [marshal a, marshal b]
    unmarshal v = case get_tag v of
                    0 -> (unmarshal $ get_field v 0, unmarshal $ get_field v 1)
                    _ -> error "OCaml tuples have should tag 0"

instance (Marshal a, Marshal b, Marshal c) => Marshal (a, b, c) where
    marshal (a, b, c) = block_constructor 0 [marshal a, marshal b, marshal c]
    unmarshal v = case get_tag v of
                    0 -> (unmarshal $ get_field v 0, unmarshal $ get_field v 1, unmarshal $ get_field v 2)
                    _ -> error "OCaml tuples have should tag 0"

instance (Marshal a, Marshal b, Marshal c, Marshal d) => Marshal (a, b, c, d) where
    marshal (a, b, c, d) = block_constructor 0 [marshal a, marshal b, marshal c, marshal d]
    unmarshal v = case get_tag v of
                    0 -> (unmarshal $ get_field v 0, unmarshal $ get_field v 1, unmarshal $ get_field v 2, unmarshal $ get_field v 3)
                    _ -> error "OCaml tuples have should tag 0"

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
-- If `f` is a function of OCaml type `int -> int` then it can be
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
-- provided that an implementation of "Tree a" and "Marshal" for both "Tree a" and "a" exist.
register_closure :: Marshal a => String -> a
register_closure name = unmarshal (get_closure name)
                    

deriveMarshal t = do
  TyConI (DataD _ _ tyvars constructors _) <- reify t

  let marshalClause :: Int -> Int -> [Con] -> Q [Clause]
      -- Constant constructors
      marshalClause recordC emptyC (NormalC name []:cs) =
          do d <- clause [conP name []]
                  (normalB [| constant_constructor emptyC |]) []
             ds <- marshalClause recordC (emptyC+1) cs
             return (d:ds)
      -- Constructors with fields
      marshalClause recordC emptyC (NormalC name fields:cs) =
          do (pats, vars) <- genPE (length fields)
             let f [] = [| [] |]
                 f (v:vars) = [| marshal $v : $(f vars) |]
             d <- clause [conP name pats]
                  (normalB [| block_constructor recordC $(f vars) |]) []
             ds <- marshalClause (recordC+1) emptyC cs
             return (d:ds)
      marshalClause _ _ [] =
          do return []

  marshalBody <- marshalClause 0 0 constructors
  return marshalBody

deriveUnmarshal t = do
  TyConI (DataD _ _ tyvars constructors _) <- reify t

  v <- newName "v"
  let vE = varE v
  let vP = varP v

  let unmarshalBlockCase :: Integer -> [Con] -> [MatchQ]
      unmarshalBlockCase recordC (NormalC name []:cs) = unmarshalBlockCase recordC cs
      unmarshalBlockCase recordC (NormalC name fields:cs) = (d:ds)
          where f :: Q Exp -> Int -> Q Exp
                f e n = if n > 0 then appE (f e (n-1)) [| unmarshal (get_field $(vE) (n-1)) |]
                        else e
                d = match (litP $ integerL recordC)
                    (normalB (f (conE name) (length fields))) []
                ds = unmarshalBlockCase (recordC+1) cs
      unmarshalBlockCase _ [] = []

  let unmarshalConstCase :: Integer -> [Con] -> [MatchQ]
      unmarshalConstCase emptyC (NormalC name []:cs) = (d:ds)
          where d = match (litP $ integerL emptyC)
                    (normalB (appE (conE name) [| [] |])) []
                ds = unmarshalConstCase (emptyC+1) cs
      unmarshalConstCase emptyC (NormalC name fields:cs) = unmarshalConstCase emptyC cs
      unmarshalConstCase _ [] = []

  let constCases = unmarshalConstCase 0 constructors
  let constCase = caseE [|val_int $(vE)|] constCases
  let blockCases = unmarshalBlockCase 0 constructors
  let blockCase = caseE [|get_tag $(vE)|] blockCases
  unmarshalBody <-
      case (constCases, blockCases) of
        ([], []) -> error "No constructors! weird..."
        (_, [])  -> clause [vP] (normalB constCase) []
        ([], _)  -> clause [vP] (normalB blockCase) []
        (_, _)   -> clause [vP] (normalB [| if is_block $(vE) then $(blockCase)
                                            else $(constCase) |]) []
  return unmarshalBody


genPE n = do
  ids <- replicateM n (newName "x")
  return (map varP ids, map varE ids)

data T = T

-- | Automatically generate an instance of "Marshal" given a datatype.
-- If you have an OCaml data type with no type variables and multiple
-- constructors:
--
-- @
-- type t = C1 of int * string | C2 of t * t | C3
-- @
--
-- Simply produce a corresponding Haskell data type, then call
-- "deriveMarshalInstance" to obtain two way conversion between the
-- Haskell and OCaml representations:
--
-- @
-- data T = C1 Int String | C2 T T | C3
-- $(deriveMarshalInstance ''T)
-- @
deriveMarshalInstance t = do
  t' <- reify t
  case t' of
    TyConI (DataD _ _ [] constructors _) -> do
        marshalBody <- deriveMarshal t
        unmarshalBody <- deriveUnmarshal t

        d <- [d| instance Marshal T where
                   marshal x = error "Unable to marshal T"
                   unmarshal x = error "Unable to unmarshal T"
              |]
        let    [InstanceD [] (AppT marshalt (ConT _))
                [FunD marshal_f m_err, FunD unmarshal_f unm_err]] = d
        -- let cxt = [ClassP (mkName "Marshal") [VarT v] | PlainTV v <- tyvars] :: Cxt
        return [InstanceD [] (AppT marshalt (ConT t))
                [FunD marshal_f marshalBody, FunD unmarshal_f [unmarshalBody]]]
    _ -> error $ "Type " ++ show t' ++ " not supported"
