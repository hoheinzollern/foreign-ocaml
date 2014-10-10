{- |
   Module      : Foreign.OCaml
   Description : Foreign Function Interface for OCaml
   Copyright   : (c) Alessandro Bruni 2014
   License     : GPL-2
   Maintainer  : albr@dtu.dk
 -}
{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE FlexibleInstances #-}
module Foreign.OCaml (caml_startup,
                      Marshal(..),
                      Value,
                      register_closure,
                      block_constructor,
                      constant_constructor,
                      --block_destructor,
                      constant_destructor,
                      is_block,
                      get_field,
                      get_tag) where

import Foreign.C
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import System.IO.Unsafe

import Data.Binary
import Data.Binary.Put
import Data.Bits
import Data.List
import Data.Int
import Data.Word

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

get_field :: Value -> Int -> Value
get_field block offset =
    unsafePerformIO $ peekElemOff (handle_val block) offset

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

is_value :: Value -> Bool
is_value v = testBit v 0

is_block :: Value -> Bool
is_block v = not $ is_value v

littleEndian =  (decode $ runPut $ putWord16host 42 :: Word8) == 42

get_tag :: Value -> CUChar
get_tag v = if littleEndian then
                unsafePerformIO $ peekByteOff (castPtr $ handle_val v) (-sizeOf v)
            else
                unsafePerformIO $ peekByteOff (castPtr $ handle_val v) (-1)

class Marshal t where
    marshal :: t -> Value
    unmarshal :: Value -> t

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
    unmarshal v = return $ unmarshal v

register_closure :: Marshal a => String -> a
register_closure name = unmarshal (get_closure name)

constant_constructor = val_int
block_constructor x args = unsafePerformIO $ do
  r <- caml_alloc (fromIntegral $ length args) x
  store_args r 0 args
  return r
    where store_args r i (x:xs) = do store_field r i x
                                     store_args r (i+1) xs
          store_args r i [] = return ()

constant_destructor = int_val
