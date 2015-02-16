{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, FlexibleContexts #-}
-- Overlapping instances is needed for marshalling strings: both an
-- instance of Marshal a => Marshal [a] and of Marshal [Char] are
-- defined, where the former is used to marshal lists and the latter
-- is used to marshal strings. When given an expression of type [Char]
-- the more specific instance should be chosen, thus correctly
-- producing an OCaml string.
module Main where

import Foreign.OCaml
import System.IO.Unsafe
import Control.Concurrent
import GHC.Generics

-- Example of an algebraic data type with two constructors, copy of
-- the OCaml data type
data Tree = Leaf String | Node (Tree) (Tree) | Test32 | Test64 Int
              deriving (Eq, Generic, Show)

instance Marshal (Tree)

-- Registering closures defined in the OCaml code
factorial = register_closure "factorial" :: Int -> Int
sum = register_closure "sum" :: Int -> Int -> Int
rotate_left = register_closure "rotate_left" :: Marshal a => Tree -> Tree
rotate_right = register_closure "rotate_right" :: Marshal a => Tree -> Tree
count = register_closure "count" :: () -> IO Int

main = do
  -- required startup of the ocaml machinery
  caml_startup []
  -- factorial example
  putStrLn $ "factorial 10 = " ++ (show $ factorial 10)
  -- example showing partial application
  let sum10 = Main.sum 10
  putStrLn $ "sum10 32 = " ++ (show $ sum10 32)
  putStrLn $ "sum10 90 = " ++ (show $ sum10 90)
  -- rotation of a tree
  let t = Node (Node (Leaf "A") (Leaf "B")) (Node (Leaf "C") (Leaf "D"))
  let t' = rotate_left t
  let t'' = rotate_right t
  putStrLn $ "Original tree: " ++ show t
  putStrLn $ "Left rotation: " ++ show t'
  putStrLn $ "Right rotation: " ++ show t''
  -- IO values are strictly evaluated
  n0 <- count ()
  n1 <- count ()
  putStrLn $ "n1 = " ++ show n1
  putStrLn $ "n0 = " ++ show n0
