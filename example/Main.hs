{-# LANGUAGE OverlappingInstances #-}
{-# LANGUAGE DeriveDataTypeable,BangPatterns #-}
-- Overlapping instances is needed for marshalling strings: both an
-- instance of Marshal a => Marshal [a] and of Marshal [Char] are
-- defined, where the former is used to marshal lists and the latter
-- is used to marshal strings. When given an expression of type [Char]
-- the more specific instance should be chosen, thus correctly
-- producing an OCaml string.
module Main where

import Foreign.OCaml

-- Example of an algebraic data type with two constructors, copy of
-- the OCaml data type
data Tree a = Leaf a | Node (Tree a) (Tree a)
              deriving (Eq, Show, Data, Typeable)

-- Registering closures defined in the OCaml code
factorial = register_closure "factorial" :: Int -> Int
sum = register_closure "sum" :: Int -> Int -> Int
rotate_left = register_closure "rotate_left" :: (Data a) => Tree a -> Tree a
rotate_right = register_closure "rotate_right" :: (Data a) => Tree a -> Tree a
count = register_closure "count" :: () -> IO Int
print_neg = register_closure "print_neg" :: Int -> IO Int

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
  let t = Node (Node (Leaf 1) (Leaf 2)) (Node (Leaf 3) (Leaf 4)) :: Tree Int
  let t' = rotate_left t
  let t'' = rotate_right t
  putStrLn $ "Original tree: " ++ show t
  let x = marshal t :: Value
  putStrLn $ "marshalled tree: " ++ show x
  putStrLn $ "Left rotation: " ++  show t'
  putStrLn $ "Right rotation: " ++ show t''
  x <- print_neg (-10) -- FIXME: does not print a value
  putStrLn $ show x
  -- IO values are strictly evaluated
  n0 <- count ()
  n1 <- count ()
  putStrLn $ "n1 = " ++ show n1
  putStrLn $ "n0 = " ++ show n0
