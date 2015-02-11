{-# LANGUAGE OverlappingInstances #-}
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
              deriving (Eq, Show)

-- A marshal instance for the algebraic data type, hopefully in the
-- future it is not going to be needed
instance (Marshal a) => Marshal (Tree a) where
    -- Calling block_constructor with tag 0 and the list of marshalled
    -- arguments produces the first constructor definition in the
    -- OCaml side:
    marshal (Leaf x) = block_constructor 0 [marshal x]
    -- Similarly block_constructor with tag 1 and the arguments
    -- produces the second definition:
    marshal (Node t1 t2) = block_constructor 1 [marshal t1, marshal t2]
    -- Unmarshal needs to check the tag and create the respective
    -- Haskell constructors: get_tag and get_field are the functions
    -- that retrieve the information from the OCaml data type
    unmarshal v = case get_tag v of
                    0 -> Leaf (unmarshal $ get_field v 0)
                    1 -> Node (unmarshal $ get_field v 0) (unmarshal $ get_field v 1)

-- Registering closures defined in the OCaml code
factorial = register_closure "factorial" :: Int -> Int
sum = register_closure "sum" :: Int -> Int -> Int
rotate_left = register_closure "rotate_left" :: Marshal a => Tree a -> Tree a
rotate_right = register_closure "rotate_right" :: Marshal a => Tree a -> Tree a
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
