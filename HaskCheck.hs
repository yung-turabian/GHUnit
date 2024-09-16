-- @file UTest.hs
-- @author Henry Wandover
-- @date Sept. 7, 2024

{-# LANGUAGE FlexibleInstances #-}

module HaskCheck (
 hwAssertEqual,
 hwAssert,
 hwAssertTrue,
 hwAssertFalse
) where

-- @brief Compare lists.
hwLstCmp :: (Num a, Eq a) => [a] -> [a] -> Bool
hwLstCmp [] [] = True
hwLstCmp _ [] = False
hwLstCmp [] _ = False
hwLstCmp (it1:list1) (it2:list2) =
 if it1 == it2 then hwLstCmp list1 list2
 else False

{- 
 - Reference for `show`: https://hackage.haskell.org/package/base-4.20.0.1/docs/Text-Show.html
 -
 - Num type: https://en.wikibooks.org/wiki/Haskell/Type_basics_II
 -
 - Num asserts is of type Num: int, float or double; Eq can be compared, ==, and Show means `show` can be used
 -}
hwAssertEqual :: (Num a, Eq a, Show a) => String -> a -> a -> IO ()
hwAssertEqual msg expected actual =
 if expected == actual then putStrLn (msg ++ " \x1b[1;92m✓\x1b[0m")

 else putStrLn (msg ++ " \x1b[1;91m✗\x1b[0m" ++ "\n" ++ "\t>Expected= " ++ show expected 
 ++ "\n" ++ "\t>Actual= " ++ show actual)

class Assert a where
 hwAssert :: String -> a -> a -> IO ()

instance Assert Int where
 hwAssert msg expected actual =
  if expected == actual then putStrLn (msg ++ " \x1b[1;92m✓\x1b[0m")

  else putStrLn (msg ++ " \x1b[1;91m✗\x1b[0m" ++ "\n" ++ "\t>Expected= " ++ show expected ++ "\n" ++ "\t>Actual= " ++ show actual)

instance Assert [Int] where
 hwAssert msg expected actual =
  if hwLstCmp expected actual then putStrLn (msg ++ " \x1b[1;92m✓\x1b[0m")

  else putStrLn (msg ++ " \x1b[1;91m✗\x1b[0m" ++ "\n" ++ "\t>Expected= " ++ show expected ++ "\n" ++ "\t>Actual= " ++ show actual)

hwAssertTrue :: String -> Bool -> IO ()
hwAssertTrue msg actual =
 if actual == True then putStrLn (msg ++ " \x1b[1;92m✓\x1b[0m")

 else putStrLn (msg ++ " \x1b[1;91m✗\x1b[0m" ++ "\n" ++ "\t>Expected= " ++ show True ++ "\n" ++ "\t>Actual= " ++ show actual)

hwAssertFalse :: String -> Bool -> IO ()
hwAssertFalse msg actual =
 if actual == False then putStrLn (msg ++ " \x1b[1;92m✓\x1b[0m")

 else putStrLn (msg ++ " \x1b[1;91m✗\x1b[0m" ++ "\n" ++ "\t>Expected= " ++ show False ++ "\n" ++ "\t>Actual= " ++ show actual)
