
-- |
-- a utility module
module Utils where

import Data.List (group, sort)

type Error = String

-- |
-- utility to throw an error
throwErr :: Error -> Either Error a
throwErr = Left

-- |
-- if Maybe b is nothing, replace it with Left a. otherwise: Right b
maybeToEither :: a -> Maybe b -> Either a b
maybeToEither _ (Just y) = Right y
maybeToEither x Nothing  = Left x


-- |
-- returns the duplicates of a list
duplicates :: Ord a => [a] -> [a]
duplicates = map head . filter ((>1) . length) . group . sort

-- |
-- split arguments by element
splitBy :: Eq a => a -> [a] -> [[a]]
splitBy v vs = map reverse $ go [] vs
  where go xs [] = [xs]
        go xs (y:ys)
          | y == v    = xs : go [] ys
          | otherwise = go (y:xs) ys


----------------
-- Formatting
---------------

displayCommands :: String -> [(String, String)] -> [String]
displayCommands opener cmds = fmap (displayCommand opener (cmdWithSpaces n)) cmds
  where n = 8 + foldr (\str acc -> max (length (fst str)) acc) 0 cmds

cmdWithSpaces :: Int -> String -> String
cmdWithSpaces n str = str ++ replicate (n - length str) ' '

displayCommand :: String -> (String -> String) -> (String, String) -> String
displayCommand opener paddCmd (cmd, desc) = opener ++ paddCmd cmd ++ desc

dashOpener, spaceOpener :: String
dashOpener  = "  - "
spaceOpener = "    "

