
-- |
-- a utility module
module CPU.Utils where

import Data.List (group, sort)

-- |
-- replicate operation and chain it
replicateMChain :: Monad m => Int -> (a -> m a) -> a -> m a
replicateMChain n f x
  | n <= 0 = return x
  | otherwise = f x >>= replicateMChain (n-1) f

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

supplyBoth :: (a -> b -> c) -> (b -> a) -> b -> c
supplyBoth = (=<<)

mapLeft :: (a -> b) -> Either a c -> Either b c
mapLeft f (Left  x) = Left (f x)
mapLeft _ (Right x) = Right x

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

