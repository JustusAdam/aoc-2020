#!/usr/bin/env stack
-- stack --resolver lts-16.15 script --package containers --package vector

{-# LANGUAGE TupleSections, LambdaCase, FlexibleInstances, ViewPatterns #-}

import Data.List (find, span)
import Data.Maybe (fromJust)
import System.Environment
import Control.Arrow (second, (***) )
import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Char
import Control.Monad
import Control.Applicative ((<|>))
import qualified Data.Vector.Mutable as V
import Control.Monad.ST.Strict
import Data.STRef.Strict
import Data.Function

solve :: Int -> [Int] -> Int
solve preamble l =
    reverse l
    & test
    & reverse
    & zip l
    & drop preamble
    & (fst . fromJust . find (not . snd))
    & (fromJust . flip test2 l)
    & \l -> traceShow l $ minimum l + maximum l
  where
    test [] = []
    test (x:xs) = or [x' + y == x | x' <- take preamble xs, y <- take preamble xs] : test xs

    test2 target = \case
        [] -> Nothing
        (x:xs) -> (join . fmap (either (const Nothing) Just) $ go (Just $ Left (x,[x])) xs) <|> test2 target xs
      where
        go = foldl (\acc x -> case acc of
                           Just (Left (a, l)) | a == target -> Just $ Right $ reverse l
                                              | a > target -> Nothing
                                              | otherwise -> Just $ Left (a + x, x:l)
                           _ -> acc)

main = do
    [preamble, f] <- getArgs
    inp <- map read . lines <$> readFile f
    print $ solve (read preamble) inp
