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

type Program = [(String, Int)]

readInstructions :: String -> Program
readInstructions = map (interpretWords . words) . lines
  where
    interpretWords [inst, (op:(read -> rest))] = (inst, case op of '+' -> rest; '-' -> negate rest)

untilJust ac = ac >>= \case Just a -> pure a; Nothing -> untilJust ac

detectLoop :: Program -> (Bool, Int)
detectLoop p = runST $ do
    v <- V.new (length p)
    pc <- newSTRef 0
    acc <- newSTRef 0
    forM (zip [0..] p) $ \(i, inst) -> V.write v i (inst, False)

    r <- untilJust $ do
        pc' <- readSTRef pc
        if pc' >= V.length v
            then pure $ Just True
            else do
            ((i, arg), visited) <- V.read v pc'
            if visited
                then pure $ Just False
                else do
                V.modify v (second $ const True) pc'
                modifySTRef pc succ
                case i of
                    "jmp" -> modifySTRef pc (+ (pred arg))
                    "acc" -> modifySTRef acc (+ arg)
                    "nop" -> pure ()
                pure Nothing
    (r,) <$> readSTRef acc

permute :: Program -> [Program]
permute [] = []
permute (x@(i,arg):xs) = case i of
    "jmp" -> (("nop", arg):xs) : cont
    "nop" -> (("jmp", arg):xs) : cont
    _ -> cont
  where cont = map (x:) (permute xs)

main = do
    [f] <- getArgs
    inp <- readInstructions <$> readFile f
    print $ snd $ fromJust $ find fst $ map detectLoop $ permute inp
