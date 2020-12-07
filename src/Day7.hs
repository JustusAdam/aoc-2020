#!/usr/bin/env stack
-- stack --resolver lts-16.15 script --package containers

{-# LANGUAGE TupleSections, LambdaCase, FlexibleInstances #-}

import Data.List (find, span)
import Data.Maybe (fromJust)
import System.Environment
import Control.Arrow ( (***) )
import Debug.Trace
import qualified Data.Set as Set
import Data.Maybe (fromMaybe)
import Data.Char
import Text.ParserCombinators.ReadP
import Control.Monad
import Data.Map as Map
import Control.Applicative ((<|>))

type Bag = (String, String)
data Rule = Rule Bag [(Int, Bag)] deriving Show


readRule = do
    skipSpaces
    b <- bag
    skipSpaces
    void $ string "bags contain"
    skipSpaces
    others <- sepBy (do
                            num <- readS_to_P (readsPrec 0)
                            skipSpaces
                            b <- bag
                            skipSpaces
                            void $ choice [ string "bag", string "bags"]
                            pure (num, b)
                            ) (string ", ")
             <|> (string "no other bags" >> pure [])
    void $ char '.'
    skipSpaces
    pure $ Rule b others
  where bag = do
            w1 <- munch isAlpha
            skipSpaces
            w2 <- munch isAlpha
            skipSpaces
            pure (w1, w2)

parseFile f =
    case [x | (x, "") <- readP_to_S (do r <- many readRule; eof ; pure r) f] of
        [x] -> x


solve :: [Rule] -> Int
solve rulez = go [("shiny", "gold")] Set.empty
  where
    rulemap = Map.fromListWith (++) [ (t', [from]) | Rule from to <- rulez, (_, t') <- to ]
    go [] seen = Set.size seen
    go next seen =
        let next' = [to | n <- next , to <- fromMaybe [] $ Map.lookup n rulemap, not (Set.member to seen) ]
        in go next' (Set.union seen $ Set.fromList next')

solve2 :: [Rule] -> Int
solve2 rulez = rulemap Map.! ("shiny", "gold")
  where
    rulemap = Map.fromList
        [ (name, sum [quantity * (1+ fromMaybe 0 (Map.lookup type_ rulemap)) | (quantity, type_) <- nests]
          )
        | Rule name nests <- rulez
        ]

main = do
    [f] <- getArgs
    inp <- parseFile <$> readFile f
    print $ solve2 inp
