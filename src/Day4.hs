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

readField s =
    let (name,':' :rest) = span (/= ':') s
        (value , end) = span (`notElem` [' ', '\n']) rest
    in ((name, value), drop 1 end)

readPassport current other s =
    let (p, rest) = readField s in
    case rest of
        ('\n':xs) -> readPassport [] ((p:current):other) xs
        [] -> (p:current):other
        xs -> readPassport (p:current) other xs

isValid passp =
    expected `Set.isSubsetOf` fields && (fields Set.\\ expected) `Set.isSubsetOf` alsoAllowed
    && all (\(f, s) -> fromMaybe (const True) (lookup f validation) s) passp
  where
    fields = Set.fromList $ map fst passp
    expected = Set.fromList ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]
    alsoAllowed = Set.singleton "cid"
    validation =
        [("byr", \s ->  length s == 4 && let s' = read s :: Int in s' >= 1920 && s' <= 2002)
        ,("iyr", \s ->  length s == 4 && let s' = read s :: Int in s' >= 2010 && s' <= 2020)
        ,("eyr", \s ->  length s == 4 && let s' = read s :: Int in s' >= 2020 && s' <= 2030)
        , ("hgt", \s -> let (num, rest) = span isDigit s
                            n = read num
                       in case rest of
                              "cm" -> n >= 150 && n <= 193
                              "in" -> n >= 59 && n <= 76
                              _ -> False

                  )
        , ("hcl", \s -> case s of
                           ('#':rest) -> all (`elem` (['0'..'9'] ++ ['a'..'f'])) rest
                           _ -> False
                  )
        , ("ecl", (`elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]))
        , ("pid", \s -> length s == 9 && all (`elem` ['0'.. '9']) s)
        ]

main = do
    [f] <- getArgs
    inp <- readPassport [] [] <$> readFile f
    print $ length $ filter isValid inp
