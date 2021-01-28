-- Copyright (c) Facebook, Inc. and its affiliates.

-- (c) The University of Glasgow 2006

module Util.String
  ( capitalize
  , decapitalize
  , toArgs
  , strip
  ) where

import Data.Char (isSpace, toUpper, toLower)
import Control.Applicative

-- | For processing a string representing a list of arguments into a list of
-- strings, handling surrounding quotes, brackets and spaces. From GHC's Util
-- module. Reproduced here so it can be used without a dependency on GHC.
toArgs :: String -> Either String   -- Error
                           [String] -- Args
toArgs str
    = case dropWhile isSpace str of
      s@('[':_) -> case reads s of
                   [(args, spaces)]
                    | all isSpace spaces ->
                       Right args
                   _ ->
                       Left ("Couldn't read " ++ show str ++ " as [String]")
      s -> toArgs' s
 where
  toArgs' :: String -> Either String [String]
  -- Remove outer quotes:
  -- > toArgs' "\"foo\" \"bar baz\""
  -- Right ["foo", "bar baz"]
  --
  -- Keep inner quotes:
  -- > toArgs' "-DFOO=\"bar baz\""
  -- Right ["-DFOO=\"bar baz\""]
  toArgs' s = case dropWhile isSpace s of
              [] -> Right []
              ('"' : _) -> do
                    -- readAsString removes outer quotes
                    (arg, rest) <- readAsString s
                    (arg:) `fmap` toArgs' rest
              s' -> case break (isSpace <||> (== '"')) s' of
                    (argPart1, s''@('"':_)) -> do
                        (argPart2, rest) <- readAsString s''
                        -- show argPart2 to keep inner quotes
                        ((argPart1 ++ show argPart2):) `fmap` toArgs' rest
                    (arg, s'') -> (arg:) `fmap` toArgs' s''

  readAsString :: String -> Either String (String, String)
  readAsString s = case reads s of
                [(arg, rest)]
                    -- rest must either be [] or start with a space
                    | all isSpace (take 1 rest) ->
                    Right (arg, rest)
                _ ->
                    Left ("Couldn't read " ++ show s ++ " as String")


(<||>) :: Applicative f => f Bool -> f Bool -> f Bool
(<||>) = liftA2 (||)
infixr 2 <||> -- same as (||)

-- | Strip whitespace from both beginning and end of a string.
strip :: String -> String
strip = reverse . dropWhile isSpace . reverse . dropWhile isSpace

-- | Capitalize the first character of a string.
capitalize :: String -> String
capitalize (x:xs) = toUpper x : xs
capitalize _ = []

-- | Decapitalize the first character of a string
decapitalize :: String -> String
decapitalize (x:xs) = toLower x : xs
decapitalize _ = []
