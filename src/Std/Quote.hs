{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Std.Quote
    ( QuasiQuoter(..)
    , StringPart(..), FormatAst, stringPart
    , parseFormatString
    , failingQuoter
    , parseExp, parsePat, parseDecs, parseType
    , symbol, string
    , fromFormatAst
    ) where

import "base" Data.List ( foldl1' )
import "base" Prelude ( reverse, span, elem, (||), Eq(..), Ord, Show, String, (.), error, (++), ($), Either(..), (>>), return, map )
import "base" Data.Char ( isAlphaNum, isAlpha )
import "base" GHC.Generics ( Generic )

import "template-haskell" Language.Haskell.TH
import "template-haskell" Language.Haskell.TH.Quote ( QuasiQuoter(..) )

import "haskell-src-meta" Language.Haskell.Meta.Parse qualified as HS

import "hashable" Data.Hashable ( Hashable )


failingQuoter :: String -> QuasiQuoter
failingQuoter name = QuasiQuoter
    { quoteExp  = fail "n expression"
    , quotePat  = fail " pattern"
    , quoteDec  = fail " declaration"
    , quoteType = fail " type"
    }
  where
    fail :: String -> a
    fail s = error $ name ++ " can not be applied as a" ++ s


data StringPart
    = Literal String
    | AntiQuote String
  deriving (Show, Eq, Ord, Generic )
instance Hashable StringPart
type FormatAst = [[StringPart]]

stringPart :: (String -> a) -> (String -> a) -> StringPart -> a
stringPart f _ (Literal a) = f a
stringPart _ f (AntiQuote a) = f a

lit, anti, var :: String -> StringPart
lit = Literal . reverse
anti = AntiQuote . reverse
var = AntiQuote

unFormatString :: String -> String -> [[StringPart]]
unFormatString [] []         = []
unFormatString a []          = [[lit a]]
unFormatString a ('\\':x:xs) = unFormatString (x:a) xs
unFormatString a "\\"        = unFormatString ('\\':a) []
unFormatString a ('}':xs)    = (anti a : b) : bs
  where
    (b:bs) = parseFormatString [] xs
unFormatString a (x:xs)      = unFormatString (x:a) xs

parseFormatString :: String -> String -> [[StringPart]]
parseFormatString [] []          = []
parseFormatString a []           = [[lit a]]
parseFormatString a ('\n':xs)    = [lit a] : parseFormatString "" xs
parseFormatString a ('\\':x:xs)  = parseFormatString (x:a) xs
parseFormatString a "\\"         = parseFormatString ('\\':a) []
parseFormatString a ('$':x:xs)
    | x == '_' || isAlpha x = [lit a, var (x:pre)] : parseFormatString [] post
    | x == '{' = (lit a : b) : bs
  where
    (b:bs) = unFormatString [] xs
    (pre, post) = span isIdent xs
    isIdent c = isAlphaNum c || c `elem` "_'"
parseFormatString a (x:xs)       = parseFormatString (x:a) xs


parseExp :: String -> ExpQ
parseExp = parseQ HS.parseExp [e| () |]

parsePat :: String -> PatQ
parsePat = parseQ HS.parsePat [p| () |]

parseDecs :: String -> Q [Dec]
parseDecs = parseQ HS.parseDecs $ return []

parseType :: String -> TypeQ
parseType = parseQ HS.parseType [t| () |]

parseQ :: (String -> Either String a) -> Q a -> String -> Q a
parseQ p f s = case p s of
    Left e  -> reportError e >> f
    Right t -> return t

symbol :: String -> TypeQ
symbol = return . LitT . StrTyLit

string :: String -> ExpQ
string = return . LitE . StringL

fromFormatAst
    :: (Q a -> Q a -> Q a)
    -> (Q a -> Q a -> Q a)
    -> (String -> Q a)
    -> (String -> Q a)
    -> FormatAst -> Q a
fromFormatAst cat catLines s q = foldl1' catLines . map (foldl1' cat . map (stringPart s q))
