{-# LANGUAGE OverloadedStrings #-}

module Yesod.Csp.TH (
    source
    , withSourceList
    , reportUri
    , sandbox
    , sandboxOptions
    , directive
    , csp
  ) where

import           Control.Applicative
import           Data.Attoparsec.Text
import           Data.Generics
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.Text                 as T
import qualified Language.Haskell.TH       as TH
import           Language.Haskell.TH.Quote
import           Yesod.Csp

csp :: QuasiQuoter
csp = QuasiQuoter {
      quoteExp = \str -> do
        let c = parseOnly directive (T.pack str)
        case c of
          Left err -> error $ "csp parsing error: " ++ err -- compile time error
          Right x -> dataToExpQ (const Nothing `extQ` antiCsp) x
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

antiCsp :: Source -> Maybe (TH.Q TH.Exp)
antiCsp (MetaSource x) = Just . return $ TH.AppE (TH.ConE (TH.mkName "Host")) (TH.VarE (TH.mkName (T.unpack x)))
antiCsp _ = Nothing

metaSource :: Parser Source
metaSource = do
  _ <- char '$'
  x <- many (digit <|> letter)
  return $ MetaSource (T.pack x)

source :: Parser Source
source = wildcard
         <|> none
         <|> self
         <|> dataScheme
         <|> https
         <|> host
         <|> unsafeInline
         <|> unsafeEval
         <|> parseNonce
         <|> metaSource
  where wildcard = string "*" *> pure Wildcard
        none = string "'none'" *> pure None
        self = string "'self'" *> pure Self
        dataScheme = string "data:" *> pure DataScheme
        parseNonce :: Parser Source
        parseNonce = do
          _ <- char '\''
          _ <- string "nonce"
          _ <- char '-'
          n <- takeTill (== '\'')
          return $ nonce n
        host :: Parser Source
        host = do
          u <- takeTill separated
          case escapeAndParseURI u of
            Nothing -> fail "host"
            Just uri -> return $ Host uri
        https = do
          _ <- string "https:"
          c <- peekChar
          case c of
            (Just ' ') -> return Https
            (Just ';') -> return Https
            Nothing -> return Https
            _ -> fail "https"
        unsafeInline = string "unsafe-inline" *> pure UnsafeInline
        unsafeEval = string "unsafe-eval" *> pure UnsafeEval

separated :: Char -> Bool
separated x = x == ';' || x == ' '

-- Safe to head and tail these sources as they come from the `sepBy1` combinator
mkWithSource :: (NonEmpty Source -> Directive) -> [Source] -> Parser Directive
mkWithSource f x = pure $ f (head x :| tail x)

withSourceList :: Parser Directive
withSourceList = defaultSrc
                 <|> scriptSrc
                 <|> scriptSrc
                 <|> styleSrc
                 <|> imgSrc
                 <|> connectSrc
                 <|> fontSrc
                 <|> objectSrc
                 <|> mediaSrc
                 <|> frameSrc
  where defaultSrc = d "default-src" DefaultSrc
        scriptSrc = d "script-src" ScriptSrc
        styleSrc = d "style-src" StyleSrc
        imgSrc = d "img-src" ImgSrc
        connectSrc = d "connect-src" ConnectSrc
        fontSrc = d "font-src" FontSrc
        objectSrc = d "object-src" ObjectSrc
        mediaSrc = d "media-src" MediaSrc
        frameSrc = d "frame-src" FrameSrc
        d x y = string x >> s >> slist >>= mkWithSource y
        slist = sepBy1 source (char ' ')
        s = spaces

spaces :: Parser ()
spaces = many space *> pure ()

reportUri :: Parser Directive
reportUri = do
  _ <- string "report-uri"
  _ <- spaces
  u <- takeTill separated
  case escapeAndParseURI u of
    Nothing -> fail "reportUri" -- n.b. compile time error
    Just uri -> return $ ReportUri uri

sandbox :: Parser Directive
sandbox = do
  _ <- string "sandbox"
  _ <- spaces
  x <- sepBy sandboxOptions spaces
  return $ Sandbox x

sandboxOptions :: Parser SandboxOptions
sandboxOptions = allowForms
                 <|> allowScripts
                 <|> allowSameOrigin
                 <|> allowTopNavigation
  where allowForms = string "allow-forms" *> pure AllowForms
        allowScripts = string "allow-scripts" *> pure AllowScripts
        allowSameOrigin = string "allow-same-origin" *> pure AllowSameOrigin
        allowTopNavigation = string "allow-top-navigation" *> pure AllowTopNavigation

separator :: Parser ()
separator = comma *> (spaces *> pure ())
  where comma = string ";"

directive :: Parser DirectiveList
directive = sepBy d separator <* endOfInput
  where d = withSourceList <|> reportUri <|> sandbox
