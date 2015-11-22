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
import           Data.List.NonEmpty        (NonEmpty (..))
import qualified Data.Text                 as T
import           Language.Haskell.TH.Quote
import           Yesod.Csp

csp :: QuasiQuoter
csp = QuasiQuoter {
      quoteExp = \str -> do
        let c = parseOnly directive (T.pack str)
        case c of
          Left err -> error $ "csp parsing error: " ++ err -- compile time error
          Right x -> dataToExpQ (const Nothing) x
    , quotePat  = undefined
    , quoteType = undefined
    , quoteDec  = undefined
    }

source :: Parser Source
source = wildcard
         <|> none
         <|> self
         <|> dataScheme
         <|> https
         <|> host
         <|> unsafeInline
         <|> unsafeEval
  where wildcard = string "*" *> pure Wildcard
        none = string "'none'" *> pure None
        self = string "'self'" *> pure Self
        dataScheme = string "data:" *> pure DataScheme
        host :: Parser Source
        host = do
          u <- takeTill (\x -> x == ';' || x == ' ')
          case escapeAndParseURI u of
            Nothing -> fail "Not a URI"
            Just uri -> return $ Host uri
        https = do
          _ <- string "https:"
          c <- peekChar
          case c of
            (Just ' ') -> return Https
            (Just ';') -> return Https
            Nothing -> return Https
            _ -> fail "Not a https marker"
        unsafeInline = string "unsafe-inline" *> pure UnsafeInline
        unsafeEval = string "unsafe-eval" *> pure UnsafeEval

-- Safe to head and tail these sources as they come from the `some` combinator
mkWithSource :: (NonEmpty Source -> Directive) -> [Source] -> Parser Directive
mkWithSource f x = pure $ f (head x :| tail x)

withSourceList :: Parser Directive
withSourceList = types
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
        s = string " "
        types = defaultSrc
                <|> scriptSrc
                <|> scriptSrc
                <|> styleSrc
                <|> imgSrc
                <|> connectSrc
                <|> fontSrc
                <|> objectSrc
                <|> mediaSrc
                <|> frameSrc

reportUri :: Parser Directive
reportUri = do
  _ <- string "report-uri"
  _ <- string " "
  u <- takeTill (\x -> x == ';' || x == ' ')
  case escapeAndParseURI u of
    Nothing -> fail "Not a Report URI" -- n.b. compile time error
    Just uri -> return $ ReportUri uri

sandbox :: Parser Directive
sandbox = do
  _ <- string "sandbox"
  _ <- string " "
  x <- sepBy sandboxOptions (char ' ')
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

directive :: Parser DirectiveList
directive = sepBy d (string "; ") <* endOfInput
  where d = withSourceList <|> reportUri <|> sandbox
