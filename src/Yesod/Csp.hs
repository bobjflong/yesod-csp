{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings  #-}
-- | Add <http://content-security-policy.com/ CSP> headers to Yesod apps.
-- This helps reduce the risk of exposure to XSS and bad assets.
module Yesod.Csp (
  cspPolicy
  , getCspPolicy
  , cspMiddleware
  , EscapedURI
  , escapeAndParseURI
  , escapedTextForNonce
  , nonce
  , DirectiveList
  , Directive(..)
  , SourceList
  , Source(..)
  , SandboxOptions(..)
  , textSource
  ) where

import qualified Data.CaseInsensitive as CI
import           Data.Data          (Data)
import           Data.List.NonEmpty
import           Data.Text
import qualified Data.Text          as T
import qualified Data.Text.Encoding as TE
import           Data.Typeable      (Typeable)
import           Network.URI
import           Yesod.Core
import           Network.Wai        (Middleware, mapResponseHeaders,
                                     modifyResponse)

-- | Adds a "Content-Security-Policy" header to your response.
--
-- > getExample1R :: Handler Html
-- > getExample1R = do
-- >   -- only allow scripts from my website
-- >   cspPolicy [ScriptSrc (Self :| [])]
-- >   defaultLayout $ do
-- >     addScriptRemote "http://httpbin.org/i_am_external"
-- >     [whamlet|hello|]
--
cspPolicy :: (MonadHandler m) => DirectiveList -> m ()
cspPolicy = addHeader cspHeaderName . directiveListToHeader

cspHeaderName :: Text
cspHeaderName = "Content-Security-Policy"

-- | Returns a generated Content-Security-Policy header.
getCspPolicy :: DirectiveList -> Text
getCspPolicy = directiveListToHeader

-- | Creates a WAI 'Middleware' to add a Content-Security-Policy
-- header to every response.
cspMiddleware :: DirectiveList -> Middleware
cspMiddleware = addHeaderMiddleware . mkHeader . directiveListToHeader
  where
    addHeaderMiddleware = modifyResponse . mapResponseHeaders . insertAt 5
    mkHeader dltext = (cspHeaderNameBS, TE.encodeUtf8 dltext)
    cspHeaderNameBS = CI.mk $ TE.encodeUtf8 cspHeaderName

insertAt :: Int -> a -> [a] -> [a]
insertAt n x xs =
  let (h, t) = Prelude.splitAt n xs
  in h ++ x : t

newtype EscapedURI = EscapedURI { uri :: URI } deriving (Eq, Data, Typeable)

newtype EscapedText = EscapedText { text :: String } deriving (Eq, Data, Typeable)

instance Show EscapedURI where
  show x = show (uri x)

instance Show EscapedText where
  show x = mconcat ["'nonce-", text x, "'"]

toEscape :: String
toEscape = ";'* "

notEscapable :: Char -> Bool
notEscapable = not . flip elem toEscape

-- | Escapes ';' '\'' and ' ', and parses to URI
escapeAndParseURI :: Text -> Maybe EscapedURI
escapeAndParseURI = fmap EscapedURI . parseURI . escapeURIString notEscapable . unpack

-- | Escapes Text to be a valid nonce value
escapedTextForNonce :: String -> EscapedText
escapedTextForNonce = EscapedText . Prelude.filter notEscapable

-- | Escapes a Text value, returning a valid Nonce
nonce :: Text -> Source
nonce = Nonce . escapedTextForNonce . unpack

directiveListToHeader :: DirectiveList -> Text
directiveListToHeader = intercalate "; " . fmap textDirective

w :: Text -> SourceList -> Text
w = wrap

wrap :: Text -> SourceList -> Text
wrap k x = mconcat [k, " ", textSourceList x]

textSourceList :: SourceList -> Text
textSourceList = T.unwords . toList . filtered
  where filtered = fmap textSource . filterOut

-- * and none should be alone if present
filterOut :: SourceList -> SourceList
filterOut x | Wildcard `elem` x = Wildcard :| []
filterOut x | None `elem` x = None :| []
            | otherwise = x

-- | Represents a location from which assets may be loaded.
data Source = Wildcard
              | None
              | Self
              | DataScheme
              | Host EscapedURI
              | Https
              | UnsafeInline
              | UnsafeEval
              | Nonce EscapedText
              | MetaSource Text deriving (Eq, Show, Data, Typeable)

-- | A list of allowed sources for a directive.
type SourceList = NonEmpty Source

textSource :: Source -> Text
textSource Wildcard = "*"
textSource None = "'none'"
textSource Self = "'self'"
textSource DataScheme = "data:"
textSource (Host x) = (pack . show) x
textSource Https = "https:"
textSource UnsafeInline = "'unsafe-inline'"
textSource UnsafeEval = "'unsafe-eval'"
textSource (MetaSource _) = ""
textSource (Nonce x) = (pack . show) x

-- | A list of restrictions to apply.
type DirectiveList = [Directive]

-- | A restriction on how assets can be loaded.
-- For example @ImgSrc@ concerns where images may be loaded from.
data Directive = DefaultSrc SourceList
                 | ScriptSrc SourceList
                 | StyleSrc SourceList
                 | ImgSrc SourceList
                 | ConnectSrc SourceList
                 | FontSrc SourceList
                 | ObjectSrc SourceList
                 | MediaSrc SourceList
                 | FrameSrc SourceList
                 | FrameAncestors SourceList
                 -- | Applies a sandbox to the result. <http://content-security-policy.com/ See here> for more info.
                 | Sandbox [SandboxOptions]
                 | ReportUri EscapedURI deriving (Eq, Show, Data, Typeable)

-- | Configuration options for the sandbox.
data SandboxOptions = AllowForms
                      | AllowScripts
                      | AllowSameOrigin
                      | AllowTopNavigation deriving (Eq, Show, Data, Typeable)

textDirective :: Directive -> Text
textDirective (DefaultSrc x) = w "default-src" x
textDirective (ScriptSrc x) =  w "script-src" x
textDirective (StyleSrc x) =  w "style-src" x
textDirective (ImgSrc x) =  w "img-src" x
textDirective (ConnectSrc x) =  w "connect-src" x
textDirective (FontSrc x) =  w "font-src" x
textDirective (ObjectSrc x) =  w "object-src" x
textDirective (MediaSrc x) =  w "media-src" x
textDirective (FrameSrc x) =  w "frame-src" x
textDirective (FrameAncestors x) =  w "frame-ancestors" x
textDirective (ReportUri t) = mconcat ["report-uri ", (pack . show) t]
textDirective (Sandbox []) = "sandbox"
textDirective (Sandbox s) = mconcat ["sandbox ", T.unwords . fmap textSandbox $ s]
  where textSandbox AllowForms = "allow-forms"
        textSandbox AllowScripts = "allow-scripts"
        textSandbox AllowSameOrigin = "allow-same-origin"
        textSandbox AllowTopNavigation = "allow-top-navigation"
