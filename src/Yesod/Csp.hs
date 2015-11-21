{-# LANGUAGE OverloadedStrings #-}
-- | Add <http://content-security-policy.com/ CSP> headers to Yesod apps.
-- This helps reduce the risk of exposure to XSS and bad assets.
module Yesod.Csp (
  cspPolicy
  , getCspPolicy
  , DirectiveList
  , Directive(..)
  , SourceList
  , Source(..)
  , SandboxOptions(..)
  ) where

import           Data.List.NonEmpty
import qualified Data.Sequences     as S
import           Data.Text
import           Network.URI
import           Yesod.Core

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
cspPolicy = addHeader "Content-Security-Policy" . directiveListToHeader

-- | Returns a generated Content-Security-Policy header.
getCspPolicy :: DirectiveList -> Text
getCspPolicy = directiveListToHeader

directiveListToHeader :: DirectiveList -> Text
directiveListToHeader = S.intercalate "; " . fmap textDirective

w :: Text -> SourceList -> Text
w = wrap

wrap :: Text -> SourceList -> Text
wrap k x = mconcat [k, " ", textSourceList x]

textSourceList :: SourceList -> Text
textSourceList = S.unwords . toList . filtered
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
              | Host URI
              | Https
              | UnsafeInline
              | UnsafeEval deriving (Eq)

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
                 -- | Applies a sandbox to the result. <http://content-security-policy.com/ See here> for more info.
                 | Sandbox [SandboxOptions]
                 | ReportUri URI

-- | Configuration options for the sandbox.
data SandboxOptions = AllowForms
                      | AllowScripts
                      | AllowSameOrigin
                      | AllowTopNavigation

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
textDirective (ReportUri t) = mconcat ["report-uri ", (pack . show) t]
textDirective (Sandbox []) = "sandbox"
textDirective (Sandbox s) = mconcat ["sandbox ", S.unwords . fmap textSandbox $ s]
  where textSandbox AllowForms = "allow-forms"
        textSandbox AllowScripts = "allow-scripts"
        textSandbox AllowSameOrigin = "allow-same-origin"
        textSandbox AllowTopNavigation = "allow-top-navigation"
