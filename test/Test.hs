{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Data.Attoparsec.Text
import           Data.List.NonEmpty
import           Data.Maybe
import           Test.Hspec
import           Yesod                hiding (get)
import           Yesod.Csp
import           Yesod.Csp.TH
import           Yesod.Test

data Test = Test
mkYesod "Test" [parseRoutes| / HomeR GET |]

instance Yesod Test

getHomeR :: Handler Html
getHomeR = do
  cspPolicy [ScriptSrc (Self :| []), StyleSrc (Https :| [Self])]
  defaultLayout [whamlet|hello|]

main :: IO ()
main = hspec $ yesodSpec Test $ do
  ydescribe "Generation" $ do
    yit "works" $ do
      let header = getCspPolicy [ScriptSrc (Self :| []), StyleSrc (Https :| [Self])]
      assertEq "simple header" header "script-src 'self'; style-src https: 'self'"
    yit "works with domains" $ do
      let dom = fromJust $ escapeAndParseURI "https://foo.com/bar *"
          header = getCspPolicy [ScriptSrc (Host dom :| [])]
      assertEq "foo.com script-src" header "script-src https://foo.com/bar%20%2A"
    yit "works with nonce-source" $ do
      let header = getCspPolicy [ScriptSrc (nonce "foo" :| [])]
      assertEq "basic nonce-source" header "script-src 'nonce-foo'"
    yit "works with report_uri" $ do
      let dom = fromJust $ escapeAndParseURI "https://foo.com"
          header = getCspPolicy [ReportUri dom]
      assertEq "report-uri" header "report-uri https://foo.com"
    yit "enforces wildcards" $ do
      let header = getCspPolicy [ScriptSrc (Wildcard :| [Https])]
      assertEq "* should be alone" header "script-src *"
    yit "enforces nones" $ do
      let header = getCspPolicy [ScriptSrc (None :| [Https])]
      assertEq "none should be alone" header "script-src 'none'"
  ydescribe "Headers" $
    yit "get set" $ do
      get HomeR
      assertHeader "Content-Security-Policy" "script-src 'self'; style-src https: 'self'"
  ydescribe "Sandboxes" $ do
    yit "works when empty" $ do
      let header = getCspPolicy [Sandbox []]
      assertEq "empty sandbox" header "sandbox"
    yit "works when not empty" $ do
      let header = getCspPolicy [Sandbox [AllowForms, AllowScripts]]
      assertEq "empty sandbox" header "sandbox allow-forms allow-scripts"
  ydescribe "Parsing" $ do
    yit "works" $ do
      assertEq "*" (parseOnly source "*") (Right Wildcard)
      assertEq "nonce" (parseOnly source "'nonce-foo'") (Right $ nonce "foo")
      assertEq "none" (parseOnly source "'none'") (Right None)
      assertEq "self" (parseOnly source "'self'") (Right Self)
      assertEq "data:" (parseOnly source "data:") (Right DataScheme)
      assertEq "https://foo.com" (parseOnly source "https://foo.com") (Right $ Host (fromJust (escapeAndParseURI "https://foo.com")))
      assertEq "https:" (parseOnly source "https:") (Right Https)
      assertEq "unsafe-inline" (parseOnly source "unsafe-inline") (Right UnsafeInline)
      assertEq "unsafe-eval" (parseOnly source "unsafe-eval") (Right UnsafeEval)
      assertEq "default-src self data:" (parseOnly withSourceList "default-src 'self' data:") (Right $ DefaultSrc (Self :| [DataScheme]))
      assertEq "report-uri http://hello.com" (parseOnly reportUri "report-uri http://hello.com") (Right $ ReportUri (fromJust (escapeAndParseURI "http://hello.com")))
      assertEq "sandbox allow-forms allow-scripts" (parseOnly sandbox "sandbox allow-forms allow-scripts") (Right $ Sandbox [AllowForms, AllowScripts])
    yit "works with lists" $ do
      let result = [ImgSrc $ Self :| [Https], ScriptSrc $ Host (fromJust $ escapeAndParseURI "https://foo.com") :| []]
      assertEq "scripts and images" (parseOnly directive "img-src 'self' https:; script-src https://foo.com") (Right result)
      let result = [ImgSrc $ Self :| [DataScheme, Host (fromJust $ escapeAndParseURI "https://foo.com")]]
      assertEq "data and hosts" (parseOnly directive "img-src 'self' data: https://foo.com") (Right result)
    yit "works with nonces and th" $ do
      let result = [ScriptSrc $ (nonce "foo") :| []]
      assertEq "nonces and th" [csp|script-src 'nonce-foo'|] result
    yit "works with dynamic nonces and th" $ do
      let n = "bar"
          result = [ScriptSrc $ (nonce "bar") :| []]
      assertEq "dynamic nonces and th" [csp|script-src $nonce-n|] result
    yit "works with th" $ do
      let result = [ImgSrc $ Self :| [Https], ScriptSrc $ Host (fromJust $ escapeAndParseURI "https://foo.com") :| []]
      assertEq "with th" [csp|img-src 'self' https:;  script-src https://foo.com|] result
      let url = fromJust $ escapeAndParseURI "https://foo.com"
      assertEq "with antiquoting" [csp|img-src 'self' https:; script-src $url|] result
