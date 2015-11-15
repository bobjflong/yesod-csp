{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import           Data.List.NonEmpty
import           Data.Maybe
import           Network.URI
import           Test.Hspec
import           Yesod              hiding (get)
import           Yesod.Csp
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
      assertEqual "simple header" header "script-src 'self'; style-src https: 'self'"
    yit "works with domains" $ do
      let dom = fromJust $ parseURI "https://foo.com"
          header = getCspPolicy [ScriptSrc (Host dom :| [])]
      assertEqual "foo.com script-src" header "script-src https://foo.com"
    yit "works with report_uri" $ do
      let dom = fromJust $ parseURI "https://foo.com"
          header = getCspPolicy [ReportUri dom]
      assertEqual "report-uri" header "report-uri https://foo.com"
    yit "enforces wildcards" $ do
      let header = getCspPolicy [ScriptSrc (Wildcard :| [Https])]
      assertEqual "* should be alone" header "script-src *"
    yit "enforces nones" $ do
      let header = getCspPolicy [ScriptSrc (None :| [Https])]
      assertEqual "none should be alone" header "script-src 'none'"
  ydescribe "Headers" $
    yit "get set" $ do
      get HomeR
      assertHeader "Content-Security-Policy" "script-src 'self'; style-src https: 'self'"
  ydescribe "Sandboxes" $ do
    yit "works when empty" $ do
      let header = getCspPolicy [Sandbox []]
      assertEqual "empty sandbox" header "sandbox"
    yit "works when not empty" $ do
      let header = getCspPolicy [Sandbox [AllowForms, AllowScripts]]
      assertEqual "empty sandbox" header "sandbox allow-forms allow-scripts"
