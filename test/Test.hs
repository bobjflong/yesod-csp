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
      assertEqual "Simple header" header "script-src 'self'; style-src https: 'self'"
    yit "works with domains" $ do
      let dom = fromJust $ parseURI "https://foo.com"
          header = getCspPolicy [ScriptSrc (Host dom :| [])]
      assertEqual "foo.com script-src" header "script-src https://foo.com"
    yit "works with report_uri" $ do
      let dom = fromJust $ parseURI "https://foo.com"
          header = getCspPolicy [ReportUri dom]
      assertEqual "report-uri" header "report-uri https://foo.com"
  ydescribe "Headers" $
    yit "get set" $ do
      get HomeR
      assertHeader "Content-Security-Policy" "script-src 'self'; style-src https: 'self'"
