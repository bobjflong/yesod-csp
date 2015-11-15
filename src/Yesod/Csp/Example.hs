{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Assorted examples demonstrating different policies.
module Yesod.Csp.Example where

import           Data.List.NonEmpty
import           Data.Maybe
import           Network.URI
import           Yesod              hiding (get)
import           Yesod.Csp

data Example = Example
mkYesod "Example" [parseRoutes|
  /1 Example1R GET
  /2 Example2R GET
  /3 Example3R GET
  /4 Example4R GET
  /5 Example5R GET
|]

instance Yesod Example

-- | Allows scripts from self.
getExample1R :: Handler Html
getExample1R = do
  cspPolicy [ScriptSrc (Self :| [])]
  defaultLayout $ do
    addScriptRemote "http://httpbin.org/i_am_external"
    [whamlet|hello|]

-- | Allows all styles over https.
getExample2R :: Handler Html
getExample2R = do
  cspPolicy [StyleSrc (Https :| [])]
  defaultLayout $ do
    addStylesheetRemote "http://httpbin.org/i_am_not_https"
    [whamlet|hello|]

-- | Allows images from a certain uri.
getExample3R :: Handler Html
getExample3R = do
  let dom = fromJust (parseURI "http://httpbin.org")
  cspPolicy [ImgSrc (Host dom :| [])]
  defaultLayout $
    [whamlet|
      <img src="http://httpbin.org/image">
      <!-- different scheme should not work: -->
      <img src="https://httpbin.org/image">
    |]

-- | Allows all images.
getExample4R :: Handler Html
getExample4R = do
  cspPolicy [ImgSrc (Wildcard :| [])]
  defaultLayout $
    [whamlet|
      <img src="http://httpbin.org/image">
    |]

-- | Disallows images entirely.
getExample5R :: Handler Html
getExample5R = do
  cspPolicy [ImgSrc (None :| [])]
  defaultLayout $
    [whamlet|
      <img src="http://httpbin.org/image">
    |]

-- | Run a webserver to serve these examples at /1, /2, etc.
runExamples :: IO ()
runExamples = warp 4567 Example
