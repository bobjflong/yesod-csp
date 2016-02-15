{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
-- | Assorted examples demonstrating different policies.
module Yesod.Csp.Example where

import           Data.Generics.Uniplate.Data
import           Data.List.NonEmpty
import           Data.Maybe
import           Yesod                       hiding (get)
import           Yesod.Csp
import           Yesod.Csp.TH

data Example = Example
mkYesod "Example" [parseRoutes|
  /1 Example1R GET
  /2 Example2R GET
  /3 Example3R GET
  /4 Example4R GET
  /5 Example5R GET
  /6 Example6R GET
  /7 Example7R GET POST
  /8 Example8R GET
  /9 Example9R GET
  /10 Example10R GET
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
  let dom = fromJust (escapeAndParseURI "http://httpbin.org")
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

-- | Blocks forms from being submitted
getExample6R :: Handler Html
getExample6R = do
  cspPolicy [Sandbox []]
  defaultLayout $
    [whamlet|
      <form method="post">
        <input type="submit">
    |]

getExample7R :: Handler Html
getExample7R = do
  cspPolicy [Sandbox [AllowForms]]
  defaultLayout $
    [whamlet|
      <form method="post">
        <input type="submit">
    |]

postExample7R :: Handler Html
postExample7R = do
  cspPolicy [Sandbox [AllowForms]]
  defaultLayout $
    [whamlet|yayyy|]

cdn :: Source
cdn = Host (fromJust $ escapeAndParseURI "https://cdn.com")

getExample8R :: Handler Html
getExample8R = do
  let policy = [csp|script-src 'self'|]
  cspPolicy $ addCdn <$> policy
  defaultLayout [whamlet|wooo|]
  where addCdn = transform f
        f (ScriptSrc x) = ScriptSrc $ cdn <| x
        f x = x

getExample9R :: Handler Html
getExample9R = do
  cspPolicy [csp|script-src 'nonce-foo'|]
  defaultLayout $ [whamlet|
    <script nonce="foo">
      alert("ayyyy");
  |]

getExample10R :: Handler Html
getExample10R = do
  let n = "foo"
  cspPolicy [csp|script-src $nonce-n|]
  defaultLayout $ [whamlet|
    <script nonce="foo">
      alert("ayyyy");
  |]
  
-- | Run a webserver to serve these examples at /1, /2, etc.
runExamples :: IO ()
runExamples = warp 4567 Example
