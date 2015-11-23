#### yesod-csp

The aim of this library is to make it easy to add correct [Content Security Policy](http://content-security-policy.com/) headers to your responses. This reduces the risk of loading bad assets or scripts.

#### Using the data types

The following code:

```haskell
getHomeR :: Handler Html
getHomeR = do
  cspPolicy [ScriptSrc (Self :| []), StyleSrc (Https :| [Self])]
  defaultLayout [whamlet|hello|]
```

will ensure that a `Content-Security-Policy: script-src 'self'; style-src https: 'self'` header is set. In this example we only want to load scripts from our own domain, and we only want styles that come from our domain or over https.

This is a work in progress, not battle-hardened! Use with caution and confirm you're getting the results you need.

### Examples

[This module](https://github.com/bobjflong/yesod-csp/blob/master/src/Yesod/Csp/Example.hs) contains a host of runnable example Yesod handlers which set various CSP headers.

### Template Haskell support

I'm working on Template Haskell support so you don't need to write the ADTs yourself explicitly. You can get the same compile-time checking with the familar CSP DSL:

```haskell
getHomeR :: Handler Html
getHomeR = do
  cspPolicy [csp|img-src 'self' https:; script-src https://foo.com|]
  ...
```
