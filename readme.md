#### Yesod-Csp makes it easy to add a [Content Security Policy](http://content-security-policy.com/) to your responses. This reduces the risk of loading bad assets or scripts.

#### Example

The following code:

```haskell
getHomeR :: Handler Html
getHomeR = do
  cspPolicy [ScriptSrc [Self], StyleSrc [Https, Self]]
  defaultLayout [whamlet|hello|]
```

will ensure that a `Content-Security-Policy: script-src 'self'; style-src https: 'self'` header is set. In this example we only want to load scripts from our own domain, and we only want styles that come from our domain or over https.
