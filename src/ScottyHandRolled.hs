{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-
Produce Html, Css, Javascript, Svg in the same manner I did with Blaze nodes.
Right fx's that take [attributes] & return text and build evertything with a Lazy Builder, which is what cassius uses.
-}
module ScottyHandRolled() where

import Web.Scotty (get, middleware, scotty)
import qualified Web.Scotty as S
import Network.Wai.Middleware.Static

import RIO

main = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "static")
  --get "/:word2" $ do
  get "" $ do
    S.html "fjsdkl"
          