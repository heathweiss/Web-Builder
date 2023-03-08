{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass, DeriveDataTypeable, RankNTypes #-} 
{-
Create the cypher text to create the Text package, and it's modules.
-}
module TextNodes(createTextNodes, TextPackages(..), TextTypes(..)) where

import RIO
import Lens.Micro.TH
import qualified RIO.Text as T

import NodeBuilder(ShowConstructor(showCon, showLowCon), create_, fxLabel, module_, typeLabel, createIsTypeRel, type_, type_IS_IN_MOD,
                  moduleLabel, package_, fxIsIn, moduleIsIn, pkgLabel, createReturnsRel, takes, fx_,)

data TextPackages = Text__
  deriving (Show, Data)
  deriving anyclass (ShowConstructor)

data TextModules = Data_Text_Lazy
                 | Data_Text_Lazy_Builder
  deriving (Show, Data)
  deriving anyclass (ShowConstructor)

--Text based types that are used to build Css, Html, etc.
data TextTypes = Text
               | Builder
  deriving (Show, Data)
  deriving anyclass (ShowConstructor)

-- | Create the cypher text to generate the text package, modules, and types.
createTextNodes :: Text
createTextNodes =  
    --create text pkg
    create_ (package_ Text__) [pkgLabel Text__] <>
    --create modules, add to pkg
    create_ (module_ Data_Text_Lazy) [moduleLabel Data_Text_Lazy] <>
    Data_Text_Lazy `moduleIsIn` Text__ <>
    create_ (module_ Data_Text_Lazy_Builder) [moduleLabel Data_Text_Lazy_Builder] <>
    Data_Text_Lazy_Builder `moduleIsIn` Text__ <>
    --create types, add to modules
    create_ (type_  Text) [typeLabel Text] <>
    Text `type_IS_IN_MOD` Data_Text_Lazy <>
    create_ (type_ Builder) [typeLabel Builder] <>
    Builder `type_IS_IN_MOD` Data_Text_Lazy_Builder 
      
      