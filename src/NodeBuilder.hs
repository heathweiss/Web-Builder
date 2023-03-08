{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass, DeriveDataTypeable, RankNTypes #-} 
{- | Functions to build nodes for neo4j or memgraph-}
module NodeBuilder(ShowConstructor(..), create_, fxLabel, module_, typeLabel, createIsTypeRel, type_, type_IS_IN_MOD, moduleLabel, package_,
                   fxIsIn, moduleIsIn, pkgLabel, createReturnsRel, takes, fx_) where

import RIO
import Lens.Micro.TH
import qualified RIO.Text as T

import  Data.Data(showConstr)

-- | Show pretty Constructors.
class (Typeable a, Data a) => ShowConstructor a where
  -- * Show the constructor is same case as declared. When it is a type such as Html.
  showCon ::  a -> T.Text
  default showCon ::  a -> T.Text
  showCon a = T.pack $ showConstr . toConstr $ a
  -- * Show the contructor in lower case, as needed when it represents a function.
  showLowCon :: a -> T.Text
  default showLowCon :: a -> T.Text
  showLowCon a = T.toLower $ showCon a   --T.pack $ showConstr $ toConstr a



-- Add the label attribute text to a cyper create query. 
-- Uses initial uppercase for types.
addHighLabelAttr :: forall a. ShowConstructor a => a -> T.Text
addHighLabelAttr constructor = "label:'" <> showCon constructor <> "' "
-- Add the label attribute text to a cyper create query. 
-- Uses lower case for fx_'s. 
addLowLabelAttr :: forall a. ShowConstructor a => a -> T.Text
addLowLabelAttr constructor = "label:'" <> showLowCon constructor <> "' "

pkgLabel :: forall a. ShowConstructor a => a -> T.Text
pkgLabel = addHighLabelAttr

typeLabel :: forall a. ShowConstructor a => a -> T.Text
typeLabel = addHighLabelAttr

fxLabel :: forall a. ShowConstructor a => a -> T.Text
fxLabel = addLowLabelAttr

moduleLabel :: forall a. ShowConstructor a => a -> T.Text
moduleLabel = addHighLabelAttr

addTypeAttr :: T.Text -> T.Text
addTypeAttr myType = " type:'" <> myType <> "' "

--Create an IS_IN relations, where intial type is in lowercase, for functions.
createLowIsInRelation :: forall a b. (ShowConstructor a, ShowConstructor b) => a -> b -> T.Text
createLowIsInRelation = 
  createLowHighRelBase "IS_IN"

--Create Type IS_IN Module relation.
-- 
type_IS_IN_MOD :: forall a b. (ShowConstructor a, ShowConstructor b) => a -> b -> T.Text
type_IS_IN_MOD = createHighHighRelBase "IS_IN_MOD"

--typeIsIn :: forall a b. (ShowConstructor a, ShowConstructor b) => a -> b -> T.Text
--typeIsIn = type_IS_IN_MOD

moduleIsIn :: forall a b. (ShowConstructor a, ShowConstructor b) => a -> b -> T.Text
moduleIsIn = type_IS_IN_MOD

fxIsIn :: forall a b. (ShowConstructor a, ShowConstructor b) => a -> b -> T.Text
fxIsIn = createLowIsInRelation


--creates a RETURNS relationship. iReturn is a fx_ so is lower case, and iAmReturned is a Type, so initial capital.
createReturnsRel :: forall a b. (ShowConstructor a, ShowConstructor b) => a -> b -> T.Text
createReturnsRel =
  createLowHighRelBase "RETURNS"  

--create a TAKES relation. iTake wil always be a fx_, so is lowercase, and takesMe will be a Type, so is initial capital.
takes :: forall a b. (ShowConstructor a, ShowConstructor b) => a -> b -> T.Text
takes =
  createLowHighRelBase "TAKES"

--Base fx_ to create a relation wher iAmIn is shown in lower case, and isIn is shown with intial capital.
--eg:  " CREATE ((h1)-[:IS_IN]-> (Text_Blaze_Internal))
createLowHighRelBase :: forall a b. (ShowConstructor a, ShowConstructor b) => T.Text -> a -> b -> T.Text
createLowHighRelBase relTxt iAmIn isIn  =
  " CREATE ((" <> showLowCon iAmIn <> ")-[:" <> relTxt <> "]-> (" <> showCon isIn <> "))"

createIsTypeRel :: forall a b. (ShowConstructor a, ShowConstructor b) => a -> b -> T.Text
createIsTypeRel = createHighHighRelBase "IS_TYPE"

createHighHighRelBase :: forall a b. (ShowConstructor a, ShowConstructor b) => T.Text -> a -> b -> T.Text
createHighHighRelBase relTxt iAmIn isIn =
  " CREATE ((" <> showCon iAmIn <> ")-[:" <> relTxt <> "]-> (" <> showCon isIn <> "))"

--create the node ident and type in lowercase for fx_'s. eg: "h1:h1"
createLowIdType :: forall a. (ShowConstructor a) => a -> T.Text
createLowIdType constructor =
  showLowCon constructor <> ":" <> showLowCon constructor 

--create the node ident and type in uppercase for fx_'s. eg: "Html:Html"
createHiId :: forall a. (ShowConstructor a) => a -> T.Text
createHiId constructor =
  showCon constructor <> ":" <> showCon constructor 

package_ :: forall a. (ShowConstructor a) => a -> T.Text
package_ constructor = showCon constructor <> ":Package"

module_ :: forall a. (ShowConstructor a) => a -> T.Text
module_ constructor = showCon constructor <> ":Module"

type_ :: forall a. (ShowConstructor a) => a -> T.Text
type_ constructor = showCon constructor <> ":Type"

fx_ :: forall a. (ShowConstructor a) => a -> T.Text
fx_ constructor = showLowCon constructor <> ":Function"

create_ :: T.Text -> [T.Text] -> T.Text
create_ t attribs = 
  " CREATE (" <> t <> addAttributes attribs <> ")"

addAttributes :: [T.Text] -> T.Text
addAttributes [] = ""
addAttributes (a:attribs) =
  " { " <> a <> base attribs "" <> " }"
  where
  base :: [T.Text] -> T.Text -> T.Text
  base [] workingTxt = workingTxt
  base (b:bttribs) workingTxt =  base bttribs (workingTxt <> ", " <> b)  
