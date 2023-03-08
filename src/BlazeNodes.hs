{-# LANGUAGE NoImplicitPrelude #-} 
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingStrategies, DeriveAnyClass, DeriveDataTypeable, RankNTypes #-} 
{- |
Create a set of graph nodes for use with neo4j or memgraph, to understand the haskell Blaze & Shakespeare family of libraries.
-}
module BlazeNodes(createBlazeHtmlNodes) where

import RIO
import Lens.Micro.TH
import qualified RIO.Text as T

import Database.Bolt
import Data.Default

import Algebra.Graph hiding (connect)
import qualified Algebra.Graph as AG

import qualified Prelude as P

import  Data.Data(showConstr)

import NodeBuilder(ShowConstructor(showCon, showLowCon), create_, fxLabel, module_, typeLabel, createIsTypeRel, type_, type_IS_IN_MOD,
                  moduleLabel, package_, fxIsIn, moduleIsIn, pkgLabel, createReturnsRel, takes, fx_)
import TextNodes(createTextNodes, TextPackages(..), TextTypes(Text))

-- Create a common ADT to work with all nodes within Alga.
-- Not sure that I need it, as I don't have lists, the way I did with Electrical Generators
data BlazeTypes = Html
                | Markup
                | MarkupM
                   
  deriving (Show, Data)
  deriving anyclass (ShowConstructor)

data BlazeFunctions = Body
                     | H1
                     | RenderHtml
  deriving (Show, Data)
  deriving anyclass (ShowConstructor)

-- | Package names on hackage/stackage. 
-- Use double underscore __ to differentiate from module names.
data BlazePackages = Blaze__Html
              | Blaze__Svg 
          --    | Text__
  deriving (Show, Data)
  deriving anyclass (ShowConstructor)

data BlazeSvg = SVG -- The type
              | Svg -- The fx
  deriving (Show, Data)
  deriving anyclass (ShowConstructor)

--Modules on hackage/stackage
data Modules = Text_Blaze_Internal
             | Text_Blaze_Html
             | Text_Blaze_XHtml5
             | Text_Blaze_Svg_Internal
             | Text_Blaze_Svg11
             | Text_Blaze_Html_Renderer_Text
             --non blaze
          --   | Data_Text_Lazy
          --   | Data_Text_Lazy_Builder
  deriving (Show, Data)
  deriving anyclass (ShowConstructor)

--creates the Text nodes, Blaze Svg and the Blaze Html nodes
createBlazeHtmlNodes :: IO ()
createBlazeHtmlNodes = do
  pipe <- connect $ def { user = "neo4j", password = "neo546698" } 

  let 
    -- Generate cypher query text for Blaze Html node types
    createBlazeHtmlTypesCypherTxt ::  Text
    createBlazeHtmlTypesCypherTxt = 

      ----- create packages -----
      create_ (package_ Blaze__Html) [pkgLabel Blaze__Html] <>

      
      ----- create the main blaze types -----
      --MarkupM
      create_ (type_ MarkupM) [typeLabel MarkupM] <> 
      create_ (type_ Markup) [typeLabel Markup] <>
      createIsTypeRel Markup MarkupM <>
      create_ (type_ Html) [typeLabel Html] <> 

      createIsTypeRel Html Markup <>
       
      ----- create the blaze html functions -----
      create_ (fx_ Body) [fxLabel Body] <>
      createReturnsRel Body Html <>
      takes Body Html <>

      create_ (fx_ H1) [fxLabel H1] <>
      createReturnsRel H1 Html <>
      H1 `takes` Html <>

      create_ (fx_ RenderHtml) [fxLabel RenderHtml] <>
      RenderHtml `takes` Html <>
      createReturnsRel RenderHtml Text <>
      
      ------ create Blaze Modules -------
      --Text.Blaze.Internal
      create_ (module_ Text_Blaze_Internal) [moduleLabel Text_Blaze_Internal] <>
      Text_Blaze_Internal `moduleIsIn` Blaze__Html <>
      --Text.Blaze.Html
      create_ (module_ Text_Blaze_Html) [moduleLabel Text_Blaze_Html] <>
      Text_Blaze_Html `moduleIsIn` Blaze__Html <>
      --Text_Blaze_XHtml5
      create_ (module_ Text_Blaze_XHtml5) [moduleLabel Text_Blaze_XHtml5] <>
      Text_Blaze_XHtml5 `moduleIsIn` Blaze__Html <>
      --Blaze.Html.Renderer.Text
      create_ (module_ Text_Blaze_Html_Renderer_Text) [moduleLabel Text_Blaze_Html_Renderer_Text] <>
      Text_Blaze_Html_Renderer_Text `moduleIsIn` Blaze__Html <>
      
      
      ----- associate Types & Fx_'s with the modules -----
      MarkupM `type_IS_IN_MOD` Text_Blaze_Internal <>
      Markup  `type_IS_IN_MOD` Text_Blaze_Internal <>
      Html    `type_IS_IN_MOD` Text_Blaze_Html     <>
      Body    `fxIsIn` Text_Blaze_XHtml5     <>
      H1      `fxIsIn` Text_Blaze_XHtml5     <>
      RenderHtml `fxIsIn` Text_Blaze_Html_Renderer_Text <>

      -- package blaze-svg
      create_ (package_ Blaze__Svg) [pkgLabel Blaze__Svg] <>

      --Text.Blaze.Svg.Internal
      create_ (module_ Text_Blaze_Svg_Internal) [moduleLabel Text_Blaze_Svg_Internal] <>
      Text_Blaze_Svg_Internal `moduleIsIn` Blaze__Svg <>
      create_ (type_ SVG) [typeLabel SVG] <>
      SVG `type_IS_IN_MOD` Text_Blaze_Svg_Internal <>
      createIsTypeRel SVG Markup <>

      -- Text_Blaze_Svg11
      create_ (module_ Text_Blaze_Svg11) [moduleLabel Text_Blaze_Svg11] <>
      Text_Blaze_Svg11 `moduleIsIn` Blaze__Svg <>
      create_ (fx_ Svg) [fxLabel Svg] <>
      Svg `fxIsIn` Text_Blaze_Svg11


    createBlazeHtmlTypesQuery :: BoltActionT IO [Text]
    createBlazeHtmlTypesQuery = do
      records <- query $ createTextNodes <> createBlazeHtmlTypesCypherTxt 
      forM records $ \record -> record `at` ""
  _ <- run pipe $ createBlazeHtmlTypesQuery 
  close pipe  

