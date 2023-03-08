{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{- |
View the default page: http://localhost:3000/

Create system for building Html nodes, including adding parameter and content.

Shows examples of using my Html-Builder, with Tailwind.
-}
module HtmlBuilder(viewTailwindWebPage) where
import Web.Scotty (get, middleware, scotty)
import qualified Web.Scotty as S
import Network.Wai.Middleware.Static

import Data.Monoid (mconcat)

import qualified RIO.Text as T

import qualified Data.Text.Lazy.Builder as BL

-- Use Scotty to server an example of the use of my Html builder system.
-- Primarily uses the chapter 1 of 'Tailwind CSS' book. See link in this webpage.
viewTailwindWebPage = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "static")
  --get "/:word2" $ do
  get "" $ do
    S.html $ BL.toLazyText $ 
      docType <>
      head__ <>
      body__ -- $ BL.fromText "hello"
          

{-
starting template
<!doctype html>
<html>
<head>
  <meta charset="UTF-8" />
  <meta name="viewport" content="width=device-width, initial-scale=1.0" />
  <link href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css" rel="stylesheet">
  <script src="https://cdn.tailwindcss.com"></script>
</head>
<body>
  <!-- ... -->
</body>
</html>-}
head__ = head_ $ metaUtf9 <> viewPort  <> tailWindSource <> cloudFlareSource
body__ = body_ [AttribClass ["bg-orange-100"]] $ div_  [(AttribClass ["container", "mx-auto", "bg-orange-400"])] $ 
                  header_ [AttribClass ["flex","justify-between", "items-center", "sticky", "top-0"]] $
                   (div_ [(AttribClass ["flex-shrink-0","ml-6","cursor-pointer"])] $
                     i_ [(AttribClass ["fas","fa-wind","fa-2x","text-yellow-500"])] $
                       span_ [(AttribClass ["text-3xl", "font-semibold", "text-blue-200"])] 
                             "Tailwind School" )  <> 
                    (ul_ [(AttribClass ["flex", "mr-10", "font-semibold"])] $
                      (li_ [(AttribClass ["mr-6 p-1", "border-b-2", "border-yellow-500"])] $
                        a_ [AttribClass ["cursor-default", "text-blue-200"] , (AttribHref "#")] 
                           "Home"
                      ) <>
                      (li_ [(AttribClass ["mr-6 p-1"])] $
                         a_ [AttribClass ["text-blue", "text-blue-300"] , 
                             AttribHref "https://learning.oreilly.com/library/view/tailwind-css/9781098140984/Text/tailwind1-frontmatter.html"
                            ]
                            "Tailwind book on OReilly"
                      ) <>
                      (li_ [(AttribClass ["mr-6 p-1"])] $
                         a_ [AttribClass ["text-blue", "text-blue-300"] , 
                             AttribHref "https://tailwindcss.com/docs/background-color"
                            ]
                            "Tailwind Background Color"
                      )
                    )    
      {-<li class="mr-6 p-1">
        <a class="text-white hover:text-blue-300" href="#">Videos</a>
      </li>-}
                      


-- <script src="https://cdn.tailwindcss.com"></script>
tailWindSource :: BL.Builder
tailWindSource = "<script src='https://cdn.tailwindcss.com'></script>"

cloudFlareSource :: BL.Builder
cloudFlareSource = "<link href='https://cdnjs.cloudflare.com/ajax/libs/font-awesome/5.15.4/css/all.min.css' rel='stylesheet'>"


docType :: BL.Builder 
docType = BL.fromText "<!doctype html>"

metaUtf9 :: BL.Builder
metaUtf9 =  "<meta charset='UTF-8' />"

-- <meta name='viewport' content='width=device-width, initial-scale=1.0' />
viewPort :: BL.Builder
viewPort =  "<meta name='viewport' content='width=device-width, initial-scale=1.0' />"

head_ :: BL.Builder -> BL.Builder
head_ content = 
  --"<head>" <> content <>  "</head>"
  htmlTagBuilder "head" [] content

header_ :: [Attributes] -> BL.Builder -> BL.Builder
header_ attributes content =
  --"<header " <> (mconcat (map addAttribs attributes))  <> ">" <> content <> "</header>"
  htmlTagBuilder "header" attributes content

--does body never have attributes?.
body_ :: [Attributes] -> BL.Builder -> BL.Builder
body_ attributes content = 
  --"<body>" <> content <> "</body>" 
  htmlTagBuilder "body" attributes content

i_ :: [Attributes] -> BL.Builder -> BL.Builder
i_ attributes content =
  htmlTagBuilder "i" attributes content


div_ :: [Attributes] -> BL.Builder -> BL.Builder
div_ attributes  content = 
  htmlTagBuilder "div" attributes content

span_ :: [Attributes] -> BL.Builder -> BL.Builder
span_ attributes  content = 
  htmlTagBuilder "span" attributes content

ul_ :: [Attributes] -> BL.Builder -> BL.Builder
ul_ attributes  content = htmlTagBuilder "ul" attributes content

li_ :: [Attributes] -> BL.Builder -> BL.Builder
li_ attributes  content = htmlTagBuilder "li" attributes content

a_ :: [Attributes] -> BL.Builder -> BL.Builder
a_ attributes  content = htmlTagBuilder "a" attributes content

--all the html tag building fx's, such as div_ and span_ have a common pattern, extracted here.
htmlTagBuilder :: BL.Builder -> [Attributes] -> BL.Builder -> BL.Builder
htmlTagBuilder tagName attribClassBuilder  content = 
  "<" <> tagName <> " " <> (mconcat (map addAttribs attribClassBuilder))  <> ">" <> content <> "</" <> tagName <> ">"

-- | Attributes are added to html tags, such as size, img etc.
data Attributes = AttribClass [BL.Builder]
                | AttribHref  BL.Builder

addAttribs :: Attributes -> BL.Builder
addAttribs (AttribClass []) = "class=''"
addAttribs (AttribClass attribBuilders) = "class='" <> (mconcat $ map encloseAttrib attribBuilders) <> "'"
addAttribs (AttribHref attribBuilder) = "href='" <> attribBuilder <> "'"

encloseAttrib :: BL.Builder -> BL.Builder
encloseAttrib bldr = " " <> bldr <> " "
-------------------------------------------------------------------------------------------------------------------------
--Some beginning Text.Blaze.Svg11, keep it around for now.
{-myBlazeSvgCircle :: Svg
myBlazeSvgCircle = svg ! width "300" ! height "300" $ do
             circle ! cx "50" ! cy "50" ! r "20" ! stroke "green" ! strokeWidth "4" ! fill "yellow"
             circle ! cx "100" ! cy "100" ! r "30" ! stroke "blue" ! strokeWidth "6" ! fill "green"

-------------- create some css and see what it looks like
myCss = (toCss $ T.pack "hello ") <> (toCss colorRed) <> (toCss $ T.pack " world") 
showMyCss :: IO ()
showMyCss = print myCss
-}

{-
mainn = scotty 3000 $ do
  middleware $ staticPolicy (noDots >-> addBase "static")
  --get "/:word2" $ do
  get "" $ do
    S.html $ renderHtml $ 
      html $ do
        --but should not need style, but a class, and add CSS via Cassius
        body ! style "color:red" $ do 
          h1 $ preEscapedToHtml $ renderHtml myBlazeSvgCircle
          ---------------------------------------------
          p ! class_ "styled" $ em "Context here."

-}

----------------------------------------------------------------------------------------------