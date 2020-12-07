{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Page (Route(..), renderPage) where

import Utils
import Lucid
import Data.Time
import Control.Monad
import System.FilePath
import Style (pageStyle)
import GHC.Generics (Generic)
import Data.Text hiding (take)
import Rib (IsRoute(..), Pandoc)
import Data.Aeson (FromJSON, fromJSON)

import qualified Clay as C
import qualified Data.Aeson as Aeson
import qualified Rib.Parser.Pandoc as Pandoc

-- | Route corresponding to each generated static page.
--
-- The `a` parameter specifies the data (typically Markdown document) used to
-- generate the final page text.
data Route a where
  Route_root :: Route ()
  Route_Index :: Route [(Day, Route Pandoc, Pandoc)]
  Route_Posts :: Route [(Day, Route Pandoc, Pandoc)]
  Route_Article :: FilePath -> Route Pandoc

-- | The `IsRoute` instance allows us to determine the target .html path for
-- each route. This affects what `routeUrl` will return.
instance IsRoute Route where
  routeFile = \case
    Route_root ->
      pure "."
    Route_Index ->
      pure "index.html"
    Route_Posts ->
      pure "posts.html"
    Route_Article srcPath ->
      pure $ "article" </> srcPath -<.> ".html"

-- | Define your site HTML here
renderPage :: Route a -> a -> Html ()
renderPage route val = html_ [lang_ "en"] $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [httpEquiv_ "x-ua-compatible", content_ "ie=edge"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        title_ routeTitle
        style_ [type_ "text/css"] $ C.render pageStyle
        link_ [rel_ "stylesheet", href_ $ append (Route_root `relativeTo` route) "/static/style.css"]
        script_ [src_ $ append (Route_root `relativeTo` route) "/static/script.js"] (""::Text)
    body_ $ do
        div_ [class_ "container"] $ do
            div_ [class_ "inner"] $ do
                a_ [href_ $ Route_Index `relativeTo` route] "Home"
                " - "
                a_ [href_ $ Route_Posts `relativeTo` route] "Posts"
                content
            footer_ "EOF"

    where
        topTitle = "Parseltongue Speech"
        prefix = append topTitle " - "
        articleTitle v = title $ getMeta v
        routeTitle :: Html ()
        routeTitle = case route of
            Route_Index -> toHtml $ append prefix "Home"
            Route_Posts -> toHtml $ append prefix "Posts"
            Route_Article _ -> toHtml $ append prefix $ articleTitle val
        renderMarkdown :: Text -> Html ()
        renderMarkdown =
            Pandoc.render . Pandoc.parsePure Pandoc.readMarkdown
        generatePages :: [(Day, Route Pandoc, Pandoc)] -> Html ()
        generatePages articles = div_ $ ul_ $ forM_ articles $ \(date, r, src) ->
                                    li_ [class_ "pages"] $ do
                                        let meta = getMeta src
                                        toHtml $ (pack $ show date) `append` " -> "
                                        a_ [href_ $ r `relativeTo` route] $ toHtml $ strip (title meta)
                                        renderMarkdown `mapM_` description meta
        content :: Html ()
        content = case route of
                    Route_Index -> do
                        header_ $ h1_ $ toHtml topTitle 
                        h1_ "About Here"
                        p_ "放点瞎写的文档，可能会有一些值得读的东西(=・ω・=)"
                        h1_ "About Me"
                        p_ "学生，喜折腾，最近感兴趣的主题有NixOS、Haskell、Java"
                        h1_ "Links"
                        ul_ $ li_ $ a_ [href_ "https://github.com/Ninlives"] "Github"
                        h1_ "Recent Posts"
                        generatePages (take 10 val)
                    Route_Posts -> do
                        header_ $ h1_ "Posts"
                        generatePages val
                    Route_Article _ -> do
                        header_ $ h1_ $ toHtml $ articleTitle val
                        article_ $ do
                            Pandoc.getToC val
                            Pandoc.render val


-- | Metadata in our markdown sources
data SrcMeta
  = SrcMeta
      { title :: Text,
        -- | Description is optional, hence `Maybe`
        description :: Maybe Text
      }
  deriving (Show, Eq, Generic, FromJSON)

-- | Get metadata from Markdown's YAML block
getMeta :: Pandoc -> SrcMeta
getMeta src = case Pandoc.extractMeta src of
  Nothing -> error "No YAML metadata"
  Just (Left e) -> error $ unpack e
  Just (Right val) -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v

