{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Style (pageStyle)
import qualified Clay as C
import Control.Monad
import Data.Aeson (FromJSON, fromJSON)
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Text (Text, append, pack)
import Development.Shake
import GHC.Generics (Generic)
import Lucid
import Main.Utf8
import Rib (IsRoute, Pandoc)
import qualified Rib
import qualified Rib.Parser.Pandoc as Pandoc
import Text.Pandoc
import Text.Pandoc.Extensions
import System.FilePath
import Text.Regex.PCRE
import Data.Time
import Data.List

-- | Route corresponding to each generated static page.
--
-- The `a` parameter specifies the data (typically Markdown document) used to
-- generate the final page text.
data Route a where
  Route_Index :: Route [(Day, Route Pandoc, Pandoc)]
  Route_Posts :: Route [(Day, Route Pandoc, Pandoc)]
  Route_Article :: FilePath -> Route Pandoc

-- | The `IsRoute` instance allows us to determine the target .html path for
-- each route. This affects what `routeUrl` will return.
instance IsRoute Route where
  routeFile = \case
    Route_Index ->
      pure "index.html"
    Route_Posts ->
      pure "posts.html"
    Route_Article srcPath ->
      pure $ "article" </> srcPath -<.> ".html"

-- | Main entry point to our generator.
--
-- `Rib.run` handles CLI arguments, and takes three parameters here.
--
-- 1. Directory `content`, from which static files will be read.
-- 2. Directory `dest`, under which target files will be generated.
-- 3. Shake action to run.
--
-- In the shake action you would expect to use the utility functions
-- provided by Rib to do the actual generation of your static site.
main :: IO ()
main = withUtf8 $ do
  Rib.run "content" "docs" generateSite

-- | Shake action for generating the static site
generateSite :: Action ()
generateSite = do
  -- Copy over the static files
    Rib.buildStaticFiles ["static/**"]
    let writeHtmlRoute :: Route a -> a -> Action ()
        writeHtmlRoute r = Rib.writeRoute r . Lucid.renderText . renderPage r
    -- Build individual sources, generating .html for each.
    articles <-
        Rib.forEvery ["*.md"] $ \srcPath -> do
            let r = Route_Article srcPath
                date :: Day
                date = case takeFileName srcPath =~ ("^\\d{4}-\\d{2}-\\d{2}" :: String) of
                         "" -> parseDate "1970-01-01"
                         dt -> parseDate dt
                       where parseDate = parseTimeOrError True defaultTimeLocale "%Y-%m-%d"
            doc <- Pandoc.parse readMarkdown srcPath
            writeHtmlRoute r doc
            pure (date, r, doc)
    let sortedArticles = sortBy (\(a,_,_) (b,_,_) -> compare b a) articles
    writeHtmlRoute Route_Index sortedArticles
    writeHtmlRoute Route_Posts sortedArticles
    where readMarkdown _ = Pandoc.readMarkdown $ def { readerExtensions = getDefaultExtensions "markdown" }

-- | Define your site HTML here
renderPage :: Route a -> a -> Html ()
renderPage route val = html_ [lang_ "en"] $ do
    head_ $ do
        meta_ [charset_ "utf-8"]
        meta_ [httpEquiv_ "x-ua-compatible", content_ "ie=edge"]
        meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
        title_ routeTitle
        -- style_ [type_ "text/css"] $ styleToCss zenburn
        style_ [type_ "text/css"] $ C.render pageStyle
    body_ $ do
        div_ [class_ "container"] $ do
            div_ [class_ "inner"] $ do
                a_ [href_ "/index.html"] "Home"
                " - "
                a_ [href_ "/posts.html"] "Posts"
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
                                        a_ [href_ (Rib.routeUrlRel r)] $ toHtml $ title meta
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
  Just (Left e) -> error $ T.unpack e
  Just (Right val) -> case fromJSON val of
    Aeson.Error e -> error $ "JSON error: " <> e
    Aeson.Success v -> v

