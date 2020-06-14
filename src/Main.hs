{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lucid
import Data.List
import Main.Utf8
import Data.Time
import Text.Pandoc
import Text.Regex.PCRE
import System.FilePath
import Development.Shake
import Page (Route(..), renderPage)

import qualified Rib
import qualified Rib.Parser.Pandoc as Pandoc

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

