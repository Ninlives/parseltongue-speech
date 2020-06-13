{-# LANGUAGE OverloadedStrings #-}

module Style where

import Prelude hiding ((**), rem, replicate, not)
import Clay hiding (type_, title, map)
import Clay.Selector (selectorFromText)
import Clay.Text (Content(..))
import qualified Clay as C
import qualified Clay.Elements as E
import qualified Clay.Text as T
import qualified Clay.Stylesheet as S
import qualified Clay.Media as M
import Gruvbox as G
import TextShow
import Data.Text (Text, append, replicate)
import Control.Monad

pageStyle :: Css
pageStyle = do
    star ? do
        boxSizing borderBox
        textRendering geometricPrecision
    html ? do
        fontSize $ pct 100
        fontFamily fonts fontFamilies
        minHeight $ pct 100
        maxWidth $ pct 100
        backgroundColor G.bg
        color G.fg
    body ? do
        fontSize $ rem 1
        lineHeight $ rem 1.5
        margin (px 0) (px 0) (px 0) (px 0)
        wordWrap breakWord
        minHeight $ pct 100
        maxWidth $ pct 100
    h [1..6] <> blockquote <> code <> footer <> header <> li <> ol <> p <> section <> ul ? do
        float none
        margin (px 0) (px 0) (px 0) (px 0)
        padding  (px 0) (px 0) (px 0) (px 0)

    header |> h1 ? do
        position relative
        display inlineBlock
        display tableCell
        color G.blue
        fontSize (rem 1.2)
        padding (rem 1.5) (rem 0) (rem 2) (rem 0)
        overflow hidden
    header |> h1 # after ? do
        content $ stringContent $ replicate 120 "="
        position absolute
        bottom $ rem 0.75
        left $ rem 0
    header |> h1 # before ? do
        content $ stringContent ""

    h [1..6] ** a <> a ? do
        color G.red_l
        textDecoration none
    h [1..6] ? do
        lineHeight $ em 1.3
        fontSize $ rem 1.1
        color G.green
        position relative
        marginBottom $ rem 0.75
    forM [1..6] $ \n -> do
        h [n] # before ? do
            display inline
            content $ stringContent $ append (replicate n "#") " "

    li ? do
        position relative
        display block
        paddingLeft $ px 20
        marginBottom $ rem 0.2
    li # after ? do
        position absolute
        top $ px 0
        left $ px 0
    ul |> li # after ? (content $ stringContent "-")
    ol ? do
        "counter-reset" -: "a"
    ol |> li # after ? do
        "content" -: "counter(a) \".\""
        "counter-increment" -: "a"

    ":not(li)" ? do
        p <> ol <> ul <? do
            marginBottom $ rem 1

    blockquote ? do
        position relative
        paddingLeft $ rem 1
        marginTop $ rem 0.5
        marginBottom $ rem 1
        overflow hidden
        color G.gray
    blockquote # after ? do
        content $ stringContent $ replicate 120 ">\\A"
        whiteSpace T.pre
        position absolute
        top $ px 0
        left $ px 0
    blockquote |> p ? do
        marginBottom $ rem 0
    blockquote <> code ? fontSize (rem 1)

    E.em ? fontStyle italic
    E.em # after <> E.em # before ? do
        content $ stringContent "/"
        display inline

    strong ? do
        color G.yellow
    strong # after <> strong # before ? do
        content $ stringContent "*"
        display inline
    
    ":not(pre)" ? do
        code <? do
            color G.aqua
            fontFamily fonts fontFamilies
        code # after <> code # before <? do
            content $ stringContent "`"
            display inline

    pre ? do
        backgroundColor G.bgh
        color G.green

    footer ? do
        color G.bg2
        bottom $ rem 0
        position relative

    ".container" ? do
        maxWidth $ pct 60
        minHeight $ pct 100
        margin (px 0) auto (px 0) auto
        marginLeft $ pct 20
        padding (px 0) (rem 1) (px 0) (rem 1)

    S.query M.screen [M.maxWidth (px 500)] $ ".container" ? do
        maxWidth $ pct 100
        marginLeft $ rem 0
        marginRight $ rem 0

    ".inner" ? do
        minHeight $ pct 95

    where
        h :: [Int] -> Selector
        h ns = foldl1 (<>) $ map (\n -> selectorFromText $ append "h" $ showt n) ns
        fonts = [
            "Menlo",
            "Monaco",
            "Lucida Console",
            "Liberation Mono",
            "DejaVu Sans Mono",
            "Bitstream Vera Sans Mono",
            "Courier New"
            ]
        fontFamilies = [
            monospace,
            serif
            ]
