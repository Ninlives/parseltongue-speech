{-# LANGUAGE OverloadedStrings #-}
module Utils where

import Data.Text
import System.FilePath
import Rib (IsRoute, routeFile)

relativeTo :: IsRoute r => r a -> r b -> Text
relativeTo ra rb = relativePathTo pa pb
    where pa = getPath ra
          pb = getPath rb
          getPath :: IsRoute r => r a -> FilePath
          getPath r = case (routeFile r) of
                        Just p -> p
                        Nothing -> ""

relativePathTo :: FilePath -> FilePath -> Text
relativePathTo a b = pack $ joinPath $ f pa pb
    where pa = splitDirectories $ a
          pb = splitDirectories $ b

          f [x] [_] = [x]
          f (x:xs) (y:ys)
              | x == y = f xs ys
              | otherwise = f' (x:xs) (y:ys)
          f x _ = x

          f' x [_] = x
          f' x (_:ys) = ".." : f' x ys
          f' x _ = x
