{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified GHC.IO.Encoding as E
import           Hakyll

config :: Configuration
config = defaultConfiguration

main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    hakyllWith config $ do
        match "css/*" $ do
            route idRoute
            compile compressCssCompiler

        match "images/*" $ do
            route idRoute
            compile copyFileCompiler

        match "*.md" $ do
            route   $ setExtension "html"
            compile $ pandocCompiler
                >>= loadAndApplyTemplate "templates/default.html" defaultContext
                >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

