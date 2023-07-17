{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified GHC.IO.Encoding as E
import           System.Environment (getEnvironment)
import           Control.Monad
import           Hakyll

config :: Configuration
config = defaultConfiguration

getEnvVariableExpr :: String -> IO String
getEnvVariableExpr envKey = do
    env <- getEnvironment
    case lookup envKey env of
        Nothing -> error $ "Environment variable " ++ envKey ++ " not defined."
        Just value -> pure value

main :: IO ()
main = do
    E.setLocaleEncoding E.utf8
    reference <- getEnvVariableExpr "REFERENCE"
    appVersion <- getEnvVariableExpr "VERSION"
    asterixSpecsRevision <- getEnvVariableExpr "ASTERIX_SPECS_REVISION"
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

        -- templates
        let files =
                [ ("python", defaultContext
                   <> constField "reference" reference
                   <> constField "version" appVersion
                   <> constField "astReference" asterixSpecsRevision
                    )
                ]

        forM_ files $ \(name, ctx) -> do
            let n a b = fromFilePath (a <> name <> b)
            create [n "" ".md"] $ compile $ do
                makeItem ("" :: String)
                    >>= loadAndApplyTemplate (n "templates/" ".md") ctx

            create [n "" ".html"] $ do
                route idRoute
                compile $ do
                    load $ n "" ".md"
                    >>= renderPandoc
                    >>= (\(Item _a b) -> pure (Item (n "" ".html") b))
                    >>= loadAndApplyTemplate "templates/default.html" defaultContext
                    >>= relativizeUrls

        match "templates/*" $ compile templateBodyCompiler

