module Main where

import Rollover

import Control.Exception (try)
import Control.Concurrent.Async (wait)
import Data.Text (pack)
import GHC.Stack (currentCallStack)
import System.Environment (getArgs)
import System.IO (IOMode(..), openFile)

main :: IO ()
main = do
    [apiKeyText, environmentText, codeVersionText] <- fmap pack <$> getArgs
    let apiKey = ApiKey apiKeyText
        environment = Environment environmentText
        codeVersion = CodeVersion codeVersionText
        go = do
            Left e <- try (openFile "/does/not/exist" ReadMode)
            stack <- currentCallStack
            a <- recordException apiKey environment codeVersion e stack
            wait a
    go
