module Main where

import Rollover

import Control.Exception
import Control.Concurrent.Async
import Data.Text
import GHC.Stack
import System.Environment
import System.IO

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
