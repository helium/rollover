{-# LANGUAGE OverloadedStrings #-}

module Main where

import Rollover

import Control.Exception (try)
import Control.Concurrent.Async (wait)
import Data.Text (pack)
import GHC.Stack (currentCallStack)
import Network.Socket (inet_addr)
import System.Environment (getArgs)
import System.IO (IOMode(..), openFile)

main :: IO ()
main = do
    [apiKeyText, environmentText, codeVersionText] <- fmap pack <$> getArgs
    ipAddr <- inet_addr "10.0.1.5"
    let apiKey = ApiKey apiKeyText
        environment = Environment environmentText
        codeVersion = CodeVersion codeVersionText
        request = Request
                    { _method = "GET"
                    , _url = "/users/foo"
                    , _queryString = "sort=desc&foo=bar"
                    , _reqIp = ipAddr
                    , _headers = [("Foo", "bar"), ("Baz", "quz")]
                    }
    rbInfo <- rollbarInfo apiKey environment codeVersion
    Left e <- try (openFile "/does/not/exist" ReadMode)
    stack <- currentCallStack
    a <- recordException rbInfo (Just request) e stack
    wait a
