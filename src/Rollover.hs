{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rollover
    ( ApiKey(..)
    , Environment(..)
    , CodeVersion(..)
    , ExceptionInfo(..)
    , StackFrame(..)
    , RollbarException(..)
    , RollbarItem(..)
    , exceptionInfo
    , stackFrameParser
    , recordException
    ) where

import Control.Concurrent.Async
import Control.Monad
import Control.Exception
import Data.Aeson
import Data.Attoparsec.Text
import Data.Proxy
import Data.Text (Text, pack, intercalate, splitOn)
import Data.Typeable
import Network.Wreq

newtype ApiKey = ApiKey
    { _unApiKey :: Text }
    deriving (Show, Eq, ToJSON)

newtype Environment = Environment
    { _unEnvironment :: Text }
    deriving (Show, Eq, ToJSON)

newtype CodeVersion = CodeVersion
    { _unCodeVersion :: Text }
    deriving (Show, Eq, ToJSON)

newtype Host = Host
    { _unHost :: Text }
    deriving (Show, Eq, ToJSON)

data ExceptionInfo = ExceptionInfo
    { _exceptionType :: Text
    , _exceptionDesc :: Text
    } deriving (Show, Eq)

instance ToJSON ExceptionInfo where
    toJSON ExceptionInfo{..} =
        object [ "class" .= _exceptionType
               , "message" .= _exceptionDesc
               ]

data StackFrame = StackFrame
    { _module   :: Text
    , _function :: Text
    , _lineNo   :: Int
    , _colNo    :: Int
    } deriving (Show, Eq)

instance ToJSON StackFrame where
    toJSON StackFrame{..} =
        object [ "filename" .= _module
               , "method" .= _function
               , "lineno" .= _lineNo
               , "colno" .= _colNo
               ]

data RollbarException = RollbarException
    { _exceptionInfo :: ExceptionInfo
    , _trace :: [StackFrame]
    } deriving (Show, Eq)

instance ToJSON RollbarException where
    toJSON RollbarException{..} =
        object [ "frames" .= _trace
               , "exception" .= _exceptionInfo
               ]

data RollbarItem = RollbarItem
    { _apiKey :: ApiKey
    , _environment :: Environment
    , _codeVersion :: CodeVersion
    , _exception :: RollbarException
    }

instance ToJSON RollbarItem where
    toJSON RollbarItem{..} =
        object [ "access_token" .= _apiKey
               , "data" .=
                   object [ "environment" .= _environment
                          , "code_version" .= _codeVersion
                          , "body" .=
                                object [ "trace" .= _exception ]
                          ]
               ]

exceptionInfo :: SomeException -> ExceptionInfo
exceptionInfo (SomeException e) =
    ExceptionInfo eType eDesc
    where eType = (pack . show . typeRep . asProxyTypeOf) e
          eDesc = pack (displayException e)

stackFrameParser :: Parser StackFrame
stackFrameParser = do
    moduleAndFunction <- takeWhile1 (/= ' ')
    space
    char '('
    fileName <- takeWhile1 (/= ':')
    string ":("
    lineNo <- decimal
    char ','
    column <- decimal
    string ")-("
    _ <- decimal
    char ','
    _ <- decimal
    string "))"

    let splitUp = splitOn "." moduleAndFunction
        function = last splitUp
        modName = intercalate "." (init splitUp)
    return StackFrame
        { _module = modName
        , _function = function
        , _lineNo = lineNo
        , _colNo = column
        }

recordException
    :: ApiKey
    -> Environment
    -> CodeVersion
    -> SomeException
    -> [String]
    -> IO (Async ())
recordException apiKey environment codeVersion exception stackTrace =
    async (void (post "https://api.rollbar.com/api/1/item/" (toJSON rbItem)))
    where rbItem = RollbarItem apiKey environment codeVersion exc
          exc = RollbarException (exceptionInfo exception) trace
          trace = either (const []) id
                    (sequence (parseOnly stackFrameParser . pack <$> stackTrace))
