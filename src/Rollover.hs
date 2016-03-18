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

import Control.Applicative ((<|>))
import Control.Concurrent.Async (Async, async)
import Control.Monad (void)
import Control.Exception (SomeException(..), displayException)
import Data.Aeson (ToJSON(..), (.=), object)
import Data.Attoparsec.Text (Parser, takeWhile1, string, decimal, space, char, parseOnly)
import Data.Proxy (asProxyTypeOf)
import Data.Text (Text, pack, intercalate, splitOn)
import Data.Typeable (typeRep)
import Network.Wreq (post)

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

int :: Parser Int
int = decimal

moduleAndFunctionParser :: Parser Text
moduleAndFunctionParser = takeWhile1 (/= ' ')

fileNameParser :: Parser Text
fileNameParser =
    space >> char '(' >> takeWhile1 (/= ':') >> string ":"

multiLineLocationParser :: Parser (Int, Int)
multiLineLocationParser = do
    _ <- string "("
    lineNo <- int
    void (char ',')
    column <- int
    void $ string ")-(" >> int >> char ',' >> int >> string ")"
    return (lineNo, column)

singleLineLocationParser :: Parser (Int, Int)
singleLineLocationParser = do
    lineNo <- int
    _ <- string ":"
    column <- int
    _ <- string "-"
    void int
    return (lineNo, column)

stackFrameParser :: Parser StackFrame
stackFrameParser = do

    moduleAndFunction <- moduleAndFunctionParser

    void fileNameParser

    (lineNo, column) <- multiLineLocationParser <|> singleLineLocationParser

    _ <- string ")"

    let splitUp = splitOn "." moduleAndFunction
        function = last splitUp
        modName = intercalate "." (init splitUp)
    return StackFrame
        { _module = modName
        , _function = function
        , _lineNo = lineNo
        , _colNo = column
        }

lenientStackFrameParse :: Text -> StackFrame
lenientStackFrameParse frame =
    case parseOnly stackFrameParser frame of
        (Right f) -> f
        (Left _) ->
            StackFrame frame frame 0 0

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
          trace = lenientStackFrameParse . pack <$> stackTrace
