{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rollover.Internal
    ( ApiKey(..)
    , CodeVersion(..)
    , Environment(..)
    , ExceptionInfo(..)
    , Host(..)
    , Request(..)
    , RollbarException(..)
    , RollbarItem(..)
    , StackFrame(..)
    , exceptionInfo
    , stackFrameParser
    , lenientStackFrameParse
    ) where

import Control.Applicative ((<|>))
import Control.Exception (SomeException(..), displayException)
import Control.Monad (void)
import Data.Aeson (ToJSON(..), Value, (.=), object)
import Data.Attoparsec.Text (Parser, takeWhile1, string, decimal, space, char, parseOnly)
import Data.Bits ((.&.), shiftR)
import Data.ByteString (ByteString)
import Data.CaseInsensitive (original)
import Data.Monoid ((<>))
import Data.Proxy (asProxyTypeOf)
import Data.Text (Text, pack, intercalate, splitOn)
import Data.Text.Encoding (decodeUtf8With)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Typeable (typeRep)
import Data.Version (showVersion)
import Data.Word (Word32)
import Network.HTTP.Types (RequestHeaders)

import Paths_rollover (version)

-- | Rollbar API key
newtype ApiKey = ApiKey
    { _unApiKey :: Text }
    deriving (Show, Eq, ToJSON)

-- | Textual description of the environment, such as @\"development\"@,
-- @\"staging\"@, or @\"production\"@.
newtype Environment = Environment
    { _unEnvironment :: Text }
    deriving (Show, Eq, ToJSON)

-- | Textual description of the code version.
-- It is recommended to use the output of @\git rev-parse --short HEAD@
newtype CodeVersion = CodeVersion
    { _unCodeVersion :: Text }
    deriving (Show, Eq, ToJSON)

newtype Host = Host
    { _unHost :: Text }
    deriving (Show, Eq, ToJSON)

-- | Information about the request (if there was one) that was being fullfilled
-- while the exception occurred.
data Request = Request
    { _method :: Text
    , _url :: Text
    , _queryString :: Text
    , _reqIp :: Word32 -- ^ Typically constructed with @Network.Socket.inet_addr@
    , _headers :: RequestHeaders
    } deriving (Show, Eq)

instance ToJSON Request where
    toJSON Request{..} =
        object [ "method" .= _method
               , "url" .= _url
               , "query_string" .= _queryString
               , "user_ip" .= formatIp _reqIp
               , "headers" .= formatHeaders _headers
               ]
        where formatHeaders :: RequestHeaders -> Value
              formatHeaders = object . fmap headerToValue
              headerToValue (h, v) =
                let textHeaderName = lenientUtf8Decode (original h)
                    textHeaderValue = toJSON (lenientUtf8Decode v)
                in (textHeaderName, textHeaderValue)

formatIp :: Word32 -> Text
formatIp word =
    let d = (word .&. 0xff000000) `shiftR` 24
        c = (word .&. 0x00ff0000) `shiftR` 16
        b = (word .&. 0x0000ff00) `shiftR` 8
        a = (word .&. 0x000000ff)
        text = pack . show
        dot x y = x <> "." <> y
    in text a `dot` text b `dot` text c `dot` text d

lenientUtf8Decode :: ByteString -> Text
lenientUtf8Decode = decodeUtf8With lenientDecode

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
    , _environment :: Maybe Environment
    , _codeVersion :: Maybe CodeVersion
    , _host :: Maybe Host
    , _req :: Maybe Request
    , _exception :: RollbarException
    }

instance ToJSON RollbarItem where
    toJSON RollbarItem{..} =
        object [ "access_token" .= _apiKey
               , "data" .=
                   object [ "environment" .= _environment
                          , "code_version" .= _codeVersion
                          , "body" .=
                                object [ "trace" .= _exception
                                       , "notifier" .= notifier
                                       ]
                          , "server" .=
                                object [ "host" .= _host ]
                          , "request" .= _req
                          ]
               ]


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

notifier :: Value
notifier =
    object [
        "name" .= ("rollover" :: Text)
      , "version" .= showVersion version
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
