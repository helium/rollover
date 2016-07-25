{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-|

Client for the <https://rollbar.com Rollbar> exception tracking service.
There is an example in @app/Main.hs@. There's really only one function that
needs to be used, 'recordException'. In order to get a stack trace, you should
compile your code with profiling: @stack build --trace@.

-}

module Rollover
    (
      -- * Types
      ApiKey(..)
    , Environment(..)
    , CodeVersion(..)
    , Request(..)
    , RollbarInfo(..)
    -- * Functions
    , recordException
    , rollbarInfo
    ) where


import Control.Concurrent.Async (Async, async)
import Control.Exception (SomeException(..))
import Control.Monad (void)
import Data.Aeson (ToJSON(..))
import Data.Text (pack)
import Network.HostName (getHostName)
import Network.Wreq (post)

import Rollover.Internal

-- | The non-exception arguments to 'recordException'. This value will probably
-- be created once, when your app starts, and will be shared on all calls to
-- 'recordException'. The functions 'rollbarInfoWithHostname' and
-- 'defaultRolbarInfo' are helpers for creating this record.
data RollbarInfo = RollbarInfo
    { _riApiKey :: ApiKey
    , _riHost :: Maybe Host
    , _riEnvironment :: Maybe Environment
    , _riCode :: Maybe CodeVersion
    } deriving (Show, Eq)

-- | Create a 'RollbarInfo' with all fields defined. This function uses
-- the C function @gethostname(3)@ to retrieve the hostname of the machine.
rollbarInfo :: ApiKey -> Environment -> CodeVersion -> IO RollbarInfo
rollbarInfo apiKey env codeVersion = do
    hostName <- getHostName
    let host = Host (pack hostName)
    return RollbarInfo
                { _riApiKey = apiKey
                , _riEnvironment = Just env
                , _riCode = Just codeVersion
                , _riHost = Just host
                }

-- | Report an exception to Rollbar. The @exception@ argument is the result
-- of calling @GHC.Stack.currentCallStack@.
--
-- Example:
--
-- >  do
-- >    ipAddr <- inet_addr "10.0.1.5"
-- >    let apiKey = ApiKey "my-rollbar-api-key"
-- >        environment = Environment "staging"
-- >        codeVersion = CodeVersion "deadbeef"
-- >        request = Request
-- >                      { _method = "GET"
-- >                      , _url = "/users/foo"
-- >                      , _queryString = "sort=desc&foo=bar"
-- >                      , _reqIp = ipAddr
-- >                      , _headers = [("Foo", "bar"), ("Baz", "quz")]
-- >                      }
-- >    rbInfo <- rollbarInfo apiKey environment codeVersion
-- >    Left e <- try (openFile "/does/not/exist" ReadMode)
-- >    stack <- currentCallStack
-- >    a <- recordException rbInfo (Just request) e stack
-- >    wait a
recordException
    :: RollbarInfo
    -> Maybe Request
    -> SomeException
    -> [String]
    -> IO (Async ())
recordException RollbarInfo{..} mRequest exception stackTrace =
    async (void (post "https://api.rollbar.com/api/1/item/" (toJSON rbItem)))
    where rbItem = RollbarItem _riApiKey _riEnvironment _riCode _riHost mRequest exc
          exc = RollbarException (exceptionInfo exception) trace
          trace = lenientStackFrameParse . pack <$> stackTrace
