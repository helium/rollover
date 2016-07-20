{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rollover
    ( ApiKey(..)
    , Environment(..)
    , CodeVersion(..)
    , RollbarInfo(..)
    , defaultRolbarInfo
    , recordException
    , rollbarInfoWithHostname
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

-- | Create a 'RollbarInfo' with '_riApiKey' defined, and '_riHost' populated
-- with the C function @gethostname(3)@.
rollbarInfoWithHostname :: ApiKey -> IO RollbarInfo
rollbarInfoWithHostname apiKey = do
    hostName <- getHostName
    let defaults = defaultRolbarInfo apiKey
        host = Host (pack hostName)
    return (defaults { _riHost = Just host })

-- | Create a 'RollbarInfo' with '_riApiKey' defined, and @Nothing@ for all
-- other values.
defaultRolbarInfo :: ApiKey -> RollbarInfo
defaultRolbarInfo apiKey =
    RollbarInfo { _riApiKey = apiKey
                , _riHost = Nothing
                , _riEnvironment = Nothing
                , _riCode = Nothing
                }

-- | Report an exception to Rollbar. The @exception@ argument is the result
-- of calling @GHC.Stack.currentCallStack@.
--
-- Example:
--
-- >  do
-- >    let apiKey = ApiKey "my-rollbar-api-key"
-- >        environment = Environment "staging"
-- >        codeVersion = CodeVersion "deadbeef"
-- >    rollbarWithHost <- rollbarInfoWithHostname apiKey
-- >    let rbInfo = rollbarWithHost
-- >                        { _riEnvironment = Just environment
-- >                        , _riCode = Just codeVersion
-- >                        }
-- >    Left e <- try (openFile "/does/not/exist" ReadMode)
-- >    stack <- currentCallStack
-- >    a <- recordException rbInfo e stack
-- >    wait a
recordException
    :: RollbarInfo
    -> SomeException
    -> [String]
    -> IO (Async ())
recordException RollbarInfo{..} exception stackTrace =
    async (void (post "https://api.rollbar.com/api/1/item/" (toJSON rbItem)))
    where rbItem = RollbarItem _riApiKey _riEnvironment _riCode exc
          exc = RollbarException (exceptionInfo exception) trace
          trace = lenientStackFrameParse . pack <$> stackTrace
