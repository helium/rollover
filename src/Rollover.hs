{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Rollover
    ( ApiKey(..)
    , Environment(..)
    , CodeVersion(..)
    , RollbarInfo(..)
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
-- >    let apiKey = ApiKey "my-rollbar-api-key"
-- >        environment = Environment "staging"
-- >        codeVersion = CodeVersion "deadbeef"
-- >    rbInfo <- rollbarInfo apiKey environment codeVersion
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
    where rbItem = RollbarItem _riApiKey _riEnvironment _riCode _riHost exc
          exc = RollbarException (exceptionInfo exception) trace
          trace = lenientStackFrameParse . pack <$> stackTrace
