{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Attoparsec.Text (parseOnly)
import Test.Tasty
import Test.Tasty.HUnit

import Rollover (StackFrame(..), stackFrameParser)

stackFrameTest :: IO ()
stackFrameTest = do
    let t = "Airship.Internal.Decision.o17 (src/Airship/Internal/Decision.hs:(700,1)-(705,52))"
        res = parseOnly stackFrameParser t
        expected = Right StackFrame
                    { _module = "Airship.Internal.Decision"
                    , _function = "o17"
                    , _lineNo = 700
                    , _colNo = 1
                    }
    assertEqual "stack frame parsing" expected res

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "Stack frame parsing" stackFrameTest
  ]

main :: IO ()
main = defaultMain unitTests
