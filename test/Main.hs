module Main (main) where

import qualified FundsStack
import Test.Tasty (TestTree)
import qualified Test.Tasty
import Test.Tasty.HUnit

main :: IO ()
main =
  Test.Tasty.defaultMain tests

tests :: TestTree
tests =
  Test.Tasty.testGroup
    "FundsStack.pullFrom tests"
    [ testCase "when enough balance but less than total" $
        FundsStack.pullFrom 10 [("alice", 100)]
          @?= ( [("alice", 10)]
              , [("alice", 90)]
              )
    , testCase "when enough balance but less than total (with tail)" $
        FundsStack.pullFrom 10 [("alice", 100), ("bob", 42)]
          @?= ( [("alice", 10)]
              , [("alice", 90), ("bob", 42)]
              )
    , testCase "balance is exactly the same as needed" $
        FundsStack.pullFrom 10 [("alice", 10)]
          @?= ( [("alice", 10)]
              , []
              )
    , testCase "balance is exactly the same as needed (with tail)" $
        FundsStack.pullFrom 10 [("alice", 10), ("bob", 42)]
          @?= ( [("alice", 10)]
              , [("bob", 42)]
              )
    , testCase "balance is less than needed in the head" $
        FundsStack.pullFrom 11 [("alice", 10), ("bob", 5)]
          @?= ( [("alice", 10), ("bob", 1)]
              , [("bob", 4)]
              )
    , testCase "pull more than available" $
        FundsStack.pullFrom 100 [("alice", 10), ("bob", 5)]
          @?= ( [("alice", 10), ("bob", 5)]
              , []
              )
    , testCase "pull remaining is zero" $
        FundsStack.pullFrom 10 [("alice", 10), ("bob", 5)]
          @?= ( [("alice", 10)]
              , [("bob", 5)]
              )
    , testCase "pull zero" $
        FundsStack.pullFrom 0 [("alice", 10), ("bob", 5)]
          @?= ( []
              , [("alice", 10), ("bob", 5)]
              )
    , testCase "merge fragmented funds" $
        FundsStack.pullFrom 10 [("alice", 5), ("alice", 5)]
          @?= ( [("alice", 10)]
              , []
              )
    , testCase "trim zeros" $
        FundsStack.pullFrom 5 [("alice", 0), ("alice", 5)]
          @?= ( [("alice", 5)]
              , []
              )
    , testCase "merge fragmented store with zeros" $
        FundsStack.pullFrom 10 [("alice", 5), ("alice", 0), ("alice", 5)]
          @?= ( [("alice", 10)]
              , []
              )
    ]