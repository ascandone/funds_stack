module Main (main) where

import FundsStack (Sender (..))
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
        FundsStack.pullFrom 10 [Sender "alice" Nothing 100]
          @?= ( [Sender "alice" Nothing 10]
              , [Sender "alice" Nothing 90]
              )
    , testCase "when enough balance but less than total (with tail)" $
        FundsStack.pullFrom 10 [Sender "alice" Nothing 100, Sender "bob" Nothing 42]
          @?= ( [Sender "alice" Nothing 10]
              , [Sender "alice" Nothing 90, Sender "bob" Nothing 42]
              )
    , testCase "balance is exactly the same as needed" $
        FundsStack.pullFrom 10 [Sender "alice" Nothing 10]
          @?= ( [Sender "alice" Nothing 10]
              , []
              )
    , testCase "balance is exactly the same as needed (with tail)" $
        FundsStack.pullFrom 10 [Sender "alice" Nothing 10, Sender "bob" Nothing 42]
          @?= ( [Sender "alice" Nothing 10]
              , [Sender "bob" Nothing 42]
              )
    , testCase "balance is less than needed in the head" $
        FundsStack.pullFrom 11 [Sender "alice" Nothing 10, Sender "bob" Nothing 5]
          @?= ( [Sender "alice" Nothing 10, Sender "bob" Nothing 1]
              , [Sender "bob" Nothing 4]
              )
    , testCase "pull more than available" $
        FundsStack.pullFrom 100 [Sender "alice" Nothing 10, Sender "bob" Nothing 5]
          @?= ( [Sender "alice" Nothing 10, Sender "bob" Nothing 5]
              , []
              )
    , testCase "pull remaining is zero" $
        FundsStack.pullFrom 10 [Sender "alice" Nothing 10, Sender "bob" Nothing 5]
          @?= ( [Sender "alice" Nothing 10]
              , [Sender "bob" Nothing 5]
              )
    , testCase "pull zero" $
        FundsStack.pullFrom 0 [Sender "alice" Nothing 10, Sender "bob" Nothing 5]
          @?= ( []
              , [Sender "alice" Nothing 10, Sender "bob" Nothing 5]
              )
    , testCase "merge fragmented funds" $
        FundsStack.pullFrom 10 [Sender "alice" Nothing 5, Sender "alice" Nothing 5]
          @?= ( [Sender "alice" Nothing 10]
              , []
              )
    , testCase "trim zeros" $
        FundsStack.pullFrom 5 [Sender "alice" Nothing 0, Sender "alice" Nothing 5]
          @?= ( [Sender "alice" Nothing 5]
              , []
              )
    , testCase "merge fragmented store with zeros" $
        FundsStack.pullFrom 10 [Sender "alice" Nothing 5, Sender "alice" Nothing 0, Sender "alice" Nothing 5]
          @?= ( [Sender "alice" Nothing 10]
              , []
              )
    , testCase "colored senders (do not merge)" $
        FundsStack.pullFrom
          10
          [ Sender "alice" (Just "red") 5
          , Sender "alice" (Just "blue") 5
          ]
          @?= (
                [ Sender "alice" (Just "red") 5
                , Sender "alice" (Just "blue") 5
                ]
              , []
              )
    , testCase "colored senders (to be merged)" $
        FundsStack.pullFrom
          10
          [ Sender "alice" (Just "red") 5
          , Sender "alice" (Just "red") 5
          ]
          @?= (
                [ Sender "alice" (Just "red") 10
                ]
              , []
              )
    ]