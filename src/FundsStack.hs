{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module FundsStack (
  pullFrom,
  Sender (..),
) where

import qualified Control.Monad
import qualified Control.Monad.State.Strict as St

data Sender = Sender
  { name :: String
  , color :: Maybe String
  , amount :: Int
  }
  deriving (Eq, Show)

pull :: Int -> StackState [Sender]
pull 0 = return []
pull pulled = do
  St.modify' compactSenders
  top <- pop
  case top of
    Nothing -> return []
    Just s | pulled <= s.amount -> do
      Control.Monad.when (s.amount /= pulled) $
        push s{amount = s.amount - pulled}
      return [s{amount = pulled}]
    Just s -> do
      senders <- pull (pulled - s.amount)
      return $ s : senders

{- | Avoid having split but subsequent senders with the same name (merge them together)

 (also handlers zero in between)
-}
compactSenders :: [Sender] -> [Sender]
compactSenders store =
  case store of
    hd : (Sender _ _ 0) : tl -> compactSenders (hd : tl)
    s1 : s2 : tl | s1.name == s2.name && s1.color == s2.color -> s1{amount = s1.amount + s2.amount} : tl
    _ ->
      store

-- State monad boilerplate
type StackState = St.State [Sender]

pullFrom :: Int -> [Sender] -> ([Sender], [Sender])
pullFrom = St.runState . pull

pop :: StackState (Maybe Sender)
pop = St.state $ \acc -> case acc of
  [] -> (Nothing, [])
  hd : tl -> (Just hd, tl)

push :: Sender -> StackState ()
push s = St.modify' (s :)
