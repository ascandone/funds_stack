{-# LANGUAGE OverloadedRecordDot #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module FundsStack (
  pullFrom,
  pullColoredFrom,
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

data ColorMode
  = AnyColor
  | SpecificColor (Maybe String)

pull :: ColorMode -> Int -> StackState [Sender]
pull _ 0 = return []
pull colorMode pulled = do
  St.modify' compactSendersTop
  top <- pop
  case (top, colorMode) of
    (Nothing, _) -> return []
    (Just s, SpecificColor neededColor_) | s.color /= neededColor_ -> do
      senders <- pull colorMode pulled
      push s
      return senders
    (Just s, _) | pulled <= s.amount -> do
      Control.Monad.when (s.amount /= pulled) $
        push s{amount = s.amount - pulled}
      return [s{amount = pulled}]
    (Just s, _) -> do
      senders <- pull colorMode (pulled - s.amount)
      return $ compactSendersTop (s : senders)

{- | Avoid having split but subsequent senders with the same name (merge them together)

 (also handlers zero in between)
-}
compactSendersTop :: [Sender] -> [Sender]
compactSendersTop store =
  case store of
    hd : (Sender _ _ 0) : tl -> compactSendersTop (hd : tl)
    s1 : s2 : tl | s1.name == s2.name && s1.color == s2.color -> s1{amount = s1.amount + s2.amount} : tl
    _ ->
      store

-- State monad boilerplate
type StackState = St.State [Sender]

pullFrom :: Int -> [Sender] -> ([Sender], [Sender])
pullFrom = St.runState . pull AnyColor

pullColoredFrom :: Maybe String -> Int -> [Sender] -> ([Sender], [Sender])
pullColoredFrom col = St.runState . pull (SpecificColor col)

pop :: StackState (Maybe Sender)
pop = St.state $ \acc -> case acc of
  [] -> (Nothing, [])
  hd : tl -> (Just hd, tl)

push :: Sender -> StackState ()
push s = St.modify' (s :)
