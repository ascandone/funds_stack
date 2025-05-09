{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}
module FundsStack (pullFrom) where

import qualified Control.Monad
import qualified Control.Monad.State.Strict as St

type Senders = [(String, Int)]

pull :: Int -> StackState Senders
pull 0 = return []
pull pulled = do
  St.modify' compactSenders
  top <- pop
  case top of
    Nothing -> return []
    Just (name, avl) | pulled <= avl -> do
      Control.Monad.when (avl /= pulled) $
        push name (avl - pulled)
      return [(name, pulled)]
    Just (name, avl) -> do
      senders <- pull (pulled - avl)
      return $ (name, avl) : senders

{- | Avoid having split but subsequent senders with the same name (merge them together)

 (also handlers zero in between)
-}
compactSenders :: Senders -> Senders
compactSenders store =
  case store of
    hd : (_, 0) : tl -> compactSenders (hd : tl)
    (name1, amt1) : (name2, amt2) : tl | name1 == name2 -> (name1, amt1 + amt2) : tl
    _ ->
      store

-- State monad boilerplate
type StackState = St.State Senders

pullFrom :: Int -> Senders -> (Senders, Senders)
pullFrom = St.runState . pull

pop :: StackState (Maybe (String, Int))
pop = St.state $ \acc -> case acc of
  [] -> (Nothing, [])
  hd : tl -> (Just hd, tl)

push :: String -> Int -> StackState ()
push name amt = St.modify' ((name, amt) :)
