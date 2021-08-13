{-# LANGUAGE CPP #-}

#ifdef ghcjs_HOST_OS
{-# LANGUAGE JavaScriptFFI
  #-}
#endif

{- | Code used by the RTS

 -}

module GHCJS.Prim.Internal ( blockedIndefinitelyOnMVar
                           , blockedIndefinitelyOnSTM
                           , wouldBlock
                           , ignoreException
                           , setCurrentThreadResultException
                           , setCurrentThreadResultValue
                           ) where

import           Control.Exception

import           GHCJS.Prim
import           GHC.Exts

wouldBlock :: SomeException
wouldBlock = toException WouldBlockException

blockedIndefinitelyOnMVar :: SomeException
blockedIndefinitelyOnMVar = toException BlockedIndefinitelyOnMVar

blockedIndefinitelyOnSTM :: SomeException
blockedIndefinitelyOnSTM = toException BlockedIndefinitelyOnSTM

ignoreException :: SomeException -> IO ()
ignoreException _ = return ()

setCurrentThreadResultException :: SomeException -> IO ()
#ifdef ghcjs_HOST_OS
setCurrentThreadResultException e
  | Just WouldBlockException <- fromException e =
      js_setCurrentThreadResultWouldBlock
  | Just (JSException v _) <- fromException e =
      js_setCurrentThreadResultJSException v
  | otherwise =
      js_setCurrentThreadResultHaskellException (toJSString (show e))
#else
setCurrentThreadResultException _ = pure ()
#endif

setCurrentThreadResultValue :: IO JSVal -> IO ()
setCurrentThreadResultValue x = js_setCurrentThreadResultValue =<< x

#ifdef ghcjs_HOST_OS

foreign import javascript unsafe
  "h$setCurrentThreadResultWouldBlock();"
  js_setCurrentThreadResultWouldBlock :: IO ()

foreign import javascript unsafe
  "h$setCurrentThreadResultJSException($1);"
  js_setCurrentThreadResultJSException :: JSVal -> IO ()

foreign import javascript unsafe
  "h$setCurrentThreadResultHaskellException($1);"
  js_setCurrentThreadResultHaskellException :: JSVal -> IO ()

foreign import javascript unsafe
  "h$setCurrentThreadResultValue($1);"
  js_setCurrentThreadResultValue :: JSVal -> IO ()

#else

js_setCurrentThreadResultWouldBlock :: IO ()
js_setCurrentThreadResultWouldBlock = pure ()

js_setCurrentThreadResultJSException :: JSVal -> IO ()
js_setCurrentThreadResultJSException _ = pure ()

js_setCurrentThreadResultHaskellException :: JSVal -> IO ()
js_setCurrentThreadResultHaskellException _ = pure ()

js_setCurrentThreadResultValue :: JSVal -> IO ()
js_setCurrentThreadResultValue _ = pure ()

#endif
