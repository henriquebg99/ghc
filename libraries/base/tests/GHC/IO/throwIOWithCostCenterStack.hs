module Main where

import Control.Exception
import GHC.IO

data CustomException = CustomException deriving (Show)

instance Exception CustomException

main :: IO ()
main =
  catch
    (throwIOWithCostCenterStack CustomException)
    ( \(e :: SomeExceptionWithLocation) -> case e of
        SomeExceptionWithLocation _ bts -> print bts
    )
