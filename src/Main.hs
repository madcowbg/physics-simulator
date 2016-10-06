-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------
module Main (
    main
) where

import A.B
import A.C
import A.D
import Swarm.Utils
import A.E

import GHC.Conc
import Data.Time

import Physics.Tests
--import System.Time
-- This strang looking comment adds code only needed when running the
-- doctest tests embedded in the comments
-- $setup
-- >>> import Data.List (stripPrefix)

-- | Simple function to create a hello message.
-- prop> stripPrefix "Hello " (hello s) == Just s
hello :: String -> String
hello s = "Hello " ++ s

main :: IO ()
--main = putStrLn (hello "World")
--main = print( fib(50))
--main = print (show doFibC)
--main = print (show (A.D.sqrt 9))
--main = do
--    print ("#threads = " ++ show GHC.Conc.numCapabilities)
--    start <- getCurrentTime
--    --print (length(erathostenes 10000000))
--    --print (primesMPE !! 1000000)
--    print (length (erathostenesD 1000000)) --15485867
--    --print (erathostenes2 500)
--    now <- getCurrentTime
--    let timePassed = diffUTCTime now start
--    print (show timePassed)
---- :set +s
--main = runBasicDemo
main = runOrbitsDemo
