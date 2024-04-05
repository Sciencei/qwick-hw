{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.IO.Class (liftIO)
import Control.Monad (unless)
import GHC.IO.Handle (hSetBuffering)
import Hedgehog.Internal.Runner (RunnerConfig(..), checkGroup)
import Hedgehog (MonadGen, Property, property, forAll, (===), checkParallel, Group (Group))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BSL8
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import qualified System.Process as P
import QwickHw (doThingsAndStuff)
import System.Exit (exitFailure)
import System.IO (BufferMode(LineBuffering), stdout, stderr)
import Hedgehog.Main (defaultMain)

-- | spawn a node process running the old function hooked up to stdin and stdout
getOldFunctionResult
  :: [T.Text]
  -> IO [T.Text]
getOldFunctionResult input
  =   either error id
  .   A.eitherDecode
  .   BSL8.pack
  <$> P.readProcess "node" ["original-function.js"] (BSL8.unpack $ A.encode input)

-- | an element, weighted to usually have at least one space
element :: MonadGen m => m T.Text
element = Gen.frequency
  [ (1, Gen.text (Range.exponential 0 255) Gen.alphaNum)
  , (19, fmap T.unwords $ Gen.list (Range.linear 2 5) $ Gen.text (Range.exponential 0 255) Gen.alphaNum)
  ]

-- | for all lists we might generate, the output of our new function should match our old function
matchesOldFunction :: Property
matchesOldFunction = property $ do
  input <- forAll $ Gen.list (Range.exponential 0 255) element
  oldResult <- liftIO $ getOldFunctionResult input
  doThingsAndStuff input === oldResult

test :: IO Bool
test = checkParallel $ Group "Main" [("Output of the new function matches the old function", matchesOldFunction)]

main = defaultMain [test]
