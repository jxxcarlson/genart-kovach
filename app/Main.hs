{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

-- SOURCE: https://www.kovach.me/posts/2018-03-07-generating-art.html

module Main where

import           Control.Arrow
import           Control.Concurrent
import           Control.Monad.Random
import           Control.Monad.Reader
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Data.Foldable            (for_)
import           Data.List                (nub)
import           Data.Semigroup           ((<>))
import           Data.Time.Clock.POSIX
import           Graphics.Rendering.Cairo
import           Linear.V2
import           Linear.Vector
import qualified Numeric.Noise.Perlin     as P
import           Text.Printf

data World = World
  { worldWidth  :: Int
  , worldHeight :: Int
  , worldSeed   :: Int
  , worldScale  :: Double
  }

type Generate a = RandT StdGen (ReaderT World Render) a

-- | Lift a Cairo action into a Generate action
cairo :: Render a -> Generate a
cairo = lift . lift

renderSketch :: Generate ()
renderSketch = pure () -- no-op

main :: IO ()
main = do
  seed <- round . (*1000) <$> getPOSIXTime
  let
    stdGen = mkStdGen seed
    width = 60
    height = 60
    scaleAmount = 20

    scaledWidth = round $ fromIntegral width * scaleAmount
    scaledHeight = round $ fromIntegral height * scaleAmount

  surface <- createImageSurface FormatARGB32 scaledWidth scaledHeight
  -- The "world" thinks the width and height are the initial values, not scaled.
  let world = World width height seed scaleAmount

  void
    . renderWith surface
    . flip runReaderT world
    . flip runRandT stdGen
    $ do
      cairo $ scale scaleAmount scaleAmount
      renderSketch

  putStrLn "Generating art..."
  surfaceWriteToPNG surface
    $ "images/example_sketch/"
    <> show seed <> "-" <> show (round scaleAmount :: Int) <> ".png"
  surfaceWriteToPNG surface "images/example_sketch/latest.png"
