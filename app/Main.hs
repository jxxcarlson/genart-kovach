{-# LANGUAGE RecordWildCards  #-}
{-# LANGUAGE TypeApplications #-}

{-

The code below comes from a blog post by Benjamin Kovach

  https://www.kovach.me/posts/2018-03-07-generating-art.html

In the main `do` block below, use `renderBlankSketch` instead of `renderSketch` to
render a "blank drawing."  You can print the blank  drawing and let your kids color it in.

-}

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

data Quad = Quad
  { quadA :: V2 Double
  , quadB :: V2 Double
  , quadC :: V2 Double
  , quadD :: V2 Double
  } deriving (Eq, Ord)


type Generate a = RandT StdGen (ReaderT World Render) a

-- | Lift a Cairo action into a Generate action
-- lift :: Monad m => m a -> t m a
-- Lift a computation from the argument monad to the constructed monad.
cairo :: Render a -> Generate a
cairo = lift . lift

getSize :: Num a => Generate (a, a)
getSize = do
  (w, h) <- asks (worldWidth &&& worldHeight)
  pure (fromIntegral w, fromIntegral h)

fillScreen :: (Double -> Render a) -> Double -> Generate ()
fillScreen color opacity = do
  (w, h) <- getSize @Double
  cairo $ do
    rectangle 0 0 w h
    color opacity *> fill

hsva :: Double -> Double -> Double -> Double -> Render ()
hsva h s v = setSourceRGBA channelRed channelGreen channelBlue
 where RGB{..} = hsv h s v

eggshell :: Double -> Render ()
eggshell = hsva 71 0.13 0.96


dark :: Double -> Render ()
dark = hsva 215 1 0.3

dim :: Double
dim = 1.7

fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

genQuadGrid :: Generate [Quad]
genQuadGrid = do
  (w, h) <- getSize @Int
  vectors <- replicateM 800 $ do
    v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
    pure $ v ^* 2
  pure . nub . flip map vectors $ \v ->
    let v' = fromIntegralVector v
    in Quad v' (v' ^+^ V2 0 dim) (v' ^+^ V2 dim dim) (v' ^+^ V2 dim 0)

renderClosedPath :: [V2 Double] -> Render ()
renderClosedPath (V2 x y:vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
  closePath
renderClosedPath [] = pure ()

renderQuad :: Quad -> Render ()
renderQuad Quad{..} = renderClosedPath [quadA, quadB, quadC, quadD]

teaGreen :: Double -> Render ()
teaGreen = hsva 81 0.25 0.94

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

lightBlue :: Double -> Render ()
lightBlue = hsva 215 0.13 0.76

ochre :: Double -> Render ()
ochre = hsva 51 0.13 0.86


quadAddNoise :: Quad -> Generate Quad
quadAddNoise Quad{..} = do
  perlinSeed <- fromIntegral <$> asks worldSeed

  let
    perlinOctaves = 5
    perlinScale = 0.1
    perlinPersistance = 0.5
    perlinNoise
      = P.perlin (round perlinSeed) perlinOctaves perlinScale perlinPersistance
    perlin2d (V2 x y)
      = P.noiseValue perlinNoise (x + perlinSeed, y + perlinSeed, perlinSeed) - 0.5
    addNoise v = let noise = perlin2d v in v ^+^ V2 (noise / 5) (noise / 8)

  pure $ Quad
    (addNoise quadA)
    (addNoise quadB)
    (addNoise quadC)
    (addNoise quadD)

renderBlankSketch :: Generate ()
renderBlankSketch = do
  fillScreen dark 1

  cairo $ setLineWidth 0.15

  quads <- genQuadGrid
  noisyQuads <- traverse quadAddNoise quads

  cairo $ for_ noisyQuads $ \quad -> do
    renderQuad quad
    darkGunmetal 1 *> stroke

renderSketch :: Generate ()
renderSketch = do
  fillScreen dark 1

  cairo $ setLineWidth 0.15

  quads <- genQuadGrid
  noisyQuads <- traverse quadAddNoise quads

  for_ noisyQuads $ \quad -> do
    strokeOrFill <- weighted [(fill, 0.4), (stroke, 0.6)]
    color <- uniform
       [ lightBlue
       , vividTangerine
       , englishVermillion
       , ochre
       ]
    cairo $ do
      renderQuad quad
      color 1 *> strokeOrFill

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
