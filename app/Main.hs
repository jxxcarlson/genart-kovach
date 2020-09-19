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
import Control.Monad.Random
    ( uniform,
      weighted,
      evalRandIO,
      runRandT,
      replicateM,
      void,
      mkStdGen,
      MonadRandom(getRandomR),
      RandT,
      StdGen,
      MonadTrans(lift) )
import           Control.Monad.Reader
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSV
import           Data.Foldable            (for_)
import           Data.List                (nub)
import           Data.Semigroup           ((<>))
import           Data.Time.Clock.POSIX
import Graphics.Rendering.Cairo
    ( closePath,
      createImageSurface,
      fill,
      lineTo,
      moveTo,
      newPath,
      rectangle,
      renderWith,
      scale,
      setLineWidth,
      setSourceRGBA,
      stroke,
      surfaceWriteToPNG,
      Render,
      Format(FormatARGB32) )
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

fromIntegralVector :: V2 Int -> V2 Double
fromIntegralVector (V2 x y) = V2 (fromIntegral x) (fromIntegral y)

origin_ :: V2 Double 
origin_ = V2 30 30

genPoints :: Int -> Generate [V2 Double]
genPoints n = replicateM n $ V2 <$> getRandomR  (-1, 1) <*> getRandomR (-1, 1)

genBrownianPath :: Int -> Generate [V2 Double]
genBrownianPath n = liftM2 (scanl (^+^)) (pure origin_) (genPoints n)



genQuadGrid :: Generate [Quad]
genQuadGrid = do
  (w, h) <- getSize @Int
  vectors <- replicateM 800 $ do
    v <- V2 <$> getRandomR (3, w `div` 2 - 3) <*> getRandomR (3, h `div` 2 - 3)
    pure $ v ^* 2
  pure . nub . flip map vectors $ \v ->
    let v' = fromIntegralVector v
    in Quad v' (v' ^+^ V2 0 1.5) (v' ^+^ V2 1.5 1.5) (v' ^+^ V2 1.5 0)

renderClosedPath :: [V2 Double] -> Render ()
renderClosedPath (V2 x y:vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
  closePath
renderClosedPath [] = pure ()

testPath :: [V2 Double]
testPath = [V2 30 30, V2 30 35, V2 35 35, V2 35 30]


renderedTestPath :: Render ()
renderedTestPath = renderPath testPath

renderPath :: [V2 Double] -> Render ()
renderPath (V2 x y:vs) = do
  newPath
  moveTo x y
  for_ vs $ \v -> let V2 x' y' = v in lineTo x' y'
renderPath [] = pure ()

---  COLORS ---

teaGreen :: Double -> Render ()
teaGreen = hsva 81 0.25 0.94

vividTangerine :: Double -> Render ()
vividTangerine = hsva 11 0.40 0.92

englishVermillion :: Double -> Render ()
englishVermillion = hsva 355 0.68 0.84

darkGunmetal :: Double -> Render ()
darkGunmetal = hsva 170 0.30 0.16

--- RENDER ---

renderBlankSketch :: Generate ()
renderBlankSketch = do
  fillScreen eggshell 1

  cairo $ setLineWidth 0.15


renderSketch :: Generate ()
renderSketch = do
  fillScreen eggshell 1

  cairo $ setLineWidth 0.15
  
  points <- genBrownianPath 1000
  cairo $ do 
    renderPath points
    (englishVermillion 280) *> stroke

-- evalRandIO $ renderPath <$> (pathV2 30 30 200) :: IO (Render ())


--- MAIN ---

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
