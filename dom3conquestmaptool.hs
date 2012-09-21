module Main where

import qualified Data.Map as Map
import qualified Data.Set as Set

import Control.Arrow
import Control.Exception
import Control.Monad
import Control.Monad.Trans.RWS
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.IO.DevIL
import Data.Map (Map)
import Data.Maybe
import Foreign
import System.Environment
import System.Exit
import System.IO
import System.Posix.Files

import Config
import Types


--------------------------------------------------------------------------------

main = do
  -- Args: map image, conf file, output path
  argv <- getArgs
  when (length argv /= 3) $ do
    putStrLn "Usage: dom3conquestmaptool mapimage-file conf-file output-file"
    exitFailure
  
  let [imagePath, confPath, outPath] = argv
  
  -- Load map image with DevIL
  img <- runIL $ readImage imagePath
  
  -- Load conf
  conf <- loadConfig confPath
  
  -- Parse provinces out of the image
  provs <- findProvinces img
  
  -- Do floodfill on the pixel data, based on province ownerships from conf
  floodfill conf provs img
  
  -- DevIL refuses to overwrite files, so delete the target file first if it exists
  whenM (fileExist outPath) $
    removeLink outPath
  
  -- Save modified image to file
  runIL $ writeImage outPath img
  
  exitSuccess


-- | Finds the province dot coordinates from the image.
findProvinces :: Image -> IO [(Int, (Int, Int))]
findProvinces img =
  case img of
    RGB  pixs -> findProvinces' pixs
    RGBA pixs -> findProvinces' pixs
    BGR  pixs -> findProvinces' pixs
    BGRA pixs -> findProvinces' pixs
  where
    findProvinces' pixs =
      let Z :. maxY :. maxX :. _ = extent pixs
          whites = do
            y <- [0 .. maxY - 1]
            x <- [0 .. maxX - 1]
            guard (pixs `unsafeIndex` (ix3 y x 0) == 0xff &&
                   pixs `unsafeIndex` (ix3 y x 1) == 0xff &&
                   pixs `unsafeIndex` (ix3 y x 2) == 0xff)
            return (y, x)
      in return $ zip [1 ..] whites


-- | Floodfill depth-first from each of the province dots. Handle wraparound in the
--   neighbor selection.
floodfill :: Config -> [(Int, (Int, Int))] -> Image -> IO ()
floodfill conf provs img =
  case img of
    RGB  pixs -> floodfill' pixs RGB  0 1 2
    RGBA pixs -> floodfill' pixs RGBA 0 1 2
    BGR  pixs -> floodfill' pixs BGR  2 1 0
    BGRA pixs -> floodfill' pixs BGRA 2 1 0
  
  where
    floodfill' pixs wrap rc gc bc = withForeignPtr (toForeignPtr pixs) $ \ptr -> do
      let Z :. maxY :. maxX :. chans = extent pixs
      
      forM_ provs $ \(pnum, (y0, x0)) -> do
        -- What's our colour for this province?
        let owner = provinces conf Map.! pnum
            Colour { red = r, green = g, blue = b } = nations conf Map.! owner
        
        -- Find colourable pixels
        let toFill = seek conf pixs rc gc bc y0 x0
        
        -- Paint them
        forM_ toFill $ \(y, x) -> do
          pokeElemOff ptr (y * maxX * chans + x * chans + rc) r
          pokeElemOff ptr (y * maxX * chans + x * chans + gc) g
          pokeElemOff ptr (y * maxX * chans + x * chans + bc) b


seek :: Config -> Array F DIM3 Word8 ->
        Int -> Int -> Int ->
        Int -> Int ->
        [(Int, Int)]
seek conf pixs rc gc bc y0 x0 = snd $ execRWS (seek' y0 x0) () Set.empty
  where
    Z :. maxY :. maxX :. chans = extent pixs
    
    visit y x = modify $ Set.insert (y, x)
    isVisited y x = gets $ Set.member (y, x)
    
    isBorder y x =
      let r = pixs `unsafeIndex` ix3 y x rc
          g = pixs `unsafeIndex` ix3 y x gc
          b = pixs `unsafeIndex` ix3 y x bc
      in return $
         r == red   (borderColour conf) &&
         g == green (borderColour conf) &&
         b == blue  (borderColour conf)
    
    inBounds y x =
      let is = y >= 0 && y < maxY &&
               x >= 0 && x < maxX
      in if is
         then Just (y, x)
         else if wraparound conf
              then Just ((y + maxY) `mod` maxY, (x + maxX) `mod` maxX)
              else Nothing
    
    seek' y x = do
      -- Do nothing if we've already visited
      visited <- isVisited y x
      when (not visited) $ do
        -- Mark this pixel visited
        visit y x
        
        -- Is it border? If it is, we're done
        border <- isBorder y x
        when (not border) $ do
          -- Add this pixel to the ones we need to colour
          tell [(y, x)]
          
          -- Walk to the neighbouring pixels
          let up    = inBounds (y + 1) x
              down  = inBounds (y - 1) x
              right = inBounds y (x + 1)
              left  = inBounds y (x - 1)
          when (isJust up)    $ uncurry seek' $ fromJust up
          when (isJust down)  $ uncurry seek' $ fromJust down
          when (isJust right) $ uncurry seek' $ fromJust right
          when (isJust left)  $ uncurry seek' $ fromJust left

whenM :: Monad m => m Bool -> m () -> m ()
whenM pred act = pred >>= flip when act
