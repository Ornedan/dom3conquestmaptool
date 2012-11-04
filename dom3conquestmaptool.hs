{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Main where

import qualified Data.Map as Map

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Trans.RWS.Strict
import Control.Monad.ST
import Data.Array.Repa
import Data.Array.Repa.Repr.ForeignPtr
import Data.Array.Repa.IO.DevIL
import Data.Array.ST
import Data.Map (Map)
import Data.Word
import Foreign (pokeElemOff, withForeignPtr)
import System.Directory
import System.Environment
import System.Exit

import Config
import Types




--------------------------------------------------------------------------------

main = do
  -- Args: map image, conf file, output path
  argv <- getArgs
  when (length argv /= 4) $ do
    putStrLn "Usage: dom3conquestmaptool mapimage-file borderimage-file conf-file output-file"
    exitFailure
  
  let [imagePath, borderPath, confPath, outPath] = argv
  
  -- Load images
  img <- runIL $ readImage imagePath
  bImg <- runIL $ readImage borderPath
  
  -- Load conf
  conf <- loadConfig confPath
  
  -- Create the new image
  work img bImg conf outPath

  -- exit(1)
  exitSuccess

work img bImg conf outPath = do
  -- Parse provinces out of the image
  provs <- findProvinces img
  
  -- Do floodfill on the pixel data, based on province ownerships from conf
  floodfill conf provs bImg img
  
  -- DevIL refuses to overwrite files, so delete the target file first if it exists
  whenM (doesFileExist outPath) $
    removeFile outPath
  
  -- Save modified image to file
  runIL $ writeImage outPath img


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
floodfill :: Config -> [(Int, (Int, Int))] -> Image -> Image -> IO ()
floodfill conf provs bordersImg img =
  let (pixs, rc, gc, bc) = case img of
        RGB  pixs -> (pixs, 0, 1, 2)
        RGBA pixs -> (pixs, 0, 1, 2)
        BGR  pixs -> (pixs, 2, 1, 0)
        BGRA pixs -> (pixs, 2, 1, 0)
      (bPixs, bRc, bGc, bBc) = case bordersImg of
        RGB  pixs -> (pixs, 0, 1, 2)
        RGBA pixs -> (pixs, 0, 1, 2)
        BGR  pixs -> (pixs, 2, 1, 0)
        BGRA pixs -> (pixs, 2, 1, 0)
  in floodfill' pixs rc gc bc bPixs bRc bGc bBc
  
  where
    floodfill' pixs rc gc bc bPixs bRc bGc bBc = 
      withForeignPtr (toForeignPtr pixs) $ \ptr -> do
        let Z :. maxY :. maxX :. chans = extent pixs
        
        forM_ provs $ \(pnum, (y0, x0)) -> 
          when (pnum `Map.member` provinces conf) $ do
            -- What's our colour for this province?
            let owner = provinces conf Map.! pnum
                Colour { red = r, green = g, blue = b } = nations conf Map.! owner
            
            -- Find colourable pixels
            let toFill = seek conf bPixs bRc bGc bBc y0 x0
            
            -- Paint them
            forM_ toFill $ \(y, x) -> do
              pokeElemOff ptr (y * maxX * chans + x * chans + rc) r
              pokeElemOff ptr (y * maxX * chans + x * chans + gc) g
              pokeElemOff ptr (y * maxX * chans + x * chans + bc) b


seek :: Config -> Array F DIM3 Word8 ->
        Int -> Int -> Int ->
        Int -> Int ->
        [(Int, Int)]
seek conf pixs rc gc bc y0 x0 = runST $ do
  visited0 <- newArray ((0,0), (maxY, maxX)) False
  
  execRWST (seek' y0 x0) visited0 [] >>= return . fst
  
  where
    Z :. maxY :. maxX :. chans = extent pixs
    
    visit y x = do
      arr <- ask
      lift $ writeArray arr (y, x) True
    isVisited y x =  do
      arr <- ask
      lift $ readArray arr (y, x)
    
    found y x = modify $ ((y,x):)
    
    isBorder y x =
      let r = pixs `unsafeIndex` ix3 y x rc
          g = pixs `unsafeIndex` ix3 y x gc
          b = pixs `unsafeIndex` ix3 y x bc
      in return $
         r == red   (borderColour conf) &&
         g == green (borderColour conf) &&
         b == blue  (borderColour conf)
    
    inBounds y x = y >= 0 && y < maxY && x >= 0 && x < maxX
    
    wrap y x =
      if wraparound conf
      then ((y + maxY) `mod` maxY, (x + maxX) `mod` maxX)
      else (y, x)
    
    seek' :: Int -> Int -> RWST (STUArray s (Int, Int) Bool) () [(Int, Int)] (ST s) ()
    seek' y x = do
      -- Do nothing if we've already visited
      beenHere <- isVisited y x
      when (inBounds y x && not beenHere) $ do
        -- Mark this pixel visited
        visit y x
        
        -- Is it border? If it is, we're done
        border <- isBorder y x
        when (not border) $ do
          -- Add this pixel to the ones we need to colour
          found y x
          
          -- Walk to the neighbouring pixels
          --- Up
          uncurry seek' $ wrap (y + 1) x
          --- Down
          uncurry seek' $ wrap (y - 1) x
          --- Right
          uncurry seek' $ wrap y (x + 1)
          --- Left
          uncurry seek' $ wrap y (x - 1)


whenM :: Monad m => m Bool -> m () -> m ()
whenM pred act = pred >>= flip when act
