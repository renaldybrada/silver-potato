module Main where

import System.Random
import Control.Monad (forM)
import Control.Monad.State (execStateT)
import PongConstants
import CetakMonad
import Graphics.Gloss

type Ball = (Point, Vector)

drawBalls :: [Ball] -> Picture
drawBalls balls = pictures (zipWith drawBall colors balls)
  where
    colors = cycle [yellow]
    drawBall c ((x,y), (dx,dy)) 
      = translate x y (color c (circleSolid ballRadius))

updateBalls :: Float -> [Ball] -> [Ball]
updateBalls dt balls = map (updateBall dt) balls
  
updateBall :: Float -> Ball -> Ball
updateBall dt ((x,y),(dx,dy)) = do
  let writeLog = appendFile "log.txt" $ "Hello World\n" 
  ((x',y'), (dx',dy'))
  where (x',dx') = clip x dx (maxX-ballRadius)
        (y',dy') = clip y dy (maxY-ballRadius)
        clip h dh max
          | h' > max = (max, -dh - velocityAdditional)
          | h' < -max= (-max, -dh + velocityAdditional)
          | otherwise = (h', dh)
          where h' = h + dt*(dh)

randomBall :: Float -> Float -> Float -> IO Ball
randomBall maxX maxY initVelo = do
  x <- randomRIO (-maxX,maxX) 
  y <- randomRIO (-maxY,maxY) 
  let dx = initVelo
  let dy = initVelo
  return ((x,y),(dx,dy)) 

window :: Float -> Float -> Display
window maxX maxY = InWindow "Balls"  (2*floor maxX,2*floor maxY) (0,0)

main :: IO ()
main = do
  execStateT cetakState "\n\nAplikasi Bounce Ball"
  execStateT cetakState "====================\n"
  execStateT cetakState "ball radius (pixels) :"
  ballRad <- getLine
  execStateT cetakState "ball speed (pixels) :"
  ballSpeed <- readLn
  execStateT cetakState "velocity increment (pixels) :"
  veloIncrement <- getLine
  execStateT cetakState "frame width (300): "
  maxWidth <- readLn
  execStateT cetakState "frame height (300): "
  maxHeight <- readLn
  balls <- forM [1..numBalls] (\_ -> randomBall maxWidth maxHeight ballSpeed)
  simulate 
    (window maxWidth maxHeight) 
    black 
    fps 
    balls 
    drawBalls 
    (\_ -> updateBalls)