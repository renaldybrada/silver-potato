module PongConstants where

numBalls :: Int          
numBalls = 1
                   
ballRadius :: Float
ballRadius = 20

maxX, maxY :: Float
maxX = 300
maxY = 300

initVelo :: Float
initVelo = 100

velocityAdditional :: Float
velocityAdditional = 50

fps :: Int
fps = 60