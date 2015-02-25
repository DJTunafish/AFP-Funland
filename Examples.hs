module Examples (
    module Turtle
   ,spiral
   ,spiralInfinite
   ,square
   ,star
   ,parallelHeart
   )where

import Turtle

main = runProgram $ parallelHeart 150

-- |'spiral' returns a program that performs forward and right
-- sequentially and loops until the size reaches 100 or more
spiral :: Double -> Double -> Program
spiral size angle =
  case size > 100 of
    True  -> [die]
    False ->
      concat [[forward size, right angle], prog]
      where prog = spiral (size+2) angle

-- |'spiralInfinite' returns a program that performs forward
-- and right sequentially and never stops
spiralInfinite :: Double -> Double -> Program
spiralInfinite size angle =
  concat [[forward size, right angle], prog]
  where prog = spiralInfinite (size+2) angle

-- |'square' draws a square
square :: Double -> Program
square size = [times 4 [forward size, right 90], die]

-- |'star' draws a star
star :: Double -> Program
star size = [times 5 [forward size, right 144], die]

-- |'parallelHeart' draws a heart in parallel
parallelHeart :: Double -> Program
parallelHeart size =
  [penUp, backward size, penDown] ++ (leftHalf <|> rightHalf)
  where leftHalf  = [left 45, forward size, right 90
                    , forward (size/2), right 90, forward (size/2), die]
        rightHalf = [right 45, forward size, left 90
                    , forward (size/2), left 90, forward (size/2), die]
