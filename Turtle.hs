{-# LANGUAGE GADTs #-} 
-- |This module allows users to implement Turtle Graphics by constructing
-- Programs that consist of basic Actions.
-- The module uses the HGL library for graphics by translating Programs
-- to the Graphics data type
module Turtle (
  module Graphics.HGL,
  module Data.Fixed

  -- * The turtle type(s)
  , Program

  -- * Primitive operations
  , forward, right
  , penUp, penDown
  , color
  , die, idle
  , lifespan
  , times, forever
  , (>*>), (<|>)

  -- * Derived operations
  , backward, left

  -- * Run functions
  , runProgram

  -- * Programs
 {- , spiral, spiralInfinite
  , square, star
  , parallelHeart-}

  ) where

import Graphics.HGL
import Data.Fixed

--------------------------------
-- Declarations of data types --
--------------------------------

-- |A 'Program' is a list of Actions
type Program = [Action]

-- An 'Action' is a command that a Turtle can process and perform
data Action where
  Forward    :: Double     -> Action
  Backward   :: Double     -> Action
  TRight     :: Double     -> Action
  TLeft      :: Double     -> Action
  PenUp      :: Action
  PenDown    :: Action
  Color      :: Color   -> Action
  Die        :: Action
  Idle       :: Action
  Limited    :: Int     -> Program -> Action
  Lifespan   :: Int     -> Program -> Action
  Times      :: Int     -> Program -> Action
  Forever    :: Program -> Action
  Sequential :: Action  -> Action  -> Action
  Parallel   :: Program -> Program -> Action
  deriving Show --for testing purposes

-- The 'Turtle' data type consists of six attributes
-- 'location' describes the Turtle's current position
-- 'orientation' describes the Turtle's current angle at which it is facing
-- 'life' indicates whether or not the Turtle is alive
-- 'toggleDraw' indicates whether or not the Turtle can draw
-- 'penColor' is the current color of the TPen
-- 'deathClock' describes the time a Turtle has to live
data Turtle =
  Ttl { location    :: Point
      , orientation :: Double
      , life        :: Bool
      , toggleDraw  :: Bool
      , penColor    :: Color
      , deathclock  :: Int
      } deriving Show

-------------------------------------------
-- Constructor functions for the Actions --
-------------------------------------------

-- | Returns an action that moves the turtle n steps forward
forward = Forward
-- | Returns an action that moves the turtle n steps backward
backward = Backward
-- | Returns an action that rotates the turtle n degrees right
right = TRight
-- | Returns an action that rotates the turtle n degrees left
left = TLeft
-- | Returns an action that disables the turtle from drawing
--   anything
penUp = PenUp
-- | Returns an action that enables the turtle to draw
penDown = PenDown
-- | Returns an action that changes the color of the turtles 
--   pen to the given color
color = Color
-- | Returns an action that "kills" the turtle, so that
--   it will ignore any instructions from future instructions
die = Die
-- | Returns an action that does nothing
idle = Idle
-- | Executes a given number of instructions from the given
--  program
limited = Limited
-- | Executes a given number of instructions from the given
--   program, before executing the "die" action
lifespan = Lifespan
-- | Returns an action that will execute the given action
--   a given number of times
times = Times
-- | Returns an action that will endlessly execute the 
--   given program
forever = Forever
-- | Given two actions, returns an action that executes the first
--   action, followed by the second.
(>*>) = Sequential

-- | Given two programs, executes both programs in parallel
(<|>) :: Program -> Program -> Program
(<|>) p q = [Parallel p q]

--p <|> q <|> r = [Parallel p q] <|> r = Parallel [Parallel p q] r

----------------------------------------------
-- run-functions and their helper functions --
----------------------------------------------
-- |'runProgram' takes a 'Program' and creates a
-- Turtle that executes the Actions in a Window
runProgram :: Program -> IO ()
runProgram p = runGraphics $ do
   w <- openWindowEx "Turtle!" Nothing (300, 300) DoubleBuffered (Just 1000)
   drawInWindow w (polygon [(0,0),(0,300),(300,300),(300,0)]) 
   onTick w p turtleInit

-- 'onTick' runs one action at a time if the Turtle is alive
-- If the next action is a parallel execution, 'runParallel' will handle it
onTick :: Window -> Program -> Turtle -> IO ()
onTick _w [] _t    = return ()
onTick w (p:ps) t  =
  case life t' of
    False -> return ()
    True  ->
      case p of
        (Parallel p1 q) ->
          runParallel w [(p1, t'), (q, t')]
        _ ->
          do
            (p', t2) <- runAction p t' w 
            putStr "Turtle performed "
            printAction p
            putStr "\n"
            onTick w (p' ++ ps) t2
  where t' = deathAndTaxes t

-- 'deathAndTaxes' counts down a Turtle's 'deathclock'
-- and kills it when its time has come
deathAndTaxes :: Turtle -> Turtle
deathAndTaxes t = 
    if deathclock t > -1
    then case deathclock t of
            0  -> t {deathclock = -1, life = False}
            n  -> t {deathclock = n-1}
    else t

-- 'runParallel' executes multiple Programs in parallel
-- with the help of runParallelAction
runParallel :: Window -> [(Program, Turtle)] -> IO ()
runParallel w [] = return () 
runParallel w pt = do
  putStrLn "Following actions performed in parallel: "
  pt' <- mapM (runParallelAction w) pt
  runParallel w (concat pt')
                    
-- 'runParallelAction' executes multiple 'Program's in parallel
-- by one Action per 'Program' each
runParallelAction :: Window -> (Program, Turtle) -> IO [(Program, Turtle)] 
runParallelAction _w ([], t)                        = return []
runParallelAction w (((Parallel p q):ps), t) = return [(ps, t), (p, t), (q, t)]
runParallelAction w (p, t)                   =
  case life t of
    False -> return []
    True ->
      do
        (p', t'') <- runAction (head p) t' w
        putStr "  "
        printAction (head p)
        putStr "\n"
        return $ [((p' ++ tail p), t'')]
        where
          t' = deathAndTaxes t

-- A turtle with standard values
turtleInit :: Turtle
turtleInit =
  Ttl { location = (150,150), orientation = 270, life = True
      , toggleDraw = True, penColor = Black, deathclock = -1}


-- 'interpretProgram' takes a program and pattern matches for the actions
-- 'Limited', 'Forever', 'Sequential', and 'Times'. If any of these cases
-- are found the function converts them to programs without these constructors
interpretProgram :: Program -> Program
interpretProgram []                    = []
interpretProgram ((Limited n p):ps)    =
  (take n (interpretProgram p)) ++ interpretProgram ps
interpretProgram ((Forever p):ps)      =
  cycle $ interpretProgram p
interpretProgram ((Sequential a b):ps) =
  interpretProgram [a] ++ interpretProgram [b] ++ interpretProgram ps
interpretProgram ((Times n p):ps)      =
  (concat (take n (cycle [interpretProgram p]))) ++ interpretProgram ps
interpretProgram (p:ps)                =
  [p] ++ interpretProgram ps

-- 'runAction' executes the Action given to the turtle in a given window
runAction :: Action -> Turtle -> Window -> IO (Program, Turtle)
runAction (Forward n)    t w = do
    case toggleDraw t of
        True -> do 
                  getWindowTick w
                  drawInWindow w graphic
                  return ([], t {location = (x2, y2)})
        False -> return ([], t {location = (x2, y2)})        
        where
              a         = n * cos (((orientation t)/180) * pi)
              o         = n * sin (((orientation t)/180) * pi)
              (x1, y1)  = location t
              x2        = round $ (fromIntegral x1) + a
              y2        = round $ (fromIntegral y1) + o
              graphic   = withColor (penColor t) $ line (x1, y1) (x2, y2)
runAction (TRight d)     t w =
  return $ ([], t {orientation = mod' ((orientation t) + d) 360}) 
runAction (Backward n)   t w = runAction (Forward (-n)) t w
runAction (TLeft d)      t w = runAction (TRight (-d)) t w
runAction (PenUp)        t w = return $ ([], t {toggleDraw = False})
runAction (PenDown)      t w = return $ ([], t {toggleDraw = True})
runAction (Color c)      t w = return $ ([], t {penColor = c})
runAction (Die)          t w = return $ ([], t {life = False})
runAction (Idle)         t w = return ([], t)
runAction (Lifespan n p) t w =
  if ((n >= deathclock t) && (deathclock t /= -1))
  then return (p, t)
  else return (p, t {deathclock = n})
runAction p                         t w = return (interpretProgram [p], t)

printAction :: Action -> IO ()
printAction (Forward n)      = putStr $ "forward " ++ show n
printAction (TRight n)       = putStr $ "right " ++ show n
printAction (TLeft n)        = putStr $ "left " ++ show n
printAction (Backward n)     = putStr $ "backward " ++ show n
printAction (Limited n p)    = do putStr $ "Limited " ++ show n ++ " ["
                                  printAction (head p)
                                  putStr " .. ]" 
printAction (Lifespan n p)   = do putStr $ "Lifespan " ++ show n ++ " [" 
                                  printAction $ head p
                                  putStr " .. ]"
printAction (PenUp)          = putStr "penUp"
printAction (PenDown)        = putStr "penDown"
printAction (Die)            = putStr "Turtle soup"
printAction (Idle)           = putStr "Idle"
printAction (Color c)        = putStr $ "color " ++ show c
printAction (Times n p)      = do putStr $ "times " ++ show n ++ " ["
                                  printAction $ head p
                                  putStr " .. ]"
printAction (Forever p)      = do putStr "forever [" 
                                  printAction (head p)
                                  putStr " .. ]"
printAction (Sequential a b) = do printAction a
                                  putStr " >*> "
                                  printAction b
printAction (Parallel p q)   = do putStr "[ "
                                  printAction (head p)
                                  putStr " .. ]"
                                  putStr " <|> "
                                  putStr "[ "
                                  printAction (head q)
                                  putStr " .. ]"




