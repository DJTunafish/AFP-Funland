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

  -- * Derived operations
  , backward, left

  -- * Composition operations
  , limited, lifespan
  , times, forever
  , (>*>), (<|>)

  -- * Run functions
  , runProgram, runTextual

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
  StartDeathClock :: Int -> Action
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
forward x = [Forward x]
-- | Returns an action that moves the turtle n steps backward
backward x = [Backward x]
-- | Returns an action that rotates the turtle n degrees right
right x = [TRight x]
-- | Returns an action that rotates the turtle n degrees left
left x = [TLeft x]
-- | Returns an action that disables the turtle from drawing
--   anything
penUp = [PenUp]
-- | Returns an action that enables the turtle to draw
penDown = [PenDown]
-- | Returns an action that changes the color of the turtles 
--   pen to the given color
color c = [Color c]
-- | Returns an action that "kills" the turtle, so that
--   it will ignore any instructions from future instructions
die = [Die]
-- | Returns an action that does nothing
idle = [Idle]


---------------------------
-- Composition functions --
---------------------------

-- | Executes a given number of instructions from the given
--  program
limited :: Int -> Program -> Program
limited n p = take n p
-- | Executes a given number of instructions from the given
--   program, besfore executing the "die" action
lifespan :: Int -> Program -> Program
lifespan n p = [StartDeathClock n] ++ p
-- | Returns an action that will execute the given action
--   a given number of times
times :: Int -> Program -> Program
times n p = concat (take n (cycle [p]))
-- | Returns an action that will endlessly execute the 
--   given program
forever :: Program -> Program
forever p = cycle p 
-- | Given two actions, returns an action that executes the first
--   action, followed by the second.
(>*>) :: Program -> Program -> Program
(>*>) a b = a ++ b 

-- | Given two programs, executes both programs in parallel
(<|>) :: Program -> Program -> Program
(<|>) p q = [Parallel p q]


----------------------------------------------
-- Run-functions and their helper functions --
----------------------------------------------

-- |'runProgram' takes a 'Program' and creates a
-- Turtle that executes the Actions in a Window
runProgram :: Program -> IO ()
runProgram p = runGraphics $ do
   w <- openWindowEx "Turtle!" Nothing (300, 300) DoubleBuffered (Just 1000)
   drawInWindow w (polygon [(0,0),(0,300),(300,300),(300,0)]) 
   onTick w p turtleInit

-- 'onTick' checks if the inputted Turtle is still alive.
-- If so, it will execute one instruction from the given program.
-- If not, it will halt.
onTick :: Window -> Program -> Turtle -> IO ()
onTick w [] _ = return ()
onTick w p t = case life t' of
                False -> return ()
                True  -> do
                  -- printAction (head p) t'
                  -- putStr "\n"
                  t'' <- runAction (head p) t' w
                  onTick w (tail p) t''
  where t' = deathAndTaxes t

runTextual :: Program -> IO ()
runTextual p = onTickText p turtleInit

onTickText :: Program -> Turtle -> IO ()
onTickText [] _ = return ()
onTickText p t = case life t' of
  False -> return ()
  True  -> do
    t'' <- printAction (head p) t'
    putStr "\n"
    onTickText (tail p) t''
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

calculateForward :: Double -> Turtle -> Turtle
calculateForward n t =
  t {location = (x2, y2)}
  where
    a = n * cos (((orientation t)/180) * pi)
    o = n * sin (((orientation t)/180) * pi)
    (x1, y1) = location t
    x2 = round $ (fromIntegral x1) + a
    y2 = round $ (fromIntegral y1) + o


-- 'runAction' takes an 'Action' and a 'Turtle' and executes the Action
-- on the given 'Window', changing the state of the Turtle
runAction :: Action -> Turtle -> Window -> IO Turtle
runAction (Forward n)    t w =
  case toggleDraw t of
    True  -> do 
      getWindowTick w
      drawInWindow w graphic
      return t'
--          return (t {location = (x2, y2)})
    False -> return t'
    where
              -- a         = n * cos (((orientation t)/180) * pi)
              -- o         = n * sin (((orientation t)/180) * pi)
              -- (x1, y1)  = location t
              -- x2        = round $ (fromIntegral x1) + a
              -- y2        = round $ (fromIntegral y1) + o
      graphic   = withColor (penColor t) $ line (x1, y1) (x2, y2)
      (x1, y1) = location t
      (x2, y2) = location t'
      t' = calculateForward n t
runAction (TRight d)     t w  =
  return (t {orientation = mod' ((orientation t) + d) 360})  
runAction (Backward n)   t w  = runAction (Forward (-n)) t w
runAction (TLeft d)      t w  = runAction (TRight (-d)) t w
runAction (PenUp)        t w  = return (t {toggleDraw = False})
runAction (PenDown)      t w  = return (t {toggleDraw = True}) 
runAction (Color c)      t w  = return (t {penColor = c})
runAction (Die)          t w  = return (t {life = False})
runAction (Idle)         t w  = return t
runAction (StartDeathClock n) t w = 
  if ((n >= deathclock t) && (deathclock t /= -1))
  then return t
  else return (t {deathclock = n})
runAction (Parallel p q) t w = do runParallel w [(p, t), (q, t)]
                                  return t
                                       
-- 'runParallel' checks the size of the inputted list of (Program, Turtle)
-- and passes it on to the appropriate function
runParallel :: Window -> [(Program, Turtle)] -> IO ()
runParallel w []      = -- do
--  putStrLn "Ending parallel composition"
  return () 
runParallel w [(p,t)] = -- do
--  putStrLn "Ending parallel composition"
  onTick w p t
runParallel w pt      = do
  -- putStrLn "Following actions performed in parallel: "
  pt' <- mapM (runParallelAction w) (concat $ map addClones pt)
  runParallel w (concat pt')

addClones :: (Program, Turtle) -> [(Program, Turtle)]
addClones (((Parallel p q):ps), t) = [(p, t), (q, t)]
addClones x                        = [x]

-- 'runParallelAction' executes multiple 'Program's in parallel
-- by one Action per 'Program' each
runParallelAction :: Window -> (Program, Turtle) -> IO [(Program, Turtle)] --Perhaps change this type
runParallelAction _w ([], t)                        = return []
--runParallelAction w (((Parallel p q):ps), t) = return [(ps, t), (p, t), (q, t)] --One round "consumed" here.Deal with this in runParallel?
runParallelAction w (p, t)                   =
  case life t of
    False -> return []
    True ->
      do
        -- putStr "  "
        -- printAction (head p) t
        -- putStr "\n"
        t'' <- runAction (head p) t' w
        case tail p of
          [] -> return []
          p' -> return [(p', t'')]
        where
          t' = deathAndTaxes t
          
-- 'printAction' prints the given 'Action'
printAction :: Action -> Turtle -> IO Turtle
printAction (Forward n)         t = do
  putStr $ "Forward " ++ (show n) ++ " - From "
    ++ (show (location t)) ++ " to "
    ++ (show (location t'))
  return t'
  where t' = calculateForward n t
printAction (TRight d)          t = do
  putStr $ "Right " ++ (show d)
    ++ " - Orientation from " ++ (show (orientation t))
    ++ " to " ++ (show (orientation t'))
  return t'
  where t' = t {orientation = mod' ((orientation t) + d) 360}
printAction (TLeft d)           t = do
  putStr $ "Left " ++ show d
    ++ " - Orientation from " ++ (show (orientation t))
    ++ " to " ++ (show (orientation t'))
  return t'
  where t' = t {orientation = mod' ((orientation t) + d) 360}
printAction (Backward n)        t = do
  putStr $ "Backward " ++ show n ++ " - From "
    ++ (show (location t)) ++ " to "
    ++ (show (location t'))
  return t'
  where t' = calculateForward n t
printAction (PenUp)             t = do
  putStr "PenUp"
  return $ t {toggleDraw = False}
printAction (PenDown)           t = do
  putStr "PenDown"
  return $ t {toggleDraw = True}
printAction (Die)               t = do
  putStr "Turtle soup"
  return $ t {life = False}
printAction (Idle)              t = do
  putStr "Idle"
  return t
printAction (Color c)           t = do
  putStr $ "Color " ++ show c
  return $ t {penColor = c}
printAction (StartDeathClock n) t =
  if ((n >= deathclock t) && (deathclock t /= -1))
  then do
    putStr $ "Turtle has " ++ (show (deathclock t))
      ++ " actions to live" 
    return t
  else do
    putStr $ "Turtle has " ++ (show n) ++ " actions to live"
    return (t {deathclock = n})
printAction (Parallel p q)      t = do
  putStrLn $ "Starting parallel composition"
  runParallelText [(p, t), (q, t)]
  return t
  

runParallelText :: [(Program, Turtle)] -> IO ()
runParallelText []      = do
  putStrLn "Ending parallel composition"
  return ()
runParallelText [(p,t)] = do
  putStrLn "Ending parallel composition"
  onTickText p t
runParallelText pt = do
  putStrLn "Following actions performed in parallel"
  pt' <- mapM runParallelTextAction (concat $ map addClones pt)
  runParallelText $ concat pt'

runParallelTextAction :: (Program, Turtle) -> IO [(Program, Turtle)]
runParallelTextAction ([], _) = return []
runParallelTextAction (p, t) =
  case life t of
    False -> return []
    True  -> do
      putStr "  "
      t'' <- printAction (head p) t'
      putStr "\n"
      -- t'' <- printAction (head p) t'
      case tail p of
        [] -> return []
        p' -> return [(p', t'')]
      where
        t' = deathAndTaxes t

-- A turtle with initial values
turtleInit :: Turtle
turtleInit =
  Ttl { location = (150,150), orientation = 270, life = True
      , toggleDraw = True, penColor = Black, deathclock = -1}
