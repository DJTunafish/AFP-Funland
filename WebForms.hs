{-# LANGUAGE OverloadedStrings #-}
module WebForms (
  -- * Form types
  module Web.Scotty
  , module Control.Monad.IO.Class
  , Question, QuestionType (..)
  , Answer
  -- * Run functions
  , runForm, main

  -- * Example function
  , calculatorExample
  ) where

import qualified Replay as R
import Web.Scotty
import Data.Monoid
import Data.Text.Lazy
import Control.Monad.IO.Class

type Web a = R.Replay Question Answer a

-- | A 'Question' is a list of 'QuestionType's
type Question = [QuestionType]

-- | A 'QuestionType' is a question that can be constructed in multiple ways
-- Each constructor represents a different input type in an HTML form
-- QText is a simple text input
-- QRadio represents radio buttons with a question and a list of options
-- QDrop represents a dropdown menu with a question and a list of options
data QuestionType = QText String
                  | QRadio String [String]
                  | QDrop String [String]
                  deriving (Show, Read)
-- | An 'Answer' is a list of Text input
type Answer = [Text]


main :: IO ()
main = scotty 3000 $ do
       get "/" (runWeb calculatorExample)
       post "/" (runWeb calculatorExample)

-- | The main run function of the module. Given a Web Text describing a form, 
--   it will execute a local website based on that program.   
runForm :: Web Text -> IO ()
runForm r = scotty 3000 $ do
            get "/" (runWeb r)
            post "/" (runWeb r)

-- An example that allows the user to do
-- simple binary calculations with integers.
-- Also parses so that only integer values can bu inputted
calculatorExample :: Web Text
calculatorExample = do
  ans1 <- R.ask [QDrop "Operation" ["*", "+", "-"], 
                 QText "Term1:", QText "Term2:"]
  case (parse (term1 ans1), parse (term2 ans1)) of
    ([], _)  -> calcExampleWrongInput 
    (_, [])  -> calcExampleWrongInput
    (p1, p2) -> let (i, _)  = Prelude.head p1
                    (i', _) = Prelude.head p2
                in calcExampleResult $ parseCalc (
                                         unpack (Prelude.head ans1)) i i'
  where term1 ans  = unpack (Prelude.head $ Prelude.tail ans)
        term2 ans  = unpack $ Prelude.head $ Prelude.tail $ Prelude.tail ans
        parse :: String -> [(Int, String)]
        parse ans  = (reads ans) :: [(Int, String)]

-- If the input is not an integer, this page is shown
-- that asks the user to only give integer values
calcExampleWrongInput :: Web Text
calcExampleWrongInput = do
  ans1 <- R.ask [QDrop "Operation" ["*", "+", "-"], 
                 QText "Please only input integer values:",
                 QText "Please only input integer values:"]
  case (parse (term1 ans1), parse (term2 ans1)) of
    ([], _)  -> calcExampleWrongInput
    (_, [])  -> calcExampleWrongInput
    (p1, p2) -> let (i, _)  = Prelude.head p1
                    (i', _) = Prelude.head p2
                in calcExampleResult (parseCalc (
                                         unpack (Prelude.head ans1)) i i')
  where term1 ans  = unpack (Prelude.head $ Prelude.tail ans)
        term2 ans  = unpack $ Prelude.head $ Prelude.tail $ Prelude.tail ans
        parse ans  = ((reads ans) :: [(Int, String)])

-- Returns the result of a calculation as a Text
calcExampleResult :: (String, String) -> Web Text
calcExampleResult (c, r) = do
  ans <- R.ask [QRadio (c ++ " = " ++ r ++ ". Remember result?") ["Yes", "No"]]
  case (unpack (Prelude.head ans)) of
    "Yes" -> calcExampleCarry (read r)
    "No"  -> calculatorExample

-- If the user chooses to keep the result from the previous calculation
-- this Text will be used
calcExampleCarry :: Int -> Web Text
calcExampleCarry x = do
  ans1 <- R.ask [QDrop "Operation" ["*", "+", "-"], 
                 QText "Term1:"]
  case parse (term1 ans1) of
    [] -> calcExampleWrongInput 
    p  -> let (i, _) = Prelude.head p
          in calcExampleResult (
            parseCalc (unpack (Prelude.head ans1)) x i)
  where term1 ans  = unpack (Prelude.head $ Prelude.tail ans)
        parse ans  = ((reads ans) :: [(Int, String)])
-- Helper function to the calculator example
parseCalc :: String -> Int -> Int -> (String, String)
parseCalc "+" x y = ((show x) ++ " + " ++ (show y), (show (x + y)))
parseCalc "*" x y = ((show x) ++ " * " ++ (show y), (show (x * y)))
parseCalc "-" x y = ((show x) ++ " - " ++ (show y), (show (x - y)))                              


-- Used to construct the Text for each HTML page.
-- If there is an embedded trace in the currently active page, it will be used.
runWeb :: Web Text -> ActionM () 
runWeb r = do
  t <- getTrace
  ans <- getAnswer
  res <- liftIO $ R.run r (tr ans t)
  case res of
    (Left (q, t')) ->
      html $ pack $ htmlTags ((formTags (page q)) ++ embedTrace t' 0)
    (Right text)   ->
      html text
  where tr ans t = case ans of
          []  -> t
          _   -> R.addAnswer ans t
-- Helper function to fetch the input from all the input fields
getAnswer :: ActionM  (Answer)
getAnswer = do size <- getInput "questionSize"
               case (unpack size) of
                 ""  -> return []
                 s   -> answer (read s)
  where answer :: Int -> ActionM (Answer)
        answer 0 = return []
        answer x = do a <- getInput $ pack $ "answer" ++ (show x)
                      as <- answer (x-1)
                      return $ (as ++ [a])

-- Helper function to fetch the hidden trace
getTrace :: ActionM (R.Trace Answer)
getTrace = do size <- getInput "traceSize"
              case (unpack size) of
                ""  -> return R.emptyTrace
                s   -> trace (read s)
  where trace :: Int -> ActionM (R.Trace Answer)
        trace 0 = return []
        trace x = do s <- getInput $ pack $ "hidden" ++ show x
                     a <- trace' x (read (unpack s))
                     as <- trace (x-1)
                     return (R.addAnswer a as) 
        
        trace' :: Int -> Int -> ActionM [Text]
        trace' x 0 = return []
        trace' x y = do 
                       a <- getInput $ pack $ "hidden"
                            ++ (show x) ++ "." ++ (show y)
                       as <- trace' x (y-1)
                       return (as ++ [a])
                        
-- Helper function to find a single input
getInput :: Text -> ActionM Text
getInput s = param s `rescue` \ _ -> return ""

-- Creates the page with a submit button
page :: Question -> String
page qs = questionToString qs 0 ++ inputTag "submit" "" "Submit"

-- Translates a question into a series of input tags
questionToString :: Question -> Int -> String
questionToString [] x = inputTag "hidden" "questionSize" (show x) 
questionToString ((QText q):qs) x =
  q ++ "<br>" ++ (inputTag "" tagName "") ++ "<br>"
  ++ (questionToString qs (x+1))
  where tagName = "answer" ++ (show (x+1))
questionToString ((QRadio s rs):qs) x =
  s ++ "<br>"
  ++ radioButtons (x+1) rs
  ++ (questionToString qs (x+1))
  where radioButtons :: Int -> [String] -> String
        radioButtons _ [] = ""
        radioButtons x (r:rs) =
          (inputTag "radio" ("answer" ++ (show x)) r)
          ++ r ++ "<br>" ++ radioButtons x rs
questionToString ((QDrop s rs):qs) x =
  s ++ "<br>" ++ (selectTag ("answer" ++ (show (x+1))) $ optionTags rs)
  ++ (questionToString qs (x+1))
  where selectTag n s = "<select name=" ++ n ++">" ++ s ++ "</select><br>"
        optionTags [] = ""
        optionTags (r:rs) = "<option value=" ++ r ++ ">"
                            ++ r ++ "</option>" ++ optionTags rs
        
-- Creates hidden input tags representing a trace
embedTrace :: R.Trace Answer -> Int -> String
embedTrace [] x = inputTag "hidden" "traceSize" (show x)
embedTrace ((R.Answer t):ts) x =
  (inputTag "hidden" ("hidden" ++ (show (x+1))) (show answerSize)) ++
  (embeddedAnswers) ++ ((embedTrace ts (x+1)))
  where embedAnswer :: Answer -> Int -> Int -> String -> (String, Int)
        embedAnswer [] index y txt'    =
          (txt', y)
        embedAnswer (a:as) index y txt' =
          embedAnswer as index (y+1) $ txt'
          ++ inputTag "hidden" ("hidden" ++ (show index)
                                ++ "." ++ (show (y+1))) (unpack a)
        (embeddedAnswers, answerSize) = embedAnswer t (x+1) 0 ""
        tagName = "hidden" ++ (show (x+1))

-- Generates a single input tag
inputTag :: String -> String -> String -> String
inputTag t name value = "<input " ++ t' ++ name' ++ val ++ ">"
  where val = case value of
                      "" -> ""
                      _  -> " value=" ++ value
        t'  = case t of
                "" -> ""
                _  -> "type=" ++ t ++ " "
        name' = case name of
                   "" -> ""
                   _  -> "name=" ++ name ++ " " 

formTags :: String -> String
formTags s = "<form method=post>" ++ s

htmlTags :: String -> String
htmlTags s = "<html><body>" ++ s ++ "</body></html>"







