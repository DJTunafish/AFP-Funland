{-# LANGUAGE OverloadedStrings #-}
import qualified Replay as R 
import Web.Scotty
import Data.Monoid
import Data.Text.Lazy
import Control.Monad.IO.Class

type Web a = R.Replay Question Answer a

type Question = [QuestionType]

data QuestionType = QText String | QRadio String [String] | QDrop String [String]
    deriving (Show, Read)

type Answer = [Text]

example :: Web Text
example = do ans1 <- R.ask [QRadio "Are you a boy or a girl?" ["Boy", "Girl"]]
             ans2 <- R.ask [QDrop "What panties are you wearing?" ["Shimapan", "Shiro", "Speedos"]]
             return $ pack ("<html><body>A " ++ (unpack (Prelude.head ans1)) ++ " with " ++ (unpack (Prelude.head ans2)) ++ ", eh? Niiiiice...</html></body>")
             
calculatorExample :: Web Text
calculatorExample = do ans1 <- R.ask [QDrop "Operation" ["*", "+", "-"], 
                                      QText "Term1:", QText "Term2:"]
                       case (parse (term1 ans1), parse (term2 ans1)) of
                            ([], _)  -> calcExampleWrongInput 
                            (_, [])  -> calcExampleWrongInput
                            (p1, p2) -> let (i, _)  = Prelude.head p1
                                            (i', _) = Prelude.head p2
                                        in calcExampleResult (parseCalc (unpack (Prelude.head ans1)) i i')
  where term1 ans  = unpack (Prelude.head $ Prelude.tail ans)
        term2 ans  = unpack $ Prelude.head $ Prelude.tail $ Prelude.tail ans
        parse :: String -> [(Int, String)]
        parse ans  = (reads ans) :: [(Int, String)]
      --  parse2 ans = Prelude.head $ (reads (term2 ans)) :: [(Int, String)]

                            
calcExampleWrongInput :: Web Text
calcExampleWrongInput = do ans1 <- R.ask [QDrop "Operation" ["*", "+", "-"], 
                                          QText "Please only input integer values:", QText "Please only input integer values:"]
                           case (parse (term1 ans1), parse (term2 ans1)) of
                            ([], _)  -> calcExampleWrongInput 
                            (_, [])  -> calcExampleWrongInput
                            (p1, p2) -> let (i, _)  = Prelude.head p1
                                            (i', _) = Prelude.head p2
                                        in calcExampleResult (parseCalc (unpack (Prelude.head ans1)) i i')
  where term1 ans  = unpack (Prelude.head $ Prelude.tail ans)
        term2 ans  = unpack $ Prelude.head $ Prelude.tail $ Prelude.tail ans
        parse ans  = ((reads ans) :: [(Int, String)])
     --   parse2 ans = Prelude.head $ (reads (term2 ans)) :: [(Int, String)]

calcExampleResult :: (String, String) -> Web Text
calcExampleResult (c, r) = do ans <- R.ask [QRadio (c ++ " = " ++ r ++ ". Remember result?") ["Yes", "No"]]
                              case (unpack (Prelude.head ans)) of
                                "Yes" -> calcExampleCarry (read r)
                                "No"  -> calculatorExample

calcExampleCarry :: Int -> Web Text
calcExampleCarry x = do ans1 <- R.ask [QDrop "Operation" ["*", "+", "-"], 
                                      QText "Term1:"]
                        case parse (term1 ans1) of
                            [] -> calcExampleWrongInput 
                            p  -> let (i, _) = Prelude.head p
                                  in calcExampleResult (parseCalc (unpack (Prelude.head ans1)) x i)
  where term1 ans  = unpack (Prelude.head $ Prelude.tail ans)
       -- parse :: String -> (Int, String)
        parse ans  = ((reads ans) :: [(Int, String)])
      --  parse2 ans = Prelude.head $ (reads (term2 ans)) :: [(Int, String)]


parseCalc :: String -> Int -> Int -> (String, String)
parseCalc "+" x y = ((show x) ++ " + " ++ (show y), (show (x + y)))
parseCalc "*" x y = ((show x) ++ " * " ++ (show y), (show (x * y)))
parseCalc "-" x y = ((show x) ++ " - " ++ (show y), (show (x - y)))                              


 

main :: IO ()
main = scotty 3000 $ do
       get "/" (runWeb example)
       post "/" (runWeb example)    
  
-- | The main run function of the module. Given a Web Text describing a form, 
--   it will execute a local website based on that program.   
runForm :: Web Text -> IO ()
runForm r = scotty 3000 $ do
            get "/" (runWeb r)
            post "/" (runWeb r)
    
runWeb :: Web Text -> ActionM () 
runWeb r = do
            t <- getTrace
            ans <- getAnswer
      --      liftIO $ putStrLn $ "ans:" ++ (show ans)
    --        liftIO $ putStrLn $ "t:" ++ (show (R.addAnswer ans t))
        --    liftIO $ putStrLn (show ans) 
            res <- liftIO $ R.run r (tr ans t)
            case res of
                (Left (q, t')) -> do liftIO $ putStrLn $ htmlTags (formTags((page q) ++ embedTrace t' 0))
                                  --   liftIO $ putStrLn $ (show (tr ans t))
                                  --   liftIO $ putStrLn $ (show t)
                                     html $ pack $ htmlTags ((formTags (page q)) ++ embedTrace t' 0)
                (Right text)   -> --do liftIO $ putStrLn $ unpack text
                                     html text
    where tr ans t = case ans of
                      []  -> t
                      _   -> R.addAnswer ans t

getAnswer :: ActionM  (Answer)
getAnswer = do size <- getInput "questionSize"
               case (unpack size) of
                 ""  -> return []
                 s   -> --do liftIO $ putStrLn "getAnswer"
                           answer (read s)
  where answer :: Int -> ActionM (Answer)
        answer 0 = return []
        answer x = do a <- getInput $ pack $ "answer" ++ (show x)
                      as <- answer (x-1)
                      return $ (as ++ [a])    
        
getTrace :: ActionM (R.Trace Answer)
getTrace = do size <- getInput "traceSize"
              case (unpack size) of
                ""  -> do liftIO $ putStrLn $ "No trace. s: " ++ (unpack size)
                          return R.emptyTrace
                s   -> do liftIO (putStrLn $ "Trace has size " ++ s)
                          trace (read s)
  where trace :: Int -> ActionM (R.Trace Answer)
        trace 0 = return []
        trace x = do s <- getInput $ pack $ "hidden" ++ show x
                --     liftIO $ putStrLn "trace"
                --     liftIO $ putStrLn (unpack s)
                --     liftIO $ putStrLn (show x)
                     a <- trace' x (read (unpack s))
                     as <- trace (x-1)
                     return (R.addAnswer a as) 
        
        trace' :: Int -> Int -> ActionM [Text]
        trace' x 0 = return []
        trace' x y = do 
                       a <- getInput $ pack $ "hidden" ++ (show x) ++ "." ++ (show y)
                       as <- trace' x (y-1)
                       return (as ++ [a])
                        

getInput :: Text -> ActionM Text
getInput s = param s `rescue` \ _ -> return ""

page :: Question -> String
page qs = --pack $ "<html><body>" ++ "<form method=post>" ++
          (unpack(questionToText qs 0)) ++ 
          inputTag "submit" "" "Submit" 
         -- "<input type=submit value=Submit>"

questionToText :: Question -> Int -> Text
questionToText [] x = pack $ inputTag "hidden" "questionSize" (show x) 
questionToText ((QText q):qs) x = pack $ q ++ "<br>" ++ (inputTag "" tagName "") ++ "<br>" ++ unpack (questionToText qs (x+1))
  where tagName = "answer" ++ (show (x+1))
questionToText ((QRadio s rs):qs) x = pack $ s ++ "<br>" ++ radioButtons (x+1) rs ++ unpack (questionToText qs (x+1))
  where radioButtons :: Int -> [String] -> String
        radioButtons _ [] = ""
        radioButtons x (r:rs) = (inputTag "radio" ("answer" ++ (show x)) r) ++ r ++ "<br>" ++ radioButtons x rs
questionToText ((QDrop s rs):qs) x = pack $ s ++ "<br>" ++ (selectTag ("answer" ++ (show (x+1))) $ optionTags rs) ++ (unpack (questionToText qs (x+1)))
  where selectTag n s = "<select name=" ++ n ++">" ++ s ++ "</select><br>"
        optionTags [] = ""
        optionTags (r:rs) = "<option value=" ++ r ++ ">" ++ r ++ "</option>" ++ optionTags rs
        
                         
embedTrace :: R.Trace Answer -> Int -> String
embedTrace [] x = --inputTag "hidden" "traceSize" (show x)
  "<input type=hidden name=traceSize value=" ++ (show x) ++ ">"
embedTrace ((R.Answer t):ts) x = --(inputTag "hidden" tagName (show answerSize)) ++
                                 --   embeddedAnswers ++ (embedTrace ts (x+1))
                         "<input type=hidden name=hidden" ++ (show (x+1)) ++ " value=" ++ (show answerSize) ++ ">" ++ 
                         (embeddedAnswers) ++ ((embedTrace ts (x+1)))
  where embedAnswer :: Answer -> Int -> Int -> String -> (String, Int)
        embedAnswer [] index y txt'    = (txt', y)
        embedAnswer (a:as) index y txt' = embedAnswer as index (y+1) $ txt' ++ inputTag "hidden" ("hidden" ++ (show index) ++ "." ++ (show (y+1))) (unpack a)  --"<input type=hidden name=hidden"
-- ++ (show index) ++ "." ++ (show y) ++ " value=" ++ (unpack a) ++ ">") 
        (embeddedAnswers, answerSize) = embedAnswer t (x+1) 0 "" 
        tagName = "hidden" ++ (show (x+1))

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
formTags s = "<form method=post>" ++ s -- ++ "</form>"

htmlTags :: String -> String
htmlTags s = "<html><body>" ++ s ++ "</body></html>"







