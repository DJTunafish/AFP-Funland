{-# LANGUAGE OverloadedStrings #-}
import qualified Replay as R 
import Web.Scotty
import Data.Monoid
import Data.Text.Lazy
import Control.Monad.IO.Class

type Web a = R.Replay Question Answer a

data QuestionType = QText String | QRadio String [String]
                  deriving (Show, Read)

type Question = [QuestionType]
type Answer = [Text]

example :: Web Text
example = do ans <- R.ask [QRadio "Help" ["Kill", "Save"]]
--             ans <- R.ask $ [QText "What panties are you wearing?", QText "What kind of bra?"]
             return $ pack ("<html><body>" ++ (unpack (Prelude.head ans)) ++ ", eh? Niiiiice...</html></body>")
           

main :: IO ()
main = scotty 3000 $ do
       get "/" (runWeb example)
       post "/" (runWeb example)    
    
runForm :: Web Text -> IO ()
runForm r = scotty 3000 $ do
            get "/" (runWeb r)
            post "/" (runWeb r)
    
runWeb :: Web Text -> ActionM () 
runWeb r = do
            t <- getTrace
            ans <- getAnswer
            res <- liftIO $ R.run r (tr ans t)
            case res of
                (Left (q, t')) -> html $ embedTrace t' (page q) 0
                (Right text)   -> html text
    where tr ans t = case ans of
                      []  -> t
                      _   -> R.addAnswer ans t

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
                       a <- getInput $ pack $ "hidden" ++ (show x) ++ "." ++ (show y)
                       as <- trace' x (y-1)
                       return (as ++ [a])
                        

getInput :: Text -> ActionM Text
getInput s = param s `rescue` \ _ -> return ""

page :: Question -> Text
page qs = pack $ "<html><body>" ++ "<form method=post>" ++
          (unpack(questionToText qs 0)) ++ 
          "<input type=submit value=Submit>"
 
questionToText :: Question -> Int -> Text
questionToText ([]) x = pack $ "<input type=hidden name=questionSize value=" ++ (show x) ++ ">"
questionToText ((QText q):qs) x = pack $ "<p>" ++ q ++ "</p>" ++
                         "<p><input name=answer" ++ (show (x+1)) ++ "></p>" ++ (unpack (questionToText qs (x+1)))
questionToText ((QRadio qr rs):qrs) x = pack $ qr ++ "<br>" ++ (radioButtons (x+1) rs) ++ unpack (questionToText qrs (x+1))
  where radioButtons :: Int -> [String] -> String
        radioButtons _ [] = ""
        radioButtons x (r:rs) = "<input type=radio name=answer" ++ (show x) ++ " value=" ++ r ++ ">"++ r ++"<br>"++radioButtons x rs
                         
embedTrace :: R.Trace Answer -> Text -> Int -> Text
embedTrace []  txt x = pack $ (unpack txt) ++ "<input type=hidden name=traceSize value=" ++ (show x) ++ "></body></html>"
embedTrace ((R.Answer t):ts) txt x = pack $ (unpack txt) ++ "<input type=hidden name=hidden" ++ (show (x+1)) ++ " value=" ++ (show answerSize) ++ ">" ++ 
                          (unpack embeddedAnswers) ++ (unpack (embedTrace ts "" (x+1)))
  where embedAnswer :: Answer -> Int -> Int -> Text -> (Text, Int)
        embedAnswer [] index y txt'    = (txt', y)
        embedAnswer (a:as) index y txt'= embedAnswer as index (y+1) $ pack ((unpack txt') ++ "<input type=hidden name=hidden"
                                         ++ (show index) ++ "." ++ (show y) ++ " value=" ++ (unpack a) ++ ">") 
        (embeddedAnswers, answerSize) = embedAnswer t (x+1) 1 "" 

--index.y



