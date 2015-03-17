import System.IO (hGetContents, hClose)
import System.Process (runInteractiveProcess)

dates = [mkd y m | m <- [1..12], y <- [1901..2000]]
  where mkd y m = show y ++ "-" ++ show m ++ "-01"


getDate s = do
  (_, out, _, _) <- runInteractiveProcess "date" ["--date=" ++ s] Nothing Nothing
  r <- hGetContents out
  return r

getDates [] = return []
getDates (s:ss) = do r <- getDate s
                     print r -- How can I force evaluation of r without print here?
                     rs <- getDates ss
                     return (r:rs)
                     
answer = do ss <- getDates dates
            return $ sum $ map (\(c1:c2:_) -> if [c1,c2] == "Su" then 1 else 0) ss

main = print =<< answer
