import System.Random (randomIO)
import Control.Monad (replicateM)

data Door = First | Second | Third deriving Eq

door :: IO Door
door = do
  l <- randomIO
  r <- randomIO
  case (l,r) of
    (True, True)  -> return First
    (True, False) -> return Second
    (False, True) -> return Third
    _             -> door

data Strategy = Stay | Switch deriving Eq

simulateMH :: IO Strategy
simulateMH = do
  goat <- door
  guess1 <- door
  -- stay wins if they picked the goat originally
  let stay = if guess1 == goat then True else False
  -- switch loses if they picked the goat originally (as they switch),
  -- and win if they didn't pick it (as they will switch to it)
  let switch = if guess1 == goat then False else True
  return $ case (stay,switch) of
    (True,False) -> Stay
    (False,True) -> Switch
    _ -> error "one of switch or stay always wins."

count :: Int
count = 1000000

main :: IO ()
main = do
  results <- replicateM count simulateMH
  let stays = (fromIntegral $ length $ filter (== Stay) results) / (fromIntegral count)
  let switches = 1 - stays
  putStrLn "Staying wins: "
  putStrLn $ show $ stays
  putStrLn "Switching wins: "
  putStrLn $ show $ switches
