{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Random
import Control.Monad.State
import Control.Monad.State.Class
import System.Random.Shuffle
import Data.List
import Data.Ord

data Player = Player {
  amount :: Double,
  score :: Int
  } deriving Show


simulatePlayer :: Player -> State Double Player
simulatePlayer (Player {amount, score}) = do
  c <- get
  if c - amount > 0
    then do put (c - amount)
            return (Player {amount, score = score + 1})
    else do put 1
            return (Player {amount, score})

simulatePot :: [Player] -> [Player]
simulatePot players = fst $ runState m 1
  where m = mapM simulatePlayer players

newPlayer :: (Monad m, RandomGen g) => RandT g m Player
newPlayer = Player <$> getRandomR (0.0, 1.0) <*> return 0

printPlayers :: MonadIO m => [Player] -> m ()
printPlayers players = do
  let sorted = reverse $ sortOn coffee players
  forM_ (take 20 sorted) $ \p@Player{amount} ->
    liftIO $ putStrLn $ (show $ coffee p) ++ ":  " ++ (show amount)
  where coffee p = (fromIntegral $ score p) * amount p


prog :: (RandomGen g) => RandT g IO ()
prog = do
  playerBase <- replicateM 100 newPlayer

  result <- foldl' (>>=) (return playerBase) $ replicate 10000 $ \dayN -> do
    dayNPlus1 <- shuffleM dayN
    return $ simulatePot dayNPlus1

  printPlayers result

main :: IO ()
main = do
  prng <- getStdGen
  (_, prng') <- runRandT prog prng
  setStdGen prng'
  return ()
