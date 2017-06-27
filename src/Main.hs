{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Monad.Random
import Control.Monad.ST
import Control.Monad.State.Class
import System.Random.Shuffle
import Data.List
import Data.Ord
import Data.STRef
import System.IO

data Player = Player {
  amount :: !Double,
  score :: !Int
  } deriving Show


simulatePlayer :: STRef s Double -> Player -> ST s Player
simulatePlayer pot (Player {amount, score}) = do
  c <- readSTRef pot
  if c - amount > 0
    then do writeSTRef pot (c - amount)
            return (Player {amount, score = score + 1})
    else do writeSTRef pot 1
            return (Player {amount, score})

simulatePot :: [Player] -> [Player]
simulatePot players = runST m
  where m = do
          pot <- newSTRef 1
          mapM (simulatePlayer pot) players

newPlayer :: (Monad m, RandomGen g) => RandT g m Player
newPlayer = Player <$> getRandomR (0.0, 0.1) <*> return 0

printPlayers :: MonadIO m => [Player] -> Handle -> Int -> m ()
printPlayers players h rounds = do
  let sorted = reverse $ sortOn coffee players
      lineFunction = \p@Player{amount} -> (show amount) ++ ":  " ++ (show $ coffee p)
  forM_ (take 20 sorted) $ \p ->
    liftIO $ putStrLn $ lineFunction p
  forM_ sorted $ \p ->
    liftIO $ hPutStrLn h $ lineFunction p
  where coffee p = (fromIntegral $ score p) * amount p / fromIntegral rounds


prog :: (RandomGen g) => RandT g IO ()
prog = do
  playerBase <- replicateM 100 newPlayer >>= (\ps -> return $ ps ++ [Player 0.99 0])

  let rounds = 1000000

  result <- foldl' (>>=) (return playerBase) $ replicate rounds $ \dayN -> do
    dayNPlus1 <- shuffleM dayN
    return $ simulatePot dayNPlus1

  liftIO $ withFile "result.dat" WriteMode $ \h ->
    printPlayers result h rounds

main :: IO ()
main = do
  prng <- getStdGen
  (_, prng') <- runRandT prog prng
  setStdGen prng'
  return ()
