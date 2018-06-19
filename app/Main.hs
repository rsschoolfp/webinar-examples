{-# LANGUAGE NoImplicitPrelude #-}

module Main where

import Prelude (IO, pure, Int, Show(show), String, (+), (++), getLine, const, (<$>), ($), show, putStrLn, (++), length, (.), (>>=))
import Data.Functor ((<$))

data Month
  = January
  | February
  | May
  | December
  deriving (Show)

-- data Date = Date Int Month Int

-- now
-- x days ago
-- month, year

data DisplayedDate
  = Now
  | DaysAgo Int
  | Date Month Int

formatDate :: DisplayedDate -> String
formatDate Now = "now"
formatDate (DaysAgo 0) = "today"
formatDate (DaysAgo days) = show days ++ " days ago"
formatDate (Date month year) = show month ++ ", " ++ show year

swap :: (a, b) -> (b, a)
swap (a, b) = (b, a)

main :: IO ()
-- getLine :: IO String
-- length :: String -> Int
-- show :: Int -> String
-- putStrLn :: String -> IO ()
-- IO String -> (String -> IO ()) -> IO ()
-- main = (show . length) <$> getLine >>= putStrLn

main = do
  line <- getLine
  putStrLn $ show $ length line
  -- putStrLn $ line ++ " !! " ++ anotherLine
