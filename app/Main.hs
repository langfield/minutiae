{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import Data.Data
import Data.List
import Data.Semigroup ((<>))
import Data.Text
import Data.Time
import Data.Typeable

import Control.Lens

import Example.Sheets

import Text.Pretty.Simple

import Options.Applicative

import Network.Google.Prelude
import Network.Google.Sheets

newtype Args =
  Args
    { id :: String
    }

sample :: Parser Args
sample =
  Args <$>
  strOption
    (long "id" <>
     metavar "SHEET_ID" <> help "The id of the spreadsheet to download.")

-- The main function only serves to parse command-line arguments.
main :: IO ()
main = demo =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        (fullDesc <>
         progDesc "Download a Google sheet." <>
         header "diurnal - a scheduling system.")

data Block =
  Block
    { title :: String
    , completion :: Float
    , weight :: Float
    , actualMins :: Int
    , mins :: Int
    , time :: UTCTime
    }
  deriving (Data, Typeable)

parseBlock :: [JSONValue] -> Block
parseBlock blockList
  | Data.List.length blockList /= 6 = error "Bad."
  | otherwise =
    Block
      { title = parseJSON (blockList !! 0) :: String
      , completion = 0.5
      , weight = 0.5
      , actualMins = 1
      , mins = 1
      , time = UTCTime (fromGregorian 2021 03 05) (secondsToDiffTime 0)
      }

-- We pattern match on an object of type ``Args``, where ``id`` is the
-- parameter for the ``id`` field.
demo :: Args -> IO ()
demo (Args id) = do
  putStrLn $ "Downloading sheet:" ++ id
    -- Get the first day only, for now.
  valueRange <- exampleGetValue (pack id) "A2:G75"
  print (typeOf valueRange)
  let jsonValueArray :: [[JSONValue]] = valueRange ^. vrValues
  print (typeOf jsonValueArray)
  let row = jsonValueArray !! 0
  let block = parseBlock row
  let bLen =
        Prelude.map constrFields . dataTypeConstrs . dataTypeOf $
        (undefined :: Block)
  print (typeOf bLen)
  pPrint row
  print (typeOf row)
-- If we don't match on first pattern, we simply return.
demo _ = return ()
