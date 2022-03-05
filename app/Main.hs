{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import Data.Aeson
import Data.Aeson.Types
import Data.ByteString.Lazy.UTF8
import Data.Data
import Data.List
import Data.Scientific
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

sample :: Options.Applicative.Parser Args
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

-- Convert from JSONValue to String.
jsonValueToString :: Value -> Maybe String
jsonValueToString (String s) = Just (unpack s)
jsonValueToString _ = Nothing

jsonStringValueToInt :: Value -> Maybe Int
jsonStringValueToInt (String s) = Just (read (unpack s) :: Int)
jsonStringValueToInt _ = Nothing

jsonStringValueToFloat :: Value -> Maybe Float
jsonStringValueToFloat (String s) = Just (read (unpack s) :: Float)
jsonStringValueToFloat _ = Nothing

jsonValueToUTCTime :: Value -> Maybe UTCTime
jsonValueToUTCTime (String s) = Just (read (unpack s) :: UTCTime)
jsonValueToUTCTime _ = Nothing

parseJSONClockTime :: Value -> Day -> Maybe UTCTime
parseJSONClockTime (String s) day =
  Just
    (UTCTime
       day
       (parseTimeOrError True defaultTimeLocale "%H:%M" (unpack s) :: DiffTime))
parseJSONClockTime _ _ = Nothing

data Block =
  Block
    { title :: Maybe String
    , completion :: Maybe Float
    , weight :: Maybe Float
    , actualMins :: Maybe Int
    , mins :: Maybe Int
    , late :: Maybe Int
    , time :: Maybe UTCTime
    }
  deriving (Data, Typeable, Show)

-- Get number of fields in ``Block``.
getBlockLength :: Int
getBlockLength =
  Prelude.length $
  Prelude.head $
  Prelude.map constrFields . dataTypeConstrs . dataTypeOf $ (undefined :: Block)

parseBlock :: [JSONValue] -> Day -> Block
parseBlock blockList day
  | Data.List.length blockList /= getBlockLength =
    error "Number of columns in block is wrong!"
  | otherwise =
    Block
      { title = jsonValueToString $ Prelude.head blockList :: Maybe String
      , completion = jsonStringValueToFloat (blockList !! 1) :: Maybe Float
      , weight = jsonStringValueToFloat (blockList !! 2) :: Maybe Float
      , actualMins = jsonStringValueToInt (blockList !! 3) :: Maybe Int
      , mins = jsonStringValueToInt (blockList !! 4) :: Maybe Int
      , late = jsonStringValueToInt (blockList !! 5) :: Maybe Int
      , time = parseJSONClockTime (blockList !! 6) day :: Maybe UTCTime
      }

-- Dummy function until we actually implement this.
getDay :: [JSONValue] -> Day
getDay _ = fromGregorian 2022 03 06

-- We pattern match on an object of type ``Args``, where ``id`` is the
-- parameter for the ``id`` field.
demo :: Args -> IO ()
demo (Args id) = do
  putStrLn $ "Downloading sheet:" ++ id
  -- Get the first day only, for now.
  valueRange <- exampleGetValue (pack id) "A2:G75"
  let jsonValueArray :: [[JSONValue]] = valueRange ^. vrValues
  let row = Prelude.head jsonValueArray
  let day = getDay row
  let block = parseBlock row day
  pPrint block
-- If we don't match on first pattern, we simply return.
demo _ = return ()
