{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib

import Data.Aeson
import Data.Aeson.Types
import Data.Data (Data, gmapQ, constrFields, dataTypeOf, dataTypeConstrs)
import Data.Generics.Aliases (ext1Q)
import Data.Maybe (isNothing, fromJust, catMaybes)
import Data.Scientific
import Data.Semigroup ((<>))
import Data.Text (pack, unpack)
import Data.Time
import Data.Typeable

import Control.Lens

import Example.Sheets

import Text.Pretty.Simple

import Options.Applicative

import Network.Google.Prelude (JSONValue)
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

-- TODO: Fix this so it doesn't error-out.
parseJSONClockTime :: Value -> Day -> Maybe UTCTime
parseJSONClockTime (String s) day =
  Just
    (UTCTime
       day
       (parseTimeOrError True defaultTimeLocale "%H:%M" (unpack s) :: DiffTime))
parseJSONClockTime _ _ = Nothing

data Block =
  Block
    { title :: String
    , completion :: Float
    , weight :: Float
    , actualMins :: Int
    , mins :: Int
    , late :: Int
    , time :: UTCTime
    }
  deriving (Data, Typeable, Show)

-- Get number of fields in ``Block``.
getBlockLength :: Int
getBlockLength =
  length $
  head $ map constrFields . dataTypeConstrs . dataTypeOf $ (undefined :: Block)

-- Check if there are any ``Nothing`` values in a ``Block`` instance.
anyNothing :: (Data d) => d -> Bool
anyNothing = or . gmapQ (const False `ext1Q` isNothing)

-- Parse a block from a fixed-length list of JSONValues.
parseBlock :: Day -> [JSONValue] -> Maybe Block
parseBlock day blockList
  | length blockList /= getBlockLength =
    error "Number of columns in block is wrong!"
  | isNothing title = Nothing
  | isNothing completion = Nothing
  | isNothing weight = Nothing
  | isNothing actualMins = Nothing
  | isNothing mins = Nothing
  | isNothing late = Nothing
  | isNothing time = Nothing
  | otherwise =
    Just
      Block
        { title = fromJust title
        , completion = fromJust completion
        , weight = fromJust weight
        , actualMins = fromJust actualMins
        , mins = fromJust mins
        , late = fromJust late
        , time = fromJust time
        }
  where
    title = jsonValueToString $ head blockList :: Maybe String
    completion = jsonStringValueToFloat (blockList !! 1) :: Maybe Float
    weight = jsonStringValueToFloat (blockList !! 2) :: Maybe Float
    actualMins = jsonStringValueToInt (blockList !! 3) :: Maybe Int
    mins = jsonStringValueToInt (blockList !! 4) :: Maybe Int
    late = jsonStringValueToInt (blockList !! 5) :: Maybe Int
    time = parseJSONClockTime (blockList !! 6) day :: Maybe UTCTime

-- Dummy function until we actually implement this.
getDay :: [[JSONValue]] -> Day
getDay _ = fromGregorian 2022 03 06

-- We pattern match on an object of type ``Args``, where ``id`` is the
-- parameter for the ``id`` field.
demo :: Args -> IO ()
demo (Args id) = do
  putStrLn $ "Downloading sheet:" ++ id
  -- Get the first day only, for now.
  valueRange <- exampleGetValue (pack id) "A2:G75"
  let rows :: [[JSONValue]] = valueRange ^. vrValues
  let day = getDay rows
  let blocks = catMaybes $ map (parseBlock day) rows
  -- print blocks
  putStrLn "Hello"
-- If we don't match on first pattern, we simply return.
demo _ = return ()
