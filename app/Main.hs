{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Lib
import Data.Text
import Data.Typeable
import Control.Lens
import Text.Pretty.Simple

import Example.Sheets
import Network.Google.Sheets

import Options.Applicative
import Data.Semigroup ((<>))

newtype Args = Args {id :: String}

sample :: Parser Args
sample = Args
      <$> strOption
          ( long "id"
         <> metavar "SHEET_ID"
         <> help "The id of the spreadsheet to download." )

-- The main function only serves to parse command-line arguments.
main :: IO ()
main = demo =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Download a Google sheet."
     <> header "diurnal - a scheduling system." )

-- We pattern match on an object of type ``Args``, where ``id`` is the
-- parameter for the ``id`` field.
demo :: Args -> IO ()
demo (Args id) = do
    putStrLn $ "Downloading sheet:" ++ id
    valueRange <- exampleGetValue (pack id) "A1:A2"
    print (typeOf valueRange)
    pPrint valueRange

-- If we don't match on first pattern, we simply return.
demo _ = return ()
