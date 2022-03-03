{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Data.Text
import Example.Sheets

import Options.Applicative
import Data.Semigroup ((<>))

data Sample = Sample
  {id :: String}

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "id"
         <> metavar "SHEET_ID"
         <> help "The id of the spreadsheet to download." )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Download a Google sheet."
     <> header "diurnal - a scheduling system." )

-- We pattern match on an object of type ``Sample``, where ``h`` is the
-- parameter for the ``hello`` field, and ``n`` is the parameter for the
-- ``enthusiasm`` parameter. Note that we specify that the ``quiet`` fields
-- must have the value ``False``. If we do not match on this first pattern, we
-- simply return without doing anything.
greet :: Sample -> IO ()
greet (Sample id) = do
    putStrLn $ "Downloading sheet:" ++ id
    ss <- exampleGetSheet $ pack id
    putStrLn "Downloaded sheet."
greet _ = return ()
