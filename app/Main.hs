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

    let sheets :: [Sheet] = view sprSheets ss
    putStrLn $ show $ typeOf sheets
    putStrLn $ show $ Prelude.length sheets

    let sheet = sheets !! 1
    pPrint sheet

    -- let formats = sheet ^. sConditionalFormats
    -- putStrLn $ show $ formats

    -- let rowGroups = sheet ^. sRowGroups
    -- putStrLn $ show $ rowGroups

    -- let sdata = sheet ^. sData
    -- putStrLn $ show $ sdata
    -- putStrLn $ show $ typeOf sdata

    -- let grid = sdata !! 0
    -- putStrLn $ show $ grid
    -- putStrLn $ show $ typeOf grid

    putStrLn "Downloaded sheet."
greet _ = return ()
