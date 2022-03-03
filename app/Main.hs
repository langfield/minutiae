{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Example.Sheets

import Options.Applicative
import Data.Semigroup ((<>))

data Sample = Sample
  { hello      :: String
  , quiet      :: Bool
  , enthusiasm :: Int }

sample :: Parser Sample
sample = Sample
      <$> strOption
          ( long "hello"
         <> metavar "TARGET"
         <> help "Target for the greeting" )
      <*> switch
          ( long "quiet"
         <> short 'q'
         <> help "Whether to be quiet" )
      <*> option auto
          ( long "enthusiasm"
         <> help "How enthusiastically to greet"
         <> showDefault
         <> value 1
         <> metavar "INT" )

main :: IO ()
main = greet =<< execParser opts
  where
    opts = info (sample <**> helper)
      ( fullDesc
     <> progDesc "Print a greeting for TARGET"
     <> header "hello - a test for optparse-applicative" )

-- We pattern match on an object of type ``Sample``, where ``h`` is the
-- parameter for the ``hello`` field, and ``n`` is the parameter for the
-- ``enthusiasm`` parameter. Note that we specify that the ``quiet`` fields
-- must have the value ``False``. If we do not match on this first pattern, we
-- simply return without doing anything.
greet :: Sample -> IO ()
greet (Sample h False n) = do
    putStrLn $ "Hello, " ++ h ++ replicate n '!'
    putStrLn "Downloading sheet."
    ss <- exampleGetSheet "0"
    putStrLn "Downloaded sheet."
greet _ = return ()
