-- | Main entry point for COTS
module Main (main) where

import COTS.CLI (runCLI)

main :: IO ()
main = runCLI 