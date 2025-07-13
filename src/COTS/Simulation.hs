-- | Simulation module for COTS
module COTS.Simulation
  ( simulateTransaction
  , simulatePlutusScript
  ) where

import COTS.Simulation.Core (simulateTransaction)
import COTS.Simulation.Plutus (simulatePlutusScript) 