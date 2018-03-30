{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Contracts.ERC20 where

import           Network.Ethereum.Web3
import           Network.Ethereum.Web3.TH

[abiFrom|abis/ERC20.json|]


