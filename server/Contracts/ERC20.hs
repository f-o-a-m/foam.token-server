{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE TemplateHaskell #-}

module Contracts.ERC20 where

import           Network.Ethereum.Contract.TH

[abiFrom|abis/ERC20.json|]


