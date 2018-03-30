# token-server

## Purpose
This repo is meant as a sandbox for ideas in other backend applications. We are expirementing here with
1. graph-ql like queries between using both the blockchain and postgres as a single datasource
2. records as a way to separate the types used by the database from the rest of the API

## Requirements
See the Makefile for required environment variables
1. postgres database
2. access to a synced main-net node where you have permission to install filters
3. an ERC20 address (the default is the OmiseGo token address)

## Implementation
This project consists of two smaller executables

1. token-indexer : This is one of our standard indexing pipelines that monitors erc20 transfer events from the given
contract.

2. token-server : This is a standard postgres + servant + docs Rest service. It  uses the [composite](https://github.com/ConferHealth/composite) packages for records. The reason is that they provide an interface for their records with most other packages we would like to use -- swagger, aeson, and opalaye (postgres). 

## Haxl
Currently we have limited use of Haxl (just batch fetching token balances), but it is possible to create a `DataSource` that uses both the blockchain and postgres as backends, potentially creating some really interesting queries.
