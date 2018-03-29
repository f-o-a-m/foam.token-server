.PHONY: all clean hlint stylish

PGHOST ?= "localhost"
PGPORT ?= "5432"
PGUSER ?= "postgres"
PGPASSWORD ?= "password"
PGDATABASE ?= "token_db"
NODE_URL ?= "http://geth-rinkeby-deploy.foam.svc.cluster.local:8545"
TOKEN_ADDRESS ?=



all:
	stack

stack:
	stack install
clean:
	stack clean

hlint:
	hlint src "--ignore=Parse error" -XTypeApplications

stylish:
	find ./src -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
	find ./app -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i

create-token-db: stack
	PGHOST=$(PGHOST) \
	PGPORT=$(PGPORT) \
	PGUSER=$(PGUSER) \
	PGDATABASE=$(PGDATABASE) \
	PGPASSWORD=$(PGPASSWORD) \
	create-token-db

token-server: stack
	PGHOST=$(PGHOST) \
	PGPORT=$(PGPORT) \
	PGUSER=$(PGUSER) \
	PGDATABASE=$(PGDATABASE) \
	PGPASSWORD=$(PGPASSWORD) \
	NODE_URL=$(NODE_URL) \
	token-server

token-indexer: stack
	PGHOST=$(PGHOST) \
	PGPORT=$(PGPORT) \
	PGUSER=$(PGUSER) \
	PGDATABASE=$(PGDATABASE) \
	PGPASSWORD=$(PGPASSWORD) \
	NODE_URL=$(NODE_URL) \
	TOKEN_ADDRESS=$(TOKEN_ADDRESS) \
  STARTING_BLOCK=$(STARTING_BLOCK) \
	token-server
