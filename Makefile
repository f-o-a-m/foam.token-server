.PHONY: all clean hlint stylish

PGHOST ?= "localhost"
PGPORT ?= "5432"
PGUSER ?= "postgres"
PGPASSWORD ?= "password"
PGDATABASE ?= "token_db"
NODE_URL ?= "http://parity-proxy.foam.svc.cluster.local:8645"
TOKEN_ADDRESS ?= "0xd26114cd6ee289accf82350c8d8487fedb8a0c07"



all:
	stack

stack:
	stack install
clean:
	stack clean

hlint:
	hlint server "--ignore=Parse error" -XTypeApplications
	hlint app "--ignore=Parse error" -XTypeApplications
	hlint db "--ignore=Parse error" -XTypeApplications
	hlint indexer "--ignore=Parse error" -XTypeApplications

stylish:
	find ./server -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
	find ./app -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
	find ./db -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
	find ./indexer -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i

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
