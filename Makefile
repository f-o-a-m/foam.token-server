.PHONY: all clean hlint stylish

export

PGHOST ?= localhost
PGPORT ?= 5432
PGUSER ?= postgres
PGPASSWORD ?= password
PGDATABASE ?= token_db

# This is the OmiseGo ERC20 main-net contract address.
TOKEN_ADDRESS ?= 0xd26114cd6EE289AccF82350c8d8487fedB8A0C07
# STARTING_BLOCK ?= 5509406

help: ## Ask for help!
	@grep -E '^[a-zA-Z_-]+:.*?## .*$$' $(MAKEFILE_LIST) | sort | awk 'BEGIN {FS = ":.*?## "}; {printf "\033[36m%-30s\033[0m %s\n", $$1, $$2}'

all:
	stack

stack: ## Build all haskell dependencies.
	stack install
clean: ## Clean all haskell dependencies.
	stack clean

hlint: ## lint all haskell files according to the .hlint.yaml file.
	hlint server "--ignore=Parse error" -XTypeApplications
	hlint app "--ignore=Parse error" -XTypeApplications
	hlint db "--ignore=Parse error" -XTypeApplications
	hlint indexer "--ignore=Parse error" -XTypeApplications

stylish: ## format all haskell files according to the .stylish-haskell.yaml file.
	find ./server -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
	find ./app -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
	find ./db -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
	find ./indexer -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i

create-token-db: stack ## run the postgres migrations.
	create-token-db

token-server: stack ## start the token server.
	NODE_URL=$(NODE_URL) \
	token-server

token-indexer: stack ## start the token indexer.
	NODE_URL=$(NODE_URL) \
	token-indexer
