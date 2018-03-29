.PHONY: all clean hlint stylish

PGHOST ?= "localhost"
PGPORT ?= "5432"
PGUSER ?= "postgres"
PGPASSWORD ?= "password"
PGDATABASE ?= "foam_db"

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

token-server: stack
	PGHOST=$(PGHOST) \
	PGPORT=$(PGPORT) \
	PGUSER=$(PGUSER) \
	PGDATABASE=$(PGDATABASE) \
	PGPASSWORD=$(PGPASSWORD) \
	token-server
