.PHONY: all clean hlint stylish

all:
	stack

stack:
	stack install
clean:
	stack clean

hlint:
	hlint api server "--ignore=Parse error" -XTypeApplications

stylish:
	find ./server -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i
	find ./api -name "*.hs" | xargs stylish-haskell -c ./.stylish_haskell.yaml -i

