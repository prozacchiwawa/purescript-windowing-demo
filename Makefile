.PHONY: index.js

all: index.js

index.js: src/*.purs
	pulp browserify -t $@

