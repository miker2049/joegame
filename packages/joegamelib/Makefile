##
# joegamelib
#
# @file
# @version 0.1

PNPM = pnpm
VITE = ./node_modules/.bin/vite

all: dist/joegamelib.iife.js

node_modules:
	$(PNPM) --filter . --config.dedupe-peer-dependents=false i

srcfiles := $(shell find src -iname "*.ts")

dist/joegamelib.iife.js: node_modules $(srcfiles)
	$(VITE) build

# end
