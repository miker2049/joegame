##
# mapscripts
#
# @file
# @version 0.1
all: cli
esbuild = ./node_modules/.bin/esbuild
esbuild_args = --format=esm --target=deno1.30.3 --platform=node --bundle

$(esbuild):
	pnpm i --filter=mapscripts

esm/%.js: src/%.ts $(esbuild)
	$(esbuild) $(esbuild_args) --outfile=$@ $<

cli: esm/TiledMap.js esm/WorldGenerator.js esm/embedTilesetOffline.js esm/saturator.js esm/utils.js esm/parseCompressed.js

tmapX=100
tmapY=100
tmapW=25
tmapH=25
tmapOut=../../assets/maps/cs-test.json
tmapImageOut=cs-test.png
test-map: cli
	deno run --import-map=./deno.json --allow-read --allow-write cli/get-world-map.deno.ts ./src/world-settings-simple.json ../../assets/maps/desert-stamps2.json $(tmapOut) $(tmapX) $(tmapY) $(tmapW) $(tmapH)

test-map-image: cli
	deno run --import-map=./deno.json --allow-read --allow-write cli/map-picture.deno.ts ./src/world-settings.json ../../assets/maps/desert-stamps2.json $(tmapImageOut) $(tmapX) $(tmapY) $(tmapW) $(tmapH)

rebuild: cli

.PHONY: cli test-map test-map-image rebuild

# end
