##
# mapscripts
#
# @file
# @version 0.1
all: cli
esbuild = ./node_modules/.bin/esbuild
esbuild_args = --format=esm --target=deno1.30.3 --platform=node --bundle

.ONESHELL:
offsets := $(shell seq 0 40 975)
mapslist := $(foreach x,$(offsets),$(foreach y,$(offsets), desert_$y-$x.json))

$(mapslist):
	deno run --import-map=./deno.json --allow-read --allow-write \
	cli/get-world-map.deno.ts ./src/world-settings-simple.json ../../assets/maps/desert-stamps2.json \
	$@ $(shell echo $@ | cut -d_ -f2 | cut -d- -f1) \
	$(shell echo $@ | cut -d_ -f2 | cut -d- -f2 | cut -d. -f1) 25 25
map: $(mapslist)

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


all-maps: cli
	@for x in $$(seq 0 40 975); do \
		for y in $$(seq 0 40 975); do \
            make test-map tmapX=$$x tmapY=$$y tmapW=$(tmapW) tmapH=$(tmapH) tmapOut=../../assets/maps/map-$$x-$$y.json; \
		done \
	done


rebuild: cli

.PHONY: cli test-map test-map-image rebuild map

# end
