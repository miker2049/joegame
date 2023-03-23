##
# mapscripts
#
# @file
# @version 0.1
all: cli
esbuild = ./node_modules/.bin/esbuild
esbuild_args = --format=esm --target=deno1.30.3 --platform=node \
--external:fflate --external:nock --external:aws-sdk --external:mock-aws-s3 --external:"node:fs" --bundle

.PHONY: cli test-map test-map-image rebuild map

.ONESHELL:
offsets := $(shell seq 0 40 200)
mapslist := $(foreach x,$(offsets),$(foreach y,$(offsets), desert/desert_$y-$x.json))

$(mapslist):
	deno run --unstable --import-map=./deno.json --allow-read --allow-write \
	cli/get-world-map.deno.ts ./src/world-settings-simple.json ../../assets/maps/desert-stamps2.json \
	$@ $(shell echo $@ | cut -d_ -f2 | cut -d- -f1) \
	$(shell echo $@ | cut -d_ -f2 | cut -d- -f2 | cut -d. -f1) 25 25
map: $(mapslist)

$(esbuild):
	pnpm i --filter=mapscripts

esm/%.js: src/%.ts $(esbuild)
	$(esbuild) $(esbuild_args) --outfile=$@ $<

cli: esm/TiledMapCompressed.js esm/TiledMap.js esm/WorldGenerator.js esm/embedTilesetOffline.js esm/saturator.js esm/utils.js esm/hasher.js

tmapX=100
tmapY=100
tmapW=25
tmapH=25
tmapOut=../../assets/maps/cs-test.json
tmapImageOut=cs-test.png
test-map: cli
	deno run --import-map=./deno.json --allow-read --allow-write cli/get-world-map.deno.ts ./src/world-settings-simple.json ../../assets/maps/desert-stamps2.json $(tmapOut) $(tmapX) $(tmapY) $(tmapW) $(tmapH)

.PHONY: test-map-image
test-map-image: cli
	deno run --import-map=./deno.json --allow-read --allow-write cli/map-picture.deno.ts ./src/world-settings.json ../../assets/maps/desert-stamps2.json $(tmapImageOut) $(tmapX) $(tmapY) $(tmapW) $(tmapH)


rebuild: cli


# db gen coords
.PHONY: gen-coords-for-db
gen-coords-for-db:
	deno run -A --unstable cli/genDBcoords.deno.ts

test-gen-polar: esm/utils.js
	deno run -A cli/genCoordsCSV.deno.ts convos.csv convo-coorded.csv 0,0 1 && gnuplot -e "csv='convo-coorded.csv'" -c plotpointsCSV.gp

.PHONY: clean
clean:
	rm esm/*

# end
