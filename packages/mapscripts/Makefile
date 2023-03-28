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



$(esbuild):
	pnpm i --filter=mapscripts

esm/%.js: src/%.ts $(esbuild)
	$(esbuild) $(esbuild_args) --outfile=$@ $<

cli: esm/TiledMapCompressed.js esm/TiledMap.js esm/WorldGenerator.js esm/embedTilesetOffline.js esm/saturator.js esm/utils.js esm/hasher.js


# Basic map rendering recipe
mmapX=100
mmapY=100
mmapW=25
mmapH=25
mmap_out=../../assets/maps/cs-test.json
mmap_settings=./src/world-settings-simple.json
mmap_stamps=../../assets/maps/desert-stamps2.json
mmapImageOut=cs-test.png
.PHONY: make-map
make-map: esm/WorldGenerator.js esm/TiledMap.js esm/saturator.js esm/TiledMapCompressed.js esm/TiledMapInflated.js
	deno run --import-map=./deno.json --allow-read --allow-write cli/get-world-map.deno.ts $(mmap_settings) $(mmap_stamps) $(mmap_out) $(mmapX) $(mmapY) $(mmapW) $(mmapH)

.ONESHELL:
offsets := $(shell seq 0 40 200)
mapslist := $(foreach x,$(offsets),$(foreach y,$(offsets), desert/desert_$y-$x.json))
$(mapslist):
	make make-map mmap_out=$@ mmapX=$(shell echo $@ | cut -d_ -f2 | cut -d- -f1) \
	mmapY=$(shell echo $@ | cut -d_ -f2 | cut -d- -f2 | cut -d. -f1) mmapW=25 mmapH=25

.PHONY: map
map: $(mapslist)

.PHONY: test-map-image
test-map-image: cli
	deno run --import-map=./deno.json --allow-read --allow-write cli/map-picture.deno.ts ./src/world-settings.json ../../assets/maps/desert-stamps2.json $(tmapImageOut) $(tmapX) $(tmapY) $(tmapW) $(tmapH)


# db gen coords
.PHONY: gen-coords-for-db
gen-coords-for-db: esm/hasher.js esm/utils.js
	deno run -A --unstable cli/genDBcoords.deno.ts

test-gen-polar: esm/utils.js
	deno run -A cli/genCoordsCSV.deno.ts convos.csv convo-coorded.csv 0,0 1 && gnuplot -e "csv='convo-coorded.csv'" -c plotpointsCSV.gp

.PHONY: saturate-map
sat-map-in=../../assets/maps/mmm.json
sat-map-out=../../assets/maps/mmm-out.json
saturate-map: esm/utils.js esm/saturator.js esm/TiledMapCompressed.js esm/TiledMapInflated.js esm/TiledMap.js
	deno run --allow-read --allow-write cli/saturate-map.ts $(sat-map-in) $(sat-map-out)

.PHONY: clean
clean:
	rm esm/*

# end
