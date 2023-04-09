##
# mapscripts
#
# @file
# @version 0.1

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
make-map:
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
test-map-image:
	deno run --import-map=./deno.json --allow-read --allow-write cli/map-picture.deno.ts ./src/world-settings.json ../../assets/maps/desert-stamps2.json $(tmapImageOut) $(tmapX) $(tmapY) $(tmapW) $(tmapH)


# db gen coords
.PHONY: gen-coords-for-db
gen-coords-for-db: esm/hasher.js esm/utils.js
	deno run -A --unstable cli/genDBcoords.deno.ts

.PHONY: test-gen-polar
test-gen-polar:
	deno run -A cli/genCoordsCSV.deno.ts convos.csv convo-coorded.csv 0,0 1 && gnuplot -e "csv='convo-coorded.csv'" -c plotpointsCSV.gp

.PHONY: saturate-map
sat-map-in=../../assets/maps/mmm.json
sat-map-out=../../assets/maps/mmm-out.json
saturate-map:
	deno run --allow-read --allow-write cli/saturate-map.ts $(sat-map-in) $(sat-map-out)

.PHONY: test
test:
	deno test -A --no-check src/
# end
