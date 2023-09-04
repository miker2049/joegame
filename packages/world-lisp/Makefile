##
# world
#
# @file
# @version 0.1



ix=0
iy=0
iw=256
ih=256
iout=map.png
.PHONY: build run csv-to-db echoo

SBCL = sbcl --dynamic-space-size 100000

WORLD := $(SBCL) \
		--load world.asd \
		--load tile-server/tile-server.asd \
		--eval '(ql:quickload "world")'


LISP_FILES := $(wildcard *.lisp)

./world: $(LISP_FILES)
	$(WORLD) \
		--eval "(sb-ext:save-lisp-and-die #p\"world\" :toplevel #'world:main :executable t :compression 12)"

run:
	$(WORLD) \
		--eval '(world:main)'

image:
	$(WORLD) \
		--eval '(world:mapimg-cli)' $(ix) $(iy) $(iw) $(ih)

csv-to-db:
ifndef file
	@echo 'table', 'file', and 'db' need to be specified!
else ifndef table
	@echo 'table', 'file', and 'db' need to be specified!
else ifndef db
	@echo 'table', 'file', and 'db' need to be specified!
else
	echo -e ".separator ","\n.import $(file) $(table)" | sqlite3 $(db)
endif




.ONESHELL:
mw_size := 10240 #20480
offsets := $(shell seq 0 256 $(mw_size))
mapslist := $(foreach x,$(offsets),$(foreach y,$(offsets), tile-server/static/mwtiles/mw-$x-$y.png))

.PHONY: world-image
world-image: ./world
	./world world-image -x $(ix) -y $(iy) -w $(iw) -h $(ih) -o $(iout)

MW_TILE_PATH = tile-server/static/mwtiles

$(MW_TILE_PATH):
	mkdir -p $@

$(mapslist): ./world $(MW_TILE_PATH)
	make -s world-image \
		ix=$(shell echo $@ | cut -d- -f3) \
		iy=$(shell echo $@ | cut -d- -f4 | cut -d. -f1 ) \
		iw=256 \
		ih=256 \
		iout=$@

mw: $(mapslist)


.PHONY: test
test:
	sbcl \
     --non-interactive \
     --eval '(asdf:load-asd "world.asd")' \
     --eval '(ql:quickload :world)' \
     --eval '(asdf:test-system :world)'


# end
