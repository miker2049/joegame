##
# world
#
# @file
# @version 0.1

compression=2

ix=0
iy=0
iw=256
ih=256
iout=map.png
.PHONY: build run csv-to-db echoo

SBCL = sbcl --dynamic-space-size 100000

WORLD := $(SBCL) \
        --non-interactive \
		--eval '(asdf:load-asd (truename "./world.asd"))' \
		--eval '(ql:quickload "world")' \
		--eval '(ql:quickload "world/server")'

LISP_FILES := $(wildcard *.lisp)
SERVER_FILES := server/app.lisp $(wildcard server/src/*.lisp) \
server/static/js/joegamelib.min.js server/static/js/synth-worklet.js

all: ./world ./ts

web-libs: server/static/js/joegamelib.min.js

server/static/js/joegamelib.min.js:
	make -C ../..  public/joegamelib.min.js
	cp ../../public/joegamelib.min.js $@

server/static/js/synth-worklet.js:
	make -C ../midi dist/synth-worklet.js
	cp ../midi/dist/synth-worklet.js $@

foreign-web-libs: server/static/js/hyperscript.js server/static/js/htmx.js server/static/css/pico.min.css

server/static/js/hyperscript.js:
	curl -L "https://unpkg.com/hyperscript.org@0.9.12" -o $@

server/static/js/htmx.js:
	curl -L "https://unpkg.com/htmx.org@1.9.2" -o $@

server/static/css/pico.min.css:
	curl -L "https://cdn.jsdelivr.net/npm/@picocss/pico@2/css/pico.slate.min.css" -o $@

clean-foreign-web-libs:
	rm  server/static/js/hyperscript.js server/static/js/htmx.js server/static/css/pico.min.css

world:
	$(WORLD) --eval '(world:main)'

./bin/world: $(LISP_FILES)
	$(WORLD) --eval '(asdf:make :world)'
	# $(WORLD) \
	# 	--eval "(sb-ext:save-lisp-and-die \"./world\" :toplevel #'world:main :executable t :compression $(compression))"

./bin/ts: $(SERVER_FILES) foreign-web-libs
	$(WORLD) --eval '(asdf:make :world/server)'
	# $(WORLD) \
	# 	--eval "(sb-ext:save-lisp-and-die \"$@\" :toplevel #'server:start* :executable t :compression $(compression))"

server: $(SERVER_FILES)
	$(WORLD) \
		--eval "(server:start :port 5005)"

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
mapslist := $(foreach x,$(offsets),$(foreach y,$(offsets), server/static/mwtiles/mw-$x-$y.png))

.PHONY: world-image
world-image: ./world
	./world world-image -x $(ix) -y $(iy) -w $(iw) -h $(ih) -o $(iout)

MW_TILE_PATH = server/static/mwtiles

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

# server


# clean
.PHONY: clean
clean:
	rm server/static/js/joegamelib.min.js server/static/js/synth-worklet.js
	make -C ../.. clean
# end
