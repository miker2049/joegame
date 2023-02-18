##  -*- indent-tabs-mode: t; -*-
# joegame
#
# @file
# @version 0.1
cloud_pics = public/mapclouds1.png public/mapclouds1-noshadow.png public/mapclouds1-shadow.png public/mapclouds2.png  public/mapclouds2-noshadow.png  public/mapclouds2-shadow.png
site_files = public/index.html public/map.png public/style.css public/joegame-lib.min.js $(cloud_pics)

.PHONY: site deps publish-site sync-assets clean-emacs clean-site clean-map clean

# Site
# utils


sass = ./site/node_modules/.bin/node-sass
# scss_args =
esbuild = ./site/node_modules/.bin/esbuild
esbuild_args = --minify --format=iife --bundle

$(esbuild) $(sass) &:
	pnpm i --filter site

# emacs for building
emacss:
	ln -s $(shell nix build --no-link --print-out-paths .#emacss)/bin/emacss .

clean-emacs:
	rm -f ./emacss

public/%.html : site/%.org emacss
	./emacss --script build-site.el $< $@

public/%.png : site/%.png
	cp $< $@

public/%.css : site/%.scss $(sass)
	$(sass) --output-style compressed $< > $@

site/map.png:
	./packages/mapscripts/src/cli/mapimage 1080 800 $@

clean-map:
	rm -f site/map.png

site: $(site_files)

clean-site:
	rm -f $(site_files)

publish-site: site
	pnpm wrangler pages publish public


# joegameLib

public/joegame-lib.min.js: packages/joegamelib/src/index.ts
	$(esbuild) $(esbuild_args) --global-name=joegameLib --outfile=$@ $<

# Assets
sync-assets:
	rclone -P --config rclone.conf sync assets/images joegame-assets:joegame-assets

# pnpm stuff
deps:
	pnpm i

clean: clean-site clean-emacs clean-map
# end
