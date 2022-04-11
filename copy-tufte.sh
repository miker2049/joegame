#!/usr/bin/env bash

SCRIPT_RELATIVE_DIR=$(dirname "${BASH_SOURCE[0]}")
cd $SCRIPT_RELATIVE_DIR

yarn run minify site/tufte.css > public/tufte.min.css
echo "minified, copied tufte.css"
cp -rf node_modules/tufte-css/et-book public/
echo "minified, copied tufte fonts"
