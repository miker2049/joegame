#!/usr/bin/env sh
git config diff.lfsdb.textconv 'sqlite3 $1 .dump'
