#!/usr/bin/env sh
git config diff.lfsdb.textconv "f(){ sqlite3 \"\$1\" .dump; }; f"
