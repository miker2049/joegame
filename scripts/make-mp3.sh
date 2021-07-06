#!/usr/bin/env bash
set -euo pipefail

for i in assets/audio/sounds/*.wav; do
	name=$(echo "$i" | cut -d'.' -f1)
	echo "$name"
	ffmpeg -i "$i" "${name}.mp3"
done
