#!/usr/bin/env python3
from __future__ import unicode_literals
import youtube_dl
import sys
import webvtt


def transcribe_vtt_file(path):
    vtt = webvtt.read(path)

    lines = []
    transcript = ''

    for line in vtt:
        # Strip the newlines from the end of the text.
        # Split the string if it has a newline in the middle
        # Add the lines to an array
        lines.append(line.text.strip())

    # Remove repeated lines
    previous = None
    # print(len(lines))
    for line in lines:
        if line == previous:
            continue
        transcript += " " + line + "\n"
        previous = line

    return transcript


if len(sys.argv) > 1:
    txt = transcribe_vtt_file(sys.argv[1])
    f = open('out.txt', 'w')
    f.write(txt)
    f.close()
else:
    print("no url given")
