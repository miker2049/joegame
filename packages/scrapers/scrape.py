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


class MyLogger(object):
    def debug(self, msg):
        print(msg)
        pass

    def warning(self, msg):
        # print(msg)
        pass

    def error(self, msg):
        print(msg)


def my_hook(d):
    print(d['status'])
    # if d['status'] == 'finished':
    #     print('Done downloading, now converting ...')


ydl_opts = {
    'skip_download': True,
    'logger': MyLogger(),
    'outtmpl': 'ytdl',
    'progress_hooks': [my_hook],
    'writeautomaticsub': 'en'
}

if len(sys.argv) > 1:
    with youtube_dl.YoutubeDL(ydl_opts) as ydl:
        ydl.download([sys.argv[1]])
        txt = transcribe_vtt_file('ytdl.en.vtt')
        f = open('out.txt', 'w')
        f.write(txt)
        f.close()
else:
    print("no url given")
