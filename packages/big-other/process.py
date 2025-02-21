#! /usr/bin/env nix-shell
#! nix-shell -i python3 -p python3Packages.stanza -p glibc
from stanza.server import CoreNLPClient
import json
import hashlib
import sqlite3
from multiprocessing import Pool, cpu_count, Lock
import sys
import os
import time
import stanza


mutex = Lock()
# connect to the database
conn = sqlite3.connect('bigother.db', check_same_thread=False)

# create a cursor object
cursor = conn.cursor()

max_chars = 10000
client = CoreNLPClient(
            output_format='json',
            max_char_length=max_chars,
            timeout=90000)


def process(text):
    ann = client.annotate(text)
    return ann
    # for q in ann.quote:
    #     print(q.text)
    # for m in ann.mentions:
    #     print(m.entityMentionText, m.entityType)

# clump large amounts of text into smaller chunks, based on a max character count.
# This is done character by character, so it doesn't assume new lines.
# But allow overlap to not split sentences.
def clump(text, max_chars=10000, overlap=200):
    chunks = []
    chunk = ''
    for char in text:
        chunk += char
        if len(chunk) >= max_chars:
            chunks.append(chunk)
            chunk = chunk[-overlap:]
    chunks.append(chunk)
    return chunks

def detokenize(sentence):
    text = ''
    for t in sentence["tokens"]:
        text += t["originalText"] + t["after"]
    return text

def hash_content(content):
    hash_object = hashlib.sha256(content.encode())
    hex_dig = hash_object.hexdigest()
    return hex_dig

def save_to_db(ann, book):
    cursor.execute('''CREATE TABLE IF NOT EXISTS sentences
                    (id INTEGER PRIMARY KEY,
                     book TEXT,
                     data TEXT)''')
    cursor.execute('''CREATE TABLE IF NOT EXISTS quotes
                    (id INTEGER PRIMARY KEY,
                     hash TEXT UNIQUE,
                     book TEXT,
                     data TEXT)''')
    for s in ann['sentences']:
        cursor.execute('''INSERT OR REPLACE INTO sentences (book,data) VALUES (?, ?)''',
                       (book, json.dumps(s)))
    for q in ann['quotes']:
        cursor.execute('''INSERT OR REPLACE INTO quotes (book,hash,data) VALUES (?, ?, ?)''',
                       (book, hash_content(q['text']), json.dumps(q)))
    conn.commit()

def process_chunk(args):
    chunk, book = args
    ann = process(chunk)
    mutex.acquire()
    save_to_db(ann, book)
    mutex.release()


# chunk stdin into smaller chunks, process each chunk, and print the results
def proc(filepath, name):
    with open(filepath) as f:
        text = f.read()
        chunks = clump(text, max_chars=max_chars)

        # create a pool of worker processes
        pool = Pool(processes=cpu_count()-5)

        # process each chunk concurrently
        args_list = [(chunk, name) for chunk in chunks]
        result = pool.map_async(process_chunk, args_list, chunksize=1)

        while not result.ready():
            # print progress information while waiting for the workers to finish
            processed = len(chunks) - result._number_left
            print(f"Processed {processed} of {len(chunks)} chunks of {name}.  Number left: {result._number_left}")
            time.sleep(1)

def process_files(files):
    for f in files:
        name = os.path.splitext(os.path.basename(f))[0]
        proc(f,name)


if __name__ == '__main__':
    # print(__name__)
    # files=["bj.txt", "df.txt", "dq.txt", "em.txt", "fp.txt", "ij.txt", "mb.txt", "rm.txt", "ta.txt", "td.txt", "u.txt"]
    files=[sys.argv[1]]
    process_files(files)
    conn.close()
