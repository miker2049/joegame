import sqlite3
import re
import os
import sys

def insert_book(path):
       con = sqlite3.connect( "/home/mik/projects/joegame/assets/books.db")
       db = con.cursor()
       data=open(path,"r").read()
       bid=re.search("^\d+", os.path.basename(path))
       if bid and data:
              db.execute("INSERT INTO books (\"Text#\", text_data) VALUES (?, ?) ON CONFLICT DO NOTHING", (bid.group(0),data))
       con.commit()
       con.close()

insert_book(sys.argv[1])
