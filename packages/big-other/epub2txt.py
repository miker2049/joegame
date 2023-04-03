import ebooklib
from ebooklib import epub
import sys

from bs4 import BeautifulSoup
# Open the epub file
book = epub.read_epub(sys.argv[1])

# Extract text from all chapters and concatenate into one variable
text = ''
for doc in book.get_items():
    print(doc.get_type())
    soup = BeautifulSoup(doc.get_content(),  features='lxml')
    text += soup.get_text()

#save the text to a file
with open(sys.argv[2], 'w') as f:
    f.write(text)
