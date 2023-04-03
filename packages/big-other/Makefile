sources = \
	bo_texts/ij.txt \
	bo_texts/ta.txt \
	bo_texts/rm.txt \
	bo_texts/df.txt \
	bo_texts/bj.txt \
    bo_texts/dq.txt \
    bo_texts/fp.txt \
    bo_texts/ccru.txt \
    bo_texts/td.txt

bo_texts/%.txt: epub/%.epub
	pandoc -f epub -t plain -s -o $@ $<

books: $(sources)

.PHONY: clean
clean:
	rm -f $(sources)


# end
