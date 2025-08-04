CMAKE       := cmake .. -GNinja -DCMAKE_BUILD_TYPE=Debug
MKDIR_BUILD := mkdir -p build && cd build

.PHONY: test
test:
	$(MKDIR_BUILD) && $(CMAKE) && ninja && ctest -VV

debug:
	$(MKDIR_BUILD) && $(CMAKE) && ninja

clean:
	rm -rf build
	rm -f tags
	rm -f ./test/encoding=sjis.csv
	rm -f ./test/encoding=utf8.csv
	rm -f ./test/utf8.csv
	rm -f ./test/family_crest.png

git: clean
	git add . && \
	git commit -m "$(shell hostname)" && \
	git push
