pldi: prepare
	elm make src/Spreadsheet.elm --output build/spreadsheet.js

prepare:
	rm -rf build/*
	cp -r vendor static/* build