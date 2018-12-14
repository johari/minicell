pldi: prepare
	elm make src/Spreadsheet.elm --output build/spreadsheet.js

prepare:
	rm -rf build/*
	cp -r vendor static/* build

example1:
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=LOAD(B1)'
	echo
	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''A'
	echo
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''D'
	echo
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''cities'
	echo
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=''=SP(A1,A2,A3)'

modify-target-to-d:
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''D'

modify-target-to-b:
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''B'

recalc:
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=''=SP(A1,A2,A3)'
