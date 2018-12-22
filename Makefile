pldi: prepare
	elm make src/Spreadsheet.elm --output build/spreadsheet.js

prepare:
	rm -rf build/*
	cp -r vendor static/* build

ouroboros:
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=LOAD(B1)'
	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''=GREV(A1)'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''ouroboros'

make graphviz:
	curl -X post  http://localhost:3000/minicell/A2/write.json -F 'formula=''@examples/graphviz/make-a-website-for-a-friend.dot'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=DOT(A2)'


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
	echo
	curl -X post  http://localhost:3000/minicell/B3/write.json -d 'formula=''=MF(A1,A2,A3)'

example2: example1
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''hello'
	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''Hello'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''Interconnected'

poppet:
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''/'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''=MUSTACHE(A1)'

	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''/og-image.jpg'
	curl -X post  http://localhost:3000/minicell/B2/write.json -F 'file1=''@/Users/nima/code/2017/poppet-hs/assets/og-image.jpg'

	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''/favicon.ico'
	curl -X post  http://localhost:3000/minicell/B3/write.json -F 'file1=''@/Users/nima/code/2017/poppet-hs/assets/favicon.ico'

	curl -X post  http://localhost:3000/minicell/A4/write.json -d 'formula=''/city'
	curl -X post  http://localhost:3000/minicell/B4/write.json -d 'formula=''=C4'


	curl -X post  http://localhost:3000/minicell/C4/write.json -d 'formula=''https://www.youtube.com/watch?v=nLQRtCEX-E0'
	

	# curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=''=HTTP(A1:B10)'

modify-target-to-d:
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''D'

modify-target-to-b:
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''B'

recalc:
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=''=SP(A1,A2,A3)'
