pldi: prepare
	elm make src/Spreadsheet.elm --output build/spreadsheet.js

prepare:
	rm -rf build/*
	cp -r vendor static/* build

purge:
	curl -X post  http://localhost:3000/minicell/purge.json

mysql: purge
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=MYSQL(B1)'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''=MYSQL(42)'

animation: diagrams
	curl -X post  http://localhost:3000/minicell/C4/write.json -d 'formula=''=UNIXEPOCH(A1)'
	curl -X post  http://localhost:3000/minicell/D4/write.json -d 'formula=''=MOD(C4,5)'
	curl -X post  http://localhost:3000/minicell/E2/write.json -d 'formula=''=TURN(A2,D4,5)'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''square'

diagrams: purge
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''circle'
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=''green'
	curl -X post  http://localhost:3000/minicell/B3/write.json -d 'formula=''red'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=SHAPE(B1)'
	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''=PAINT(A1,B2)'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''=PAINT(A1,B3)'
	

	# curl -X post  http://localhost:3000/minicell/D7/write.json -d 'formula=''=5'
	# curl -X post  http://localhost:3000/minicell/C7/write.json -d 'formula=''=UNIXEPOCH(D8)'
	# curl -X post  http://localhost:3000/minicell/B7/write.json -d 'formula=''=MOD(C7, D7)'
	# curl -X post  http://localhost:3000/minicell/A7/write.json -d 'formula=''=TURN(A3, B7, D7)'

	curl -X post  http://localhost:3000/minicell/A4/write.json -d 'formula=''=HCONCAT(A2, A3)'

ouroboros: purge
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=LOAD(B1)'
	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''=GREV(A1)'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''ouroboros'

audiomix: purge
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=Fri Sep 28 12:49:54 PDT 2018.wav'
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=Fri Sep 28 12:50:44 PDT 2018.wav'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula==AUDIO(B1)'
	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula==AUDIO(B2)'
	curl -X post  http://localhost:3000/minicell/A4/write.json -d 'formula==ACONCAT(A1,A2)'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula==AOLAY(A1,A2)'

cities: purge
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=A'
	curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=B'
	curl -X post  http://localhost:3000/minicell/D1/write.json -d 'formula=C'
	curl -X post  http://localhost:3000/minicell/E1/write.json -d 'formula=D'
	curl -X post  http://localhost:3000/minicell/F1/write.json -d 'formula=E'
	curl -X post  http://localhost:3000/minicell/G1/write.json -d 'formula=F'

	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=A'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=B'
	curl -X post  http://localhost:3000/minicell/A4/write.json -d 'formula=C'
	curl -X post  http://localhost:3000/minicell/A5/write.json -d 'formula=D'
	curl -X post  http://localhost:3000/minicell/A6/write.json -d 'formula=E'
	curl -X post  http://localhost:3000/minicell/A7/write.json -d 'formula=F'

	curl -X post  http://localhost:3000/minicell/C2/write.json -d 'formula=1'
	curl -X post  http://localhost:3000/minicell/B3/write.json -d 'formula==C2'

	curl -X post  http://localhost:3000/minicell/F2/write.json -d 'formula=7'
	curl -X post  http://localhost:3000/minicell/G2/write.json -d 'formula=6'

	curl -X post  http://localhost:3000/minicell/D3/write.json -d 'formula=2'
	curl -X post  http://localhost:3000/minicell/E3/write.json -d 'formula=9'
	curl -X post  http://localhost:3000/minicell/F3/write.json -d 'formula=8'
	
	curl -X post  http://localhost:3000/minicell/E4/write.json -d 'formula=3'

	curl -X post  http://localhost:3000/minicell/F5/write.json -d 'formula=4'

	curl -X post  http://localhost:3000/minicell/G6/write.json -d 'formula=5'

	curl -X post  http://localhost:3000/minicell/A9/write.json -d 'formula=''=X(A1:G7)'
	curl -X post  http://localhost:3000/minicell/A10/write.json -d 'formula=''A'
	curl -X post  http://localhost:3000/minicell/A11/write.json -d 'formula=''D'

	curl -X post  http://localhost:3000/minicell/B9/write.json -d 'formula=''=SP(A9, A10, A11)'

graphviz:
	curl -X post  http://localhost:3000/minicell/A2/write.json -F 'formula=''@examples/graphviz/make-a-website-for-a-friend.dot'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=DOT(A2)'
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=''nima'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''=CTX(A1, B2)'


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
