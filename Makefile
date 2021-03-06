pldi: prepare
	elm make src/Spreadsheet.elm --output build/spreadsheet.js

first-time-setup:
	sudo mkdir /minibox
	sudo chown -R $(USER) /minibox

prepare:
	rm -rf build/*
	cp -r vendor static/* build
	ln -s /minibox build/minicell-cache/minibox

purge:
	curl -X post  http://localhost:3000/minicell/purge.json

md: purge
	curl -X post  http://localhost:3000/minicell/C1/write.json -F 'formula=''../docs/markdown-inside-cells.md'
	curl -X post  http://localhost:3000/minicell/B1/write.json -F 'formula=''=FILE(C1)'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=MD(B1)'
	curl -X post  http://localhost:3000/minicell/A0/write.json -d 'formula=''=A1'

	curl -X post  http://localhost:3000/minicell/G2/write.json -F 'formula=''../docs/gridlets.md'
	curl -X post  http://localhost:3000/minicell/D2/write.json -F 'formula=''../docs/links-and-navigation.md'
	curl -X post  http://localhost:3000/minicell/E2/write.json -F 'formula=''../docs/latex.md'
	curl -X post  http://localhost:3000/minicell/F2/write.json -F 'formula=''../docs/graphsheet.md'
	curl -X post  http://localhost:3000/minicell/C2/write.json -F 'formula=''=F2'
	curl -X post  http://localhost:3000/minicell/B2/write.json -F 'formula=''=FILE(C2)'
	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''=MD(B2)'
	curl -X post  http://localhost:3000/minicell/A0/write.json -d 'formula=''=A2'

	curl -X post  http://localhost:3000/minicell/C3/write.json -F 'formula=''../docs/paper.md'
	curl -X post  http://localhost:3000/minicell/B3/write.json -F 'formula=''=FILE(C3)'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''=MD(B3)'
	curl -X post  http://localhost:3000/minicell/A0/write.json -d 'formula=''=A3'

git:
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''README.md'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=README(B1)'

TPL:
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''/tmp/glitch/poppet/views/test.html'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=TPL(B1)'

CAL:
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''trip'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=CAL(B1)'

DIR:
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''hi'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=DIR(B1)'
	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''=LEN(A1)'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''=LAST(A1)'

qr: purge
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''Hello World'
	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''=QR(A1)'

thesis: purge
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''poly'
	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''abstract'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''1'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''=THESIS(A1,A2,A3)'
	curl -X post  http://localhost:3000/minicell/A7/write.json -d 'formula=''=LATEX(A1:B5)'

latex:
	curl -X post  http://localhost:3000/minicell/A24/write.json -d 'formula=''=LATEX(A1:D5)'

youtube: purge
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''nLQRtCEX-E0'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=YT(B1)'

pdf: pdf-2

pdf-0: purge
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''http://localhost:8111/graphsheet.pdf'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''0'
	curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=''1'

pdf-1: pdf-0
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=''=PDF(A1,B1)'
	curl -X post  http://localhost:3000/minicell/C2/write.json -d 'formula=''=PDF(A1,C1)'

pdf-2: pdf-1
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''0'
	curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=''1'
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=''=PDF(A1,B1)'
	curl -X post  http://localhost:3000/minicell/C2/write.json -d 'formula=''=PDF(A1,C1)'

pdf-3: pdf-2
	curl -X post  http://localhost:3000/minicell/D1/write.json -d 'formula=''2'
	curl -X post  http://localhost:3000/minicell/E1/write.json -d 'formula=''3'
	curl -X post  http://localhost:3000/minicell/D2/write.json -d 'formula=''=PDF(A1,D1)'
	curl -X post  http://localhost:3000/minicell/E2/write.json -d 'formula=''=PDF(A1,E1)'

pdf-crop: purge
	# curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=''https://csiflabs.cs.ucdavis.edu/~johari/refs/rt-frp.pdf'
	curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=''http://web.cs.ucdavis.edu/~su/publications/emi.pdf'
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''https://johari.me/minicell.pdf'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''=PDF(A1,0)'
	curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=''=CROP(B1,0,300,0,0)'
	curl -X post  http://localhost:3000/minicell/D1/write.json -d 'formula=''=CROP(B1,400,290,0,300)'

	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''https://csiflabs.cs.ucdavis.edu/~johari/refs/rt-frp.pdf'
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=''=PDF(A2,0)'
	curl -X post  http://localhost:3000/minicell/C2/write.json -d 'formula=''=CROP(B2,0,300,0,0)'
	curl -X post  http://localhost:3000/minicell/D2/write.json -d 'formula=''=CROP(B2,400,410,5,300)'

	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''http://web.cs.ucdavis.edu/~su/publications/emi.pdf'
	curl -X post  http://localhost:3000/minicell/B3/write.json -d 'formula=''=PDF(A3,0)'
	curl -X post  http://localhost:3000/minicell/C3/write.json -d 'formula=''=CROP(B3,0,250,0,0)'

	curl -X post  http://localhost:3000/minicell/A4/write.json -d 'formula=''https://csiflabs.cs.ucdavis.edu/~johari/bb-king.pdf'
	curl -X post  http://localhost:3000/minicell/B4/write.json -d 'formula=''=PDF(A4,0)'
	curl -X post  http://localhost:3000/minicell/C4/write.json -d 'formula=''=CROP(B4,400,400,0,0)'

	curl -X post  http://localhost:3000/minicell/A5/write.json -d 'formula=''https://lenary.co.uk/publications/swizzle_inventor.pdf'
	curl -X post  http://localhost:3000/minicell/B5/write.json -d 'formula=''=PDF(A5,0)'
	curl -X post  http://localhost:3000/minicell/C5/write.json -d 'formula=''=PDF(A5,1)'
	curl -X post  http://localhost:3000/minicell/D5/write.json -d 'formula=''=PDF(A5,2)'
	curl -X post  http://localhost:3000/minicell/E5/write.json -d 'formula=''=PDF(A5,3)'
	curl -X post  http://localhost:3000/minicell/F5/write.json -d 'formula=''=PDF(A5,4)'


	# curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=''0' # startX
	# curl -X post  http://localhost:3000/minicell/C2/write.json -d 'formula=''200' # startY

	# curl -X post  http://localhost:3000/minicell/D2/write.json -d 'formula=''100' # lengthX
	# curl -X post  http://localhost:3000/minicell/E2/write.json -d 'formula=''400' # lengthY

	# curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''=CROP(A1,B2,C2,D2,E2)' # :)

	# curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''=PDF(C1,2)'

github: purge
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''minicell'
	curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=''=GH(B1)'
	curl -X post  http://localhost:3000/minicell/E1/write.json -F 'formula=''@examples/graphviz/issues-open.dot'
	curl -X post  http://localhost:3000/minicell/F1/write.json -F 'formula=''@examples/graphviz/issues.dot'
	curl -X post  http://localhost:3000/minicell/D1/write.json -d 'formula=''=DOT(F1)'
	curl -X post  http://localhost:3000/minicell/D2/write.json -d 'formula=''=DOT(E1)'
	curl -X post  http://localhost:3000/minicell/D3/write.json -d 'formula=''=GUNION(D1,D2)'

	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''=GUNION(C1,D1)'


	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''42'
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=''40'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=''55'
	curl -X post  http://localhost:3000/minicell/B3/write.json -d 'formula=''41'
	curl -X post  http://localhost:3000/minicell/A4/write.json -d 'formula=''31'
	curl -X post  http://localhost:3000/minicell/B4/write.json -d 'formula=''40'



mysql: purge
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula=''example1'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''SELECT city FROM STATION WHERE LAT_N > 39.7;'
	curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=''=MYSQL(A1, B1)'
	curl -X post  http://localhost:3000/minicell/D1/write.json -d 'formula=''=LEN(C1)'


animation: diagrams
	curl -X post  http://localhost:3000/minicell/C4/write.json -d 'formula=''=UNIXEPOCH(A1)'
	curl -X post  http://localhost:3000/minicell/D4/write.json -d 'formula=''=MOD(C4,5)'
	curl -X post  http://localhost:3000/minicell/E2/write.json -d 'formula=''=TURN(A2,D4,5)'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=''square'

	# curl -X post  http://localhost:3000/minicell/D5/write.json -d 'formula=''=DEP(D5)'

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

live-demo-simple: purge
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula==LOAD("observed-activity")'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=*person'
	curl -X post  http://localhost:3000/minicell/B3/write.json -d 'formula=reside'
	curl -X post  http://localhost:3000/minicell/C3/write.json -d 'formula=*house'

	curl -X post  http://localhost:3000/minicell/A13/write.json -d 'formula==XX(A3:C12)'
	curl -X post  http://localhost:3000/minicell/A14/write.json -d 'formula==XMATCH(A13, A1)'

	curl -X post  http://localhost:3000/minicell/A0/write.json -d 'formula==A13'

live-demo: purge
	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula==LOAD("observed-activity")'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=*person1'
	curl -X post  http://localhost:3000/minicell/B3/write.json -d 'formula=observe'
	curl -X post  http://localhost:3000/minicell/C3/write.json -d 'formula=*factory'
	curl -X post  http://localhost:3000/minicell/A4/write.json -d 'formula=*person2'
	curl -X post  http://localhost:3000/minicell/B4/write.json -d 'formula=observe'
	curl -X post  http://localhost:3000/minicell/C4/write.json -d 'formula=*factory'
	curl -X post  http://localhost:3000/minicell/A5/write.json -d 'formula=*person1'
	curl -X post  http://localhost:3000/minicell/B5/write.json -d 'formula=reside'
	curl -X post  http://localhost:3000/minicell/C5/write.json -d 'formula=*house'
	curl -X post  http://localhost:3000/minicell/A6/write.json -d 'formula=*person2'
	curl -X post  http://localhost:3000/minicell/B6/write.json -d 'formula=reside'
	curl -X post  http://localhost:3000/minicell/C6/write.json -d 'formula=*house'

	curl -X post  http://localhost:3000/minicell/A7/write.json -d 'formula=*person1'
	curl -X post  http://localhost:3000/minicell/B7/write.json -d 'formula=rent'
	curl -X post  http://localhost:3000/minicell/C7/write.json -d 'formula=truck'

	curl -X post  http://localhost:3000/minicell/A8/write.json -d 'formula=*person2'
	curl -X post  http://localhost:3000/minicell/B8/write.json -d 'formula=buy'
	curl -X post  http://localhost:3000/minicell/C8/write.json -d 'formula=fertilizer'

	curl -X post  http://localhost:3000/minicell/A13/write.json -d 'formula==XX(A3:C12)'
	curl -X post  http://localhost:3000/minicell/A14/write.json -d 'formula==XMATCH(A13, A1)'

	curl -X post  http://localhost:3000/minicell/A0/write.json -d 'formula==A13'

sm-gunrock-q: purge
	curl -X post  http://localhost:3000/minicell/A0/write.json -d 'formula==X(A1:G10)'

	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=A'
	curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=B'
	curl -X post  http://localhost:3000/minicell/D1/write.json -d 'formula=C'

	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=A'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=B'
	curl -X post  http://localhost:3000/minicell/A4/write.json -d 'formula=C'

	curl -X post  http://localhost:3000/minicell/D2/write.json -d 'formula=1'
	curl -X post  http://localhost:3000/minicell/B3/write.json -d 'formula=2'
	curl -X post  http://localhost:3000/minicell/D3/write.json -d 'formula=3'
	curl -X post  http://localhost:3000/minicell/B4/write.json -d 'formula=4'
	curl -X post  http://localhost:3000/minicell/C4/write.json -d 'formula=5'

sm-gunrock-p: purge
	curl -X post  http://localhost:3000/minicell/A0/write.json -d 'formula==X(A1:G10)'

	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula=A'
	curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=B'
	curl -X post  http://localhost:3000/minicell/D1/write.json -d 'formula=C'
	curl -X post  http://localhost:3000/minicell/E1/write.json -d 'formula=D'
	curl -X post  http://localhost:3000/minicell/F1/write.json -d 'formula=E'

	curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=A'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=B'
	curl -X post  http://localhost:3000/minicell/A4/write.json -d 'formula=C'
	curl -X post  http://localhost:3000/minicell/A5/write.json -d 'formula=D'
	curl -X post  http://localhost:3000/minicell/A6/write.json -d 'formula=E'

	curl -X post  http://localhost:3000/minicell/E2/write.json -d 'formula=1'
	curl -X post  http://localhost:3000/minicell/B3/write.json -d 'formula=2'
	curl -X post  http://localhost:3000/minicell/D3/write.json -d 'formula=3'
	curl -X post  http://localhost:3000/minicell/E3/write.json -d 'formula=4'
	curl -X post  http://localhost:3000/minicell/F3/write.json -d 'formula=5'
	curl -X post  http://localhost:3000/minicell/C4/write.json -d 'formula=6'
	curl -X post  http://localhost:3000/minicell/B5/write.json -d 'formula=7'
	curl -X post  http://localhost:3000/minicell/C5/write.json -d 'formula=8'
	curl -X post  http://localhost:3000/minicell/C6/write.json -d 'formula=9'

cities: purge
	curl -X post  http://localhost:3000/minicell/B2/write.json -d 'formula=sacramento'
	curl -X post  http://localhost:3000/minicell/C2/write.json -d 'formula=berkeley'
	curl -X post  http://localhost:3000/minicell/D2/write.json -d 'formula=portland'
	curl -X post  http://localhost:3000/minicell/E2/write.json -d 'formula=oakland'
	curl -X post  http://localhost:3000/minicell/F2/write.json -d 'formula=davis'
	curl -X post  http://localhost:3000/minicell/A3/write.json -d 'formula=davis'
	curl -X post  http://localhost:3000/minicell/B3/write.json -d 'formula=20'
	curl -X post  http://localhost:3000/minicell/C3/write.json -d 'formula=90'
	curl -X post  http://localhost:3000/minicell/A4/write.json -d 'formula=berkeley'
	curl -X post  http://localhost:3000/minicell/E4/write.json -d 'formula=20'
	curl -X post  http://localhost:3000/minicell/A5/write.json -d 'formula=portland'
	curl -X post  http://localhost:3000/minicell/F5/write.json -d 'formula=360'
	curl -X post  http://localhost:3000/minicell/A6/write.json -d 'formula=sacramento'
	curl -X post  http://localhost:3000/minicell/E6/write.json -d 'formula=10'


	curl -X post  http://localhost:3000/minicell/A1/write.json -d 'formula==X(A2:F6)'
	curl -X post  http://localhost:3000/minicell/C1/write.json -d 'formula=portland'
	curl -X post  http://localhost:3000/minicell/D1/write.json -d 'formula=oakland'
	curl -X post  http://localhost:3000/minicell/B1/write.json -d 'formula==SP(A1,C1,D1)'

	echo curl -X post  http://localhost:3000/minicell/A15/write.json -d 'formula==MAKE(A1:F6)'
	echo curl -X post  http://localhost:3000/minicell/B15/write.json -d 'formula==LATEX(A1:F6)'


sm-triangle-p:
	curl -X post  http://localhost:3000/minicell/F10/write.json -d 'formula=*tri2'
	curl -X post  http://localhost:3000/minicell/G10/write.json -d 'formula=*tri3'
	curl -X post  http://localhost:3000/minicell/E11/write.json -d 'formula=*tri1'
	curl -X post  http://localhost:3000/minicell/E12/write.json -d 'formula=*tri3'
	curl -X post  http://localhost:3000/minicell/F11/write.json -d 'formula=0'
	curl -X post  http://localhost:3000/minicell/G11/write.json -d 'formula=0'
	curl -X post  http://localhost:3000/minicell/F12/write.json -d 'formula=0'


sm-line2-p:
	curl -X post  http://localhost:3000/minicell/F10/write.json -d 'formula=*bot'
	curl -X post  http://localhost:3000/minicell/E11/write.json -d 'formula=*top'
	curl -X post  http://localhost:3000/minicell/F11/write.json -d 'formula=0'

sm-line3-p:
	curl -X post  http://localhost:3000/minicell/F10/write.json -d 'formula=*bot'
	curl -X post  http://localhost:3000/minicell/G10/write.json -d 'formula=*mid'
	curl -X post  http://localhost:3000/minicell/E11/write.json -d 'formula=*top'
	curl -X post  http://localhost:3000/minicell/E12/write.json -d 'formula=*mid'
	curl -X post  http://localhost:3000/minicell/G11/write.json -d 'formula=0'
	curl -X post  http://localhost:3000/minicell/F12/write.json -d 'formula=0'

sm-v-p:
	curl -X post  http://localhost:3000/minicell/F10/write.json -d 'formula=*bot'
	curl -X post  http://localhost:3000/minicell/G10/write.json -d 'formula=*top2'
	curl -X post  http://localhost:3000/minicell/E11/write.json -d 'formula=*top1'
	curl -X post  http://localhost:3000/minicell/E12/write.json -d 'formula=*top2'
	curl -X post  http://localhost:3000/minicell/F11/write.json -d 'formula=0'
	curl -X post  http://localhost:3000/minicell/F12/write.json -d 'formula=0'

sm-diamond-p:
	curl -X post  http://localhost:3000/minicell/F10/write.json -d 'formula=*bot'
	curl -X post  http://localhost:3000/minicell/G10/write.json -d 'formula=*mid2'
	curl -X post  http://localhost:3000/minicell/H10/write.json -d 'formula=*mid1'

	curl -X post  http://localhost:3000/minicell/E11/write.json -d 'formula=*mid1'
	curl -X post  http://localhost:3000/minicell/E12/write.json -d 'formula=*mid2'
	curl -X post  http://localhost:3000/minicell/E13/write.json -d 'formula=*top'

	curl -X post  http://localhost:3000/minicell/F11/write.json -d 'formula=0'
	curl -X post  http://localhost:3000/minicell/F12/write.json -d 'formula=0'

	curl -X post  http://localhost:3000/minicell/G13/write.json -d 'formula=0'
	curl -X post  http://localhost:3000/minicell/H13/write.json -d 'formula=0'


sm-triangle: cities sm-triangle-p gsm
sm-line3: cities sm-line3-p gsm
sm-line2: cities sm-line2-p gsm
sm-v: cities sm-v-p gsm
sm-diamond: cities sm-diamond-p gsm

gsm:
	curl -X post  http://localhost:3000/minicell/A0/write.json -d 'formula==A1'
	curl -X post  http://localhost:3000/minicell/B0/write.json -d 'formula==X(E10:H14)'

	curl -X post  http://localhost:3000/minicell/B5/write.json -d 'formula=10'
	curl -X post  http://localhost:3000/minicell/E3/write.json -d 'formula=66'
	curl -X post  http://localhost:3000/minicell/C5/write.json -d 'formula=12'


	curl -X post  http://localhost:3000/minicell/A8/write.json -d 'formula==IMATCH(B0,A0)'
	curl -X post  http://localhost:3000/minicell/A9/write.json -d 'formula==MATCH(B0,A0)'



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
