# Minicell


![Minicell screenshot (2018)](https://github.com/johari/minicell/blob/master/static/screenshot.png)

This project "extends the range of applications that can be tackled with spreadsheets", "without loss to usability and accessibility to non-experts."

Minicell provides first-class support for networks and graphs inside the spreadsheet environment.

It makes it easy for non-programmers to perform sophisticated graph modeling tasks,
including solving optimization problems such as shortest path problem or maximum flow.

It also eliminates typical pre-processing steps that are required to
reshape tabular data into a graph model.

We believe graph theory is an important mathematical tool,
and we believe we can make non-programmers more effective in
important (often critical) tasks by providing a natural and effective support
for graph modeling inside spreadsheets.

# Hacking

* [Evaluator](https://github.com/johari/minicell/blob/master/src/Spreadsheet/Evaluator/Parser.hs), where most of the IO in backend happens (I must rename `Parser.hs` to `Interpreter.hs`)
* [SimpleServer.hs](https://github.com/johari/minicell/blob/master/src/SimpleServer.hs)
* [Basic types in Backend](https://github.com/johari/minicell/blob/master/src/Spreadsheet/Types.hs)
* [Basic types in Frontend](https://github.com/johari/minicell/blob/master/src/Spreadsheet/Types.elm)
* [Spreadsheet interface](https://github.com/johari/minicell/blob/master/src/Spreadsheet.elm)
* [Makefile](https://github.com/johari/minicell/blob/master/Makefile)

# See also

* [Calc Intelligence](https://www.microsoft.com/en-us/research/project/calc-intelligence/) from MSR Cambridge
* [FlashExtract](https://www.youtube.com/watch?v=apTsnpsPEds)
* [FlashRelate](https://www.youtube.com/watch?v=g2Dhf4Tmp8c)
* [ExceLint](https://github.com/ExceLint/ExceLint)
* [VCF East 10 - History of VisiCalc - Bob Frankston](https://www.youtube.com/watch?v=6L2jRc6prEw)
* [Meet the inventor of the electronic spreadsheet (2017)](https://www.youtube.com/watch?v=YDvbDiJZpy0)
* [PARALLEL PAGES, VISIBLY CONNECTED](https://youtu.be/VOmm8ic4Eos?t=239)
    * Simply, [Visibly connected documents](https://youtu.be/VOmm8ic4Eos?t=716)
* ["What about graph practice, eh?"](https://twitter.com/tikhonjelvis/status/1070190089345462274)
