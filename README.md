# Minicell


![Minicell screenshot (2018)](https://github.com/johari/minicell/blob/master/static/screenshot.png)

Minicell is a rich spreadsheet enrivonment with first-class support for new values inside cells. This includes, but is not limited to
* simple shapes and diagrams along with some geometrical transformations,
* values that flow in time,
* networks and graphs, along with some formulas.

This makes it easy for [end-user programmers](https://en.wikipedia.org/wiki/End-user_development) to perform basic media manipulation tasks or find optimum answers to graph modeling problems
including solving optimization problems such as shortest path problem or maximum flow.

# Hacking

* [Install guide](https://github.com/johari/minicell/blob/master/INSTALL.md) and [Changelog](https://github.com/johari/minicell/blob/master/CHANGELOG.md)
* [Evaluator](https://github.com/johari/minicell/blob/master/src/Spreadsheet/Evaluator/Parser.hs), where most of the IO in backend happens (I must rename `Parser.hs` to `Interpreter.hs`)
    * [Implementation of `=X`](https://github.com/johari/minicell/blob/e90411cce6aa0b0f39e9f6fb844085681778085b/src/Spreadsheet/Evaluator/Parser.hs#L255-L283)
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
