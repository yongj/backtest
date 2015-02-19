# This is a script that saves test commands

choiceList = list("AAPL","MSFT","GOOG","BABA","STX","WDC")

symbol = "AAPL"
data = getSymbols(symbol, auto.assign = FALSE) 
chartSeries(data, subset='2014-9::2014-12',theme="white")
candleChart(data, subset='2014-9::2014-12',theme="white")
candleChart(data, subset='2014-9::2014-12',theme="white", name = "hahaha")
candleChart(data, subset='2014-9::2014-12',theme="white", name = "hahaha", TA="addBBands()")
candleChart(data, subset='2014-9::2014-12',theme="white", name = "hahaha", TA="addBBands();addVo()")

NULL
addBBands()
addCCI()
addADX()
addEMA()
addMACD()