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

addSMA()
addSMA(n=30)
addSMA(n=60)



myStock = read.table("test/stocklist.txt",header=TRUE,encoding="UTF-8",colClasses = "character")
save(myStock,file = "data/stocklist.RData")
load(file = "data/stocklist.RData")


myStock = read.table("test/stocklist.txt",header=TRUE,fileEncoding = "UTF-8",colClasses = "character")
