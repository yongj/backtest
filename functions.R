# This file defines all the functions that are used in strategy backtesting
# Component levels: User -> Account -> Portfolio -> Stock
# setwd("~/R/Proj_Stock")
require(blotter)
require(quantstrat)
require(diagram)
require(FinancialInstrument)
require(quantmod)
require(RAmazonS3)

############ Functions for user account management ###############
addUser <- function(userID="yj"){
  .blotter <<- new.env()  
  saveUser(userID)
  }

saveUser <- function(userID="yj",saveSymbols=FALSE,type="local"){
  if(type=="local"){
    filePath = "C:/Users/jiang_y/Documents/R/Proj_Stock/Proj_Strategy/"
    fileID = paste(filePath,"user_",userID,".dat",sep="")
    if (saveSymbols){
      .symbols=new.env()
      for (symbol in getAllSymbols()){
        data = get(symbol,envir=.GlobalEnv)
        assign(symbol,data,envir=.symbols)
      }
      save(list=c(".blotter",".strategy",".symbols"),file=fileID)
    }
    else {
      #save(list=c(".blotter",".strategy",".instrument"),file=fileID)
      save(list=c(".blotter",".strategy"),file=fileID)
    }
    fileName = paste("user_",userID,"_instrument",sep="")
    saveInstruments(file_name = fileName, dir = filePath)
  }
  if(type=="AmazonS3"){
    bucketStr = "app-account-bucket"
    folder = "Userdata"
    fileName = paste("user_",userID,".dat",sep="")
    fileID = paste(bucketStr,folder,fileName,sep="/")
    temp <- s3Save(list=c(".blotter"),file=fileID,auth=NA)
  }
}

loadUser <- function(userID="yj",type="local"){
  if(type=="local"){
    .blotter <<- new.env()
    filePath = "C:/Users/jiang_y/Documents/R/Proj_Stock/Proj_Strategy/"
    fileID = paste(filePath,"user_",userID,".dat",sep="")
    load(fileID,envir=.GlobalEnv)
    if(exists(".symbols")){
      for (symbol in ls(.symbols)){
        data = get(symbol,envir=.symbols)
        assign(symbol,data,envir=.GlobalEnv)
      }
      rm(data)
      rm(.symbols,envir=.GlobalEnv)
    }
    
    fileName = paste("user_",userID,"_instrument",sep="")
    loadInstruments(file_name = fileName, dir = filePath)
  }
  if(type=="AmazonS3"){
    .blotter <<- new.env()
    bucketStr = "app-account-bucket"
    folder = "Userdata"
    fileName = paste("user_",userID,".dat",sep="")
    fileID = paste(folder,fileName,sep="/")
    s3Load(bucket=bucketStr,name=fileID,auth=NA,envir=.GlobalEnv)
  }

  #printSummary()
}


# low-level functions
constPortf <- function(currency="USD",initDate="1950-01-01"){
  # construct an empty portfolio
  newPortf=new.env()  
  attr(newPortf,"class") <- c("blotter_portfolio","portfolio")
  attr(newPortf,"currency") <- currency
  attr(newPortf,"initDate") <- initDate
  newPortf$symbols = new.env() 
  return(newPortf)  
}

constSymbol <- function(){
  # Construct a stock symbol which can be added to an existing portfolio
  newSymbol <- new.env()
  
  
  newSymbol$posPL = numeric(11)
  attr(newSymbol$posPL,"dim") <- c(1,11)
  
  a <- -631152000
  attr(a,"tzone") <- c("")
  attr(a,"tclass") <- c("POSIXct","POSIXt")
  attr(newSymbol$posPL,"index") <- a
  
  attr(newSymbol$posPL,".indexCLASS") <- c("POSIXct","POSIXt")
  attr(newSymbol$posPL,"tclass") <- c("POSIXct","POSIXt")
  attr(newSymbol$posPL,".indexTZ") <- c("")
  attr(newSymbol$posPL,"tzone") <- c("")
  
  a=list(NULL,c("Pos.Qty","Con.Mult","Ccy.Mult","Pos.Value",
                "Pos.Avg.Cost","Txn.Value","Period.Realized.PL",
                "Period.Unrealized.PL","Gross.Trading.PL",
                "Txn.Fees","Net.Trading.PL"))
  attr(newSymbol$posPL,"dimnames") <- a
  attr(newSymbol$posPL,"class") <- c("posPL","xts","zoo")
  
  # set attributes for posPL.USD
  newSymbol$posPL.USD = numeric(11)
  attr(newSymbol$posPL.USD,"dim") <- c(1,11)
  
  a <- -631152000
  attr(a,"tzone") <- c("")
  attr(a,"tclass") <- c("POSIXct","POSIXt")
  attr(newSymbol$posPL.USD,"index") <- a
  
  attr(newSymbol$posPL.USD,".indexCLASS") <- c("POSIXct","POSIXt")
  attr(newSymbol$posPL.USD,"tclass") <- c("POSIXct","POSIXt")
  attr(newSymbol$posPL.USD,".indexTZ") <- c("")
  attr(newSymbol$posPL.USD,"tzone") <- c("")
  
  a=list(NULL,c("Pos.Qty","Con.Mult","Ccy.Mult","Pos.Value",
                "Pos.Avg.Cost","Txn.Value","Period.Realized.PL",
                "Period.Unrealized.PL","Gross.Trading.PL",
                "Txn.Fees","Net.Trading.PL"))
  attr(newSymbol$posPL.USD,"dimnames") <- a
  attr(newSymbol$posPL.USD,"class") <- c("posPL","xts","zoo")
  
  # set attributes fro txn
  newSymbol$txn = numeric(10)
  attr(newSymbol$txn,"dim") <- c(1,10)
  
  a <- -631152000
  attr(a,"tzone") <- c("")
  attr(a,"tclass") <- c("POSIXct","POSIXt")
  attr(newSymbol$txn,"index") <- a
  
  attr(newSymbol$txn,".indexCLASS") <- c("POSIXct","POSIXt")
  attr(newSymbol$txn,"tclass") <- c("POSIXct","POSIXt")
  attr(newSymbol$txn,".indexTZ") <- c("")
  attr(newSymbol$txn,"tzone") <- c("")
  
  a=list(NULL,c("Txn.Qty","Txn.Price","Txn.Value","Txn.Avg.Cost",
                "Pos.Qty","Pos.Avg.Cost","Gross.Txn.Realized.PL","Txn.Fees",
                "Net.Txn.Realized.PL","Con.Mult" ))
  attr(newSymbol$txn,"dimnames") <- a
  attr(newSymbol$txn,"class") <- c("transactions","xts","zoo")
  
  return(newSymbol)
}

constAcct <- function(currency="USD",initEq=1E6){
  myAcct = list(portfolios=list(),summary=numeric(11),Additions=0,Withdrawals=0,Interest=0)
  attr(myAcct,"currency") <- currency
  attr(myAcct,"initEq") <- initEq
  attr(myAcct,"class") <- c("portfolio_account","account")
  
  attr(myAcct$summary,"dim") <- c(1,11)
  a = -631152000
  attr(a,"tzone")=c("")
  attr(a,"tclass")=c("POSIXct","POSIXt")
  attr(myAcct$summary,"index") <- a
  attr(myAcct$summary,".indexCLASS") <- c("POSIXct","POSIXt")
  attr(myAcct$summary,"tclass") <- c("POSIXct","POSIXt")
  attr(myAcct$summary,".indexTZ")=c("")
  attr(myAcct$summary,"tzone")=c("")
  a=list(NULL,c("Additions","Withdrawals","Realized.PL","Unrealized.PL","Interest",
                "Gross.Trading.PL","Txn.Fees","Net.Trading.PL","Advisory.Fees","Net.Performance",
                "End.Eq"))
  attr(myAcct$summary,"dimnames") <- a
  attr(myAcct$summary,"class")=c("xts","zoo")
  
  for (element in c("Additions","Withdrawals","Interest")){
    attr(myAcct[[element]],"dim") = c(1,1)
    a = -631152000
    attr(a,"tzone")=c("")
    attr(a,"tclass")=c("POSIXct","POSIXt")
    attr(myAcct[[element]],"index") <- a
    attr(myAcct[[element]],".indexCLASS") <- c("POSIXct","POSIXt")
    attr(myAcct[[element]],"tclass") <- c("POSIXct","POSIXt")
    attr(myAcct[[element]],".indexTZ")=c("")
    attr(myAcct[[element]],"tzone")=c("")
    a=list(NULL,element)
    attr(myAcct[[element]],"dimnames") <- a
    attr(myAcct[[element]],"class")=c("xts","zoo")
  }
  
  
  return(myAcct)
}

constPortf4Acct <- function(){
  # Construct portfolis numeric elements that can be added to a existing account
  myPortf4Acct = numeric(9)
    
  attr(myPortf4Acct,"dim") <- c(1,9)
  a = -631152000
  attr(a,"tzone")=c("")
  attr(a,"tclass")=c("POSIXct","POSIXt")
  attr(myPortf4Acct,"index") <- a
  attr(myPortf4Acct,".indexCLASS") <- c("POSIXct","POSIXt")
  attr(myPortf4Acct,"tclass") <- c("POSIXct","POSIXt")
  attr(myPortf4Acct,".indexTZ")=c("")
  attr(myPortf4Acct,"tzone")=c("")
  a=list(NULL,c("Long.Value","Short.Value","Net.Value","Gross.Value","Realized.PL","Unrealized.PL",
                "Gross.Trading.PL","Txn.Fees","Net.Trading.PL"))
  attr(myPortf4Acct,"dimnames") <- a
  attr(myPortf4Acct,"class")=c("portfolio_summary","xts","zoo")
  
  return(myPortf4Acct)
}


addPortf <- function(portfName="myP",symbols=c("DEFAULT"),currency="USD",initDate="1950-01-01"){
  status <- list(Pass=TRUE,Msg="Portfolio added successfully")
  
  if(exists(paste("portfolio",portfName,sep="."),envir=.blotter)){
    status <- list(Pass=FALSE,Msg="Portfolio already exists. Nothing changed!")
    cat("\n",status$Msg,"\n")
    return(status)
  }
  
  if(!exists(".blotter")) .blotter <<- new.env()
  
  if (symbols!=c("DEFAULT")){
    initPortf(name=portfName,symbols=symbols,currency=currency,initDate=initDate)
  }
  else {
    newPortfName = paste("portfolio",portfName,sep=".")
    newPortf = constPortf(currency=currency,initDate=initDate)
    assign(newPortfName,newPortf,envir=.blotter)
  }
  return(status)
  # printSummary()
}

addAcct <- function(acctName="myA",portfolios=c("DEFAULT"),currency="USD",initEq=1E6){
  status <- list(Pass=TRUE,Msg="Account added successfully")
  
  if(exists(paste("account",acctName,sep="."),envir=.blotter)){
    status <- list(Pass=FALSE,Msg="Account already exists. Nothing changed!")
    cat("\n",status$Msg,"\n")
    return(status)
  }
  
  if(!exists(".blotter")) .blotter <<- new.env()
  
  if (portfolios!=c("DEFAULT")){
    initAcct(name=acctName,portfolios=portfolios,currency=currency,initEq=initEq)
  }
  else {
    newAcctName = paste("account",acctName,sep=".")
    newAcct = constAcct(currency=currency,initEq=initEq)
    assign(newAcctName,newAcct,envir=.blotter)
  }
  return(status)
  #printSummary()
}

rmPortf <- function(portf="myP"){
  status <- list(Pass=TRUE,Msg="Portfolio removed successfully!")
  portfName = paste("portfolio",portf,sep=".")
  if(!exists(portfName,envir=.blotter)){
    status <- list(Pass=FALSE,Msg="Portfolio does not exist. Nothing to remove!")
    cat("\n",status$Msg,"\n")
    return(status)
  }
  rm(list=portfName,envir=.blotter)
  return(status)
  #printSummary()
}

rmAcct <- function(acct="myA"){
  status <- list(Pass=TRUE,Msg="Account removed successfully!")
  acctName = paste("account",acct,sep=".")
  if(!exists(acctName,envir=.blotter)){
    status <- list(Pass=FALSE,Msg="Account does not exist. Nothing to remove!")
    cat("\n",status$Msg,"\n")
    return(status)
  }
  rm(list=acctName,envir=.blotter)
  return(status)
  #printSummary()
}

addSymbol2Portf <- function(symbol="DEFAULT",portf="myP"){
  status <- list(Pass=TRUE,Msg="Symbol added to portfolio successfully!")
  portfName = paste("portfolio",portf,sep=".")
  if(exists(symbol,envir=.blotter[[portfName]]$symbols)){
    status <- list(Pass=FALSE,Msg="Symbol exists under porfolio. Nothing to add!")
    cat("\n",status$Msg,"\n")
    return(status)
  }
  newSymbol = constSymbol()
  .blotter[[portfName]]$symbols[[symbol]] = newSymbol  
  return(status)
  #printSummary()
}

addPortf2Acct <- function(portf="myP",acct="myA"){
  status <- list(Pass=TRUE,Msg="Portfolio added to account successfully!")
  acctName = paste("account",acct,sep=".")
  if(!is.null(.blotter[[acctName]]$portfolios[[portf]])){
    status <- list(Pass=FALSE,Msg="Portfolio exists under account. Nothing to add!")
    cat("\n",status$Msg,"\n")
    return(status)
  }  
  newPortf4Acct = constPortf4Acct()
  .blotter[[acctName]]$portfolios[[portf]]=newPortf4Acct
  return(status)
  #printSummary()
}

rmSymbolFromPortf <- function(symbol="DEFAULT",portf="myP"){
  status <- list(Pass=TRUE,Msg="Symbol removed from portfolio successfully!")
  portfName = paste("portfolio",portf,sep=".")
  if(!exists(symbol,envir=.blotter[[portfName]]$symbols)){
    status <- list(Pass=FALSE,Msg="Symbol does not exist under porfolio. Nothing to remove!")
    cat("\n",status$Msg,"\n")
    return(status)
  }
  rm(list=symbol,envir=.blotter[[portfName]]$symbols)
  return(status)
  #printSummary()
}

rmPortfFromAcct <- function(portf="myP",acct="myA"){
  status <- list(Pass=TRUE,Msg="Portfolio removed from account successfully!")
  acctName = paste("account",acct,sep=".")
  if(is.null(.blotter[[acctName]]$portfolios[[portf]])){
    status <- list(Pass=FALSE,Msg="Portfolio does not exist under account. Nothing to remove!")
    cat("\n",status$Msg,"\n")
    return(status)
  } 
  .blotter[[acctName]]$portfolios[[portf]]=NULL
  return(status)
  #printSummary()
}

# additional functions
clearPortf <- function(portf="myP"){
  # Clear the content of a existing portfolio for rerun
  status <- list(Pass=TRUE,Msg="Portfolio cleared successfully")
  portfName = paste("portfolio",portf,sep=".")
  if(!exists(paste("portfolio",portfName,sep="."),envir=.blotter)){
    status <- list(Pass=FALSE,Msg="Portfolio does not exist. Nothing to clear!")
    cat("\n",status$Msg,"\n")
    return(status)
  }
  
  symbols = ls(.blotter[[portfName]]$symbols)
  for (symbol in symbols){
    .blotter[[portfName]]$symbols[[symbol]]=constSymbol()
  }
  return(status)
  #printSummary()
}

getAllSymbols <- function(){
  symbols = character(0)
  for (portf in ls(.blotter)){
    if (is.portfolio(.blotter[[portf]])){
      symbols=c(symbols,ls(.blotter[[portf]]$symbols))
    }
  }
  symbols = unique(symbols)
  return(symbols)
}


############## Funtions for visualization ################

buildData4FlowChart <- function(){
  # build data to plot flow chart
  dataChart <- data.frame(Index=numeric(0),
                          Name=character(0),
                          Type=character(0),
                          ParentIdx=numeric(0),
                          xPos=numeric(0),
                          yPos=numeric(0))
  
  # Add account entries to dataChart
  i <- 0
  for (acct in ls(.blotter)){
    if (is.account(.blotter[[acct]])){
      
      acctName <- strsplit(acct,split="account.")[[1]][2]
      i <- i + 1
      newEntry <- data.frame(Index=i,
                             Name=acctName,
                             Type="Account",
                             ParentIdx=0,    # account parent is user, which is set to 0
                             xPos=0,         # coordinates will be updated later
                             yPos=0)
      dataChart <- rbind(dataChart,newEntry)    
    }
  }
  nAcct <- i
  
  # Add portfolio entries to dataChart
  for (acctIdx in dataChart[dataChart$Type=="Account","Index"]){
    acctName <- paste("account",dataChart[acctIdx,"Name"],sep=".")
    for (portf in names(.blotter[[acctName]]$portfolios)){
      i <- i + 1
      newEntry <- data.frame(Index=i,
                             Name=portf,
                             Type="Portfolio",
                             ParentIdx=acctIdx,    # portfolio parent is account
                             xPos=0,         # coordinates will be updated later
                             yPos=0)
      dataChart <- rbind(dataChart,newEntry)  
    }
  }
  nPortf <- i - nAcct
  
  # Add symbol entries to dataChart
  for (portfIdx in dataChart[dataChart$Type=="Portfolio","Index"]){
    portfName <- paste("portfolio",dataChart[portfIdx,"Name"],sep=".")
    for (symbol in ls(.blotter[[portfName]]$symbols)){
      i <- i + 1
      newEntry <- data.frame(Index=i,
                             Name=symbol,
                             Type="Symbol",
                             ParentIdx=portfIdx,    # symbol parent is portfolio
                             xPos=0,         # coordinates will be updated later
                             yPos=0)
      dataChart <- rbind(dataChart,newEntry) 
    }
  }
  nSymbol <- i - nAcct - nPortf
  
  dataChart[,c("xPos","yPos")] = coordinates (c(nAcct, nPortf, nSymbol))    #update coordinates
  return(dataChart)
}

drawFlowChart <- function(){
  # Draw flowchart diagram for account
  library(diagram)
  
  # setup arrow properties
  arrowPar = list(lwd=2,
                  arr.pos=0.6,
                  arr.length=0.5)
  
  # setup box properties
  acctBox <- list(radx=0.1,
                  rady=0.1,
                  box.col="green",
                  shadow.col="darkgreen",
                  shadow.size = 0.005,
                  cex = 1.5)
  portfBox <- list(radx=0.1,
                   rady=0.05,
                   box.col="blue",
                   shadow.col="darkblue",
                   shadow.size = 0.005,
                   cex = 1.5)
  symbolBox <- list(radx=0.1,
                    rady=0.1,
                    box.col="orange",
                    shadow.col="red",
                    shadow.size = 0.005,
                    cex = 1.5)
  
  dataChart <- buildData4FlowChart()
  
  par(mar = c(2, 2, 2, 2))    # define the margins
  par(ask=FALSE)     # Don't interupt plot process
  openplotmat()  
  
  # draw arrows
  for (idx in dataChart[dataChart$Type!="Account","Index"]){
    parentIdx <- dataChart[idx,"ParentIdx"]
    straightarrow (to = c(dataChart[idx,"xPos"],dataChart[idx,"yPos"]),
                   from = c(dataChart[parentIdx,"xPos"],dataChart[parentIdx,"yPos"]),
                   lwd = arrowPar$lwd, arr.pos = arrowPar$arr.pos, arr.length = arrowPar$arr.length)
  }
  
  
  # Draw Elements  
  for (idx in dataChart[,"Index"]){
    if(dataChart[idx,"Type"]=="Account"){
      textellipse(c(dataChart[idx,"xPos"],dataChart[idx,"yPos"]), 
                  lab = dataChart[idx,"Name"],
                  radx = acctBox$radx,
                  rady = acctBox$rady,
                  box.col = acctBox$box.col, 
                  shadow.col = acctBox$shadow.col,
                  shadow.size = acctBox$shadow.size, 
                  cex = acctBox$cex)
    }
    if(dataChart[idx,"Type"]=="Portfolio"){
      textrect(c(dataChart[idx,"xPos"],dataChart[idx,"yPos"]), 
               lab = dataChart[idx,"Name"],
               radx = portfBox$radx,
               rady = portfBox$rady,
               box.col = portfBox$box.col, 
               shadow.col = portfBox$shadow.col,
               shadow.size = portfBox$shadow.size, 
               cex = portfBox$cex)
    }
    if(dataChart[idx,"Type"]=="Symbol"){
      textellipse(c(dataChart[idx,"xPos"],dataChart[idx,"yPos"]), 
                  lab = dataChart[idx,"Name"],
                  radx = symbolBox$radx,
                  rady = symbolBox$rady,
                  box.col = symbolBox$box.col, 
                  shadow.col = symbolBox$shadow.col,
                  shadow.size = symbolBox$shadow.size, 
                  cex = symbolBox$cex)
    }
    
  }
}


# work with .blotter to operate ACCOUNTS and PORTFOLIOS
printSummary <- function(){
  i=1
  for (acct in ls(.blotter)){
    if (is.account(.blotter[[acct]])){
      acctName = strsplit(acct,split="account.")[[1]][2]
      cat("\n=>ACCOUNT ",i,":\t",acctName,"\n\t",sep="")
      cat(names(.blotter[[acct]]$portfolios),"\n",sep="\t")
      i=i+1;
    }
  }
  acctNum = i-1
  i=1
  symbols = character(0)
  for (portf in ls(.blotter)){
    if (is.portfolio(.blotter[[portf]])){
      portfName = strsplit(portf,split="portfolio.")[[1]][2]
      cat("\n=>PORTFOLIO ",i,":\t",portfName,"\n\t",sep="")
      cat(ls(.blotter[[portf]]$symbols),"\n",sep="\t")
      i=i+1;
      symbols=c(symbols,ls(.blotter[[portf]]$symbols))
    }
  }
  portfNum = i-1
  symbols = unique(symbols)
  cat("\n=>Symbols:",symbols,"\n",sep=" ")
  cat("\n=>Strategy:",ls(.strategy),"\n",sep=" ")
  cat("\n=>Instruments:",ls(envir=FinancialInstrument:::.instrument),"\n",sep=" ")
  cat("\n===> TOTAL ",acctNum," ACCOUNTS AND ",portfNum," PORTFOLIOS\n",sep="")
}




# work with .instrument
prepareSymbols <- function(startDate='2014-01-01',endDate='2014-12-31'){  
  symbols = getAllSymbols()
  for (symbol in symbols){
    getSymbols(symbol, from=startDate, to=endDate, index.class="POSIXct", env = .GlobalEnv)
  }
}

prepareInstrument <- function(startDate='2014-01-01',endDate='2014-12-31'){
  #if(!exists(".instrument")) .instrument <<- new.env()
  currency("USD")  
  symbols = getAllSymbols()
  for (symbol in symbols){
    stock(symbol,currency="USD",multiplier=1)
  }
}



# work with .strategy
addStrategyFaber <- function(myStrat="Faber"){
  # add Faber strategy in .strategy environment
  strategy(myStrat,store=TRUE)
    
  add.indicator(strategy = myStrat, name = "SMA",arguments = list(x = quote(Cl(mktdata)), n=10), label="SMA10")
  add.signal(myStrat,name="sigCrossover",
             arguments = list(columns=c("Close","SMA10"),relationship="gt"),
             label="Cl.gt.SMA")
  add.signal(myStrat,name="sigCrossover",
             arguments = list(columns=c("Close","SMA10"),relationship="lt"),
             label="Cl.lt.SMA")
  
  
  # go long when close > MA
  add.rule(myStrat, name='ruleSignal',
           arguments = list(sigcol="Cl.gt.SMA",sigval=TRUE,orderqty=900,ordertype='market',orderside='long'),
           type='enter')
  # exit when close < MA
  add.rule(myStrat, name='ruleSignal',
           arguments = list(sigcol="Cl.lt.SMA",sigval=TRUE,orderqty='all',ordertype='market',orderside='long'),
           type='exit')
}
addOrderBook <- function(portf="myP",initDate='1997-12-31'){
  if(exists(paste("order_book",portf,sep="."),envir=.strategy)){
    rm(list=paste("order_book",portf,sep="."),envir=.strategy)
  }
  initOrders(portfolio=portf,initDate=initDate)
}


###### Main Functions #####

# run test
quickSetup <- function(portf="myP1",acct="myA",symbols=c("BABA","GOOG"),strat="Faber"){
  rm(.blotter,envir=.GlobalEnv)
  rm(.strategy,envir=.GlobalEnv)
  .blotter <<- new.env()
  .strategy <<- new.env()
  addPortf(portf)
  for (symbol in symbols){
    addSymbol2Portf(symbol,portf)
  }
  addPortf("myP2")
  addSymbol2Portf("SPY","myP2")
  
  addAcct(acct)
  addPortf2Acct(portf,acct)
  addPortf2Acct("myP2",acct)
  addStrategyFaber(strat)
  printSummary()
}

runBackTest <- function(strat="Faber",portf=c("myP1","myP2"),acct="myA"){
  # sanityCheck()
  prepareSymbols()
  prepareInstrument()
  for (p in portf){
    addOrderBook(p)
    clearPortf(p)
  }
  
  
  applyStrategy(strat, portfolios=portf)
  for (p in portf){
    updatePortf(p)
  }
  
  updateAcct(acct)
  updateEndEq(acct)  
}

# view results
getResults <- function(){
  # transactions
  getTxns(Portfolio="myP2",Symbol="BABA",Dates="2014")
  # trade statistics
  t(tradeStats("myP2"))
  perTradeStats("myP2")
  # order book
  getOrderBook("myP2")[,1:5]
  getOrderBook("myP2")[,6:11]
  # account summary
  a = getAccount("myA")
  last(a$summary,5)
}


# Visualize data
plotPosnChart <- function(portf="myP",symbol="IBM", date='2014::'){
  myTheme<-chart_theme()
  myTheme$col$dn.col<-'lightblue'
  myTheme$col$dn.border <- 'lightgray'
  myTheme$col$up.border <- 'lightgray'
  
  # plot performance
  chart.Posn(portf, Symbol = symbol, Dates = date,theme=myTheme,
             TA='add_SMA(n=10,col=4, on=1, lwd=2)')
}



# Generate report

