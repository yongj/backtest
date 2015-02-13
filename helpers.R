# work with .blotter to operate ACCOUNTS and PORTFOLIOS
outputStr <- function(){
  
  dataStr = ""
  
  i=1
  for (acct in ls(.blotter)){
    if (is.account(.blotter[[acct]])){
      acctName = strsplit(acct,split="account.")[[1]][2]
      dataStr = paste(dataStr,"\n=>ACCOUNT ",i,":\t",acctName,"\n\t",sep="")
      dataStr = paste(dataStr,paste(names(.blotter[[acct]]$portfolios),collapse = "\t"),"\n",sep="\t")
      i=i+1;
    }
  }
  acctNum = i-1
  i=1
  symbols = character(0)
  for (portf in ls(.blotter)){
    if (is.portfolio(.blotter[[portf]])){
      portfName = strsplit(portf,split="portfolio.")[[1]][2]
      dataStr = paste(dataStr,"\n=>PORTFOLIO ",i,":\t",portfName,"\n\t",sep="")     
      dataStr = paste(dataStr,paste(ls(.blotter[[portf]]$symbols),collapse = "\t"),"\n",sep="\t")
      i=i+1;
      symbols=c(symbols,ls(.blotter[[portf]]$symbols))
    }
  }
  portfNum = i-1
  symbols = unique(symbols)
  dataStr = paste(dataStr,"\n=>Symbols:",paste(symbols,collapse = "\t"),"\n",sep=" ")
  dataStr = paste(dataStr,"\n=>Strategy:",paste(ls(.strategy),collapse = "\t"),"\n",sep=" ")
  dataStr = paste(dataStr,"\n=>Instruments:",paste(ls(envir=FinancialInstrument:::.instrument),collapse = "\t"),"\n",sep=" ")
  dataStr = paste(dataStr,"\n===> TOTAL ",acctNum," ACCOUNTS AND ",portfNum," PORTFOLIOS\n",sep="")
  return(dataStr)
}



takeAction <- function(action="Add",item="Stock",parent="myP",name="myS"){
  if(action=="Add"&&item=="Stock"){
    status <- addSymbol2Portf(symbol=name,portf=parent)
  }
  if(action=="Add"&&item=="Portfolio"){
    status <- addPortf(portfName=name,symbols=c("DEFAULT"),currency="USD",initDate="1950-01-01")
    status <- addPortf2Acct(portf=name,acct=parent)
  }
  if(action=="Add"&&item=="Account"){
    status <- addAcct(acctName=name,portfolios=c("DEFAULT"),currency="USD",initEq=1E6)
  }
  
  if(action=="Delete"&&item=="Stock"){
    status <- rmSymbolFromPortf(symbol=name,portf=parent)
  }
  if(action=="Delete"&&item=="Portfolio"){
    status <- rmPortf(portf=name) 
    status <- rmPortfFromAcct(portf=name,acct=parent)       
  }
  if(action=="Delete"&&item=="Account"){
    status <- rmAcct(acct=name)
  }
  if(action=="Rename"){
    status <- list(Pass=FALSE,Msg="Rename feature will be added later!")
  }
  return(status)
}

acctList <- function(){
  data = list()
  i = 1
  for (acct in ls(.blotter)){
    if (is.account(.blotter[[acct]])){
      acctName = strsplit(acct,split="account.")[[1]][2]
      data[[i]]=acctName
      i=i+1;
    }
  }
  return(data)
}

potfList <- function(){
  data = list()
  i = 1
  for (portf in ls(.blotter)){
    if (is.portfolio(.blotter[[portf]])){
      portfName = strsplit(portf,split="portfolio.")[[1]][2]
      data[[i]]=portfName
      i=i+1;
    }
  }
  return(data)
}

resetUser <- function(userID="yj"){
  loadUser(userID)
}
