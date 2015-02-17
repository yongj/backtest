# server.R

source("functions.R")
source("helpers.R")
type="local" # "AmazonS3"
# initilize account
loadUser(userID="yj",type=type)
# initilize stragtety
addStrategyFaber(myStrat="Faber")


shinyServer(
  function(input, output) {
    
    # Create a reactiveValues object to show current status
    # The status info will be passed to loginfo output
    status <- reactiveValues()
    status$LastAction <- "Start"
    status$Pass <- TRUE
    status$Msg <- "Account initialized successfully!"

    observe({
      if (input$Save != 0) {
        status$LastAction <- 'Save'
        saveUser(userID="yj",type=type)
        status$Pass <- TRUE
        status$Msg <- "Save account successfully!"
      }
    })
    
    observe({
      if (input$Load != 0) {
        status$LastAction <- 'Load'
        loadUser(userID="yj",type=type)
        status$Pass <- TRUE
        status$Msg <- "Load account successfully!"
      }
    })

    observe({
      if (input$Reset != 0) {
        status$LastAction <- 'Reset'
        #loadUser(userID="yj",type=type)
        status$Pass <- TRUE
        status$Msg <- "Reset account successfully!"
      }
    })
    
    observe({
      if (input$Go != 0) {
        status$LastAction <- 'Go'
        
        actionStr = isolate({input$Action})
        itemStr = isolate({input$Item})
        parentName = isolate({input$selectParentName})
        newName = isolate({input$NewName})
        status0 <- takeAction(action=actionStr,item=itemStr,parent=parentName,name=newName)
        
        status$Pass <- status0$Pass
        status$Msg <- status0$Msg
      }
    })
                  
    output$selectParent <- renderUI({
      choiceList = switch(input$Item,
                          "Account"="yj",
                          "Portfolio"=acctList(),
                          "Stock"=potfList())
        
      selectInput("selectParentName", 
                  label = NULL, 
                  choices = choiceList, selected = 1)
      
    })
    
    output$inputNewName <- renderUI({
      newDefaultName = switch(input$Item,
                       "Account"="myA",
                       "Portfolio"="myP",
                       "Stock"="myS")
      textInput("NewName", 
                label = NULL, 
                value = newDefaultName)
      
    })

  


    output$logInfo <- renderUI({
      
      if (status$Pass == TRUE){
        logStr = paste("PASS",status$Msg,sep=": ")
        return(tags$div(HTML(paste(tags$span(style="color:green", logStr)))))
      } 
      else{
        logStr = paste("FAIL",status$Msg,sep=": ")
        return(tags$div(HTML(paste(tags$span(style="color:red", logStr)))))
      }  
    })
    
    output$accountFlowChart <- renderPlot({
      if ((input$Go==0)){drawFlowChart()}
      if ((input$Go!=0)){drawFlowChart()}
      if ((input$Save!=0)){drawFlowChart()}
      if ((input$Load!=0)){drawFlowChart()}
      if ((input$Reset!=0)){drawFlowChart()}            
    })
    
    output$StrategySummary <- renderPrint({
      summary(getStrategy("Faber"))
      })
    
    observe({
      if (input$RunBacktest != 0) {
        runBackTest(strat="Faber",portf=c("myPortfolio"),acct="myAccount")
      }
    })
    
    output$EquityCurve <- renderPlot({
      if (input$RunBacktest!=0){
        a <- getAccount("myAccount")
        equity <- a$summary$End.Eq
        plot(equity,
             main="Faber Strategy Equity Curve",
             xlim=as.POSIXct(c("2014-01-01 00:00:00","2014-12-31 03:00:00")))
      }
    })
    
    output$PosnChart <- renderPlot({
      if (input$RunBacktest!=0)
      plotPosnChart(portf="myPortfolio",symbol="SPY", date='2014::')
    })
    
    output$TxnSummary <- renderPrint({
      if (input$RunBacktest!=0)
        getTxns(Portfolio="myPortfolio",Symbol="SPY",Dates="2014")
    })
      
    
  }
)
