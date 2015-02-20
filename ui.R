# ui.R
# Defines the UI for account management

shinyUI(fluidPage(
  #titlePanel("Strategy Backtest"),
  navbarPage("STRATEGY B",
             #========================= SYMBOLS =========================#
             tabPanel("Symbols",
                      sidebarLayout(
                        
                        sidebarPanel(
                          uiOutput("selectStock"),
                          uiOutput("selectTA"),
                          dateRangeInput('dateRange',
                                         label = 'Date Range',
                                         start = "2014-09-01", end = "2014-12-31"
                          )
                        ),
                        
                        mainPanel(
                                  plotOutput("candleChart")
                        )
                      )
             ),#tabPanel("Symbols"
             
             
             #========================= BACKTEST =========================#
             tabPanel("Backtest",  
                      navlistPanel(
                        #================= TAB PANEL: ACCOUNT =================#
                        tabPanel(
                          "Account",
                          fluidRow(
                            column(5,
                                   selectInput(
                                     'Action', 'Choose Actions:', choices = list("Add", "Delete", "Rename"),
                                     selected = "Add"
                                   )
                            ),
                            
                            column(5,
                                   selectInput(
                                     'Item', 'Choose Items:', choices = list("Account", "Portfolio", "Stock"),
                                     selected = "Stock"
                                   )
                            )
                          ),
                          hr(),
                          fluidRow(
                            column(5,helpText("Select Parent:")),
                            column(5,helpText("Input Name:"))
                          ),
                          fluidRow(
                            column(5,
                                   uiOutput("selectParent")),
                            column(5,
                                   uiOutput("inputNewName")),
                            column(2,
                                   actionButton("Go", label = "Submit"))
                            
                          ),       
                          hr(),
                          fluidRow(
                            column(12,
                                   plotOutput("accountFlowChart"))
                          ),#fluidRow
                          hr(),
                          fluidRow(
                            column(12,
                                   #verbatimTextOutput("logInfo")
                                   htmlOutput("logInfo"))
                          ),#fluidRow
                          hr(),
                          fluidRow(
                            column(6,offset=0,
                                   actionButton("Save", label = "Save"),
                                   actionButton("Load", label = "Load"),
                                   actionButton("Reset", label = "Reset"))
                          )
                        ),
                        #================= TAB PANEL: STRATEGY =================#
                        tabPanel(
                          "Strategy",
                          hr(),
                          fluidRow(
                            column(3,helpText("Stragtegy:")),
                            column(3,textInput("NameStrategy", label = NULL, value = "SMA")),
                            column(3,actionButton("AddStrategy", label = "Add")),
                            column(3,actionButton("RemoveStrategy", label = "Remove"))
                          ),
                          hr(),
                          fluidRow(
                            column(3,helpText("Indicator:")),
                            column(3,textInput("NameIndicator", label = NULL, value = "SMA10")),
                            column(3,actionButton("AddIndicator", label = "Add")),
                            column(3,actionButton("RemoveIndicator", label = "Remove"))
                          ),
                          hr(),
                          fluidRow(
                            column(3,helpText("Signal:")),
                            column(3,textInput("NameSignal", label = NULL, value = "sigCrossover")),
                            column(3,actionButton("AddSignal", label = "Add")),
                            column(3,actionButton("RemoveSignal", label = "Remove"))
                          ),
                          hr(),
                          fluidRow(
                            column(3,helpText("Rule:")),
                            column(3,textInput("NameRule", label = NULL, value = "Cl.gt.SMA")),
                            column(3,actionButton("AddRule", label = "Add")),
                            column(3,actionButton("RemoveRule", label = "Remove"))
                          ),
                          hr(),
                          fluidRow(
                            column(12,verbatimTextOutput("StrategySummary"))
                          )
                        ),
                        #================= TAB PANEL: BACKTEST =================#
                        tabPanel(
                          "Backtest",
                          fluidRow(
                            column(12,actionButton("RunBacktest", label = "Run Backtest"))
                          ),
                          hr(),
                          fluidRow(
                            column(12,plotOutput("EquityCurve"))
                          ),
                          hr(),
                          fluidRow(
                            column(12,plotOutput("PosnChart"))
                          ),
                          hr(),
                          fluidRow(
                            column(12,verbatimTextOutput("TxnSummary"))
                          )
                          )
                        )
  
             ),

            
            #========================= SHARE =========================#
            tabPanel("Share")
  )
  )#fluidPage
)#shinyUI