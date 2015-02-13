# ui.R
# Defines the UI for account management

shinyUI(fluidPage(
  titlePanel("Strategy Backtest"),

    
  navlistPanel(
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
    tabPanel(
      "Strategy"
    ),
    tabPanel(
      "Backtest"
    )
  )
    
  )#fluidPage
)#shinyUI