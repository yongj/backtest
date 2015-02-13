# ui.R
# Defines the UI for account management

shinyUI(fluidPage(
  titlePanel("Account Management"),
  
  sidebarLayout(
    sidebarPanel(
                 selectInput(
                   'Action', 'Choose Actions:', choices = list("Add", "Delete", "Rename"),
                   selected = "Add"
                 ),
                 selectInput(
                   'Item', 'Choose Items:', choices = list("Account", "Portfolio", "Stock"),
                   selected = "Stock"
                 )
                 
                 
                 ),
    mainPanel(
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
                
              ),#fluidRow
#               br(),
#               fluidRow(
#                 column(12,
#                 verbatimTextOutput("summary"))
#               ),#fluidRow
              br(),
              fluidRow(
                column(12,
                       plotOutput("accountFlowChart"))
              ),#fluidRow
              br(),
              fluidRow(
                column(12,
                       #verbatimTextOutput("logInfo")
                       htmlOutput("logInfo"))
              ),#fluidRow
              fluidRow(
                column(6,offset=0,
                       actionButton("Save", label = "Save"),
                       actionButton("Load", label = "Load"),
                       actionButton("Reset", label = "Reset"))
              )
              
              )
    )
  )#fluidPage
)#shinyUI