library(shiny)
library(shinydashboard)
library(shinythemes)

model <- readRDS("credit_scoring_rf_model.rds")

# Define UI for application that draws a histogram
ui <- dashboardPage (

    dashboardHeader(title = "Credit Scoring"),
    
    dashboardSidebar(),
    
    dashboardBody(
        tabName = "features",
        fluidRow( box(valueBoxOutput("score_prediction")),
                  box(numericInput("var1", label = "Age du demandeur de credit",
                                   value = 20, min = 18))),
        
        fluidRow(box(numericInput("var2", label = "Revenu annuel du demandeur de credit",
                                  value = 10000, min = 0)),
                 box(selectInput("var3", label = "proprieté immobilière",
                                 choices = c('MORTGAGE', 'OWN', 'RENT', 'OTHER')))),
        
       fluidRow( box(numericInput("var4", label = "Depuis quand le demnadeur de crédit est il en activité professionnelle ?",
                                  value = 1, min = 0)),
                 box(selectInput("var5", label = "Motif du pret bancaire",
                                 choices = c("PERSONAL", "EDUCATION", "MEDICAL", "VENTURE", "HOMEIMPROVEMENT", "DEBTCONSOLIDATION")))),
       
        fluidRow(box(selectInput("var6", label = "Categorie du crédit",
                                 choices = c("A", "B", "C", "D", "E", "F"))),
                 box(numericInput("var7", label = "Montant du crédit",
                                  value = 20000, min = 0))),
       
       fluidRow(box(numericInput("var8", label = "Taux d'intéret du crédit (%)",
                                 value = 5, min = 0)),
                box(numericInput("var9", label = "Ratio Dette/Revenu du demandeur de crédit (valeur entre 0 et 1)",
                                 value = 0.4, min = 0, max = 1))),
       
      fluidRow( box(selectInput("var10", label = "Le demandeur de crédit est il a découvert bancaire ?",
                                choices = c("Y", "N"))),
                box(numericInput("var11", label = "Echéance des crédits en cours (en nombre d'années)",
                                 value = 3, min = 0)))  
      
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  prediction <- reactive({
    predict(
      model,
      data.frame(
        "person_age" = input$var1,
        "person_income" = input$var2,
        "person_home_ownership" = input$var3,
        "person_emp_length" = input$var4,
        "loan_intent" = input$var5,
        "loan_grade" = input$var6,
        "loan_amnt" = input$var7,
        "loan_int_rate" = input$var8,
        "loan_percent_income" = input$var9,
        "cb_person_default_on_file" = input$var10,
        "cb_person_cred_hist_length" = input$var11
      ),
      type = "raw"
    )
  })
  
  prediction_label <- reactive({
    ifelse(prediction()=="0", "Eligible au credit", "Non éligible au crédit")
  })
  
  prediction_prob <- reactive({
    predict(
      model,
      data.frame(
        "person_age" = input$var1,
        "person_income" = input$var2,
        "person_home_ownership" = input$var3,
        "person_emp_length" = input$var4,
        "loan_intent" = input$var5,
        "loan_grade" = input$var6,
        "loan_amnt" = input$var7,
        "loan_int_rate" = input$var8,
        "loan_percent_income" = input$var9,
        "cb_person_default_on_file" = input$var10,
        "cb_person_cred_hist_length" = input$var11
      ),
      type = "prob"
    )
  })
  
  prediction_color <- reactive({
      ifelse(prediction() == "0", "green", "red")
  })
  
  output$score_prediction <- renderValueBox(
    valueBox(
        value = paste(round(100 * prediction_prob()$`1`,0), "%"),
        subtitle = prediction_label(),
        color = prediction_color(),
        icon = icon('hand-holding-usd')
    )
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)
#library(rsconnect)
#rsconnect::deployApp('C:/Users/pc/OneDrive/Documents/Environement R/Risque de credit/deploy_ml_model_in_shinydashboard/app/app')
