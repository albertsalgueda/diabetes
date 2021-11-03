#App File

# Import libraries
library(shiny)
library(data.table)
library(randomForest)

# Read in the RF model
model <- readRDS("model.rds")


####################################
# User interface                   #
####################################

ui <- pageWithSidebar(
  
  # Page header
  headerPanel('Diabetes Predictor'),
  
  # Input values
  sidebarPanel(
    #HTML("<h3>Input parameters</h3>"),
    tags$label(h3('Input parameters')),
    sliderInput("Age", label = "Age", value = 30,
                min = min(dfTrain$Age),
                max = max(dfTrain$Age)
    ),
    radioButtons("Gender", label = "Gender",
                 choices = list("Male" = "Male", "Female" = "Female")),
    radioButtons("Polyuria", label = "Polyuria",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("Polydipsia", label = "Polydipsia",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("SuddenWeightLoss", label = "Sudden Weight Loss",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("Weakness", label = "Weakness",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("Polyphagia", label = "Polyphagia",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("GenitalThrush", label = "Genital Thrush",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("VisualBlurring", label = "Visual Blurring",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("Itching", label = "Itching",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("Irritability", label = "Irritability",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("DelayedHealing", label = "Delayed Healing",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("PartialParesis", label = "PartialParesis",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("MuscleStiffness", label = "Muscle Stiffness",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("Alopecia", label = "Alopecia",
                 choices = list("Yes" = "Yes", "No" = "No")),
    radioButtons("Obesity", label = "Obesity",
                 choices = list("Yes" = "Yes", "No" = "No")),
    actionButton("submitbutton", "Submit", 
                 class = "btn btn-primary")
  ),
  
  mainPanel(
    tags$label(h3('Status/Output')), # Status/Output Text Box
    verbatimTextOutput('contents'),
    tableOutput('tabledata') # Prediction results table
    
  )
)

####################################
# Server                           #
####################################

server<- function(input, output, session) {
  
  # Input Data
  datasetInput <- reactive({  
    
    df <- data.frame(Age = c(input$Age),
                     Gender = c(input$Gender),
                     Polyuria=c(input$Polyuria),
                     Polydipsia=c(input$Polydipsia),
                     SuddenWeightLoss=c(input$SuddenWeightLoss),
                     Weakness=c(input$Weakness),
                     Polyphagia=c(input$Polyphagia),
                     GenitalThrush=c(input$GenitalThrush),
                     VisualBlurring=c(input$VisualBlurring),
                     Itching=c(input$Itching),
                     Irritability=c(input$Irritability),
                     DelayedHealing=c(input$DelayedHealing),
                     PartialParesis=c(input$PartialParesis),
                     MuscleStiffness=c(input$MuscleStiffness),
                     Alopecia=c(input$Alopecia),
                     Obesity=c(input$Obesity))
    
    #Diagnosis <- NA
    #df <- rbind(df, Diagnosis)
    print(df)
    input <- df
    print(input)
    write.table(input,"input.csv", sep=",", quote = TRUE, row.names = TRUE, col.names = TRUE)
    
    test <- read.csv(paste("input", ".csv", sep=""), header = TRUE)
    
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$submitbutton>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$submitbutton>0) { 
      isolate(datasetInput()) 
    } 
  })
  
}

####################################
# Create the shiny app             #
####################################
shinyApp(ui = ui, server = server)