# CMSC 150 Final Project
# Pamintuan, John Alwin R.

library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(rhandsontable)
source("simplex.R", local = TRUE)
source("polynomialregression.R", local = TRUE)
source("qsi.R", local = TRUE)

uploadCSVPR <- function(name){
    fileInput("file1", "Upload .csv file", width = "50%", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
}

uploadCSVQSI <- function(name){
    fileInput("file2", "Upload .csv file", width = "50%", multiple = FALSE, accept = c("text/csv", "text/comma-separated-values,text/plain", ".csv"))
}

readCSVPR <- function(input){
    inFile <- input$file1
    if(is.null(inFile)) return(NA)
    
    df <- read.csv(inFile$datapath, header=FALSE)
    return(df)
}

readCSVQSI <- function(input){
    inFile <- input$file2
    if(is.null(inFile)) return(NA)
    
    df <- read.csv(inFile$datapath, header=FALSE)
    return(df)
}

#APPLICATION UI
ui <- dashboardPagePlus(
    header = dashboardHeaderPlus(
        title = "CMSC 150 Project"
    ),
    
    #SIDEBAR----------
    sidebar = dashboardSidebar(
        sidebarMenu(id = "menu",
            menuItem("Polynomial Regression", tabName = "pr"),
            menuItem("Quadratic Spline Interpolation", tabName = "qsi"),
            menuItem("Simplex Implementation", tabName = "simplex"))),
    
    #BODY----------
    body = dashboardBody(
        tabItems(
            tabItem(tabName = "pr", boxPlus(
                title = "Polynomial Regression", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                enable_sidebar = TRUE, sidebar_width = 40, sidebar_background = "black", sidebar_start_open = TRUE,
                sidebar_content = tagList(sliderInput(inputId = "deg", label = "Degree", min = 1, max = 50, value = 1, animate = TRUE, width = '75%'), numericInput(inputId = "est", label = "Estimate", value = 1)),
                width = "auto",height = "auto", enable_label = TRUE, label_text = "Click i to show/hide adjustments", label_status = "info",
                box(br(),h4("Function"), uiOutput("PRfxn"),br(), h4("Estimate"), uiOutput("PRfxnEst"), br(), br(),width = "auto")),
                box(uploadCSVPR(),width = "auto")
            ),
            tabItem(tabName = "qsi", boxPlus(
                title = "Quadratic Spline Interpolation", solidHeader = TRUE, collapsible = TRUE, closable = FALSE,
                enable_sidebar = TRUE, sidebar_width = 40, sidebar_background = "black", sidebar_start_open = TRUE,
                sidebar_content = tagList(sliderInput(inputId = "interval_page", label = "Interval", min = 1, max = 10, value = 1, animate = TRUE, width = '75%' ),numericInput(inputId = "value", label = "Value", min = 1, max = 50, value = 1)),
                width = "auto", height = "auto", enable_label = TRUE, label_text = "Click i to show/hide adjustments", label_status = "info",
                box(br(), h4("Function"), uiOutput("QSIfxn"), br(), h4("Estimate"), uiOutput("QSIfxnEst"), br(), br(), width = "auto")),
                box(uploadCSVQSI(), width = "auto")
            ),
            tabItem(tabName = "simplex", box(title = "Fairway Woods Company Shipping Analysis", rHandsontableOutput("ProjectDT"),width = "auto", collapsible = TRUE), box(title = "Simplex Tableau",rHandsontableOutput("Simplex"), width = "auto", collapsible = TRUE), box(title = "Basic Solution", verbatimTextOutput("BasicSolution"), width = "auto", collapsible = TRUE), fluidRow(box(h3(textOutput("SimplexSolution")),h5("Minimum Cost"), background = "green", width="auto")), fixedPanel(sliderInput("iteration",label = "Iteration", min = 0, max = 5, value = 0), actionButton(inputId = "compute", label = "Compute"), right = 10, bottom = 10, width = "auto"))
        )
    ),
    
    #Dashboard Page options
    footer = dashboardFooter(right_text = "Pamintuan, John Alwin R."),
    sidebar_fullCollapse = TRUE,
    skin = "blue"
)


server <- function(input, output, session) {
    
    #Polynomial Regression Outputs
    output$PRfxn <- renderUI({
        df <- readCSVPR(input)
        if(is.na(df)) helpText("Upload CSV File first!")
        else{
            ans <- PolynomialRegression(df, input$deg)$strfxn
            updateSliderInput(session, inputId = "deg", label = "Degree", max = nrow(df)-1)
            withMathJax(helpText(substring(ans,12,nchar(ans))))
        }
    })
    
    output$PRfxnEst <- renderUI({
        df <- readCSVPR(input)
        if(is.na(df)) helpText("NA")
        else{
            ans <- PolynomialRegression(df, input$deg)
            withMathJax(helpText(ans$fxn(input$est)))
        }
    })
    
    #Quadratic Spline Interpolation Outputs
    output$QSIfxn <- renderUI({
        df <- readCSVQSI(input)
        if(is.na(df)) helpText("Upload CSV File first!")
        else{
            ans <- QuadraticSplineInterpolation(df, input$value)$FunctionList
            updateSliderInput(session, inputId = "interval_page", label = "Interval", max = nrow(df)-1)
            withMathJax(helpText(substring(ans[input$interval_page],12,nchar(ans[input$interval_page]))))
        }
    })
    
    output$QSIfxnEst <- renderUI({
        df <- readCSVQSI(input)
        if(is.na(df)) helpText("NA")
        else{
            ans <- QuadraticSplineInterpolation(df, input$value)
            print(df[nrow(df),1])
            updateNumericInput(session, inputId = "value", label = "Value", min = df[1,1], max = df[nrow(df),1])
            if(input$value > df[nrow(df),1] || input$value < df[1,1]) helpText(paste("Minimum estimate:",df[1,1],"and Maximum estimate:",df[nrow(df),1]))
            else{
                box(helpText(paste("At interval",ans$index,",",df[ans$index,1],"<=",df[(ans$index + 1),1])),
                withMathJax(helpText(ans$fxn(input$value))))
            }
        }
    })
    
    #Simplex Implementation Outputs
    output$ProjectDT <- renderRHandsontable(rhandsontable(df, width = "auto", stretchH = "all"))
    observeEvent(input$compute, {
        df = hot_to_r(input$ProjectDT)
        mincost = getMinimumCost(df)
        updateSliderInput(session,"iteration", label = "Iteration", value = 0, min = 0, max = (length(mincost$Dataframe)-1), step = 1)
        output$Simplex <- renderRHandsontable(rhandsontable(as.data.frame(mincost$Dataframe[(input$iteration + 1)]), width = "auto", readOnly = TRUE))
        output$SimplexSolution <- renderText(paste(mincost$MinCost))
        output$BasicSolution <- renderPrint(mincost$BasicSolution[(input$iteration+1)])
    })
}

shinyApp(ui = ui, server = server)