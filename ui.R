
n1 <- "CalREDIE DDP Data Dashboard 0.1.0"

n2 <- "Description: The CalREDIE DDP Data Dashboard is a dashboard to automate reporting of data selected by the user that has been obtained by CalREDIE's Data Distribution Portal (DDP)."

#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
fC <- function(vec) {
  tRep <- length(vec)-1
  paste("input.ID == ",vec,    c(rep("|",tRep),""), collapse="")
}
# library(shiny)
# ui <- shinyUI(navbarPage("CalREDIE DDP Dashboard", tabPanel("DDP Data Dashboard"), tabPanel("DDP Data Forecast Model"))
shinyUI(fluidPage(theme = shinytheme("slate"),
# Define UI for application that draws a histogram
# Application title
titlePanel("DDP Data Dashboard"),
# sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
#                   label = "Search..."),
# Sidebar with a slider input for number of bins 
sidebarLayout(
  # tabsetPanel(
  sidebarPanel(
    tabsetPanel(type = "tabs",
    tabPanel("Data",
    # Choose .tsv input file
    tsvFileInput("datafile", "User data (.tsv format)"),
    uiOutput("disNme"), #Dynamic input for diseases groups
    # output$out4 <- renderPrint(input$in4)
    uiOutput("dateRange"), #Dynamic input for date range
    uiOutput("rptGroup"),
    # conditionalPanel(condition = fC(c(1,5)), uiOutput("rptGroup")),
    uiOutput("binRange"),
    # uiOutput("mapLayer"),
    
     # conditionalPanel(condition = fC(c(4))
    #                  , checkboxGroupInput("mapGroup", label = h5("Mapping Options:")
    #                                                 , inline = TRUE
    #                                                 , choiceNames = list("Points", "Density", "Cnty_Lines", "Heat")
    #                                                 , choiceValues = list(1, 2, 3, 4)
    #                                                 , selected = c(1, 2, 3))
    #                  ),
    #                  
    # conditionalPanel(condition = fC(c(1))
    #                  , checkboxGroupInput("mapGroup", label = h5("Mapping Options:")
    #                    , inline = TRUE
    #                    , choiceNames = list("Points", "Density", "Cnty_Lines", "Heat")
    #                    , choiceValues = list(1, 2, 3, 4)
    #                    , selected = c(1, 2, 3))),
    # conditionalPanel(condition = fC(c(1, 2))
    #                  ,    # Select the bins for hisograms of cases
    #                  sliderInput("bin",
    #                              "Number of Bins in Histogram:",
    #                              min = 1,
    #                              max = 100,
    #                              value = 50),
    #                  
    #                  sliderInput("bw_adjust", label = "Bandwidth adjustment:",
    #                              min = 0.2, max = 2, value = 1, step = 0.2)),
    #                  
    #                  # tabsetPanel(id = "tabs",
    #                  #   tabPanel("Map", )
    #                  # )
    #                  # )
    # 
    # #download Button for figure - name of download & title of button
    # # conditionalPanel(condition = "output.mapPlot == true",
    # # downloadButton("downloadMap", "Download")
    # # )
    # # , hr()
    # # , fluidRow(column(3, verbatimTextOutput("value")))
    # #),
    conditionalPanel(condition = fC(c(1)), textInput("histTitle", label = h5("Histogram Title:")
                                                     , value = "Enter Histogram Title:")),

    conditionalPanel(condition = fC(c(5)), textInput("cntyRnkTitle", label = h5("County Rank Title:")
                                                     , value = "Enter Title for County Cases:")),
    conditionalPanel(condition = fC(c(2,3)), checkboxGroupInput("mapBorder"
                                                                 , label = h5("Border Selections")
                                                                 , inline = TRUE
                                                                 , choiceNames = list("County"
                                                                                      , "City")
                                                                 , choiceValues = list("County"
                                                                                    , "City")
                                                                 , selected = c("County")
                                                                 )),
    conditionalPanel(condition = fC(c(2,3)), selectInput("mapLayer"
                                                        , label = h5("Measure Selection")
                                                        , choices = c("Incident Rate" = "inc_rt"
                                                                             , "Heat Map" = "ht_map"
                                                                             , "Point Map" = "pt_map")
                                                        , selected = c("Incident Rate")
                                                        )),
    conditionalPanel(condition = fC(c(2)), downloadButton("downloadMapSt", "Download MapSt")),
    # conditionalPanel(condition = fC(c(6)), downloadButton("downloadSummaryTbl", "Download Summary")),
    conditionalPanel(condition = fC(c(4)), downloadButton("downloadValues", "Download Values")),
    # conditionalPanel(condition = fC(c(4)), downloadButton("downloadValues", "Download Values")).
    conditionalPanel(condition = fC(c(1,5)), downloadButton("downloadPlot", "Download Histogram"))
    # conditionalPanel(condition = fC(c(4)), downloadButton("selectData", "Download Data"))
    # conditionalPanel(condition = fC(c(7)), downloadButton("selectPop", "Download Demographics"))

    # , conditionalPanel(condition = fC(c(2,3)), "input.tabs=='1'"
    #                  , downloadButton("downloadMap"
    #                                   , "Download Map")
    # ),
    # 
    # conditionalPanel(condition = "input.tabs=='2'"
    #                  , downloadButton("downloadValues"
    #                                   , "Download Values")
    # )
    # conditionalPanel(condition = fc(c(4)),),
    # conditionalPanel(condition = fc(c(1)), uiOutput(""))
    # conditionalPanel(condition = fC(c(1, 2, 3, 4)), textInput("histTitle", label = h5("Histogram Title:")
    #                                                  , value = "Enter Histogram Title:")),

    # conditionalPanel(condition = fC(c(1, 2)), textInput("mapTitle", label = h5("Map:")
    #                                                  , value = "Enter Map Title:")),
    # 

  #   # Select Region for map of interest
  #   selectizeInput("region", label = h5("Select Region:")
  #                  , choices = list("State" = 1, "County" = 2, "City" = 3)
  #                  , selected = 1),
  #
  #   # Choose outcomes of interest for viewing
  #   selectInput("outcome", label = h5("Case Outcomes:")
  #               , choices = list("Illness" = 1, "Death" = 2)
  #               , selected = NULL)
#   # )
# )))
# fluidRow(column(width = 5
#                 , verbatimTextOutput("click_info")
#                 )
#          )
),
tabPanel("Demographics",
         # conditionalPanel(condition = fC(c(1,2,3,4,5)), selectInput("ethnicity"
         #                                                            , label = h5("Ethnicity")
         #                                                            , choices = c(""
         #                                                                          , "")
         #                                                            , selected = c("ALL"))),
         conditionalPanel(condition = fC(c(1,2,3,4,5,6,7)), uiOutput("Sex")),
         conditionalPanel(condition = fC(c(1,2,3,4,5,6,7)), uiOutput("ethnicity")),
         conditionalPanel(condition = fC(c(1,2,3,4,5,6,7)), uiOutput("race")),
         conditionalPanel(condition = fC(c(1,2,3,4,5,6,7)), uiOutput("ageGrp")),
         conditionalPanel(condition = fC(c(1,2,3,4,5,6,7)), uiOutput("pregnant")),
         conditionalPanel(condition = fC(c(1,2,3,4,5,6,7)), uiOutput("marital"))
         # conditionalPanel(condition = fC(c(1,2,3,4,5)), selectInput("gender"
         #                                                     , label = h5("Gender")
         #                                                     , choices = c("Total"
         #                                                                   , "Male"
         #                                                                   , "Female")
         #                                                     , selected = c("Total"))
         ),

tabPanel("Table Options",
         conditionalPanel(condition = fC(c(6))
                          # && ("input.crossTbl == 'TRUE'")
                          , selectInput(inputId = "bivariate"
                                        , label = h5("Select Table Bivariate")
                                        # , inline = TRUE
                                        , choices = c("Report Group" = "RStatus"
                                                      , "Sex" = "Sex"
                                                      , "Ethnicity" = "Ethnicity"
                                                      , "Race" = "Race"
                                                      , "Age Group" = "ageGrp"
                                                      , "Marital Status" = "Marital"
                                                      , "Pregnancy Status" = "Pregnant")
                                        , selected = c("Report Group"
                                                       , "Sex"
                                                       , "Ethnicity"
                                                       , "Race"
                                                       , "Age Group"
                                                       , "Marital Status"
                                                       , "Pregnancy Status")
         )),
         conditionalPanel(condition = fC(c(6)), checkboxInput(inputId = "crossTbl"
                                                              , label = h5("Bivariate Summary Table")
                                                              , value = FALSE))
         )
),

helpText(n1)
, br()
, helpText(n2, style="color:red")
, br()
, HTML('<center><img src="CalREDIE_logo.jpg" height="65" width="275"></center>')
, helpText("LINKS",tags$a(href="https://www.cdph.ca.gov/Programs/DDP/Pages/Data-and-Statistics-.aspx",
                        h6("CalREDIE Reporting Application")),
         tags$a(href="https://cdph.ca.gov/",
                h6("Webpage of CDPH CalREDIE")),
         tags$a(href="https://www.census.gov/programs-surveys/acs/",
                h6("American Community Survey")),  style="color:blue")
# sidebarLayout(sidebarPanel(position = "right",
# fluidRow(column(10, wellPanel(
# Show a plot of the generated distribution
, id = "ID"),
mainPanel(
  hr(),
  # tabPanel("Data file", tableOutput("guess")
  #          # , value = 2
  #          ),
  # tabPanel("table of tmp_df")
  
  tabsetPanel(type = "tabs",
              tabPanel("Cases over Time", plotOutput("distPlot", width = "100%", click = "plot1_click")
                         , value = 1)
              , tabPanel("Map (Static)",  plotOutput("CRstMapPlot")
                         # , click = "plot1_click"
                         , value = 2)
              , tabPanel("Map (Zoom)",    leafletOutput("CRzMapPlot", width = 700, height = 700)
                                                   # , click = "plot1_click"
                         , value = 3)
              , tabPanel("Values",        dataTableOutput("values")
                         , value = 4) #DT:: #For output of input values chosen
              # , tabPanel("Population", dataTableOutput()) #DT:: #For output of population demographics
              , tabPanel("County Cases",  plotOutput("cntyPlot"), width = "100%"
                         , value = 5)
              , tabPanel("Population",    dataTableOutput("selectPop")
                         , value = 6)
              , tabPanel("Data",          dataTableOutput("selectData")
                                   , value = 7)
              , id = "ID" )

  )
)
# , dashboardBody(
#   tags$head(
#     tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
)
)
# sidebarSearchForm(textId = "searchText", buttonId = "searchButton",
#                   label = "Search...")