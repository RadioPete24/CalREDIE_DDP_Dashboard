#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#callModule(myModule, "myModule1", reactive(input$checkbox1))

# Define server logic required to draw a histogram
# server <- 
shinyServer(function(input, output, session){
  #function to call instance of data
  data_now <- callModule(tsvFile, "datafile", stringsAsFactors = FALSE)
  # #Change max upload size of data file
  options(shiny.maxRequestSize=10*1024^2)

  # disNme <- callModule(disNme, "diseaseName", stringAsFactors = FALSE)
  output$disNme <- renderUI({
    # observe({
    tmp_df <- as.data.frame(data_now())
    getDisNme <- unique(unlist(tmp_df$DisShort))
    selectInput(inputId = "disease", label = h5("Disease Selection:")
                , choices = getDisNme
                , selected = getDisNme
                )
    # checkboxGroupInput(inputId = "disease", label = h5("Disease Selection:")
    #                    , inline = TRUE
    #                    , choiceNames = getDisNme
    #                    # , choiceValues = c(1:length(getDisNme))
    #                    # , selected = c(1:length(getDisNme))
    #                    , choiceValues = getDisNme
    #                    , selected = getDisNme
    # )
  })
  
  output$dateRange <- renderUI({
    tmp_df <- as.data.frame(data_now())
    # Function is meant to read only the initial parameters and stay the same after selections so built without continuous rendering
    dateMin <- range(as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y"))[1]
    dateMax <- range(as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y"))[2]
    sliderInput("dateRange", label = h5("Select Date Range:")
                , min = dateMin
                , max = dateMax
                , value = c(dateMin, dateMax)
                , step = 12
                , animate = TRUE
                )
  })
  
# For generatinng data frame by disease week
  # output$weekRange <- renderUI({
  #   tmp_df <- as.data.frame (data_now())
  #   # sprintf("%02d", as.numeric(strftime(seq.POSIXt(currentDate
  #   #                                                , by = "-1 week"
  #   #                                                , length.out = 4)[4]
  #   #                                     , format = "%W")))
  #   
  # })

  output$binRange <- renderUI({
    tmp_df <- as.data.frame(data_now())
    dateMin <- range(as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y"))[1]
    dateMax <- range(as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y"))[2]
    sliderInput("bin",
                "Number of Bins in Histogram:"
                , min = 0
                , max = as.numeric(dateMax-dateMin)
                , value = as.numeric(dateMax-dateMin)/2)
  })
  
  output$rptGroup <- renderUI({
    tmp_df <- as.data.frame(data_now())
    # tmp_df <- data_now()
    getRStatNme <- unique(unlist(tmp_df$RStatus))
    checkboxGroupInput("rstat", label = "Reporting Group Status:", inline = TRUE
                     , choiceNames = getRStatNme
                     , choiceValues = getRStatNme
                     , selected = getRStatNme)
  })
  
#   sliderValues <- reactive({
#     rpt_group <- toString(input$rptGroup)
#     data.frame(
#       Name = c("Disease", "Report Group"
#                # , "bin"
#                , "dtRange", "sex", "ethnicity", "race"),
#       # Value = I(list(toString(c(input$disNme)
#       #                        , toString(c(input$rptGroup))
#       #                        # , input$binRange
#       #                        , paste(input$dateRange, collapse = " to ")
#       #                        , toString(c(input$Sex))
#       #                        , toString(c(input$ethnicity))
#       #                        , toString(c(input$race))
#       #                      , stringAsFactors = FALSE)
#       Value = c(toString(input$disNme)
#                      , rpt_group
#                      # , input$binRange
#                      , paste(input$dateRange, collapse = " to ")
#                      , toString(input$Sex)
#                      , toString(input$ethnicity)
#                      , toString(input$race))
#                      , stringAsFactors = FALSE)
#     
# # test <- (3.49*sd(tmp_df$DtEpisode))/(as.numeric(dateMax-dateMin)^(1/3))
#     })
  
  output$Sex <- renderUI({
    tmp_df <- as.data.frame(data_now())
    getSex <- unique(unlist(tmp_df$Sex))
    selectInput(inputId = "Sex"
                   , label = h5("Select Sex:")
                   , choices = getSex
                   , selected = getSex
                   , multiple = TRUE
                )
  })
  
  output$ethnicity <- renderUI({
    tmp_df <- as.data.frame(data_now())
    getEthnic <- unique(unlist(tmp_df$Ethnicity))
    checkboxGroupInput(inputId = "Ethnicity"
                       , label = h5("Select Ethnicity:"), inline = TRUE
                       , choiceNames = getEthnic
                       , choiceValues = getEthnic
                       , selected = getEthnic
    )
  })

  output$race <- renderUI({
    tmp_df <- as.data.frame(data_now())
    getRace <- unique(unlist(tmp_df$Race))
    selectInput(inputId = "Race", label = h5("Select Race:")
                   # , inline = TRUE
                , choices = getRace
                , selected = getRace
                , multiple = TRUE
                , selectize = TRUE, width = '95%'
                
    )
  })
  
  output$ageGrp <- renderUI({
    tmp_df <- as.data.frame(data_now())
    getAgeGrp <- unique(unlist(tmp_df$ageGrp))[sort.list(unique(unlist(tmp_df$ageGrp)))]
    # getAgeGrp <- str_sort(unique(unlist(tmp_df$ageGrp)), numeric = TRUE) #requires stringr package
    checkboxGroupInput(inputId = "ageGrp", label = h5("Age Group (years)")
                       , choiceNames = getAgeGrp
                       , choiceValues = getAgeGrp
                       , inline = TRUE
                       , selected = getAgeGrp)
  })
  
  output$marital <- renderUI({
    tmp_df <- as.data.frame(data_now())
    getMarital <- unique(unlist(tmp_df$Marital))
    selectInput(inputId = "marital"
                , label = h5("Select Marital Status:")
                , choices = getMarital
                , selected = getMarital
                , multiple = TRUE
    )
  })
  
  output$pregnant <- renderUI({
    tmp_df <- as.data.frame(data_now())
    getPreg <- unique(unlist(tmp_df$Pregnant))
    selectInput(inputId = "pregnant"
                , label = h5("Select Pregnancy Status:")
                , choices = getPreg
                , selected = getPreg
                , multiple = TRUE
    )
  })
  # RV <- reactiveValues(b_ui_flg=FALSE)
  # 
  # output$A_panel <- renderUI({
  #   if(input$sidebarmenu != "a") return()
  #   cat('Inside A Panel \n')
  #   sliderInput("b", "Under A", 1, 100, 50)
  # })
  # 
  # observeEvent(input$sidebarmenu, {
  #   if(input$sidebarmenu != "b") return()
  #   if (RV$b_ui_flg) return() 
  #   RV$b_ui_flg <- TRUE
  #   cat('Inside B Panel \n')
  #   output$B_panel <- renderUI({
  #     sliderInput("b", "Under B", 1, 100, 50)
  #   })
  # })
  
#Data Tables
  
  # output$dataTable <- renderUI({
  #   nm_df <- data_now()
  #   nm_df
  # })
  #To create a selectize option for disNme...
  # fluidRow(column(4,
  #        hr(),
  #        verbatimTextOutput('out4'),
  #        selectInput('in4', 'Options', c(Choose='', state.name), selectize=TRUE)
  # ))
  
  # output$values <- renderUI({paste("Hey is this?", input$rptGroup)
  #   })
  #Function that creates table for selections
  catSelect <- reactive(
    data.frame(
      Name = c("Disease", "Report Group", "dtRange", "Sex", "Ethnicity", "Race", "Age Group", "Marital", "Pregnant"),
      Value = c(toString(input$disease)
                , toString(input$rstat)
                # , input$binRange
                , paste(input$dateRange, collapse = " to ")
                , toString(input$Sex)
                , toString(input$Ethnicity)
                , toString(input$Race)
                , toString(input$ageGrp)
                , toString(input$marital)
                , toString(input$pregnant))
    )
  )

  output$values <- renderDataTable({
    catSelect()
  })
  
  # output$values <- renderDataTable({
  #   data.frame(
  #     Name = c("Disease", "Report Group", "dtRange", "Sex", "Ethnicity", "Race", "Age Group", "Marital", "Pregnant"),
  #     # Value = I(list(toString(c(input$disNme)
  #     #                        , toString(c(input$rptGroup))
  #     #                        # , input$binRange
  #     #                        , paste(input$dateRange, collapse = " to ")
  #     #                        , toString(c(input$Sex))
  #     #                        , toString(c(input$ethnicity))
  #     #                        , toString(c(input$race))
  #     #                      , stringAsFactors = FALSE)
  #     Value = c(toString(input$disease)
  #               , toString(input$rstat)
  #               # , input$binRange
  #               , paste(input$dateRange, collapse = " to ")
  #               , toString(input$Sex)
  #               , toString(input$Ethnicity)
  #               , toString(input$Race)
  #               , toString(input$ageGrp)
  #               , toString(input$marital)
  #               , toString(input$pregnant))
  #     )
  # })

#Data Visualizations
  output$distPlot <- renderPlot({
    tmp_df <- data_now()
    tmp_df$DtEpisode <- as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y")
    tmp_df <- tmp_df[(tmp_df$DtEpisode>=input$dateRange[1] & tmp_df$DtEpisode<=input$dateRange[2])
                     & tmp_df$DisShort %in% c(input$disease)
                     & tmp_df$RStatus %in% c(input$rstat)
                     & tmp_df$Sex %in% c(input$Sex)
                     & tmp_df$Race %in% c(input$Race)
                     & tmp_df$Ethnicity %in% c(input$Ethnicity)
                     & tmp_df$ageGrp %in% c(input$ageGrp)
                     & tmp_df$Marital %in% c(input$marital)
                     & tmp_df$Pregnant %in% c(input$pregnant)
                     ,]
    # tmp_df <- tmp_df %>% filter(tmp_df$DisShort %in% input$disNme) %>% filter(tmp_df$RStatus %in% rptGroup)
   p <- ggplot(data = tmp_df, aes_string(x=tmp_df$DtEpisode
                       , fill = as.factor(tmp_df$RStatus)
                       )
    ) +
      # geom_freqpoly(alpha = 0.2) +
      # geom_histogram(bins = input$binRange) +
      geom_histogram(bins = 30) +
      scale_fill_brewer(palette="Set3") +
      # stat_bin(bin = input$bin) +
      # geom_bar()+
      labs(title = input$histTitle
           , x = "Date"
           , y = "Count") +
      scale_x_date(breaks = date_breaks(width = "1 month")) +
      theme(plot.title = element_text(hjust = 0.5)
            , axis.text.x = element_text(angle = 45, hjust = 1))
   p
    # labels <- nearPoints((tmp_df))
    # hist(input$date_range #PtDiedIllness, PStatus, RStatus
    #      , breaks = input$bins
    #      , col = 'darkgray'
    #      , border = 'white', main = paste("Case Frequency from", min(x), "to", max(x), sep = " "))
  })

  output$cntyPlot <- renderPlot({
    tmp_df <- data_now()
    tmp_df$DtEpisode <- as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y")
    tmp_df <- tmp_df[(tmp_df$DtEpisode>=input$dateRange[1] & tmp_df$DtEpisode<=input$dateRange[2])
                     & tmp_df$DisShort %in% c(input$disease)
                     & tmp_df$RStatus %in% c(input$rstat)
                     & tmp_df$Sex %in% c(input$Sex)
                     & tmp_df$Race %in% c(input$Race)
                     & tmp_df$Ethnicity %in% c(input$Ethnicity)
                     & tmp_df$ageGrp %in% c(input$ageGrp)
                     & tmp_df$Marital %in% c(input$marital)
                     & tmp_df$Pregnant %in% c(input$pregnant)
                     ,]
    
    # if(4 %in% input$mapGroup){
    cnty_freq_df <- as.data.frame(table(tmp_df$LHJ, factor(tmp_df$RStatus)))
    # hist2_df <- setDT(hist2_df)[, .(Total=sum(N)), by = .(V1,V2)]
    
    p <- ggplot(cnty_freq_df, aes(x=reorder(Var1, Freq), y = Freq, fill = Var2)
                , environment = environment()) +
      geom_bar(stat = "identity") +
      coord_flip() +
      # guides(fill = guide_legend(reverse = TRUE)) +
      labs(title = input$cntyRnkTitle
           , x = "County"
           , y = "Count") 
    p
    })
  

  #   test <- leaflet(data = subdat_county
  #                   , options = leafletOptions(
  #                     zoomDelta = 0.25, zoomSnap = 0 #Function not yet available
  #                   )) %>% 
  #     setView(lng= -121.650, lat = 37.651, zoom = 6) %>% 
  #     addTiles() %>%
  #     # addProviderTiles("CartoDB.Positron") %>% (need to obtain map of provider map)
  #     addPolygons(data = subdat_county, weight=1, col = "#2d09e5", fillOpacity = 0)
  #   test %>% addProviderTiles(providers$CartoDB.DarkMatter) %>% addHeatmap(data = tmp_df, lat = ~Latitude, lng = ~Longitude, max = 0.01, radius = 12)


  output$CRzMapPlot <- renderLeaflet({
    tmp_df <- data_now()
    tmp_df$DtEpisode <- as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y")
    tmp_df <- tmp_df[(tmp_df$DtEpisode>=input$dateRange[1] & tmp_df$DtEpisode<=input$dateRange[2])
                     & tmp_df$DisShort %in% c(input$disease)
                     & tmp_df$RStatus %in% c(input$rstat)
                     & tmp_df$Sex %in% c(input$Sex)
                     & tmp_df$Race %in% c(input$Race)
                     & tmp_df$Ethnicity %in% c(input$Ethnicity)
                     & tmp_df$ageGrp %in% c(input$ageGrp)
                     & tmp_df$Marital %in% c(input$marital)
                     & tmp_df$Pregnant %in% c(input$pregnant)
                     ,]
    
    cnty_freq_df <- as.data.frame(table(tmp_df$LHJ, factor(tmp_df$RStatus)))
    
    cnty_freq_df <- as.data.table(cnty_freq_df) #From countyPlot
    cnty_freq_df[cnty_freq_df$Var1=="Berkeley",]$Var1 <- "Alameda"
    cnty_freq_df[cnty_freq_df$Var1=="Long Beach",]$Var1 <- "Los Angeles"
    cnty_freq_df <- setDT(cnty_freq_df)[, .(Freq=sum(Freq)), by = .(Var1,Var2)]
    
    #Census Data should be loaded through CRmap_00.R
    census_info <- merge(cnty_freq_df, census_info, by.x = "Var1", by.y = "GEO.display.label")
    # census_info <- left_join(cnty_freq_df, census_info, by = c("Var1"="GEO.display.label"))
    
    #Measures
    census_info$incidence_rt <- (as.numeric(census_info$Freq)*100000)/as.numeric(census_info$respop72017)
    census_info$st_err <- as.numeric(census_info$incidence_rt)/(as.numeric(census_info$respop72017)^.5)
    census_info$rel_sterr <- (census_info$st_err*100)/census_info$incidence_rt
    
    ##Need to work on developing heat map for multiple RStatus conditons##
    
    census_info <- census_info %>% dplyr::rename(id = GEO.id2, geography = Var1, total = respop72017, cases = Freq)
    
    #Transformed from standard data and local data to rename columns and format class type
    # tmp <- mutate(census_info[census_info$Var2==input$rptGroup,]
    tmp <- mutate(census_info[census_info$Var2=="Confirmed",]
                  , id = as.character(id)
                  , geography = as.character(geography)
                  , total = as.numeric(total)
                  , cases = as.numeric(cases)
                  , incidence_rt = incidence_rt)
    
    #Option #2
    df.polygon <- tract
    df.polygon@data$rec <- 1:nrow(df.polygon@data)
    # tmp <- merge(df.polygon@data, tmp,  by.x = "GEOID", by.y = "id") %>% arrange(rec)
    tmp <- left_join(df.polygon@data, tmp,  by = c("GEOID" = "id")) %>% arrange(rec)
    df.polygon@data <- tmp
    df.polygon <- df.polygon[!is.na(df.polygon@data$geography),]
    # df.polygon <- df.polygon[!is.na(df.polygon@data$geography),]
    df.polygon <- df.polygon[order(df.polygon@data$incidence_rt),]
    # renderPlot({
    p <- CRmap00(mapBorder=input$mapBorder, mapLayer=input$mapLayer, tmp_df=tmp_df)
    p
    # **This is where I will add custom function for plotting**
  })
  
  output$CRstMapPlot <- renderPlot({
    tmp_df <- data_now()
    tmp_df$DtEpisode <- as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y")
    tmp_df <- tmp_df[(tmp_df$DtEpisode>=input$dateRange[1] & tmp_df$DtEpisode<=input$dateRange[2])
                     & tmp_df$DisShort %in% c(input$disease)
                     & tmp_df$RStatus %in% c(input$rstat)
                     & tmp_df$Sex %in% c(input$Sex)
                     & tmp_df$Race %in% c(input$Race)
                     & tmp_df$Ethnicity %in% c(input$Ethnicity)
                     & tmp_df$ageGrp %in% c(input$ageGrp)
                     & tmp_df$Marital %in% c(input$marital)
                     & tmp_df$Pregnant %in% c(input$pregnant)
                     ,]
    
      hist_df <- as.data.table(as.data.frame.matrix(table(tmp_df$LHJ, factor(tmp_df$RStatus))))
      hist_df <- cbind(as.data.table(table(tmp_df$LHJ))
                       , as.data.table(as.data.frame.matrix(table(tmp_df$LHJ, factor(tmp_df$RStatus)))))
# hist_df
      hist_df[hist_df$V1=="Berkeley",]$V1 <- "Alameda"
      hist_df[hist_df$V1=="Long Beach",]$V1 <- "Los Angeles"
      # as.data.table(hist_df)

      #Using only 'confirmed' cases - possibly change to selection of Rstatus cases in the future
      hist_df <- setDT(hist_df)[, .(Freq=sum(Confirmed)), by = .(V1)]
      census_info <- merge(hist_df, census_info, by.x = "V1", by.y = "GEO.display.label")
      # census_info <- left_join(cnty_freq_df, census_info, by = c("Var1"="GEO.display.label"))

      #Measures
      census_info$incidence_rt <- (as.numeric(census_info$Freq)*100000)/as.numeric(census_info$respop72017)
      census_info$st_err <- as.numeric(census_info$incidence_rt)/(as.numeric(census_info$respop72017)^.5)
      census_info$rel_sterr <- (census_info$st_err*100)/census_info$incidence_rt

      ##Need to work on developing heat map for multiple RStatus conditons##

      census_info <- census_info %>% dplyr::rename(id = GEO.id2, geography = V1, total = respop72017, cases = Freq)
      census_info$geography <- tolower(census_info$geography)
      census_info <- dplyr::full_join(x = census_info[,incidence_rt, geography]
                                 , y = as.data.frame((california[california$region %in% "california",]))
                                 , by = c("geography" = "subregion"), all = TRUE)

      census_info[is.na(census_info$incidence_rt), "incidence_rt"] <- 0
      census_info <- census_info[complete.cases(census_info),]
      # census_info[1:6,]
      # census_info$V1 <- tolower(census_info$V1)
      # census_info <- dplyr::full_join(x = census_info[,incidence_rt, V1]
      #                                 , y = as.data.frame((california[california$region %in% "california",]))
      #                                 , by = c("V1" = "subregion"), all = TRUE)
      # 
      # census_info[is.na(census_info$incidence_rt), "incidence_rt"] <- 0
      # census_info <- census_info[complete.cases(census_info),]


      # mapBorder <- input$mapBorder
      # mapLayer <- input$mapLayer
    # p2 <- CRmap01(mapBorder=input$mapBorder, mapLayer=input$mapLayer, tmp_df=tmp_df)
      p2 <- CRmap01(mapLayer = input$mapLayer, tmp_df=tmp_df, census_info = census_info)
      p2
    })

  output$selectPop <- renderDataTable({
    tmp_df <- data_now()
    # demographic_list <- list("Sex", "ageGrp", "Race", "Ethnicity", "RStatus")
    # demographicTbl <- getSummaryTbl(tmp_df = tmp_df, tbl_list = demographic_lst, crossTbl = NULL)
    demographicTbl <- getSummaryTbl(tmp_df = tmp_df, dtRange = input$dateRange, disShort = input$disease, Rstatus = input$rstat, sex = input$Sex, race = input$Race, ethnicity = input$Ethnicity, ageGrp = input$ageGrp, marital = input$marital, pregnant = input$pregnant
                                    # , bivariate = input$bivariate, crossTbl = input$crossTbl
                                    )
    demographicTbl
  }, escape = FALSE)

  #Section for creating download feature of population demographic selection
  datasetInput <- reactive({
    tmp_df <- data_now()
    # demographic_list <- list("Sex", "ageGrp", "Race", "Ethnicity", "RStatus")
    # demographicTbl <- getSummaryTbl(tmp_df = tmp_df, tbl_list = demographic_lst, crossTbl = NULL)
    demographicTbl <- getSummaryTbl(tmp_df = tmp_df, dtRange = input$dateRange, disShort = input$disease, Rstatus = input$rstat, sex = input$Sex, race = input$Race, ethnicity = input$Ethnicity, ageGrp = input$ageGrp, marital = input$marital, pregnant = input$pregnant
                                    # , bivariate = input$bivariate, crossTbl = input$crossTbl
    )
    demographicTbl
  }
  )
  
  # output$downloadPop <- downloadHandler(
  #   filename = function(){
  #     paste(input$selectPop, ".csv", sep = "")
  #   }
  #   content = function(file){
  #     write.csv(datasetInput(), file, row.names = TRUE)
  #   }
  # )
  
 # output$selectData <- renderDataTable({
 #    tmp_df <- data_now()
 #    tmp_df <- getDataTbl(tmp_df = tmp_df, dtRange = input$dateRange, disShort = input$disease, Rstatus = input$rstat, sex = input$Sex, race = input$Race, ethnicity = input$Ethnicity, ageGrp = input$ageGrp, marital = input$marital, pregnant = input$pregnant)
 #    tmp_df
 #  })

  # output$hover_info <- renderPrint({
  #   if(!is.null(input$plot_hover))
  #     paste0(input$plot_hover$x, " ", input$plot_hover$y)
  # })
  # 
  # output$click_info <- renderUI({
  #   click <- input$plot_click   
  #   click <- nearPoints(tmp_df
  #                       , click
  #                       , threshold = 5
  #                       , maxpoints = 1
  #                       , addDist = TRUE
  #   )
  #   
  #   if (nrow(click) == 0) return(NULL)
  #   # calculate point position INSIDE the image as percent of total dimensions    
  #   # from left (horizontal) and from top (vertical)
  #   left_pct <- (click$x - click$domain$left) / (click$domain$right - click$domain$left)
  #   top_pct <- (click$domain$top - click$y) / (click$domain$top - click$domain$bottom)        
  #   # calculate distance from left and bottom side of the picture in pixels
  #   left_px <- click$range$left + left_pct * (click$range$right - click$range$left)
  #   top_px <- click$range$top + top_pct * (click$range$bottom - click$range$top)
  #   # create style property fot tooltip
  #   # background color is set so tooltip is a bit transparent
  #   # z-index is set so we are sure are tooltip will be on top 
  #   style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); "
  #                   ,"left:", left_px + 2, "px; top:", top_px + 2, "px;")
  #   # actual tooltip created as wellPanel    
  #   wellPanel(style = style
  #             , p(HTML(paste0("<b> tmp_df: </b>", rownames(click)
  #                             , "<br/>"
  #                             , "<b> y: </b>", click$mapping$y, "<br/>"
  #                             , "<b> x: </b>", click$mapping$x, "<br/>"
  #                             , "<b> Distance from left: </b>"
  #                             , left_px
  #                             , "<b>, from top: </b>"
  #                             , top_px)))
  #   )
  # }
  # )
  # # output$hover_info <- renderPrint({
  # #      if(!is.null(input$plot_hover)){
  # #          hover=input$plot_hover
  # #          dist=sqrt((hover$x-tmp_df$mpg)^2+(hover$y-tmp_df$disp)^2)
  # #          cat("Weight (lb/1000)\n")
  # #          if(min(dist) < 3)
  # #              tmp_df$wt[which.min(dist)]
  # #    }
  #  Get ready for deletions
  # output$mapPlot <- renderPlot({
  #   minDate <- as.Date(as.character(input$date_range[1]), format = '%Y-%m-%d')
  #   maxDate <- as.Date(as.character(input$date_range[2]), format = '%Y-%m-%d')
  #   # # tmp_df[(tmp_df$Latitude==""),]<-NA
  #   # # tmp_df[(tmp_df$Longitude==""),]<-NA
  #   # tmp_df[is.null(tmp_df$Longitude),]<- NA
  #   # tmp_df<- tmp_df[!is.na(tmp_df$Longitude)|!is.na(tmp_df$Latitude),]
  #   # tmp_df$Longitude <- as.numeric(as.character(tmp_df$Longitude))
  #   # tmp_df$Latitude <- as.numeric(as.character(tmp_df$Latitude))
  #   # tmp_df$DtEpisode <- as.Date(as.character(tmp_df$DtEpisode), format = '%m/%d/%y')
  # 
  #   # For county heat mapping
  #   #      as.data.frame.matrix(as.data.table(table(tmp_df$City)))
  #   # test <- as.data.frame.matrix(as.data.table(table(tmp_df$CntyGEO)))
  #   # test$V1 <- tolower(test$V1)
  #   # meh <- merge(x = test
  #   #              , y = as.data.frame((california[california$region %in% "california",]))
  #   #              , by.x = "V1"
  #   #              , by.y="subregion"
  #   #              , all= TRUE)
  # 
  # 
  #   #work on logic for switching out layers
  #   if(4 %in% input$mapGroup){
  #     heat_df <- as.data.frame.matrix(as.data.table(table(tmp_df$CntyGEO)))
  #     heat_df$V1 <- tolower(heat_df$V1)
  #     heat_df <- dplyr::full_join(x = heat_df
  #                                 , y = as.data.frame((california[california$region %in% "california",]))
  #                                 , by = c("V1"="subregion")
  #                                 , all = TRUE)
  #     heat_df[is.na(heat_df$N), "N"] <- 0
  #     heat_df <- heat_df[complete.cases(heat_df),]
  #     p <- p +
  #       geom_polygon(data = heat_df, aes(x=long, y=lat, group = group, fill = log1p(N)), na.rm = TRUE) +
  #       coord_fixed(1.3) +
  #       scale_fill_gradientn(colours=rev(brewer.pal(3, "RdYlBu")))
  #   }
  #   #   p <- p + geom_polygon(data = tmp_df, aes(x=Longitude, y=Latitude, group = group
  #   #                                            , fill = colorBuckets
  #   #                                            , fill = cases_per_county
  #   #                                            , fill = cases_per_pop
  #   #                                            , fill = cases_per_area
  #   #                                            )
  #   #                         )
  #   # }
  # 
  #   #scale_alpha
  #   # stat_density2d(aes(x = Longitude, y = Latitude, fill = ..level.., alpha = ..level..)
  #   #                , bins = 4, geom = "polygon", data = temp_df)
  #   # scale_fill_gradient(low = "black", high = "blue")
  #   p
  # }
  # )
  # 
  # # tmp_df$CntyGEO <- tolower(tmp_df$CntyGEO)
  # # test <- inner_join(counties, tmp_df, by = c("subregion" = "CntyGEO"))
  # 
  # # output$contents <- renderTable({
  # #   tmp_df()
  # # })
  # 
  # # output$test_text <- renderText({
  # #  list(input$disNme)
  # # })
  # 
  # output$values <- renderTable({
  #   sliderValues()
  #   # Include epi measurements
  #   #renderPrint({input$checkGroup })
  # })
  # 
  # # output$texts <- renderTable({
  # #   # paste(input$disNme, sep = ",")
  # #   tmp_df <- data_now()
  # #   tmp_df <- tmp_df[tmp_df$DisShort == input$disNme,]
  # # })
  # # output$disNme <- renderText({
  # #   
  # # })
  # 
  output$downloadPlot <- downloadHandler(
    filename = function(){paste(input$distPlot, '.jpg', sep = '')},
    content = function (file){
      ggsave(file, plot = last_plot(), device = "jpeg", width = 8, height = 5.5, units = "in")
      # ggsave(file, plot = last_plot(), device = "jpeg", width = 5, height = 4, units = "in", dpi = 700)
    }
  )
  
  output$downloadMapSt <- downloadHandler(
    filename = function(){paste(input$CRstMapPlot, '.jpg', sep = '')},
    content = function(file){
      ggsave(file, plot = last_plot(), device = "jpeg", width = 5, height = 5, units = "in")
    }
  )
  
  # output$downloadMapZ <- downloadHandler(
  #   filename <- function(){paste(input$mapPlot, ".jpg", sep = '')},
  #   content <- function(file){
  #     # ggsave(file, plot = plotInput(), device = device)
  #     png(file, width = 400, height = 300)
  #     dev.off()
  #   }
  # )
# 
  output$downloadValues <- downloadHandler(
    filename <- function(){paste(input$values, ".csv")},
    content = function(file){
      write.csv(catSelect(), file, row.names = FALSE)
    }
  )
  
  output$downloadSummary <- downloadHandler(
    filename <- function(){paste(input$summaryTbl, ".csv")},
    content = function(file){
      write.csv(summaryTbl(), file, row.names = FALSE)
    }
  )
  # tableOutput
  #   output$my_tooltip <- renderUI({
  #   hover <- input$plot_hover 
  #   y <- nearPoints(data(), input$plot_hover)[ ,c("mpg", input$var_y)]
  #   req(nrow(y) != 0)
  #   verbatimTextOutput("vals")
  # })
  
}
##Designating size of shiny.app in html page
# , options = list(height = 500)
)