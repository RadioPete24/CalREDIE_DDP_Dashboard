tsvFileInput <- function(id, label = h5("Choose TSV File")) {
  #namespace function
  ns <- NS(id)
  
  out <- tagList(
    fileInput(ns("datafile"), label = h5("Choose TSV File")
              , multiple = FALSE
              , accept = c("text", "text/tsv", ".tsv", ".csv")
    ),
    #Horizontal line
    tags$hr(),
    
    #Input: Checkbox for if file has header
    # checkboxInput(ns("header"), label = "Header", TRUE),
    
    # Input: Select separator ----
    radioButtons(ns("sep"), "Separator",
                 choices = c(Comma = ",",
                             Semicolon = ";",
                             Tab = "\t"),
                 selected = ",")    
  )
  out
}

#Modeule server function
tsvFile <- function(input, output, session, stringsAsFactors = FALSE){
  
  userFile <- reactive({
    validate(need(input$datafile, message = FALSE)) #if file is not at indicated input location
    input$datafile
    # if(is.null(inFile)) return (NULL)#if file is not at indicated input location
  })  
  
  data_now <- reactive({
    tmp_df <- dplyr::select(as.data.frame(fread(userFile()$datapath))
                            , -c(SSN
                            , FirstName
                            , LastName
                            , MiddleName
                            , NameSuffix
                            , HomePhone
                            , CellPhone
                            , WorkPhone
                            , Email
                            , OtherElectronicID
                            , WorkSchoolContact
                            , Guardian
                            , NOTES)
                            ) #Reading in .tsv file
    #Handle null mapping data
    tmp_df[is.null(tmp_df$Longitude),]<- NA
    tmp_df <- tmp_df[!is.na(tmp_df$Longitude)|!is.na(tmp_df$Latitude),]
    tmp_df$Longitude <- as.numeric(as.character(tmp_df$Longitude))
    tmp_df$Latitude <- as.numeric(as.character(tmp_df$Latitude))
    #Format and handle null date information
    # tmp_df$DtEpisode <- strptime(as.character(tmp_df$DtEpisode), format = '%m/%d/%Y')
    # tmp_df$DtEpisode <- as.Date(as.character(tmp_df$DtEpisode), format = '%m/%d/%y')
    # test$quintile <- ntile(tmp_df$Age, 5)
    tmp_df[tmp_df$Marital == "",]$Marital <- "Unknown"
    tmp_df[tmp_df$Pregnant == "" & tmp_df$Sex == "F",]$Pregnant <- "U"
    tmp_df[tmp_df$Pregnant == "" & tmp_df$Sex == "M",]$Pregnant <- "NA"
    tmp_df$ageGrp <- cut(tmp_df$Age, breaks = quantile(tmp_df$Age, probs = seq(0,1,0.2)), include.lowest = TRUE)
    tmp_df[tmp_df$Race == "",]$Race <- "Unknown"
    # test <- as.data.table(test)
    # test[, quintile := cut(tmp_df$Age, breaks = quantile(tmp_df$Age, probs = seq(0,1,0.2)), include.lowest = TRUE)]
    tmp_df$ageGrp <- gsub(pattern = ",", replacement = "-", x = tmp_df$ageGrp)
    tmp_df$ageGrp <- gsub(pattern = "\\(|\\]|\\[", replacement = "", x = tmp_df$ageGrp)
    
    tmp_df <- tmp_df[!is.na(tmp_df$DtEpisode),]
    tmp_df <- tmp_df[!is.null(tmp_df$DtEpisode),]
    
    tmp_df
  })
  return(data_now)
  # return(dataframe)
}

# 
# test$quintile <- with(tmp_df, cut(Age, breaks = quantile(Age, probs = seq(0, 1, by = 0.2), na.rm = TRUE), include.lowest = TRUE))
