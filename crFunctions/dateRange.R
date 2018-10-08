# myFunctions <- paste0(myCRLoc, "/crFunctions/")
# source(file.path(myFunctions, "tsvFile.R"))

date_rangeInput <- function(id, label = h5("Select Date Range:")){
  ns <- NS(id)
  out <- tagList(
    uiOutput(ns("dtRange"))
  )
  out
}

date_range <- function(input, output, session){
# output$date_range <-
  
  output$dtRange <- renderUI({
    ns <- session$ns
    tmp_df <<- as.data.frame(ns(data_now()))
    # data_now <- callModule(tsvFile, "datafile", stringsAsFactors = FALSE)
  # if(is.null(tmp_df))
  #   return()
  # switch(tmp_df,
    dateMin <- range(as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y"))[1]
    dateMax <- range(as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y"))[2]
    sliderInput("date_range",
                label = h5("Select Date Range:")
                , min = dateMin
                , max = dateMax
                , value = c(dateMin
                            , dateMax
                )
  )
})
}