getDataTbl <- function (tmp_df = tmp_df, dtRange = input$dateRange, disShort = input$disease, Rstatus = input$rstat, sex = input$Sex, race = input$Race, ethnicity = input$Ethnicity, ageGrp = input$ageGrp, marital = input$marital, pregnant = input$pregnant){
  tmp_df$DtEpisode <- as.Date(tmp_df$DtEpisode, format = "%m/%d/%Y")
  tmp_df <- tmp_df[(tmp_df$DtEpisode>=dtRange[1] & tmp_df$DtEpisode<=dtRange[2])
                   & tmp_df$DisShort %in% c(disShort)
                   & tmp_df$RStatus %in% c(Rstatus)
                   & tmp_df$Sex %in% c(sex)
                   & tmp_df$Race %in% c(race)
                   & tmp_df$Ethnicity %in% c(ethnicity)
                   & tmp_df$ageGrp %in% c(ageGrp)
                   & tmp_df$Marital %in% c(marital)
                   & tmp_df$Pregnant %in% c(pregnant)
                   ,]
  tmp_df
}