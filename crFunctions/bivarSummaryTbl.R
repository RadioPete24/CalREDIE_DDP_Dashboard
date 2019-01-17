getBivarSummaryTbl <- function(tmp_df = tmp_df, dtRange = input$dateRange, disShort = input$disease, Rstatus = input$rstat, sex = input$Sex, race = input$Race, ethnicity = input$Ethnicity, ageGrp = input$ageGrp, marital = input$marital, pregnant = input$pregnant, bivariate = input$bivariate
){
  tbl_list <- list("Sex", "ageGrp", "Race", "Ethnicity", "RStatus", "Marital", "Pregnant")
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
  summary_tbl <- lst()
  tbl_list <- tbl_list[-(which(tbl_list == c(bivariate)))]
 
  for(i in 1:length(tbl_list)){
               #Data columns
    # tbl_list[-(which(tbl_list == c("crossTbl")))]
               summary_tbl[[i]] <- data.frame(Var1 = row.names(cbind(as.data.frame.matrix(
                 xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)], data = tmp_df))
                 , as.data.frame.matrix(prop.table(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)], data = tmp_df)))))
                 , cbind(as.data.frame.matrix(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)], data = tmp_df))
                         , as.data.frame.matrix(round(prop.table(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)]
                                                                       , data = tmp_df))*100, 2))))

               # summary_tbl[[i]]$Var1 <- paste("<p style=\"text-indent: 20px\">", as.data.frame(table(tmp_df[as.character(tbl_list[[i]])]))[,1], "</p>")
               summary_tbl[[i]]$Var1 <- paste("<p style=\"text-indent: 20px\">", summary_tbl[[i]][,1], "</p>")

               temp_df <- cbind(paste0("<b>", as.character(tbl_list[[i]]), "</b>", sep = "")
                                , data.frame(matrix(NA, ncol = length(data.frame(Var1 = row.names(cbind(as.data.frame.matrix(
                                  xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)], data = tmp_df))
                                  , as.data.frame.matrix(prop.table(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)]
                                                                          , data = tmp_df)))))
                                  , cbind(as.data.frame.matrix(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)]
                                                                     , data = tmp_df))
                                          , as.data.frame.matrix(prop.table(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)]
                                                                                  , data = tmp_df))))))-1, nrow = 1)))

               colnames(temp_df) <- colnames(summary_tbl[[i]])
               summary_tbl[[i]] <- rbind(temp_df, summary_tbl[[i]])
             }
      summary_tbl <- do.call(rbind, summary_tbl)
      as.data.frame(summary_tbl)
}

##may be losing because of cross talk with own variable...

# View(as.data.frame.matrix(xtabs(~tmp_df[,as.character(tbl_list[2])] + tmp_df[,as.character(tbl_list[3])], data = tmp_df)))

# tbl_list[-(which(tbl_list == c("Race")))]