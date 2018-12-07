# getSummaryTbl <- function(tmp_df = tmp_df, tbl_list = demographic_list, crossTbl = NULL){
getSummaryTbl <- function(tmp_df = tmp_df, dtRange = input$dateRange, disShort = input$disease, Rstatus = input$rstat, sex = input$Sex, race = input$Race, ethnicity = input$Ethnicity, ageGrp = input$ageGrp, marital = input$marital, pregnant = input$pregnant
                          # , bivariate = input$bivariate, crossTbl = FALSE
                          ){
  #tbl_list is the list of features of interest formatted as c(...)
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
  
  for(i in 1:length(tbl_list)){
    tmp_summaryTbl <- cbind(as.data.frame(table(tmp_df[as.character(tbl_list[[i]])])), as.data.frame(round(prop.table(table(tmp_df[as.character(tbl_list[[i]])]))*100,2))[2])
    colnames(tmp_summaryTbl) <- c("Var1", "Freq", "Percent")
    tmp_summaryTbl$Var1 <- paste("<p style=\"text-indent: 20px\">", as.data.frame(table(tmp_df[as.character(tbl_list[[i]])]))[,1], "</p>")
    summary_tbl[[i]] <- rbind(data.frame(Var1 = paste0("<b>", as.character(tbl_list[[i]]), "</b>", sep = "")
                                         , Freq = NA
                                         , Percent = NA)
                              , tmp_summaryTbl)
  }
  summary_tbl <- do.call(rbind, summary_tbl)
  as.data.frame(summary_tbl) 
} 
#   ifelse(crossTbl,
#          for(i in 1:length(tbl_list)){
#            tmp_summaryTbl <- cbind(as.data.frame(table(tmp_df[as.character(tbl_list[[i]])])), as.data.frame(round(prop.table(table(tmp_df[as.character(tbl_list[[i]])]))*100,2))[2])
#            colnames(tmp_summaryTbl) <- c("Var1", "Freq", "Percent")
#            tmp_summaryTbl$Var1 <- paste("<p style=\"text-indent: 20px\">", as.data.frame(table(tmp_df[as.character(tbl_list[[i]])]))[,1], "</p>")
#            summary_tbl[[i]] <- rbind(data.frame(Var1 = paste0("<b>", as.character(tbl_list[[i]]), "</b>", sep = "")
#                                                 , Freq = NA
#                                                 , Percent = NA)
#                                      , tmp_summaryTbl)
#          }
#          #in the else section:
#          , for(i in 1:length(tbl_list)){
#            #Data columns
#            summary_tbl[[i]] <- data.frame(Var1 = row.names(cbind(as.data.frame.matrix(
#              xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)], data = tmp_df))
#              , as.data.frame.matrix(prop.table(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)], data = tmp_df)))))
#              , cbind(as.data.frame.matrix(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)], data = tmp_df))
#                      , as.data.frame.matrix(round(prop.table(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)]
#                                                                    , data = tmp_df))*100, 2))))
#            
#            # summary_tbl[[i]]$Var1 <- paste("<p style=\"text-indent: 20px\">", as.data.frame(table(tmp_df[as.character(tbl_list[[i]])]))[,1], "</p>")
#            summary_tbl[[i]]$Var1 <- paste("<p style=\"text-indent: 20px\">", summary_tbl[[i]][,1], "</p>")
#            
#            temp_df <- cbind(paste0("<b>", as.character(tbl_list[[i]]), "</b>", sep = "")
#                             , data.frame(matrix(NA, ncol = length(data.frame(Var1 = row.names(cbind(as.data.frame.matrix(
#                               xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)], data = tmp_df))
#                               , as.data.frame.matrix(prop.table(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)]
#                                                                       , data = tmp_df)))))
#                               , cbind(as.data.frame.matrix(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)]
#                                                                  , data = tmp_df))
#                                       , as.data.frame.matrix(prop.table(xtabs(~tmp_df[,as.character(tbl_list[[i]])] + tmp_df[,as.character(bivariate)]
#                                                                               , data = tmp_df))))))-1, nrow = 1)))
#            
#            colnames(temp_df) <- colnames(summary_tbl[[i]])
#            summary_tbl[[i]] <- rbind(temp_df, summary_tbl[[i]])
#          }
#   )
#   summary_tbl <- do.call(rbind, summary_tbl)
#   as.data.frame(summary_tbl)         
# 
# }


  
  
# Extras here  
  
  #will want to filter by year rate...

# cbind(as.data.frame.matrix(xtabs(~ageGrp + Race, data = tmp_df)), as.data.frame.matrix(prop.table(xtabs(~ageGrp + Race, data = tmp_df))))

# as.data.frame(table(tmp_df[as.character(disease_lst[[1]])]))[1]
# 
# "<p style=\"text-indent: 20px\">"

# names(as.data.frame((cbind(paste0("<b>", as.character(disease_lst[[1]]), "</b>", sep = ""), data.frame(matrix(NA, ncol = length(test)-1, nrow = 1)))))) <- names(cbind(as.data.frame.matrix(table(tmp_df[,as.character(disease_lst[[1]])], tmp_df$Race)), as.data.frame.matrix(round(prop.table(table(tmp_df[,as.character(disease_lst[[1]])], tmp_df$Race))*100,2))))

# 
# rbind(paste0("<b>", as.character(tbl_list[[i]]), "</b>", sep = ""), test[,c(1:length(test))])
# cbind(paste0("<b>", as.character(disease_lst[[1]]), "</b>", sep = ""), 
#   summary_tbl[[i]] <- as.data.frame(table(tmp_df[as.character(tbl_list[[i]]), tmp_df[as.character(crossTab)]]))
#   }
# )

# summary_tbl[[i]][1] <- paste("<b>", summary_tbl[[i]][1], "<\b>")

# tmp_summaryTbl <- cbind(as.data.frame.matrix(table(tmp_df[,as.character(tbl_list[[i]])], tmp_df$Race))
#                         , as.data.frame.matrix(round(prop.table(table(tmp_df[,as.character(tbl_list[[i]])]
#                                                                       , tmp_df$Race))*100,2)))
# 
# summary_tbl[[i]] <- data.frame(Var1 = row.names(tmp_summaryTbl), tmp_summaryTbl)
# # summary_tbl[[i]] <- cbind(paste0("<b>", as.character(tbl_list[[i]]), "</b>", sep = "")
# #                           , data.frame(matrix(NA, ncol = length(tmp_summaryTbl)-1, nrow = 1)))
# #   names(summary_tbl[[i]]) <- names(tmp_summaryTbl)
# summary_tbl[[i]] <- rbind(summary_tbl[[i]], tmp_summaryTbl)