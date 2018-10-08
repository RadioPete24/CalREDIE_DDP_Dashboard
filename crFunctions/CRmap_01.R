CRmap01 <- function(mapBorder = "County", mapLayer = "inc_rt"){
  p <- ggplot() + geom_polygon(data = california, aes(x=long, y=lat, group = group)) + 
    geom_polygon(color = "white", fill = "gray") +
    # + coord_fixed(1.3) +
    xlim(-130, -107) + ylim(31.5,43) +
    labs(title = input$mapTitle
         , x = "Longitude"
         , y = "Latitude")
  
  if(mapGroup == "pt_map"){
    if("pt_map" %in% mapLayer){
      
      p <- p + geom_point(data = tmp_df, aes(x=Longitude, y=Latitude), color = "red", shape = 42)
    }       
    if("ht_map" %in% mapLayer){
      p <- p + stat_density2d(data = tmp_df, aes(x=Longitude, y=Latitude, fill = ..level..)
                              , alpha=0.5
                              , geom ="polygon") +
        scale_fill_gradientn(colours = rev(brewer.pal(3, "Spectral")))
      
      # geom_density2d(aes(fill = ..level..), alpha=0.5, geom="polygon") +
      # scale_fill_gradientn(colours=rev(brewer.pal(3,"Spectral"))) +
    }
    if("County" %in% mapBorder){
      p <- p + geom_path(data = california, aes(x=long, y=lat, group = group), colour = "white")
    }
    #work on logic for switching out layers
    if(4 %in% measure){
      heat_df <- as.data.frame.matrix(as.data.table(table(tmp_df$CntyGEO)))
      heat_df$V1 <- tolower(heat_df$V1)
      heat_df <- dplyr::full_join(x = heat_df
                                  , y = as.data.frame((california[california$region %in% "california",]))
                                  , by = c("V1"="subregion")
                                  , all = TRUE)
      heat_df[is.na(heat_df$N), "N"] <- 0
      heat_df <- heat_df[complete.cases(heat_df),]
      p <- p + 
        geom_polygon(data = heat_df, aes(x=long, y=lat, group = group, fill = log1p(N)), na.rm = TRUE) +
        coord_fixed(1.3) +
        scale_fill_gradientn(colours=rev(brewer.pal(3, "RdYlBu")))
    }
    p
  }
}

#Static Map
# CRmapLeaflet <- function(myLHJ, myCause=0, myMeasure = "YLLper", myYear=2015, myGeo="Census Tract") {
#   
#   # county data for just 2011-2015
#   # dat.X   <- filter(datCounty,year %in% 2011:2015, CAUSE==myCause,county !="CALIFORNIA STATE")
#   
#   if (myGeo == "County"){
#     dat.1   <- filter(datCounty,year==myYear,CAUSE==myCause,Level == "gbd36")  
#     map.1   <- merge(shape_County, dat.1, by.x=c("county"), by.y = c("county"),all=TRUE)
#     map.1$lab <- map.1$county
#     yearLab <- myYear }
#   
#   if (myGeo == "Community") {
#     dat.1    <- filter(datComm,yearG==yG,CAUSE==myCause, comID != "Unknown",Level == "gbd36")
#     map.1    <- merge(shape_Comm, dat.1, by.x=c("county","comID"), by.y = c("county","comID"),all=TRUE) 
#     map.1$lab <- map.1$comName
#     yearLab <- yG    }  
#   
#   if (myGeo == "Census Tract") { 
#     dat.1    <- filter(datTract,yearG==yG,CAUSE==myCause,Level == "gbd36") 
#     dat.1    <- dat.1[dat.1$pop > 200,]  # TEMP FIX
#     map.1    <- merge(shape_Tract, dat.1, by.x=c("county","GEOID"), by.y = c("county","GEOID"),all=TRUE) 
#     map.1$lab <- map.1$GEOID
#     yearLab  <- yG    }
#   
#   #  if (cZoom) {map.1   <- map.1[map.1$county == myLHJ,]}
#   
#   if (nrow(dat.1)==0) stop("Sorry friend, but thank goodness there are none of those; could be some other error")
#   
#   map.1$WORK <-  eval(parse(text=paste0("map.1$",myMeasure)))
#   map.1$WORK[is.na(map.1$WORK)] <- 0
#   
#   pal <- colorNumeric(rev(brewer.pal(6,"RdYlBu")),  domain = map.1[["WORK"]], 6)
#   
#   mA <-leaflet(map.1)  %>% 
#     addTiles()  %>%
#     addPolygons(color = "#444444", weight = 1, fillOpacity = 0.4, fillColor = ~ pal(WORK),
#                 popup = paste(myMeasure,"<BR>",map.1[["lab"]],"<BR>",round(map.1[["WORK"]],2)   ) ) %>%
#     addLegend("bottomleft", pal = pal, values = map.1[["WORK"]],title = names(lMeasures[lMeasures==myMeasure]), opacity=.7 )
#   
#   mA
# }
# 
# 
# ##################---------------------------
# 
# CRmapLeaflet <- function(myLHJ, myCause=0, myMeasure = "YLLper", myYear=2015, myGeo="Census Tract") {
# 
# #County Data
# test <- leaflet(subdat_county) %>% 
#   setView(lng= -121.650, lat = 37.651, zoom = 5) %>% 
#   addTiles() %>%
#   # addProviderTiles("CartoDB.Positron") %>% (need to obtain map of provider map)
#   addPolygons(data = subdat_county, weight=1, col = "#2d09e5", fillOpacity = 0)
# 
# test