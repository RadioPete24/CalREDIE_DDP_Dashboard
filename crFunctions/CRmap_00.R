#ftp2.census.gov, ftp://ftp2.census.gov/geo/tiger/TIGER2017/
# url<-"http://www2.census.gov/geo/tiger/TIGER2010DP1/County_2010Census_DP1.zip"
# downloaddir<-"d:/Leaflet"
# destname<-"tiger.zip"
# download.file(url, destname)
# unzip(destname, exdir=downloaddir, junkpaths=TRUE)

# https://tile.thunderforest.com/landscape/{z}/{x}/{y}.png?apikey=<insert-your-apikey-here>

# ~~~~

CRmap00 <- function(mapBorder = "County", mapLayer = "inc_rt", tmp_df = tmp_df){
  #require(leaflet)
  # devtools::install_github("rstudio/leaflet")
  zMap_border <- leaflet(data = subdatCounty) %>%
    setView(lng= -121.0, lat = 37.651, zoom = 6) %>%
    addTiles()
  #County Data
  if("County" %in% mapBorder){
    zMap_border <- zMap_border %>%
      # zMap_border <- leafletProxy('zMap_border') %>%
      # addProviderTiles("CartoDB.Positron") %>% (need to obtain map of provider map)
      addPolygons(data = subdatCounty, weight=1, col = "#2d09e5", fillOpacity = 0)
  }
  #City data
  if("City" %in% mapBorder){
    zMap_border <- zMap_border %>%
      addPolygons(data = subdatCity, weight=1, col= "#37ce41")
  }
  #Adding record data
  if("inc_rt" %in% mapLayer){
    popup <- paste0("GEOID: ", df.polygon@data$NAME
                    , "<br>", "Incidence Rate of Counties: "
                    , round(df.polygon@data$incidence_rt,3)
                    , sep = " ")
    pal <- colorNumeric(palette = "YlGnBu"
                        , domain = log1p(df.polygon@data$incidence_rt)
                        # , n = 7
    )
    zMap_border <- zMap_border %>%
      # addProviderTiles("CartoDB.Positron") %>%
      addPolygons(data = df.polygon, fillColor = ~pal(log1p(incidence_rt)), color = "#b2aeae", fillOpacity = 0.7, weight = 0.3, smoothFactor = 0.2, popup = popup) %>%
      addLegend(pal = pal, values = df.polygon@data$incidence_rt, position = "bottomleft", title = "Incidence Rate of Cases", labFormat = labelFormat(suffix = " cases per 100000"))
  }
  if("pt_map" %in% mapLayer){
    zMap_border <- zMap_border %>%
      addCircles(data = tmp_df, lat = ~Latitude, lng = ~Longitude, color = "#ff0066")
  }
  if("ht_map" %in% mapLayer){
    zMap_border <- zMap_border %>%
      addHeatmap(data = tmp_df, lat = ~Latitude, lng = ~Longitude, max = 0.01, radius = 12)
  }
  # map %>%
  #   addPolygons(stroke = FALSE, smoothFactor = 0.2, fillOpacity = 1,
  #               color = ~qpal(gdp_md_est))
  
  # var map = L.map('map', {
  #   zoomDelta: 0.25,
  #   zoomSnap: 0
  # });
  
  zMap_border
}

# saveWidget(map1, file="map1.html", selfcontained=FALSE)
# saveWidget(map2, file="map2.html", selfcontained=FALSE)
# saveWidget(map3, file="map3.html", selfcontained=FALSE)
