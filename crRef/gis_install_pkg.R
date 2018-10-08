pkg_list <- c("sp"
              , "rgdal"
              , "rgeos"
              , "raster"
              , "rworldmap"
              , "dismo"
              , "data.table"
              , "htmlwidgets"
              , "maptools"
              , "KernSmooth"
              , "rangeMapper"
              , "dplyr"
              , "fs"
              , "leaflet"
              , "lubridate"
              #, "tigris"
              #, "acs"
              , "shiny"
              , "shinythemes"
              , "SDMTools"
              , "ggplot2"
              , "xlsx"
              , "XLConnect"
              , "foreign"
              , "openxlsx"
              , "leaflet.extras"
              , "googleVis"
              , "scales"
              , "ggmap"
              , "RColorBrewer"
              , "gridExtra"
              , "maps"
              , "zipcode"
)
pkg_list_new <- pkg_list[!(pkg_list %in% installed.packages()[,"Package"])]
if(length(pkg_list_new)>0) {
  install.packages(pkg_list_new)
}

lapply(pkg_list, require, character.only = TRUE)

#Obtaining the shapefiles to be used in the CRmap

# require(leafletR)