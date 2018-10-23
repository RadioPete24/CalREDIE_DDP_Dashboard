# ***Not working yet ***
# ***The purpose of this program is to automate an update of census information and prepare the data for dashboard use***

source(census_install_pkg)
  
require()

acs.lookup

census_info_path <- "https://api.census.gov/data/2010/sf1?key=<>&for=state:06"

#saving census data file to crData
# https://factfinder.census.gov/faces/tableservices/jsf/pages/productview.xhtml?src=bkmk
#For zipcodes
#url <- "http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_zcta510_500k.zip"
# For zips: subdat<-dat[substring(dat$GEOID10, 1, 2) == "36",]

census_info <-  read.csv(file.path("G:", "DCDC ALL","CalREDIE","Peter","Code","CalREDIE_DDP_dashboard","crData","PEP_2017_PEPANNRES_with_ann.csv"), stringsAsFactors = FALSE)

census_info <- PEP_2017_PEPANNRES_with_ann[-1,]
# census_info$GEO.display.label <- tolower(census_info$GEO.display.label)

census_info$GEO.display.label <- gsub(", California", "", census_info$GEO.display.label)
census_info$GEO.display.label <- gsub(" County", "", census_info$GEO.display.label)
census_info$GEO.display.label <- gsub(" City", "", census_info$GEO.display.label)
census_info$GEO.display.label <- gsub(" city", "", census_info$GEO.display.label)
census_info$GEO.display.label <- gsub(" town", "", census_info$GEO.display.label)

#Will check which is more efficient
#census_info[,4:ncol(census_info)] <- census_info[,4:ncol(census_info)] %>% mutate_all(as.character)
census_info[,4:ncol(census_info)] <- lapply(census_info[,4:ncol(census_info)], as.character)
census_info <- as.data.table(census_info)


#Shapefile data for county information
# tract <- readOGR(dsn=".", layer = "cb_2017_us_county_20m.shp")
tract <- readOGR(dsn = file.path("G:","DCDC ALL","CalREDIE","Peter","Code","CalREDIE_DDP_dashboard","crData"), layer = "cb_2017_us_county_20m")
tract@data$GEOID<-as.character(tract@data$GEOID)

#save(tract, file = file.path(getwd(), "crData", "cb_2017_us_county_20m_tract.RData"))

#require(maptools)
#May need to use anothe function in the 'broom' package for later fortifications if function is deprecated.
ggtract<-fortify(tract, region = "GEOID") 
#require(broom)
#ggtract <- tidy(tract)

# join tabular data
ggtract<-left_join(ggtract, tmp, by=c("id")) 

#Converting data.frame to a SpatialPolygonDataFrame: Convert each tract to a Polygon -> Polygons object -> SpatialPolygons object:

#function for creating a Polygons object for 
# input tractname

# polyFunc<-function(groupname, polyDat){
#   poly<-filter(polyDat, id==groupname) %>% 
#     select(long, lat)
#   return(Polygons(list(Polygon(poly)), groupname))
# }
# 
# # tracts <- distinct(ggtract, id, incidence_rt)
# tracts2 <- distinct(ggtract, geography, id, incidence_rt)
# tractname <- tracts2$id
# polygons<-lapply(tractname, function(x) polyFunc(x, polyDat=ggtract)) 
# 
# # lapply(tractname, function(x) x@dat)
# sp.polygon<-SpatialPolygons(unlist(polygons))
# df.polygon<-SpatialPolygonsDataFrame(sp.polygon, 
#                                      data=data.frame(row.names=tractname, tracts2))
# df.polygon <- df.polygon[order(df.polygon$incidence_rt),]

#from df.polygon, counties can now be filled in with addPolygon in leaflet...

#for ggplot...

# test %>% addPolygons(data=mapCounties, fillColor = ~pal(test2$incidence_rt), stroke = FALSE)




# #hist2_df <- as.data.frame(table(tmp_df$LHJ, factor(tmp_df$RStatus))) -- Pulled from whatever data is used...
# hist2_df <- as.data.table(hist2_df) #From countyPlot
# hist2_df[hist2_df$Var1=="Berkeley",]$Var1 <- "Alameda"
# hist2_df[hist2_df$Var1=="Long Beach",]$Var1 <- "Los Angeles"
# hist2_df <- setDT(hist2_df)[, .(Freq=sum(Freq)), by = .(Var1,Var2)]
# 
# census_info <- left_join(hist2_df, census_info, by = c("Var1"="GEO.display.label"))

# #Measures
# census_info$incidence_rt <- (as.numeric(census_info$Freq)*100000)/as.numeric(census_info$respop72017)
# census_info$st_err <- as.numeric(census_info$incidence_rt)/(as.numeric(census_info$respop72017)^.5)
# census_info$rel_sterr <- (census_info$st_err*100)/census_info$incidence_rt
# 
# ##Need to work on developing heat map for multiple RStatus conditons##
# 
# census_info <- census_info %>% rename(id = GEO.id2, geography = Var1, total = respop72017, cases = Freq)

#Transformed from standard data and local data
# tmp <- mutate(census_info[census_info$Var2=="Confirmed",]
#                 , id = as.character(id)
#                 , geography = as.character(geography)
#                 , total = as.numeric(total)
#                 , cases = as.numeric(cases)
#                 , incidence = incidence_rt)
