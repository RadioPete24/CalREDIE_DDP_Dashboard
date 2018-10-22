# =====================================================================================
# "Global.R" file                                                                     |
#            designate folder locations                                               |
#            load packages                                                            |
#            read in shape and data files                                             |
#            creates sub-site for "San Joaquin Public Health Consortium"              |
#            load functions                                                           |
#            read key "info" files                                                    |
#            creates vectors and contants used for Shiny app                          |
#                                                                                     |   
# =====================================================================================

#-- Set Locations Etc----------------------------------------------------------------------

# myDrive  <- "E:"  
# myPlace  <- paste0(myDrive,"/0.CBD/myCBD")  
# myLoc1   <- myPlace                             # used by some of myFunctions  --- no longer?                     

myCRLoc <- getwd()   # for Shiny.io
myGIS_install <- paste0(myCRLoc, "/crRef/")
myFunctions <- paste0(myCRLoc, "/crFunctions/")
myData <- paste0(myCRLoc, "/crData/")

#-- Load Packages --------------------------------------------------------------------------

source(file.path(myGIS_install, "gis_install_pkg.R"))
#library(shinythemes)
# library(shinymaterial)

#-- Run Application ------------------------------------------------------------------------
# myFunction_list <- c("tsvFile.R", "CRmap_00.R", "dateRange.R")
# sapply(file.path(paste0(myFunctions, myFunction_list)), FUN = function(x){ifelse (file.exists(x), source(x), warning("file does not exists"))}, .GlobalEnv)

source(file.path(myFunctions, "tsvFile.R"))
source(file.path(myFunctions, "CRmap_00.R")) # Zoom Map
# source(file.path(myFunctions, "CRmap_01.R")) # Static Map
#source(file.path(myFunctions, "dateRange.R"))

# USE consistent map projection system throughout all app code !
proj1 <- "+proj=aea +lat_1=34 +lat_2=40.5 +lat_0=0 +lon_0=-120 +x_0=0 +y_0=-4000000 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"
proj2 <- "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"

# USE THIS 
# new appraoch from Zev for simplifying path names, etc 
# don't have to keep tract of leading or following "/" !
# check to make sure this is supported on CDPH Shiny Server?
# library(fs)

# ICD coding (not used for this specific application)
# gbdMap0    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.xlsx"), sheet="main"))  # OLD
# gbdMap0    <- as.data.frame(read_excel( path(myPlace,"myInfo////gbd.ICD.Map.xlsx/"), sheet="main"))    # NEW, with extra "/" as examples

# --- CR Key Inputs --------------------------------------------------------------------------------------

#Shapefile data for county, community, census...
# shape_County       <- readOGR(paste0(myPlace,"/myData/shape_County.shp")) 
# shape_Comm         <-readOGR(paste0(myPlace,"/myData/shape_Comm.shp"))   # Read Shape Files
# shape_Tract        <- readOGR(paste0(myPlace,"/myData/shape_Tract.shp"))  

#simple feature objects
# shape_Tract        <- read_rds(path(myPlace,"/myData/shape_Tract.rds"))
# shape_Comm         <- read_rds(path(myPlace,"/myData/shape_Comm.rds"))
# shape_County       <- read_rds(path(myPlace,"/myData/shape_County.rds"))

# shape_Tract$GEOID  <- as.character(shape_Tract$GEOID)    
# shape_Tract$county <- as.character(shape_Tract$county)   

#not quite...
#library(sf)
#shape_Tract <-  st_read(paste0(myPlace,"/myData/shape_Tract.shp"),stringsAsFactors = FALSE)  

# whichData  <-  "fake"

#Data for disease specific (not used for this application)
# load(path(myPlace,"/myData/",whichData,"datTract.R"))
# load(path(myPlace,"/myData/",whichData,"datComm.R"))
# load(path(myPlace,"/myData/",whichData,"datCounty.R"))
# load(path(myPlace,"/myData/",whichData,"datState.R"))

#Load Census Population data (originally formatted from updateCRdata.R)
load(file.path(myData, "PEP_2017_census_info.RData")) #Population Count Data
load(file.path(myData, "cb_2017_us_county_20m_tract.Rdata")) #County tract data
#Increase program size for run
# options(shiny.maxRequestSize=10*1024^2)

#select shapefile in path to shapefile data
filename <- gsub(".shp", "", list.files(myData, pattern = ".shp", full.names = FALSE))

#Import shapefile data for county
datCounty <- readOGR(datPath, "tl_2017_us_county")
#filtering California shapefile data from general data
subdatCounty <- datCounty[substring(datCounty$GEOID, 1, 2) == "06",]
#Import shapefile data for city
subdatCity <- readOGR(datPath, "Cities2015")

# --- Data for ggplot mapping --------------------------------------------------------------------------

mTitle       <- "CalREDIE DDP Data Dashboard 0.1.0"
data(county.fips)
# data(zip_codes)
data(us.cities)
#Get California County Data for static map information
dat_county <- county.fips[which(substring(county.fips$fips, 1, 1)=='6'),]

dat_county <- mutate(dat_county
                     , fips = substring(dat_county$fips, 2)
                     , polyname = substring(dat_county$polyname, 12))

#-- Load Info Files and Functions ------------------------------------------------------------------------

# gbdMap0    <- as.data.frame(read_excel(paste0(myPlace,"/myInfo/gbd.ICD.Map.xlsx"), sheet="main"))

# source(paste0(myCRLoc,"/crFunctions/cbdCutPoint0.R"))
# 
# source(paste0(myCRLoc,"/crFunctions/cbdMap0.R"))
# source(paste0(myCRLoc,"/crFunctions/cbdMapX.R"))
# 
# source(paste0(myCRLoc,"/crFunctions/cbdMap0Leaflet.R"))
# source(paste0(myCRLoc,"/crFunctions/rankCausesSelectGeo.R")) 
# source(paste0(myCRLoc,"/crFunctions/rankCausesSelectGeoTable.R"))
# source(paste0(myCRLoc,"/crFunctions/trend.R"))
# source(paste0(myCRLoc,"/crFunctions/rankGeosSelectCause.R"))
# 
# 
# source(paste0(myCRLoc,"/crFunctions/makeSES.R"))

# --- Shiny Stuff and Constants ---------------------------------------------------------------------------

#lMeasures <- c("YLL","m.YLL","YLLper","Ndeaths","cDeathRate","aRate","med.age","SMR")
lMeasures <- c("YLL","m.YLL","YLLper","Ndeaths","cDeathRate","aRate",          "SMR")

lMeasuresC <- c("Years of Life Lost (YLL)",
                "Mean YLL","Years of Life Lost per 100,000 population",
                "Number of deaths","Crude Death Rate",
                "Age-Adjusted Death Rate",
                #   "Median Age",
                "Standard Mortality Ratio")

names(lMeasures) <- lMeasuresC


# nC        <- 5
# myColor1  <- rev(brewer.pal(nC,"RdYlBu"))
# 
# yG <- "2011-2015"

#myYear <- 2013
# myLHJ  <- "Colusa"; myLev <- 1; myCause <- 104   # myCause  <- "Diabetes mellitus"

# --- END --------------------------------------------------------------------------------------------------