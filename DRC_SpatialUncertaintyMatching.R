#Loading shapefiles
library(maptools)

#Clipping tools
library(rgeos)

#Load in DHS
DHS_file <- "/home/aiddata/Desktop/R_Repo/DRC_DolanBenYishay/Input_Data/CDGE61FL.shp"
DHS <- readShapeSpatial(DHS_file, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#Load in Clipped Settlement Points
GRUMP_file <- "/home/aiddata/Desktop/R_Repo/DRC_DolanBenYishay/Input_Data/GRUMP/glpv_clipped.shp"
GRUMP <- readShapeSpatial(GRUMP_file, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#Load in a boundary for presentation
DRC_Bnd_File <- "/home/aiddata/Desktop/R_Repo/DRC_DolanBenYishay/Input_Data/GADM/COD_adm0.shp"
DRC_Boundary <- readShapePoly(DRC_Bnd_File, proj4string=CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))

#Make sure it all makes sense so far
plot(DRC_Boundary)
points(DHS, col="red")
points(GRUMP, col="blue")

#For each DHS, we want to make our best guess as to the settlement it belongs to
#These are the rules of displacement:
#In order to ensure that respondent confidentiality is maintained, we randomly displace the GPS 
#latitude/longitude positions for all DHS, MIS, and AIS surveys. 
#The displacement is randomly carried out so that:
#  -Urban clusters are displaced up to 2 kilometers. 
#-Rural clusters are displaced up to 5 kilometers, with 1% of the rural clusters displaced up to 10 kilometers.

#To attempt to overcome this, we do a search and identify all possible settlements within 5km of each DHS point.
#Each DHS then gets a % chance (equally split according to the number of hits within the radius) we could identify the true settlement.
#We then select the closest settlement for assignment (currently arbitrary).

#For each DHS point, create a 5KM buffer
#First, we need to project from degrees to meters
#Lambert conformal conic will do the job - we reproject everything to be clean.
GRUMP_LCC <- spTransform(GRUMP, CRS( "+init=epsg:4058" ) ) 
DHS_LCC <- spTransform(DHS, CRS( "+init=epsg:4058" ) ) 
DRC_LCC <- spTransform(DRC_Boundary, CRS( "+init=epsg:4058" ) ) 

#Make sure it all still makes sense...
plot(DRC_LCC)
points(DHS_LCC, col="red")
points(GRUMP_LCC, col="blue")

#Buffer the DHS
DHS_buffer <- gBuffer( DHS_LCC, width=5000, byid=TRUE )
# Add data, and write to shapefile for later use
DHS_buffer <- SpatialPolygonsDataFrame( DHS_buffer, data=DHS_buffer@data )
writePolyShape(DHS_buffer, "/home/aiddata/Desktop/R_Repo/DRC_DolanBenYishay/DHS_buffer.shp")

#Identify the number of settlements that fall within each DHS buffer
DHS_buffer@data["SetCnt"] <- NA
for(shape_bnd in 1:length(DHS_buffer))
{
  #Select this iterations buffer polygon
  it_buf <- DHS_buffer[shape_bnd,]
  
  #Count the number of settlements that fall within it
  it_cnt <- length(GRUMP_LCC[!is.na(over(GRUMP_LCC, as(it_buf, "SpatialPolygons"))),])
  
  #Save it back to the DHS_buffer spatial frame
  DHS_buffer@data["SetCnt"][shape_bnd,] <- it_cnt

}

#See how feasible this is with public data
summary(DHS_buffer@data["SetCnt"])

#Max of 1 indicates we never had 2 settlements match
#However, Mean of .17 indicates only 17% of the DHS points had a match at all using this approach.