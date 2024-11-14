# STEP ONE: GENERATE IMAGE FOOTPRINTS

## Generating image footprints as polygons of .tif extent

#Set wd
setwd("C:/Users/evaen/OneDrive/Documents/GitHub/BomaProject/Data/images_predictions")
#Open required libraries
library(tidyverse)
library(terra)
library(sf)
#install.packages("geojsonf")
library(geojsonsf)

#Use for loop to work through all images (.tif files)
#Set up sub-directories to loop through
dirs<-list.dirs(full.names = TRUE, recursive = FALSE) #List of sub-directories
dirs #Check 
dirs<-dirs[-length(dirs)]#Remove Boma Footprints sub-directory


## Save identifying information for footprints from predictions.geojson files

#Extract dates for each polygon shapefile from corresponding .geojson file
metaDF<-data.frame(name=character(),date=character(),conservancy=character(),
                   stringsAsFactors=FALSE) #Create empty dataframe to populate
gjsn<-list.files(dirs,pattern='\\.geojson$',full.names=TRUE) #Specify files
gjsn
#Create for loop to iterate through sub-directories
for(i in 1:length(gjsn)){
  idInfo<-geojson_sf(gjsn[i]) #Read .geojson
  #Extract desired information
  #Select data in row 1 of i (identical across rows)
  name<-idInfo$image[1] #To match to footprint polygons
  date<-idInfo$date[1] #To filter by date
  conservancy<-idInfo$conservancy[1] #Option: to filter by conservancy 
  #Save to dataframe for all images
  metaDF<-rbind(metaDF,data.frame(name=name,date=date,conservancy=conservancy,
                                  stringsAsFactors=FALSE))
}
#Check
print(metaDF)
#Check metaDF for NAs
na<-which(is.na(metaDF))
na #integer(0)
#To clear DF if issues
#rm(metaDF)

## Create polygon footprints from satellite image .tif files

#Specify the images: .tif files
images<-list.files(dirs,pattern='\\.tif$',full.names=TRUE)
images

#Set folder for results
output<-"C:/Users/evaen/OneDrive/Documents/GitHub/BomaProject/Data/images_predictions/Boma Footprints"
#If directory not detected, the code below creates the output folder
#Only required for first time to successfully run code
#If output directory doesn't exist
if (!dir.exists(output)) {
  dir.create(output, recursive = TRUE)
}
# Check if issue
#if (dir.exists(output)) {
# print("Output directory does exist")
#}

#Create list object to save output footprints to in R workspace
footprintList<-list()


#####NOT CLIPPED TO MMNR/TANZANIA SOUTHERN EXTENT
#Create polygon shapefiles of the .tif files for footprint
#Use for loop to work through all sub-directories (.tif files)
for(i in 1:length(images)){
  ID<-basename(images[i]) #Assign unique identifier with date and conservancy
  r<-rast(images[i]) #Convert .tif to SpatRaster
  ext<-ext((r),cells=NULL)
  pg<-as.polygons(ext) #Polygon created: now of class SpatVector in terra
  crs(pg)<-"EPSG:32736" #Set crs
  class(pg)
  
  #Annotate polygon attribute table with info extracted from .geojson file
  metadata<-metaDF[metaDF$name==ID,] #Match ID to corresponding row from newDF
  if(nrow(metadata)>0){ #Create condition to identify if metadata missing
    #Specify data from newDF
    pg$name<-metadata$name
    pg$date<-metadata$date
    pg$conservancy<-metadata$conservancy
  } else{
    warning("No matching metadata for",ID) #This should not arise
  }
  
  #Save polygon to list in R
  footprintList[[ID]]<-pg
  
  #Set path for results to be written to
  result<-file.path(output, paste0("ImageFootprint_", ID, "_.shp"))
  # Print the result path to check
  print(result)
  #Save shapefile to specified folder
  writeVector(pg, filename = result, overwrite = TRUE)
}

#########################################

#STEP TWO: CALCULATE TOTAL FOOTPRINT

#Combining the .shp files of individual images

#Select all the .shp files from the output folder
shpFiles<-list.files(output,pattern='\\.shp$',full.names=TRUE)
shpList<-lapply(shpFiles,read_sf) #Read shapefiles as a list
#Create sf dataframe
sfdf<-shpFiles |>
  map(st_read) |>
  bind_rows()
#Check/Explore df
class(sfdf)
print(sfdf)
class(sfdf$name) #character
class(sfdf$date) #character
class(sfdf$conservanc) #character
class(sfdf$geometry) #sfc_POLYGON sfc
#If rerunning code, ensure obs = number of input images
#sfdf<-sfdf[1:140,] 

#Adding additional info into sf dataframe
#Calculate area of each footprint
sfdf$area<-as.numeric(st_area(sfdf))
sfdf$area<-(sfdf$area)/1000000 #Convert into square km


#Export dataframe as one polygon: .shp file
output<-"C:/Users/evaen/OneDrive/Documents/GitHub/BomaProject/Data/images_predictions/Boma Footprints/Total Footprint"
#If output directory doesn't exist
if (!dir.exists(output)) {
  dir.create(output, recursive = TRUE)
}
totalFP<-file.path(output,paste0("TotalImageFootprint.shp"))
#Merge all footprint geometry features
sfdf_union<-st_union(sfdf)
sfdf_union
#Save total footprint shapefile
st_write(sfdf_union,totalFP,append=FALSE) #Append=FALSE overwrites any prexisting version


#########################################

#STEP THREE: CALCULATE % OVERLAP AMONG ALL POSSIBLE PAIRS OF POLYGONS


#Find all possible intersections
interPairs<-st_intersects(sfdf) #Creates sgbp list
print(interPairs) #Check
rm(overlapDF)
#Create empty dataframe for saving % overlap results
overlapDF<-data.frame(name=character(),parentID=numeric(),date=character(),
                      conservancy=character(),
                      n.overlaps=numeric(),childID=numeric(),
                      pctOverlap=numeric(),
                      stringsAsFactors=FALSE)

for(i in 1:nrow(sfdf)){
  #Data for testing polygon as parent
  name<-sfdf[i,"name"]
  parentID<-i
  date<-sfdf[i,"date"]
  conservancy<-sfdf[i,"conservanc"]
  parent<-sfdf[i,"geometry"]
  n.overlaps<-length(interPairs[[i]])
  
  #Data for comparing potential child images
  for(j in interPairs[[i]]){
    if(i==j)next #Skip if polygon is the same
    #Child metadata
    childID<-j
    child<-sfdf[j,"geometry"]
    childCover<-st_area(child)
    overlap<-st_intersection(parent,child)
    overlapCover<-st_area(overlap)
    pctOverlap<-(overlapCover/childCover)*100
    
    #Write results to overlapDF
    overlapDF<-rbind(overlapDF,data.frame(name=name,parentID=parentID,date=date,
                                          conservancy=conservancy,
                                          n.overlaps=n.overlaps,
                                          childID=childID,
                                          pctOverlap=pctOverlap,
                                          stringsAsFactors=FALSE))
    
    
  }
  
}

#Save overlapDF to personal computer
output<-"C:/Users/evaen/OneDrive/Documents/GitHub/BomaProject/Data/images_predictions/Boma Footprints/Overlap Data"
pctOverlapDF<-file.path(output,paste0("FootprintOverlap.csv"))
st_write(overlapDF,pctOverlapDF,append=FALSE)
