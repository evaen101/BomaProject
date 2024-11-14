# STEP ONE: GENERATE IMAGE FOOTPRINTS

## Generating image footprints as polygons of .tif extent

#Use for loop to work through all images (.tif files)
#Set up loop for sub-folders
images<-list.files(full.names = T, recursive = F) #List of subfolders in directory
images<-images[-141] #Remove R script saved to folder
images #Check script removed
#Create for loop
for(i in 1:length(images)){
  ID<-images[i] #Assign unique identifier with date and conservancy
  in.list<-list.files(path=images[i],recursive = T,pattern = (endsWith(".tif")))
  tif<-in.list[1]
  r<-rast(tif) #Convert .tif to SpatRaster
  ext<-ext((r),cells=NULL)
  pg<-as.polygons(ext)
  crs(pg)<-"EPSG:32736"
  writeVector(pg,filename=paste0("ImageFootprint_",ID,"_.shp"),overwrite=T)
  
}

?list.files
#C:\Users\evaen\Downloads\images_predictions
r2<-rast("C:/Users/evaen/Downloads/images_predictions/images_predictions/23AUG28081218_M2AS_P002_mara_north_conservancy/23AUG28081218-M2AS-508116995060_01_P002--mara_north_conservancy--20230828_081218.tif")
ID<-names(r2)[1]
extent2<-ext(r2,cells=NULL)
extent2
a2<-as.polygons(extent2)
crs(a2)<-"EPSG:32736"
writeVector(a2,filename=paste0("testFootprintPolygon",ID,".shp"),overwrite=TRUE)

