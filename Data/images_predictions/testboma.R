#CRS for Images
#coord. ref. : WGS 84 / UTM zone 36S (EPSG:32736)

setwd("C:/Users/evaen/Downloads/images_predictions/images_predictions")
#install.packages('tiff')
#install.packages('shapefiles')
#install.packages('xml2')
#install.packages('XML')
#install.packages('tibble')
#install.packages('st')
library(sf)
library(sp)
library(st)
library(xml2)
library(XML)
library(shapefiles)
library(tiff)
library(terra)
library(tibble)
data<-read_xml("23AUG28081217_M2AS_P001_mara_north_conservancy/23AUG28081217-M2AS-508116995060_01_P001--mara_north_conservancy--20230828_081217.tif.aux.xml")
data
xml_structure(data)
data_xml<-xmlParse(data)
data
geo<-xml_text(xml_find_all(data,".//GeoTransform"))
geo
class(geo)
points<-as.numeric(unlist(strsplit(geo,",")))
class(points)
points
#Label the geotransform points
ULx<-points[1] #upper left pixel x-coord
pixelWidth<-points[2] #w-e pixel resolution/pixel width
rowRot<-points[3] #row rotation (typically zero)
ULy<-points[4] #upper left y-coord
colRot<-points[5] #column rotation
pixelHeight<-points[6] #n-s pixel resolution/pixel height

######################################

r<-rast("C:/Users/evaen/Downloads/images_predictions/images_predictions/23AUG28081217_M2AS_P001_mara_north_conservancy/23AUG28081217-M2AS-508116995060_01_P001--mara_north_conservancy--20230828_081217.tif")
r
str(r)
names(r)
ID<-names(r)[1]
ID
?SpatRaster
extent<-ext(r,cells=NULL)
extent
class(extent)
?xmin
d<-xmin(r)
class(d)
a<-as.polygons(extent)
class(a)
crs(a)<-"EPSG:32736"
crs(a)
writeVector(a,filename=paste0("testFootprintPolygon.shp"),overwrite=TRUE)



r2<-rast("C:/Users/evaen/Downloads/images_predictions/images_predictions/23AUG28081218_M2AS_P002_mara_north_conservancy/23AUG28081218-M2AS-508116995060_01_P002--mara_north_conservancy--20230828_081218.tif")
ID<-names(r2)[1]
extent2<-ext(r2,cells=NULL)
extent2
a2<-as.polygons(extent2)
crs(a2)<-"EPSG:32736"
writeVector(a2,filename=paste0("testFootprintPolygon",ID,".shp"),overwrite=TRUE)


vect(extent,extent2,crs="EPSG:32736")
?vect
?merge

lapply()





?SpatVectorCollection

#################

geoVal<-as.numeric(geo)
geoVal
geotib<-tibble(geo)
geotib
geo_sf<-st_as_sf(geo,coords=c("X","Y"),crs=32736)

###########################
rast(image,subds=0, lyrs=NULL, drivers=NULL, opts=NULL,win=NULL, snap="near", vsi=FALSE, raw=FALSE)
readTIFF("23AUG28081217_M2AS_P001_mara_north_conservancy/23AUG28081217_M2AS_P001_mara_north_conservancy.tif")
str(image)

###################
image<-'23AUG28081217_M2AS_P001_mara_north_conservancy/23AUG28081217_M2AS_P001_mara_north_conservancy.tif'
class(image)
convert<-readTIFF(image)
test1<-as.polygonsimagetest1<-as.polygons(image)

polygon<-read.shp("TestFootprint-10JUL16081856-M2AS-508116994010_01_P004--mara_north_conservancy--20100716_081856.shp")
plot(polygon)

#Get extent of tif file
str(image)
extent(image)

data<-read_xml("23AUG28081217_M2AS_P001_mara_north_conservancy/23AUG28081217-M2AS-508116995060_01_P001--mara_north_conservancy--20230828_081217.tif.aux.xml")
