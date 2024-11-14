
#Combining the .shp files of individual images
str(footprintList)
total<-st_sf(footprintList,crs="EPSG:32736")
total<-st_sf(name)

#Create empty dataframe to merge all footprint info to
mergedDF<-data.frame(name=character(),date=numeric(),conservancy=character(),
                     xmin=numeric(),xmax=numeric(),ymin=numeric(),ymax=numeric(),
                     stringsAsFactors = FALSE)
#Use for loop to extract polygon extents and save to df
for(i in seq_along(footprintList)){
  footprint<-footprintList[[i]]
  extent<-ext(footprint)
  #Extract metadata
  name<-footprint$name[1]
  date<-footprint$date[1]
  conservancy<-footprint$conservancy[1]
  #Individually extract extent data
  xmin<-extent[1]
  xmax<-extent[2]
  ymin<-extent[3]
  ymax<-extent[4]
  #Save to mergedDF
  mergedDF<-rbind(mergedDF,data.frame(name=name,date=date,
                                      conservancy=conservancy,
                                      xmin=xmin,xmax=xmax,
                                      ymin=ymin,ymax=ymax,
                                      stringsAsFactors = FALSE))
}



#Select all the .shp files from the output folder
shpFiles<-list.files(output,pattern='\\.shp$',full.names=TRUE)
shpList<-lapply(shpFiles,read_sf) #Read shapefiles as a list
#shpList retains Geometry column
sfdf<-data.frame(name=character(),date=numeric(),conservancy=character(),
)
#Create sf dataframe by joining all .shp polygons from output path
for(i in 1:seq_along(footprintList)){
  sf<-st_read(footprintList[[i]])
  sfdf<-rbind()
}
sfdf<-do.call(rbind,lapply(footprintSVC,st_read))

#########################################

#STEP THREE: CALCULATE % OVERLAP AMONG AL POSSIBLE PAIRS OF POLYGONS

#Required libraries
library(sf)
library(dplyr)

#Extract footprint shapefiles from local drive
path<-"C:/Users/evaen/OneDrive/Documents/Boma Footprints"
shpFiles<-list.files(path,pattern='\\.shp$',full.names=TRUE)
shpDF<-st_sf(name=character(),date=numeric(),conservancy=character(),
                  geometry=st_sfc(),crs = 32736)
#Create for loop to read through polygons and combine into sf dataframe
for(i in 1:length(shpFiles)){
  sfObj<-st_read(shpFiles[i])
  if(crs(sfObj)!=32736){
    sfObj<-st_transform(sfObj,32736)
  }
  #sfObj<-st_as_sf(read)
  shpDF<-rbind(shpDF,sfObj)
}


#Find all possible intersections
interPairs<-st_intersects(shpDF) #Creates sgbp list
print(interPairs) #Check
#Create empty dataframe for saving % overlap results
overlapDF<-data.frame(name=character(),parentID=numeric(),date=character(),conservancy=character(),
                      n.overlaps=numeric(),childID=numeric(),pctOverlap=numeric(),
                      stringsAsFactors=FALSE)

#rm(overlapDF)

#Create for loop to calculate percent overlap of all possible pairs of polygons

for(i in 1:nrow(sfdf)){
#Generate metadata for new df
name<-sfdf$name[i]
parentID<-(i)
date<-sfdf$date[i]
conservancy<-sfdf$cnsrvnc[i]

#Define area for polygon i
#This will be treated as if parent image
#p<-st_as_sf((shpDF$geometry[i]))
#p
p<-shpDF[1]
c<-shpDF[2]
#p
#class(shpDF$geometry[i])
#parent<-expanse(p,transform=TRUE)
n.overlaps<-length(interPairs[[i]])

#Create for loop to iterate through index of polygons intersecting i
for(j in interPairs[[i]]){
#Skip intersections of itself
if (i==j) next
#New metadata
childID<-(j)
#Define area for polygon j
#These will be treated as if child images
c<-shpDF[1]
#c
#class(c)
#child<-expanse(c,transform=TRUE)

#Calculate area of parent and child overlap
overlap<-st_intersection(p,c)
#Now make parent geometry into area for percentage calculation
#childArea<-as.numeric(st_area(child))
#parentArea<-as.numeric(st_area(parent))
#Calculate percentage of child polygon covered by parent, polygon i
pctOverlap<-(overlap/c)*100 #Make percentage

#Write results to dataframe
overlapDF<-rbind(overlapDF,data.frame(name=name,parentID=parentID,date=date,
                                      conservancy=conservancy,
                                      n.overlaps=n.overlaps,
                                      childID=childID,
                                      pctOverlap=pctOverlap,
                                      stringsAsFactors=FALSE))
}

}


st_covered_by(p,c)
?writeValues
st_boundary(p)

######################################

class(sfObj$geometry)
i=1
?st_read
class(shpDF[[1]])
rm(shpDF)
class(shpFiles[i])
st_read(shpFiles)
st_intersects
?read_sf()
?st_intersection()

i=131
j=118
?st_intersection
#Create for loop to calculate percent overlap of all possible pairs of polygons
#for(i in 1:nrow(sfdf)){
#Generate metadata for new df
name<-sfdf$name[i]
parentID<-(i)
date<-sfdf$date[i]
conservancy<-sfdf$cnsrvnc[i]

#Define area for polygon i
#This will be treated as if parent image
p<-vect(sfdf$geometry[i])
parent<-expanse(p,transform=TRUE)
n.overlaps<-length(interPairs[[i]])

#Create for loop to iterate through index of polygons intersecting i
#for(j in interPairs[[i]]){
#Skip intersections of itself
if (i==j) next
#New metadata
childID<-(j)
#Define area for polygon j
#These will be treated as if child images
c<-vect(sfdf$geometry[j])
child<-expanse(c,transform=TRUE)

#Calculate area of parent and child overlap
overlap<-intersect(p,c)
expanse(overlap)
str(overlap[[1]])
expanse(overlap)
o<-expanse(intersect(parent,child))
plot(o)
plot(overlap[[1]]$ccp)
area(o[1])
#Now make parent geometry into area for percentage calculation
childArea<-as.numeric(st_area(child))
parentArea<-as.numeric(st_area(parent))
#Calculate percentage of child polygon covered by parent, polygon i
pctOverlap<-(overlap/c)*100 #Make percentage

#Write results to dataframe
overlapDF<-rbind(overlapDF,data.frame(name=name,parentID=parentID,date=date,
                                      conservancy=conservancy,
                                      n.overlaps=n.overlaps,
                                      childID=childID,
                                      pctOverlap=pctOverlap,
                                      stringsAsFactors=FALSE))
}

}
