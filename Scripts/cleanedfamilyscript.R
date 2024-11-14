### BOMA FOOTPRINTS: SORTING OVERLAP
library(dplyr)
#Set new working directory
setwd("C:/Users/evaen/OneDrive/Documents/GitHub/BomaProject/Data/images_predictions/Boma Footprints")
path<-file.path("C:/Users/evaen/OneDrive/GitHub/BomaProject/Data/images_predictions/Boma Footprints/Overlap Data")
data<-read.csv("/Overlap Data/FootprintOverlap.csv")
head(data)
class(data$date.date)
rm(sumOverlapDF)
sumOverlapDF<-data.frame(name=character(),date=numeric(),parentID=character(),
                         conservancy=character(),n.overlaps=numeric(),
                         totalPctOverlap=numeric(),stringsAsFactors = FALSE)

#Use for loop
for(i in unique(data$parentID)){ 
  subset<-filter(data,parentID==i) #Specify to use all rows with same parentID
  #Metadata can be taken from first row in subset since all identical
  name<-subset$name.name[1] #Name of reference file
  date<-subset$date.date[1] #Date satellite image taken
  conservancy<-subset$conservancy.conservanc[1] #Conservancy in image
  parentID<-subset$parentID[1] #Unique ID for each polygon
  N.overlaps<-as.numeric(subset$n.overlaps[1]) #Number of overlaps for reference
  #Sum together all value of PctOverlap for same parentID value for total
  totalPctOverlap<-sum(subset$pctOverlap)
  #Write to new dataframe
  sumOverlapDF<-rbind(sumOverlapDF,data.frame(name=name,date=date,parentID=parentID,
                                              conservancy=conservancy,n.overlaps=N.overlaps,
                                              totalPctOverlap=totalPctOverlap,stringsAsFactors=FALSE))
}

#Separate into distinct conservancies
#Use this information to identify suitable parent images for each conservancy
#Oloolaimutia
olo<-sumOverlapDF[sumOverlapDF$conservancy=="oloolaimutia",]
olo<-olo[order(-olo$totalPctOverlap),]
nrow(olo) #17 footprints
#Pardamat
pard<-sumOverlapDF[sumOverlapDF$conservancy=="pardamat_community_conservation_area",]
nrow(pard) #56 footprints
pard<-pard[order(-pard$totalPctOverlap),]
#Mara North
mara<-sumOverlapDF[sumOverlapDF$conservancy=="mara_north_conservancy",]
nrow(mara) #67 footprints
mara<-mara[order(-mara$totalPctOverlap),]


## CREATE GROUPS OF PARENTS TO CHILDREN

rm(parents)
#Create dataframe of parent footprints as manually identified
parents<-data[data$parentID %in% c(1,3,7,8,10,12,13,22,36),]

#Create dataframe removing Oloolaimutia since no orphans/parentID = 22
olo<-parents[parents$conservancy.conservanc=="oloolaimutia",] #No orphans
all<-parents[!parents$conservancy.conservanc=="oloolaimutia",]
#Remove any instances of parent images as children
all<-all[!all$childID %in% c(1,3,7,8,10,12,13,22,36),]

min<-filter(all,all$pctOverlap<60)
maj<-filter(all,all$pctOverlap>=60)
orph<-setdiff(unique(min$childID),unique(maj$childID))
length(orph)
orphans<-filter(all, childID %in% orph)

##Create index of orphan footprint data to refer to later
rm(orphanIndex)
orphanIndex<-data.frame(orphanID=character(),conservancy=character(),parent=character(),
                        overlap=numeric(),stringsAsFactors = FALSE)
for(i in unique(orphans$childID)){ #Iterate through IDs of orphans
  orphanID<-i #take original childD to be called ID
  conservancy<-unique(data$conservancy.conservanc[data$parentID == i])[1]#home conservancy of i from data
  #Use for loop to iterate over all parent rows where i is child
  parentTochild<-orphans[orphans$childID==i,] #Filter i in orphan object
  for(j in 1:nrow(parentTochild)){
    parent<-parentTochild$parentID[j] #for each iteration, take the parentID in row
    overlap<-parentTochild$pctOverlap[j] #take pctOverlap from the row
    
    orphanIndex<-rbind(orphanIndex,data.frame(orphanID=orphanID,conservancy=conservancy,parent=parent,
                                              overlap=overlap,stringsAsFactors = FALSE))
  }
}
#Orphans are images where no parent identified at present provides min 60% cover

##Assign children to parents based on best pctCover
assignedPairs<-maj %>% group_by(childID) %>% slice_max(pctOverlap)
print(assignedPairs)
##Identify any remaining childIDs where identical pctOverlap values exist
conflicts<-assignedPairs %>% group_by(childID) %>% filter(n()>1)
#Remove these duplicate childID rows from assignedPairs
assignedPairs<-assignedPairs %>% anti_join(conflicts,by= c("childID", "parentID"))
#All duplicates have 100% overlap with parents - filter for oldest parent
conflictPairs<-conflicts %>% group_by(childID) %>% slice_min(date.date)
assignedDF<-rbind(assignedPairs,conflictPairs)
#Double check for any duplicates
dupCheck<-assignedDF %>% group_by(childID) %>% filter(n()>1) #0
assignedDF<-assignedDF[order(assignedDF$parentID,assignedDF$childID),]
#Now create a directory of families (parent to string of childIDs)
rm(familyIndex)
familyIndex<-data.frame(ID=character(),conservancy=character(),
                        groupedChildren=integer(),stringsAsFactors = FALSE)
for(i in unique(assignedDF$parentID)){
  subset<-filter(assignedDF,parentID==i)
  ID<-subset$parentID[1]
  conservancy<-subset$conservancy.conservanc[1]
  groupedChildren<-toString(subset$childID)
  familyIndex<-rbind(familyIndex,data.frame(ID=ID,conservancy=conservancy,
                                            groupedChildren=groupedChildren,
                                            stringsAsFactors = FALSE))
}
#Add in Oloolaimutia 
assignedDF<-rbind(assignedDF,olo)
assignedDF<-assignedDF[order(assignedDF$parentID,assignedDF$childID),]
#Now create a directory of families (parent to string of childIDs)
rm(familyIndex)
familyIndex<-data.frame(ID=character(),conservancy=character(),
                        groupedChildren=integer(),stringsAsFactors = FALSE)
for(i in unique(assignedDF$parentID)){
  subset<-filter(assignedDF,parentID==i)
  ID<-subset$parentID[1]
  conservancy<-subset$conservancy.conservanc[1]
  groupedChildren<-toString(subset$childID)
  familyIndex<-rbind(familyIndex,data.frame(ID=ID,conservancy=conservancy,
                                            groupedChildren=groupedChildren,
                                            stringsAsFactors = FALSE))
}


