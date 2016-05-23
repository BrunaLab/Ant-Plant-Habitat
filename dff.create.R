#' IPMpack requires a dataframe called 'dff'. The function "dff.create" creates that dataframe
#' 
dff.create <- function(x) {
  
  # Begin by selecting data for each census-censuys transition
  
  dff.12 <- subset(x, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                 domatia.1,domatia.2, surv12, ant.1, ant.2))
  dff.23<- subset(x, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                domatia.2,domatia.3, surv23, ant.2, ant.3))
  dff.34<- subset(x, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                domatia.3,domatia.4, surv34, ant.3, ant.4))
  dff.45<- subset(x, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                domatia.4,domatia.5, surv45, ant.4, ant.5))
  dff.56<- subset(x, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                domatia.5,domatia.6, surv56, ant.5, ant.6))
  dff.67<- subset(x, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                domatia.6,domatia.7, surv67, ant.6, ant.7))
  dff.7<- subset(x, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                               domatia.7, domatia.7, surv67, ant.7, ant.1))
  
  #it was easiest to create frames that were matching sizes. Because census 7 is the last one, there is no 7-8 transition to record
  #As placeholders, I inlcuded a second domatia.7, surv67, and ant.1. All of these need to be replaced with NA
  dff.7[,6:7]<-NA
  dff.7[,"ant.1"]<-NA
  
  
  #Adds a column noting the different transition intervals. Note that because 7 is the last census, that gets "NA" placed into it.
  dff.12[,"interval"]<-1
  dff.23[,"interval"]<-2
  dff.34[,"interval"]<-3
  dff.45[,"interval"]<-4
  dff.56[,"interval"]<-5
  dff.67[,"interval"]<-6
  dff.7[,"interval"]<-NA
  
  
  #this adds the most "coarse" level data on reproduction - did a plant flower or fruit: yes (1) or no (0)?
  #Recall we only have these data for surveys 4-6
  dff.45[,"rep"]<-x[,"rep4"]
  dff.56[,"rep"]<-x[,"rep5"]
  dff.67[,"rep"]<-x[,"rep6"]
  
  #this adds slightly more detail = how many fruit/flowers did a plant have, if it reproduced?
  dff.45[,"rep2"]<-x[,"fruitsflowers.4"]
  dff.56[,"rep2"]<-x[,"fruitsflowers.5"]
  dff.67[,"rep2"]<-x[,"fruitsflowers.6"]
  
  #These frames are the same size. Bind them up!
  names(dff.12)<-names(dff.23)<-names(dff.34)<-names(dff.7)
  bound<-rbind(dff.12,dff.23,dff.34, dff.7)
  
  #These frames are the same size, so binfd them up too!
  names(dff.45)<-names(dff.56)<-names(dff.67)
  bound2<-rbind(dff.45, dff.56,dff.67)
  
  # add columns with NA for rep and rep2 to intervals 12, 23, 34, and 7 (even though )recall - didn't survey in these years
  bound[,"rep"]<-NA
  bound[,"rep2"]<-NA
  
  #now the different dataframes are the same size and can bind them all up.
  names(bound)<-names(bound2)
  dff<-rbind(bound,bound2)
  
  #rename your columns in your new bound dataframe
  names(dff)[names(dff)=="domatia.6"] <- "size"
  names(dff)[names(dff)=="domatia.7"] <- "sizeNext"
  names(dff)[names(dff)=="surv67"] <- "surv"
  names(dff)[names(dff)=="rep"] <- "fec0"
  names(dff)[names(dff)=="ant.6"] <- "ant"
  names(dff)[names(dff)=="ant.7"] <- "antNext"
  names(dff)[names(dff)=="rep2"] <- "fec1"
  
  
  dff<-dff[!is.na(dff[,8]),]
  
  #Choose your covariates and rename 2 columns - "ant" and "antNext" columns  
  #to match what is needed by IPMpack
  dff <- subset(dff, select = c(size, sizeNext,surv, ant, antNext,fec0, fec1))
  
  names(dff)[names(dff)=="ant"] <- "covariate"
  names(dff)[names(dff)=="antNext"] <- "covariateNext"
  
  #remove all rows hat don't have a measurement in  size or sizeNext. 
  #This is simply housekeeping resulting from conversion from wide to long
  dff<-dff[complete.cases(dff[,1] | dff[,2]),]
  
  
  #log-transorm "size" and "sizeNext" to ensure more normal residuals 
  #Suggestion: Rob Salguero-Gomez
  dff$size=log(dff$size+1)  
  dff$sizeNext=log(dff$sizeNext+1)
  
  #the final dff as needed by IPM pack
  dff
  
}
