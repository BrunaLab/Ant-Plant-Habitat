
lambda1<-function(DATA){

  
  dff.12 <- subset(DATA, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                    domatia.1,domatia.2, surv12, ant.1, ant.2))
  dff.23<- subset(DATA, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                   domatia.2,domatia.3, surv23, ant.2, ant.3))
  dff.34<- subset(DATA, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                   domatia.3,domatia.4, surv34, ant.3, ant.4))
  dff.45<- subset(DATA, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                   domatia.4,domatia.5, surv45, ant.4, ant.5))
  dff.56<- subset(DATA, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                   domatia.5,domatia.6, surv56, ant.5, ant.6))
  dff.67<- subset(DATA, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                   domatia.6,domatia.7, surv67, ant.6, ant.7))
  dff.7<- subset(DATA, select = c(unique.plant.id, plant.species,canopy.cover, topography, 
                                  domatia.7, domatia.7, surv67, ant.7, ant.1))
  dff.7[,6:7]<-NA
  
  dff.12[,"interval"]<-1
  dff.23[,"interval"]<-2
  dff.34[,"interval"]<-3
  dff.45[,"interval"]<-4
  dff.56[,"interval"]<-5
  dff.67[,"interval"]<-6
  dff.7[,"interval"]<-NA
  dff.7[,"ant.1"]<-NA
  
  #this adds the coarsest info we have on reproduction - did a plant fruit, yes (1) or no (0)?
  dff.56[,"rep"]<-DATA[,"rep5"]
  dff.67[,"rep"]<-DATA[,"rep6"]
  dff.7[,"rep"]<-DATA[,"rep7"]
  
  #this adds slightly more detail = how many fruit/flowers did a plant have, if it reproduced?
  dff.56[,"rep2"]<-DATA[,"fruitsflowers.5"]
  dff.67[,"rep2"]<-DATA[,"fruitsflowers.6"]
  dff.7[,"rep2"]<-DATA[,"fruitsflowers.7"]
  
  names(dff.12)<-names(dff.23)<-names(dff.34)<-names(dff.45)
  bound<-rbind(dff.12,dff.23,dff.34, dff.45)
  
  names(dff.7)<-names(dff.56)<-names(dff.67)
  bound2<-rbind(dff.56,dff.67,dff.7)
  
  bound[,"rep"]<-NA
  bound[,"rep2"]<-NA
  
  names(bound)<-names(bound2)
  dff<-rbind(bound,bound2)
  
  #rename your columsn in your new bound frame
  names(dff)[names(dff)=="domatia.6"] <- "size"
  names(dff)[names(dff)=="domatia.7"] <- "sizeNext"
  names(dff)[names(dff)=="surv67"] <- "surv"
  names(dff)[names(dff)=="rep"] <- "fec0"
  names(dff)[names(dff)=="ant.6"] <- "ant"
  names(dff)[names(dff)=="ant.7"] <- "antNext"
  names(dff)[names(dff)=="rep2"] <- "fec1"
  
  #Choose your covariates and rename columns  to match what is needed by IPMpack
  dff <- subset(dff, select = c(size, sizeNext,surv, ant, antNext,fec0, fec1))
  
  names(dff)[names(dff)=="ant"] <- "covariate"
  names(dff)[names(dff)=="antNext"] <- "covariateNext"
  
  #removee all rows hat don't have a measurement in  size or sizeNext. This is simply housekeeping.
  dff<-dff[complete.cases(dff[,1] | dff[,2]),]
  
  #log-transorm size and sizeNext to ensure more normal residuals
  dff$size=log(dff$size+1)  
  dff$sizeNext=log(dff$sizeNext+1)
  
  #head(dff,50)
  #summary(dff)
  
  #this is to add seedlings from 9-ha plot to dff
  #Csdlgs9ha<-read.csv("cremaseedlings.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  #dff<-rbind(Csdlgs9ha, dff)
  #CsdlgsBT<-read.csv("cremaseedlingsBT.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
  #dff<-rbind(CsdlgsBT, dff)
  
  ###These will allow you to up the sample sizes by including specific transitions from all the pooled data.  EG, there arent enough plants that always had C, so we can include transitions CC transitions from plants that had CC at least once
  #dff<-dff[dff$covariate=="C",]
  #dff<-dff[dff$covariateNext != "P" ,]
  #dff<-dff[dff$covariateNext != "none" ,]
  #dff<-dff[dff$covariateNext != "none" ,]
  #dff<-dff[dff$covariate != "none" ,]
  #dff<-dff[dff$covariate != "C+P" ,]
  
  
  
  
  
  ##This is an attempt to estimate something close to a germination rate. it is done at the "whole "whole dataset level", meaning
  ##I simply added totaled the number of seedlings and divided by number of fruits produced.  No variation - could go back and calclulate at the plot level, if interested in an estiamte of variation
  ##However, will vary by habitat type depending on selection of gap or canopy, plateu or lowland.
  #unless otherwise indicated, use the constant below, which is the value for all habitats and s[ecies of ant occupant combined
  #total fruits produced sum(dff$fec1, na.rm=TRUE)
  #totalfruits<-sum(dff$fec1, na.rm=TRUE)
  #total seedlings produced
  #sdlgs<-nrow(dff[is.na(dff$size),])
  #seedlings per fruit
  #sdlgs_per_fruit<-sdlgs/totalfruits
  #fec2<-sdlgs_per_fruit
  fec2=0.04618
  
  ##########
  #IPMpack
  ##########
  
  #plot(dff$size,dff$sizeNext)
  ###Note!! You must change the functions for explanatory variables if you wish to include size^2 or Size^3 in the polynomial regression
  
  gr1 <- makeGrowthObj(dff, Formula = sizeNext~size)
  #gr1 <- makeGrowthObj(dff, Formula = sizeNext~size+size2)
  sv1 <- makeSurvObj(dff, Formula = surv~size+size2)
  #sv1 <- makeSurvObj(dff, Formula = surv~size)
  
  #gr1
  #sv1
  
  par(mfrow = c(1,2), bty ="l", pty = "m")
  #p1 <- picGrow(dff, gr1)
  #p2<-picSurv(dff, sv1, ncuts = 50)
  Pmatrix <- makeIPMPmatrix(nBigMatrix = 50 , minSize = -2, maxSize = 6, growObj = gr1, survObj = sv1, correction="constant")
  #diagnosticsPmatrix(Pmatrix, growObj = makeGrowthObj(dff), survObj = makeSurvObj(dff), dff = dff)
  slotNames(Pmatrix)
  Pmatrix@meshpoints
  #persp(Pmatrix)
  LE <- meanLifeExpect(Pmatrix)
  pTime <- passageTime(mean(dff$size, na.rm = TRUE), Pmatrix)
  #plot(Pmatrix@meshpoints, pTime)
  #plot(Pmatrix@meshpoints, LE)
  
  ########
  ##text from Supp methods of MEtcalf et all MEE paper
  ##note that even though fec1~size+I(size^2) provided a better fit to the data, 
  #it was giving unrealisically high estimates of per capita fecundity and hence throwing the LTRE. We decided to 
  #use just linear.
  
  fv1 <- makeFecObj(dataf=dff, Formula = c(fec0~size, fec1~size),
                    Family = c("binomial", "poisson"),
                    Transform = c("none", "none"),
                    meanOffspringSize = mean(dff[is.na(dff$size)==TRUE & 
                                                   is.na(dff$sizeNext)==FALSE, "sizeNext"]),
                    sdOffspringSize = sd(dff[is.na(dff$size)==TRUE &
                                               is.na(dff$sizeNext)==FALSE, "sizeNext"]),
                    fecConstants=data.frame(fec2=fec2),
                    offspringSplitter=data.frame(continuous=1),
                    vitalRatesPerOffspringType=data.frame(NA), 
                    fecByDiscrete=data.frame(NA))
  
  ######
  ####
  #this is the original fv1 from the IPMpack
  #fv1 <- makeFecObj(dff, Formula = fec0~size, Family = "binomial", Transform = "none") #Note vignette is missing "1" in "fec1"
  ####
  
  
  #####MUST MATCH VALUES WITH Pmatrix ABOVE
  Fmatrix <- makeIPMFmatrix(nBigMatrix = 50, minSize = -2, maxSize = 6,  fecObj = fv1, correction = "discretizeExtremes")
  
  IPM <- Pmatrix + Fmatrix
  truelambda<-Re(eigen(IPM)$value[1])
  truelambda
  
  return(truelambda)
}