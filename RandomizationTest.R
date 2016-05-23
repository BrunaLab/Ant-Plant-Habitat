#For PC
#setwd("U:/Emilio's Folder Current/RESEARCH/Ant-Plants/A-P Demography")
#setwd("C:/Users/Emilio Bruna/Desktop/Backup 23 May 2013/RESEARCH/Ant-Plants/A-P Demography")

#For Mac

library(gdata)
library(lattice)
library(MuMIn)
library(IPMpack)
library(popbio)
library(boot)
library(popdemo)
library(dplyr)
library(tidyr)

rm(list=ls())

ALLDATA<-read.csv("Ant-Plant-Demography_2september2014.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )
sapply(ALLDATA, class)

##################
##################
#THIS SECTION WILL CALCLULATE THE NUMBER OF TRANSITIONS IN ANT RESIDENT EACH PLANT HAD
##################
##################

#add a column of zeros.  this will be to count the number of species transitions
ALLDATA_MERGED<-ALLDATA

#CALCLULATE THE TYPE OF ANT TRANSITIONS FROM YEAR TO YEAR (WILL USE THIS TO SET UP DEMOGRAPOHIC WORK WITH ONLY CERTAIN TRANSITIONS)
#transition 1
A<-paste(ALLDATA$ant.1,ALLDATA$ant.2)
A<-as.data.frame(A)
colnames(A)<-c("transition1")
ALLDATA_MERGED<-cbind(ALLDATA_MERGED,A)

#transition 2
A<-paste(ALLDATA$ant.2,ALLDATA$ant.3)
A<-as.data.frame(A)
colnames(A)<-c("transition2")
ALLDATA_MERGED<-cbind(ALLDATA_MERGED,A)

#transition 3
A<-paste(ALLDATA$ant.3,ALLDATA$ant.4)
A<-as.data.frame(A)
colnames(A)<-c("transition3")
ALLDATA_MERGED<-cbind(ALLDATA_MERGED,A)

#transition 4
A<-paste(ALLDATA$ant.4,ALLDATA$ant.5)
A<-as.data.frame(A)
colnames(A)<-c("transition4")
ALLDATA_MERGED<-cbind(ALLDATA_MERGED,A)

#transition 5
A<-paste(ALLDATA$ant.5,ALLDATA$ant.6)
A<-as.data.frame(A)
colnames(A)<-c("transition5")
ALLDATA_MERGED<-cbind(ALLDATA_MERGED,A)

#transition 6
A<-paste(ALLDATA$ant.6,ALLDATA$ant.7)
A<-as.data.frame(A)
colnames(A)<-c("transition6")
ALLDATA_MERGED<-cbind(ALLDATA_MERGED,A)

######################
######################
#THIS SECTION CALCLULATES THE NUMBER OF TIMES YOU HAVE A CERTAIN ANT. IT WILL BE USED TO SEPERATE THE PLANTS THAT ARE THE SMAE ANT SPECIES THROUGHOUT ALL CENSUS INTERVALS
######################
######################

ALLDATA_MERGED$counter.C<-0
ALLDATA_MERGED$counter.A<-0
ALLDATA_MERGED$percent.A<-0
ALLDATA_MERGED$counter.P<-0
ALLDATA_MERGED$counter.CP<-0
ALLDATA_MERGED$counter.none<-0
ALLDATA_MERGED$counter.sum<-0
ALLDATA_MERGED$sumC<-0
ALLDATA_MERGED$sumP<-0
ALLDATA_MERGED$percent.C<-0
ALLDATA_MERGED$percent.P<-0
ALLDATA_MERGED$percent.none<-0

#WHat ant was the plant in survey 1?
ALLDATA_MERGED$counter.C<-ifelse(ALLDATA_MERGED$ant.1=="C", ALLDATA_MERGED$counter.C+1, ALLDATA_MERGED$counter.C+0)
ALLDATA_MERGED$counter.P<-ifelse(ALLDATA_MERGED$ant.1=="P", ALLDATA_MERGED$counter.P+1, ALLDATA_MERGED$counter.P+0)
ALLDATA_MERGED$counter.none<-ifelse(ALLDATA_MERGED$ant.1=="none", ALLDATA_MERGED$counter.none+1, ALLDATA_MERGED$counter.none+0)
ALLDATA_MERGED$counter.CP<-ifelse(ALLDATA_MERGED$ant.1=="C+P", ALLDATA_MERGED$counter.CP+1, ALLDATA_MERGED$counter.CP+0)
ALLDATA_MERGED$counter.A<-ifelse(ALLDATA_MERGED$ant.1=="A", ALLDATA_MERGED$counter.A+1, ALLDATA_MERGED$counter.A+0)

#WHat ant was the plant in survey 2?
ALLDATA_MERGED$counter.C<-ifelse(ALLDATA_MERGED$ant.2=="C", ALLDATA_MERGED$counter.C+1, ALLDATA_MERGED$counter.C+0)
ALLDATA_MERGED$counter.P<-ifelse(ALLDATA_MERGED$ant.2=="P", ALLDATA_MERGED$counter.P+1, ALLDATA_MERGED$counter.P+0)
ALLDATA_MERGED$counter.none<-ifelse(ALLDATA_MERGED$ant.2=="none", ALLDATA_MERGED$counter.none+1, ALLDATA_MERGED$counter.none+0)
ALLDATA_MERGED$counter.CP<-ifelse(ALLDATA_MERGED$ant.2=="C+P", ALLDATA_MERGED$counter.CP+1, ALLDATA_MERGED$counter.CP+0)
ALLDATA_MERGED$counter.A<-ifelse(ALLDATA_MERGED$ant.2=="A", ALLDATA_MERGED$counter.A+1, ALLDATA_MERGED$counter.A+0)

#WHat ant was the plant in survey 3?
ALLDATA_MERGED$counter.C<-ifelse(ALLDATA_MERGED$ant.3=="C", ALLDATA_MERGED$counter.C+1, ALLDATA_MERGED$counter.C+0)
ALLDATA_MERGED$counter.P<-ifelse(ALLDATA_MERGED$ant.3=="P", ALLDATA_MERGED$counter.P+1, ALLDATA_MERGED$counter.P+0)
ALLDATA_MERGED$counter.none<-ifelse(ALLDATA_MERGED$ant.3=="none", ALLDATA_MERGED$counter.none+1, ALLDATA_MERGED$counter.none+0)
ALLDATA_MERGED$counter.CP<-ifelse(ALLDATA_MERGED$ant.3=="C+P", ALLDATA_MERGED$counter.CP+1, ALLDATA_MERGED$counter.CP+0)
ALLDATA_MERGED$counter.A<-ifelse(ALLDATA_MERGED$ant.3=="A", ALLDATA_MERGED$counter.A+1, ALLDATA_MERGED$counter.A+0)

#WHat ant was the plant in survey 4?
ALLDATA_MERGED$counter.C<-ifelse(ALLDATA_MERGED$ant.4=="C", ALLDATA_MERGED$counter.C+1, ALLDATA_MERGED$counter.C+0)
ALLDATA_MERGED$counter.P<-ifelse(ALLDATA_MERGED$ant.4=="P", ALLDATA_MERGED$counter.P+1, ALLDATA_MERGED$counter.P+0)
ALLDATA_MERGED$counter.none<-ifelse(ALLDATA_MERGED$ant.4=="none", ALLDATA_MERGED$counter.none+1, ALLDATA_MERGED$counter.none+0)
ALLDATA_MERGED$counter.CP<-ifelse(ALLDATA_MERGED$ant.4=="C+P", ALLDATA_MERGED$counter.CP+1, ALLDATA_MERGED$counter.CP+0)
ALLDATA_MERGED$counter.A<-ifelse(ALLDATA_MERGED$ant.4=="A", ALLDATA_MERGED$counter.A+1, ALLDATA_MERGED$counter.A+0)

#WHat ant was the plant in survey 5?
ALLDATA_MERGED$counter.C<-ifelse(ALLDATA_MERGED$ant.5=="C", ALLDATA_MERGED$counter.C+1, ALLDATA_MERGED$counter.C+0)
ALLDATA_MERGED$counter.P<-ifelse(ALLDATA_MERGED$ant.5=="P", ALLDATA_MERGED$counter.P+1, ALLDATA_MERGED$counter.P+0)
ALLDATA_MERGED$counter.none<-ifelse(ALLDATA_MERGED$ant.5=="none", ALLDATA_MERGED$counter.none+1, ALLDATA_MERGED$counter.none+0)
ALLDATA_MERGED$counter.CP<-ifelse(ALLDATA_MERGED$ant.5=="C+P", ALLDATA_MERGED$counter.CP+1, ALLDATA_MERGED$counter.CP+0)
ALLDATA_MERGED$counter.A<-ifelse(ALLDATA_MERGED$ant.5=="A", ALLDATA_MERGED$counter.A+1, ALLDATA_MERGED$counter.A+0)

#WHat ant was the plant in survey 6?
ALLDATA_MERGED$counter.C<-ifelse(ALLDATA_MERGED$ant.6=="C", ALLDATA_MERGED$counter.C+1, ALLDATA_MERGED$counter.C+0)
ALLDATA_MERGED$counter.P<-ifelse(ALLDATA_MERGED$ant.6=="P", ALLDATA_MERGED$counter.P+1, ALLDATA_MERGED$counter.P+0)
ALLDATA_MERGED$counter.none<-ifelse(ALLDATA_MERGED$ant.6=="none", ALLDATA_MERGED$counter.none+1, ALLDATA_MERGED$counter.none+0)
ALLDATA_MERGED$counter.CP<-ifelse(ALLDATA_MERGED$ant.6=="C+P", ALLDATA_MERGED$counter.CP+1, ALLDATA_MERGED$counter.CP+0)
ALLDATA_MERGED$counter.A<-ifelse(ALLDATA_MERGED$ant.6=="A", ALLDATA_MERGED$counter.A+1, ALLDATA_MERGED$counter.A+0)

#WHat ant was the plant in survey 7?
ALLDATA_MERGED$counter.C<-ifelse(ALLDATA_MERGED$ant.7=="C", ALLDATA_MERGED$counter.C+1, ALLDATA_MERGED$counter.C+0)
ALLDATA_MERGED$counter.P<-ifelse(ALLDATA_MERGED$ant.7=="P", ALLDATA_MERGED$counter.P+1, ALLDATA_MERGED$counter.P+0)
ALLDATA_MERGED$counter.none<-ifelse(ALLDATA_MERGED$ant.7=="none", ALLDATA_MERGED$counter.none+1, ALLDATA_MERGED$counter.none+0)
ALLDATA_MERGED$counter.CP<-ifelse(ALLDATA_MERGED$ant.7=="C+P", ALLDATA_MERGED$counter.CP+1, ALLDATA_MERGED$counter.CP+0)
ALLDATA_MERGED$counter.A<-ifelse(ALLDATA_MERGED$ant.7=="A", ALLDATA_MERGED$counter.A+1, ALLDATA_MERGED$counter.A+0)

summary(ALLDATA_MERGED)

##################
##################
################
##each plant necessitates a different denominator when alclulating the % of time occupied by each species (i.e., for totcoca it is sum of C and A, while for maieta it is C+P, P, and C
#consequently, must do the division AFTER you select the plant species you wish to do your analyses on.  You could calclulate the different denominators as a function of 
#which plant you have, but its just easier to do it later
################
##################
##################

#THIS SEPERATES THE DATASETS BY PLANT SPECIES 
###FOR MAIETA
MG<-ALLDATA_MERGED[ALLDATA_MERGED$plant.species=="Mg",]

MG$counter.sum<-MG$counter.C+MG$counter.P+MG$counter.none+MG$counter.CP
MG$sumC<-MG$counter.C+MG$counter.CP
MG$sumP<-MG$counter.P+MG$counter.CP
MG$percent.C<-MG$sumC/MG$counter.sum
MG$percent.P<-MG$sumP/MG$counter.sum
MG$percent.none<-(MG$counter.none)/MG$counter.sum
summary(MG)



#FOR TOCOCA
TB<-ALLDATA_MERGED[ALLDATA_MERGED$plant.species=="Tb",]

TB$counter.sum<-TB$counter.A+TB$counter.C+TB$counter.none
TB$percent.C<-TB$counter.C/TB$counter.sum
TB$percent.A<-TB$counter.A/TB$counter.sum
TB$percent.none<-TB$counter.none/TB$counter.sum

summary(TB)

########
#select plant species of choice
DATA<-MG
#DATA<-TB 

foo<-DATA[,44:49]
foo<-gather(foo, "Interval", "transition", 1:6)
foo[foo==' '] <- NA
drop.levels(foo)
foo<-na.omit(foo)
foo$transition<-as.factor(foo$transition)
as.data.frame(foo)
str(foo)
summarize(foo)
summary(foo$transition)
group_by
transtable<-ftable(foo$transition)





#Choose what you want to work on (100% C, 100% P, 100% none, mixed, can eventually vary from 0-1 (i.e. lambda vs freq of occupancy by P))
#DATA<-DATA[DATA$percent.C<1 & DATA$percent.P<1 ,]
#DATA<-DATA[DATA$percent.C==1,]
#DATA<-DATA[DATA$percent.P==1,]
#DATA<-DATA[DATA$percent.C<1,]
# DATA<-DATA[DATA$percent.C<1 & DATA$percent.P>0 ,]
DATA<-DATA[DATA$percent.C==0 & DATA$percent.P>=0 ,]

DATA<-drop.levels(DATA)
summary(DATA)
plantdensity<-ftable(DATA$topography, DATA$plant.species, DATA$canopy.cover)
plantdensity<-ftable(DATA$plant.species, DATA$topography, DATA$canopy.cover, DATA$sumC)
plantdensity<-ftable(DATA$plant.species, DATA$counter.sum, DATA$percent.none)

plantdensity
#how to split a vector into smaller ones, randomly assigning individuals to each vector 
#test<-1:12
#split(test,sample(1:3)) #three vectors of same length
#split(test, sample(rep(1:3, c(2, 3, 7)))) #three vectors of lengths 2,3, and 7

reps=1000

lambdasP <- vector()
lambdasmix <- vector()
lambdasC <- vector()


#########
##RANDOMIZATION TESTS
#########


for(i in 1:reps){
  
  DATA_mix<-split(DATA, sample(rep(1:3, c(58, 42, 398))))$'1'
  DATA_C<-split(DATA, sample(rep(1:3, c(58, 42, 398))))$'2'
  DATA_P<-split(DATA, sample(rep(1:3, c(58, 42, 398))))$'3'
    

  lambdasP <- c(lambdasP, lambda1(DATA_P))
  lambdasmix <- c(lambdasmix, lambda1(DATA_mix))
  lambdasC <- c(lambdasC, lambda1(DATA_C))
  
} 

lambdas<-cbind(lambdasP,lambdasC,lambdasmix)
lambdas<-as.matrix(lambdas)
PminusC<-lambdas[,1]-lambdas[,2]
Pminusmix<-lambdas[,1]-lambdas[,3]
Cminusmix<-lambdas[,2]-lambdas[,3]
hist(lambdasP)
hist(lambdasmix)
hist(lambdasC)
hist(PminusC)
hist(Pminusmix)
hist(Cminusmix)

realP<-1.23
realC<-1.16
realmix<-1.06

realPminusC<-realP-realC
realPminusmix<-realP-realmix
realCminusmix<-realC-realmix

ProbPC<-((sum(PminusC > realPminusC)+1)/(reps+1))
ProbPmix<-((sum(Pminusmix > realPminusmix)+1)/(reps+1))
ProbCmix<-((sum(Cminusmix > realCminusmix)+1)/(reps+1))

 #P_P<-(sum(randomizationlambdasP > truelambdaP)/reps)
#P_C<-(sum(randomizationlambdasC > truelambdaC)/reps)
#P_mix<-(sum(randomizationlambdas_mix > truelambda_mix)/reps)
#P_P
#P_C
#P_mix
