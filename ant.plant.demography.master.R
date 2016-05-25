
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

source("transitions.R")
source("split.transitions.MG.R")
source("split.transitions.TB.R")
source("dff.create.R")
source("IPMpack.EB.R")
source("bootstrap.lambdas.R") 
source("upper.CI.R")
source("lower.CI.R")

########   CALCLULATE THE NUMBER OF TRANSITIONS IN ANT RESIDENT EACH PLANT HAD     ######## 
#Uses three functions - one to calclulate the number of transitions for the whole 
#dataset, and then two two that divide the dataset into subsets for Maieta and Tococa
transitions<-transitions(ALLDATA)
MG<-split.transitions.MG(transitions)
TB<-split.transitions.TB(transitions)
###########################################################################################  


########################################################################################### 
#############   WITH WHAT PLANT SPECIES WILL YOU BE CONDUCTING ANALYSES?     #############
########################################################################################### 
#select plant species with which you are going to conduct analyses.  
#Note that the Ecology paper used only Maieta guianensis, so while there is an option to 
#select TB with which to do analyses, the code is only set up to do analyses with MG (i.e., there
#are only options to select the proportion of time a plant is occupied by C & P.  Doing 
#analyses with TB will require writing new code to include Azteca.
DATA<-MG
DATA<-drop.levels(DATA)
DATA$site.id<-as.factor(DATA$site.id)
DATA$block<-as.factor(DATA$block)
# str(DATA)
# summary(DATA)

############################################################################################################# 
########     IN WHAT COMBINATION OF CANOPY COVER & TOPOGRAPHY WILL YOU BE CONDUCTING ANALYSES?   ############ 
############################################################################################################# 
#In the Ecology paper we did not explictly consider habitat, so this section is not used
#I have included it because in we will be considering how demography varies by habitat in future papers.
#Toggle these on or off depending on what combination of habitat/topography you want to study

# TOPOGRAPHY

#DATA<-DATA[DATA$topography=="plateau",] # only plants on plateaus, irrespective of gap or closed canopy
DATA_STR<-DATA[DATA$topography=="streamside",] # only plants on streamsides, irrespective of gap or closed canopy

#CANOPY COVER 
DATA_SG<-DATA[DATA_STR$canopy.cover=="gap",] #only plants in streamside gaps
DATA_SF<-DATA[DATA_STR$canopy.cover=="forest",] #only plants in streamside forest



########################################################################################## 
###########     WITH WHICH ANT OCCUPANT WILL YOU BE CONDUCTING ANALYSES?      ############
##########################################################################################
# Choose what you subset of plants with which to do analyses. Note that the Bruna et al 2014 Ecology paper used
# only plants occupied in all suerveys (interest was in effect of partner ID, so controlling for effect of being empty
# by only using plants 100% P, 100% C or 100% mixed).  The current paper is interested in habitat effects, which might 
# include being empty for a while since few ants in a site.   

# ALL STREAMSIDE PLANTS (BOTH GAPS AN UNDERSTORIES)

# these were the ones used in the 2014 Ecology paper - plants occupied 100% of surveys
# DATA.C.STR<-DATA_STR[DATA_STR$prop.C==1,]  #Colonized by Crematogaster in 100% of surveys
# DATA.P.STR<-DATA_STR[DATA_STR$prop.P==1,] #Colonized by Pheidole in 100% of surveys
# DATA.switch.STR<-DATA_STR[DATA_STR$sumC>0 & DATA_STR$sumP>0,] #Plants that switched partners at least once, were not vacant in any surveys 
# DATA.both.STR<-rbind(DATA.C.STR,DATA.P.STR,DATA.switch.STR)

DATA.C.STR<-DATA_STR[DATA_STR$sumC>0 & DATA_STR$sumP==0,]  #Colonized by Crematogaster or empty (Crema at least 1 time)
DATA.P.STR<-DATA_STR[DATA_STR$counter.none>0 & DATA_STR$sumP>0 & DATA_STR$sumC==0,] #Colonized by Pheidole or empty (colonized by Pheidole at least 1 time)
DATA.switch.STR<-DATA_STR[DATA_STR$sumC>0 & DATA_STR$sumP>0,] #Plants that switched partners at least once, may or may not have been vacant in one survey
DATA.both.STR<-rbind(DATA.C.STR,DATA.P.STR,DATA.switch.STR)

# PLANTS IN STREAMSIDE GAPS
DATA.C.SG<-DATA_SG[DATA_SG$sumC>0 & DATA_SG$sumP==0,]  #Colonized by Crematogaster or empty (Crema at least 1 time)
DATA.P.SG<-DATA_SG[DATA_SG$counter.none>0 & DATA_SG$sumP>0 & DATA_SG$sumC==0,] #Colonized by Pheidole or empty (colonized by Pheidole at least 1 time)
DATA.switch.SG<-DATA_SG[DATA_SG$sumC>0 & DATA_SG$sumP>0,] #Plants that switched partners at least once, may or may not have been vacant in one survey
DATA.both.SG<-rbind(DATA.C.STR,DATA.P.STR,DATA.switch.STR)

# PLANTS IN STREAMSIDE FOREST
DATA.C.SF<-DATA_SF[DATA_SF$sumC>0 & DATA_SF$sumP==0,]  #Colonized by Crematogaster or empty (Crema at least 1 time)
DATA.P.SF<-DATA_SF[DATA_SF$counter.none>0 & DATA_SF$sumP>0 & DATA_SF$sumC==0,] #Colonized by Pheidole or empty (colonized by Pheidole at least 1 time)
DATA.switch.SF<-DATA_SF[DATA_SF$sumC>0 & DATA_SF$sumP>0,] #Plants that switched partners at least once, may or may not have been vacant in one survey
DATA.both.SF<-rbind(DATA.C.STR,DATA.P.STR,DATA.switch.STR)



#this is just a little snippet to tell you how many plants you have after you have made your choices
#it's a good way of making sure you have chosen what you think you have chosen.  
#Be sure to change the identifyier after DATA (ie DATA.C, DATA.P, DATA.switch) to the one you want.

#DATA.C<-drop.levels(DATA.C)
# # summary(DATA.both.SF)
# plantdensity<-ftable(DATA.C$topography, DATA.C$plant.species, DATA.C$canopy.cover)
# #plantdensity

############################################################################################################# 
########################     PUT THE DATA IN THE FORMAT REQUIRED BY IPMpack   ############################### 
############################################################################################################# 
####IPMpack requires a dataframe called 'dff'"'. The function "dff.create" creates one for each of your datasets.
dff.switch.STR<-dff.create(DATA.switch.STR)
dff.both.STR<-dff.create(DATA.both.STR)
dff.C.STR<-dff.create(DATA.C.STR)
dff.P.STR<-dff.create(DATA.P.STR)

dff.switch.SG<-dff.create(DATA.switch.SG)
dff.both.SG<-dff.create(DATA.both.SG)
dff.C.SG<-dff.create(DATA.C.SG)
dff.P.SG<-dff.create(DATA.P.SG)

dff.switch.SF<-dff.create(DATA.switch.SF)
dff.both.SF<-dff.create(DATA.both.SF)
dff.C.SF<-dff.create(DATA.C.SF)
dff.P.SF<-dff.create(DATA.P.SF)

dff.all<-dff.create(DATA_STR) #a dff of all plants in the dataset

 

############################################################################################################# 
####################     INCREASE SAMPLE SIZE OF CREMATOGASTER SEEDLINGS WITH DATA    ####################### 
####################     FROM PREVIOUSLY PUBLISHED STUDY (BRUNA ET AL 2009 PLOS ONE)  ####################### 
############################################################################################################# 
#this is to add seedlings from 9-ha plot to dff
Csdlgs9ha<-read.csv("cremaseedlings.csv", dec=".", header = TRUE, sep = ",", check.names=FALSE )


dff.switch.STR<-rbind(Csdlgs9ha, dff.switch.STR)
dff.both.STR<-rbind(Csdlgs9ha, dff.both.STR)
dff.C.STR<-rbind(Csdlgs9ha, dff.C.STR)
dff.switch.SF<-rbind(Csdlgs9ha, dff.switch.SF)
dff.both.SF<-rbind(Csdlgs9ha, dff.both.SF)
dff.C.SF<-rbind(Csdlgs9ha, dff.C.SF)
dff.switch.SG<-rbind(Csdlgs9ha, dff.switch.SG)
dff.both.SG<-rbind(Csdlgs9ha, dff.both.SG)
dff.C.SG<-rbind(Csdlgs9ha, dff.C.SG)
dff.all<-rbind(Csdlgs9ha, dff.all)



############################################################################################################# 
####################     ESTIMATE VALUE FOR SEED GERMINATION TO BE USED AS FEC2 IN IPMpack    ############### 
############################################################################################################# 
# This is an attempt to estimate something close to a germination rate. it is done at the "whole "whole dataset level", meaning
# I simply added totaled the number of Maieta seedlings recruiting during the study and divided by number of fruits produced.  
# All plants in all habitats / canopy covers were pooled. Future studies evaluating habitat x topography effects on demography
# will evaluate if this vairies in different habitat types, but in the Ecology paper I used the constant below, which is the 
# value for all habitats and species of ant occupant combined.
# can modify to suite needs with 

#####DO YOU NEED TO DO THIS AT THE HABITAT LEVEL???

totalfruits<-sum(dff.all$fec1, na.rm=TRUE) #total fruits produced 
sdlgs<-nrow(dff.all[is.na(dff.all$size),]) #total seedlings produced
sdlgs_per_fruit<-sdlgs/totalfruits #seedlings per fruit
fec2<-sdlgs_per_fruit
fec2

##################################################################### 
###############     CALCULATE LAMBDA USING IPMpack    ############### 
##################################################################### 

# IPMpack.EB has a number of critical things that must be altered in it. these include the 
# choosing between the different funcitonal forms for growth, survicorship, and reproduction
# size of the matrixs (i.e., number of categories), minSize, maxSize, etc. Must go in and 
# manipulate in the function
# the function also runs all the diagnistics.

lambda.switch.str<-IPMpack.EB(dff.switch.STR, fec2)
lambda.both.str<-IPMpack.EB(dff.both.STR, fec2)
lambda.C.str<-IPMpack.EB(dff.C.STR, fec2)
lambda.P.str<-IPMpack.EB(dff.P.STR, fec2)

lambda.switch.sg<-IPMpack.EB(dff.switch.SG, fec2)
lambda.both.sg<-IPMpack.EB(dff.both.SG, fec2)
lambda.C.sg<-IPMpack.EB(dff.C.SG, fec2)
lambda.P.sg<-IPMpack.EB(dff.P.SG, fec2)

lambda.switch.sf<-IPMpack.EB(dff.switch.SF, fec2)
lambda.both.sf<-IPMpack.EB(dff.both.SF, fec2)
lambda.C.sf<-IPMpack.EB(dff.C.SF, fec2)
lambda.P.sf<-IPMpack.EB(dff.P.SF, fec2)

lambda.all<-IPMpack.EB(dff.all, fec2)



##################################################################### 
############     BIAS CORRECTED CONFIDENCE INTERVALS    ############# 
#####################################################################

# How many bootstrap runs would you like to do?  Note: if you are doing more than a few dozen
#you shoudl go into IPMpack.ED and comment out the graphs so it will go more quickly
bootreps<-1000

#this will calclulate the upper and lower 95% Confidence Intervals 
#for the "alternating-partner" population
bstrap.lambdas.switch<-bootstrap.lambdas(dff.switch, bootreps) 
upper.CI.switch<-upper.CI(bstrap.lambdas.switch,lambda.switch, bootreps)
lower.CI.switch<-lower.CI(bstrap.lambdas.switch,lambda.switch, bootreps)

#this will calclulate the upper and lower 95% Confidence Intervals
#for the Crematogaster population
bstrap.lambdas.C<-bootstrap.lambdas(dff.C, bootreps) 
upper.CI.C<-upper.CI(bstrap.lambdas.C,lambda.C, bootreps)
lower.CI.C<-lower.CI(bstrap.lambdas.C,lambda.C, bootreps)

#this will calclulate the upper and lower 95% Confidence Intervals 
#for the Pheidole population
bstrap.lambdas.P<-bootstrap.lambdas(dff.P, bootreps) 
upper.CI.P<-upper.CI(bstrap.lambdas.P,lambda.P, bootreps)
lower.CI.P<-lower.CI(bstrap.lambdas.P,lambda.P, bootreps)


##################################################################### 
###############     SENSITIVITIES AND ELASTICITIES    ############### 
#####################################################################

##  NEED TO CONVERT THESE BELOW TO FUNCTIONS


sensitivity <- sens(IPM)
elasticity <- elas(IPM)


par(mfrow = c(2, 1), bty = "l", pty = "m")
image(Pmatrix@meshpoints, Pmatrix@meshpoints, t(elasticity), main = "Elasticity", xlab = "Number of shoots in time t",ylab = "Number of shoots in time t+1")
image(Pmatrix@meshpoints, Pmatrix@meshpoints, t(sensitivity), main = "Sensitivity", xlab = "Number of shoots in time t",ylab = "Number of shoots in time t+1")



res <- sensParams(growObj = gr1, survObj = sv1, fecObj = fv1, nBigMatrix = 50, minSize = -2, maxSize = 6)
res

###Typo in IPMpack code - should be res$sens and res$elas
par(mfrow = c(2, 1), bty = "l", pty = "m")
barplot(res$sens, main = expression("Parameter sensitivity of "*lambda),las = 2, cex.names = 0.5)
barplot(res$elas, main = expression("Parameter elasticity of "*lambda), las = 2, cex.names = 0.5)

