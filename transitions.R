#' THIS functions will calclulate the number and type of changes ("transitions") in the species of ant occupant each plant 
#' in the demographic dataset underwent. These 'transitions' could be that the resident ant species remains the same or changes
#' It could also mean that the the plant goes from unoccupied to occupied or vice-versa.
 
transitions <- function(x) {

# Renamed THE FILE YOU INPUT AS "TRANSITIONS". 
  TRANSITIONS<-x
  
  #CALCLULATE THE TYPE OF ANT TRANSITIONS FROM YEAR TO YEAR (WILL USE THIS TO SET UP DEMOGRAPOHIC WORK WITH ONLY CERTAIN TRANSITIONS)
  #transition 1
  T1<-paste(x$ant.1,x$ant.2)
  T1<-as.data.frame(T1)
  colnames(T1)<-c("transition1")
  TRANSITIONS<-cbind(TRANSITIONS,T1)
  
  #transition 2
  T2<-paste(x$ant.2,x$ant.3)
  T2<-as.data.frame(T2)
  colnames(T2)<-c("transition2")
  TRANSITIONS<-cbind(TRANSITIONS,T2)
  
  #transition 3
  T3<-paste(x$ant.3,x$ant.4)
  T3<-as.data.frame(T3)
  colnames(T3)<-c("transition3")
  TRANSITIONS<-cbind(TRANSITIONS,T3)
  
  #transition 4
  T4<-paste(x$ant.4,x$ant.5)
  T4<-as.data.frame(T4)
  colnames(T4)<-c("transition4")
  TRANSITIONS<-cbind(TRANSITIONS,T4)
  
  #transition 5
  T5<-paste(x$ant.5,x$ant.6)
  T5<-as.data.frame(T5)
  colnames(T5)<-c("transition5")
  TRANSITIONS<-cbind(TRANSITIONS,T5)
  
  #transition 6
  T6<-paste(x$ant.6,x$ant.7)
  T6<-as.data.frame(T6)
  colnames(T6)<-c("transition6")
  TRANSITIONS<-cbind(TRANSITIONS,T6)
  
  
  # THIS SECTION CALCLULATES THE NUMBER OF TIMES YOU HAVE A CERTAIN ANT. 
  # IT WILL BE USED TO SEPERATE THE PLANTS THAT HAVE THE SAME ANT SPECIES IN ALL CENSUSES 
  
  TRANSITIONS$counter.C<-0
  TRANSITIONS$counter.A<-0
  TRANSITIONS$percent.A<-0
  TRANSITIONS$counter.P<-0
  TRANSITIONS$counter.CP<-0
  TRANSITIONS$counter.none<-0
  TRANSITIONS$counter.sum<-0
  TRANSITIONS$sumC<-0
  TRANSITIONS$sumP<-0
  TRANSITIONS$percent.C<-0
  TRANSITIONS$percent.P<-0
  TRANSITIONS$percent.none<-0
  
  # What ant was the plant in survey 1?
  TRANSITIONS$counter.C<-ifelse(TRANSITIONS$ant.1=="C", TRANSITIONS$counter.C+1, TRANSITIONS$counter.C+0)
  TRANSITIONS$counter.P<-ifelse(TRANSITIONS$ant.1=="P", TRANSITIONS$counter.P+1, TRANSITIONS$counter.P+0)
  TRANSITIONS$counter.none<-ifelse(TRANSITIONS$ant.1=="none", TRANSITIONS$counter.none+1, TRANSITIONS$counter.none+0)
  TRANSITIONS$counter.CP<-ifelse(TRANSITIONS$ant.1=="C+P", TRANSITIONS$counter.CP+1, TRANSITIONS$counter.CP+0)
  TRANSITIONS$counter.A<-ifelse(TRANSITIONS$ant.1=="A", TRANSITIONS$counter.A+1, TRANSITIONS$counter.A+0)
  
  # What ant was the plant in survey 2?
  TRANSITIONS$counter.C<-ifelse(TRANSITIONS$ant.2=="C", TRANSITIONS$counter.C+1, TRANSITIONS$counter.C+0)
  TRANSITIONS$counter.P<-ifelse(TRANSITIONS$ant.2=="P", TRANSITIONS$counter.P+1, TRANSITIONS$counter.P+0)
  TRANSITIONS$counter.none<-ifelse(TRANSITIONS$ant.2=="none", TRANSITIONS$counter.none+1, TRANSITIONS$counter.none+0)
  TRANSITIONS$counter.CP<-ifelse(TRANSITIONS$ant.2=="C+P", TRANSITIONS$counter.CP+1, TRANSITIONS$counter.CP+0)
  TRANSITIONS$counter.A<-ifelse(TRANSITIONS$ant.2=="A", TRANSITIONS$counter.A+1, TRANSITIONS$counter.A+0)
  
  # What ant was the plant in survey 3?
  TRANSITIONS$counter.C<-ifelse(TRANSITIONS$ant.3=="C", TRANSITIONS$counter.C+1, TRANSITIONS$counter.C+0)
  TRANSITIONS$counter.P<-ifelse(TRANSITIONS$ant.3=="P", TRANSITIONS$counter.P+1, TRANSITIONS$counter.P+0)
  TRANSITIONS$counter.none<-ifelse(TRANSITIONS$ant.3=="none", TRANSITIONS$counter.none+1, TRANSITIONS$counter.none+0)
  TRANSITIONS$counter.CP<-ifelse(TRANSITIONS$ant.3=="C+P", TRANSITIONS$counter.CP+1, TRANSITIONS$counter.CP+0)
  TRANSITIONS$counter.A<-ifelse(TRANSITIONS$ant.3=="A", TRANSITIONS$counter.A+1, TRANSITIONS$counter.A+0)
  
  # What ant was the plant in survey 4?
  TRANSITIONS$counter.C<-ifelse(TRANSITIONS$ant.4=="C", TRANSITIONS$counter.C+1, TRANSITIONS$counter.C+0)
  TRANSITIONS$counter.P<-ifelse(TRANSITIONS$ant.4=="P", TRANSITIONS$counter.P+1, TRANSITIONS$counter.P+0)
  TRANSITIONS$counter.none<-ifelse(TRANSITIONS$ant.4=="none", TRANSITIONS$counter.none+1, TRANSITIONS$counter.none+0)
  TRANSITIONS$counter.CP<-ifelse(TRANSITIONS$ant.4=="C+P", TRANSITIONS$counter.CP+1, TRANSITIONS$counter.CP+0)
  TRANSITIONS$counter.A<-ifelse(TRANSITIONS$ant.4=="A", TRANSITIONS$counter.A+1, TRANSITIONS$counter.A+0)
  
  # What ant was the plant in survey 5?
  TRANSITIONS$counter.C<-ifelse(TRANSITIONS$ant.5=="C", TRANSITIONS$counter.C+1, TRANSITIONS$counter.C+0)
  TRANSITIONS$counter.P<-ifelse(TRANSITIONS$ant.5=="P", TRANSITIONS$counter.P+1, TRANSITIONS$counter.P+0)
  TRANSITIONS$counter.none<-ifelse(TRANSITIONS$ant.5=="none", TRANSITIONS$counter.none+1, TRANSITIONS$counter.none+0)
  TRANSITIONS$counter.CP<-ifelse(TRANSITIONS$ant.5=="C+P", TRANSITIONS$counter.CP+1, TRANSITIONS$counter.CP+0)
  TRANSITIONS$counter.A<-ifelse(TRANSITIONS$ant.5=="A", TRANSITIONS$counter.A+1, TRANSITIONS$counter.A+0)
  
  # What ant was the plant in survey 6?
  TRANSITIONS$counter.C<-ifelse(TRANSITIONS$ant.6=="C", TRANSITIONS$counter.C+1, TRANSITIONS$counter.C+0)
  TRANSITIONS$counter.P<-ifelse(TRANSITIONS$ant.6=="P", TRANSITIONS$counter.P+1, TRANSITIONS$counter.P+0)
  TRANSITIONS$counter.none<-ifelse(TRANSITIONS$ant.6=="none", TRANSITIONS$counter.none+1, TRANSITIONS$counter.none+0)
  TRANSITIONS$counter.CP<-ifelse(TRANSITIONS$ant.6=="C+P", TRANSITIONS$counter.CP+1, TRANSITIONS$counter.CP+0)
  TRANSITIONS$counter.A<-ifelse(TRANSITIONS$ant.6=="A", TRANSITIONS$counter.A+1, TRANSITIONS$counter.A+0)
  
  # What ant was the plant in survey 7?
  TRANSITIONS$counter.C<-ifelse(TRANSITIONS$ant.7=="C", TRANSITIONS$counter.C+1, TRANSITIONS$counter.C+0)
  TRANSITIONS$counter.P<-ifelse(TRANSITIONS$ant.7=="P", TRANSITIONS$counter.P+1, TRANSITIONS$counter.P+0)
  TRANSITIONS$counter.none<-ifelse(TRANSITIONS$ant.7=="none", TRANSITIONS$counter.none+1, TRANSITIONS$counter.none+0)
  TRANSITIONS$counter.CP<-ifelse(TRANSITIONS$ant.7=="C+P", TRANSITIONS$counter.CP+1, TRANSITIONS$counter.CP+0)
  TRANSITIONS$counter.A<-ifelse(TRANSITIONS$ant.7=="A", TRANSITIONS$counter.A+1, TRANSITIONS$counter.A+0)
  
  TRANSITIONS
  
}
