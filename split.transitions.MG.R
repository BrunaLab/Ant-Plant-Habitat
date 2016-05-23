#' Each plant species needs a different denominator when calculating the % of time it is occupied 
#' by each species (i.e., for Tococa it is sum of Crema (C) and Azteca (A), while for Maieta 
#' it is C+P, P, and C (Crematogaster and Pheidole).  This function simply pulls out all the 
#' Maieta and from the dataframe in which you have all the transitions and calclulates
#' the proportion of surveys in which it is occupied by each ant species

split.transitions.MG <- function(x) {
  MG<-x[x$plant.species=="Mg",]
  
  MG$counter.sum<-MG$counter.C+MG$counter.P+MG$counter.none+MG$counter.CP
  MG$sumC<-MG$counter.C+MG$counter.CP
  MG$sumP<-MG$counter.P+MG$counter.CP
  MG$prop.C<-MG$sumC/MG$counter.sum
  MG$prop.P<-MG$sumP/MG$counter.sum
  MG$prop.none<-(MG$counter.none)/MG$counter.sum
  MG
  
  
  
}
