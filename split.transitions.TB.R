#' Each plant species needs a different denominator when calculating the % of time it is occupied 
#' by each species (i.e., for Tococa it is sum of Crema (C) and Azteca (A), while for Maieta 
#' it is C+P, P, and C (Crematogaster and Pheidole).  This function simply pulls out all the 
#' Tococa and from the dataframe in which you have all the transitions and calclulates
#' the proportion of surveys in which it is occupied by each ant species

split.transitions.TB <- function(x) {  
  
  TB<-x[x$plant.species=="Tb",]
  
  TB$counter.sum<-TB$counter.A+TB$counter.C+TB$counter.none
  TB$prop.C<-TB$counter.C/TB$counter.sum
  TB$prop.A<-TB$counter.A/TB$counter.sum
  TB$prop.none<-TB$counter.none/TB$counter.sum
  
  TB
  
}
