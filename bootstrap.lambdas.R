#' this function generates a vector of bootstrap lambda values with which you can calclulate the
#' upper and lower 95% boas corrected confidence intervals

bootstrap.lambdas <- function (x,y) {
  
  dff_original<-x
  bootreps<-y
  
  
  bootlambdas <- vector()
  
  for(i in 1:bootreps){
    dff<-dff_original[sample(nrow(dff_original),replace=TRUE),]
    
    lambda<-IPMpack.EB(dff, fec2)
    
    bootlambdas <- c(bootlambdas, lambda)
  } 
  
  bootlambdas
  
}