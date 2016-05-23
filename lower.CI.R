#' this function will use the output from function "bootstrap.lambdas" and calclulate the 
#' lower 95% bias-corrected confidence intervals

lower.CI <- function(x,y,z) {
  
  bootlambdas<-x
  lambda<-y
  bootreps<-z
  
  #bootstrap corrected confcidence intervals
  
  #now can proceed with calculating the bias corrected confidence intervals
  
  P<-(sum(bootlambdas > lambda)/bootreps)
  P
  
  
  #BIAS CORRECTED CONFIDENCE INTERVALS
  
  
  
  if (P>0.5) {
    newP<-(1-P)
    z_p<- -0.862+1.202*(sqrt(-0.639-1.664*log(newP)))
  } else {z_p<- -(-0.862+1.202*(sqrt(-0.639-1.664*log(P))))}
  
  zupper<-(z_p*2+1.96);
  zlower<-(z_p*2-1.96); 
  
  #calculation of lower phi
  if (zlower>0) {
    philower<-(1-0.5*exp(-0.717*zlower-0.416*zlower^2))
  } else {philower<- 1-(1-0.5*exp(-0.717*(-zlower)-0.416*(-zlower)^2))}
  
  #calculation of upper phi
  if (zupper>0) {
    phiupper<- (1-0.5*exp(-0.717*zupper-0.416*zupper^2));
  } else {phiupper=1-(1-0.5*exp(-0.717*(-zupper)-0.416*(-zupper)^2))}
  
  #FInally calculate the bias-corrected confidence intervals
  upperci=sort(bootlambdas)[ceiling(bootreps*phiupper)]
  lowerci=sort(bootlambdas)[ceiling(bootreps*philower)]
  
  
  lowerci
  

}