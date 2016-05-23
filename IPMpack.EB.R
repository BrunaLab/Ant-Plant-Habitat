#' this function uses the code from IPMpack to calclulate lamda of your population 
#' using integral projection models.  uses the output of create.dff and a value for 
#' fec2 (seed germination).  for additional details see Metcalf et al MEE.

IPMpack.EB <- function(x, y) {
  
  dff<-x  # IPMpack uses dff, I kept as is to match the code in the package
  fec2<-y # IPMpack uses fec2, I kept as is to match the code in the package
  
  # The code is what is in package IPMpack unless noted
  plot(dff$size,dff$sizeNext)
  
  # Note!! You must change the functions for explanatory variables if you wish 
  # to include size^2 or Size^3 in the polynomial regression
  
  #gr1 <- makeGrowthObj(dff, Formula = sizeNext~size+size2)
  gr1 <- makeGrowthObj(dff, Formula = sizeNext~size)
  
  #sv1 <- makeSurvObj(dff, Formula = surv~size)
  sv1 <- makeSurvObj(dff, Formula = surv~size+size2)
   
  gr1
  sv1
  
  # To change the coefficients in your sv2 function once you decide what they should be, use the following
  # sv1@fit[[1]]=(c(1.1491, 1.5018)) #intercept, size
  # sv1
  
  # If you plan on including stochastically varying covariates
  # sv1 <- makeSurvObj(dataf = dff, Formula = surv~size+covariate)
  # gr1 <- makeGrowthObj(dataf = dff, Formula = sizeNext~size+covariate)
  
  
  par(mfrow = c(1,2), bty ="l", pty = "m")
  p1 <- picGrow(dff, gr1)
  p2<-picSurv(dff, sv1, ncuts = 50)
  
  Pmatrix <- makeIPMPmatrix(nBigMatrix = 50 , minSize = -2, maxSize = 6, growObj = gr1, survObj = sv1, correction="constant")
  
  #To check diagnostic graphs the next line needs to remove # 
  #diagnosticsPmatrix(Pmatrix, growObj = makeGrowthObj(dff), survObj = makeSurvObj(dff), dff = dff)
  slotNames(Pmatrix)
  Pmatrix@meshpoints
  persp(Pmatrix)
  
  LE <- meanLifeExpect(Pmatrix)
  pTime <- passageTime(mean(dff$size, na.rm = TRUE), Pmatrix)
  
  plot(Pmatrix@meshpoints, pTime)
  plot(Pmatrix@meshpoints, LE)
  

  # this is the original fv1 from the IPMpack; use the code from Metclaf et al MEE paper below instead
  # fv1 <- makeFecObj(dff, Formula = fec0~size, Family = "binomial", Transform = "none") #Note vignette is missing "1" in "fec1"
  
  # text from Supp methods of MEtcalf et all MEE paper
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
  
  
  
  # MUST MATCH VALUES for nBigMatrix, minSize, and mazSize WITH Pmatrix ABOVE
  Fmatrix <- makeIPMFmatrix(nBigMatrix = 50, minSize = -2, maxSize = 6,  fecObj = fv1, correction = "discretizeExtremes")
  
  IPM <- Pmatrix + Fmatrix
  lambda<-Re(eigen(IPM)$value[1])
  lambda
  
  
}


############################################################################################# 
###############     CHANGING THE FUNCIONAL FORM OF GROWTH AND SURV OBJECTS    ############### 
#############################################################################################

# This section is only used if you need to change the functional form of 
# the growth / surv objects if necessary. For details see Metcalf et al. MME paper

# setClass("growthObjSaturate", representation(paras = "numeric", sd = "numeric"))

# fSaturate <- function(size, pars) {
#   u <- exp(pmin(pars[1] + pars[2] * size, 50))
#   u <- pars[3] * 1/(1+u)
#   return(u)}


# wrapSaturate <- function(par, dataf) {
#   pred <- fSaturate(dataf$size, par[1:3])
#   ss <- sum((pred - dataf$sizeNext)^2, na.rm = TRUE)
#   return(ss)}

# tmp <- optim(c(1, 1, 1), wrapSaturate, dataf = dff, method = "Nelder-Mead")
# tmp


# resids <- fSaturate(dff$size, tmp$par) - dff$sizeNext
# sdSaturate <- sd(resids, na.rm = TRUE)

# gr1 <- new("growthObjSaturate")
# gr1@paras <- tmp$par
# gr1@sd <- sdSaturate

# setMethod("growth", c("numeric", "numeric", "numeric", "growthObjSaturate"),
#           function(size, sizeNext, cov, growthObj){
#             mux <- fSaturate(size, growthObj@paras)
#             sigmax <- growthObj@sd
#             u <- dnorm(sizeNext, mux, sigmax, log = F)
#             return(u);})

# gr1

