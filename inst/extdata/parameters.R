agz <- paste0(seq(from = 0, by = 5, len = 17), "-", seq(from = 4, by = 5, len = 17))
agz[length(agz)] <- "80+"

## === TB hyperparms
hyperparms <- list(
  staticfoi = 1, #static or dynamic based on >0 or not
  ## --------------------------------------------------- transmission
  ## bet=list(meanlog=log(10),sdlog=0.75),         #bet,      #beta
  ptn=list(shape1=20.7,shape2=77.9),            #psi:protn Andrews
  ## previous version:  foi=list(meanlog=log(1e-2),sdlog=0.5)
  foi=list(meanlog=-4.495672,sdlog=0.2717918),    #ari0: to give 20% (10% to 30%) LTBI over 20 years
  ## --------------------------------------------------- progression
  stabilization=list(meanlog=0.62, sdlog=0.068),       #kappa:arig Ragonnet
  progn_fast=list(meanlog=-2.837,sdlog=0.32),         #eps: pp Ragonnet
  progn_slow=list(meanlog=-6.89,sdlog=0.58),         #nu: Ragonnet
  rel=list(meanlog=-3.95,sdlog=0.27),         #omega: relapse Crampin NOTE x-ref
  ## --------------------------------------------------- detection
  CDRa=list(shape1=41.80,shape2=5.22),        #UK: CDR based on WHO data & Laura Anderson data
  ## --------------------------------------------------- timescales
  drn=list(meanlog=1.1,sdlog=0.2),               #durnX log(3)
  ## --------------------------------------------------- CFRs
  txf=list(shape1=157+5,shape2= 3426-157-5), #UK data:
  ## https://www.gov.uk/government/publications/tuberculosis-in-england-2023-report-data-up-to-end-of-2022/tb-treatment-and-outcomes-england-2022
  CFR=list(shape1=25.48, shape2= 33.78),
  ## --------------------------------------------------- other
  m=list(meanlog=log(0.94),sdlog=0.1), #multiplier for population transmission = R
  tptHR = list(meanlog=-1.772,sdlog=0.089), #HR TPT protection in TBI+, Martinez et al 0.17 (0.14-0.2)
  tpt_drn = 20,       #durn of TPT: Salazar-Austin
  wsn = list(meanlog=-0.693,sdlog=0.97),  #durn AS TB D from getLNparms(0.5,1) = Frascella half of TB SC + 1ydrn
  mHR = list(meanlog=0.131,sdlog=0.071),             #post-TB mortality HR
  att_time = 0.5,      #duration of ATT
  late_post_time=2,    #duration defining early post-TB
  mort=0.02,            #mortality rate
  hrqolptb=list(meanlog=-3.324,sdlog=0.486), # HRQoL decrement while post TB
  hrqol = list(shape1 = 21.15177, shape2 = 42.36706) # GBD decrement 0.333 (0.224–0.454)
  )


qfun <- function(u,L){
  x <- NULL
  if(names(L)[1]=='meanlog') x <- qlnorm(u,L[[1]],L[[2]])
  if(names(L)[1]=='shape1') x <- qbeta(u,L[[1]],L[[2]])
  if(names(L)[1]=='mean') x <- qnorm(u,L[[1]],L[[2]])
  if(names(L)[1]=='shape') x <- qgamma(u,L[[1]],scale=L[[2]])
  if(is.null(x)) x <- u
  x
}

uv2ps <- function(u,HP,returnlist=TRUE){
  for(i in 1:length(HP)){
    if(is.list(HP[[i]])){
      u[i] <- qfun(u[i],HP[[i]])
    } else { #fixed value
      u[i] <- HP[[i]]
    }
  }
  if(returnlist){
    u <- as.list(u)
    names(u) <- names(HP)
  }
  u
}

## === tb parms

parms <- uv2ps(rep(0.5, length(hyperparms)),hyperparms) # natural histor
rCDR <- 1.04
CDR <- c(2*parms$CDRa*rCDR/(rCDR +1), 2*parms$CDRa/(rCDR +1))
parms$CDR <- CDR
parms$CDRa <- NULL








 
 

  





