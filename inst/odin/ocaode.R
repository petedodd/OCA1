## ODEs for model
## ** suggested indices **
## demographic:
##  i = age        (compulsory)
##  j = sex        (compulsory)
##  k = nativity   (optional)
##  l = risk group (optional)
## TB:
##  i5 = post-TB   (compulsory)
##  i6 = strain    (optional)
##  i7 = protection from TPT + vaccination (optional)
##  (i8 = spare)
## optional => needs to work with dim = 1 so can be turned off

## == inputs & interpolation for demography
nage <- user()                            #number age cats
nnat <- user()                            #number of nativity cats
nrisk <- user()                           #number of risk cats
npost <- user()                           #number of post-TB cats
nstrain <- user()                         #number of TB strains
nprot <- user()                           #number of TB protection strata
birthrisk[] <- user()                       #which risk at birth?
r[] <- user()                             #aging rate
## demographic aging/mortality/net migration
popdat[,,] <- user()      #population dynamics
BB[,] <- user()           #birth rate
ttp[] <- user()           #time points for data
## interpolation
bz[] <- interpolate(ttp,BB,"linear")
omega[,] <- interpolate(ttp,popdat,"linear")
tol <- 1e-12 #tolerance safety

## by TB
popinitU[,,,,,,] <- user() #initial population
popinitE[,,,,,,] <- user() #initial population
popinitL[,,,,,,] <- user() #initial population
popinitA[,,,,,,] <- user() #initial population
popinitS[,,,,,,] <- user() #initial population
popinitT[,,,,,,] <- user() #initial population

## useful masks:
native[1] <- 1
native[2:nnat] <- 0
tbbottom[1:npost,1:nstrain,1:nprot] <- 0
tbbottom[1,1,1] <- 1 #CHECK overwrite works : only 1 for tb indices = 1

## ageing:
ageinU[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i>1) r[i-1] * Uninfected[i-1,j,k,l,i5,i6,i7] else bz[j] * native[k] * birthrisk[l] * tbbottom[i5,i6,i7]
ageinE[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i>1) r[i-1] * Learly[i-1,j,k,l,i5,i6,i7] else 0
ageinL[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i>1) r[i-1] * Llate[i-1,j,k,l,i5,i6,i7] else 0
ageinA[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i>1) r[i-1] * Asymp[i-1,j,k,l,i5,i6,i7] else 0
ageinS[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i>1) r[i-1] * Symp[i-1,j,k,l,i5,i6,i7] else 0
ageinT[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i>1) r[i-1] * Treat[i-1,j,k,l,i5,i6,i7] else 0


## NOTE once TB states are introduced, will need to handle U differently for strains
## == initial state
initial(Uninfected[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinitU[i,j,k,l,i5,i6,i7]
initial(Learly[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinitE[i,j,k,l,i5,i6,i7]
initial(Llate[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinitL[i,j,k,l,i5,i6,i7]
initial(Asymp[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinitA[i,j,k,l,i5,i6,i7]
initial(Symp[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinitS[i,j,k,l,i5,i6,i7]
initial(Treat[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinitT[i,j,k,l,i5,i6,i7]



## == model dynamics

## --- TB parameters
staticfoi <- user(0)
foi <- user(5e-3)
stabilization <- user(0.23) #stabilization rate early -> late TBI
progn_slow <- user(0.003)    #slow progression rate
progn_fast <- user(0.1)    #fast progression rate
progn_symp <- user(0.5)    #symptom progression rate
detect_asymp <- user(0.0)  #detection hazard, asymptomatic
symptb_inversedurn <- user(3.0)
symptb_CFR <- user(0.5)
mortality_treated <- user(0.1)        #CFR treated TB
treatment_inversedurn <- user(2)      #ATM because o/w leak
progn_posttb[] <- user(0)               #progression thru layers of posttb
relapse <- user(1e-2)                 #relapse rate
tbi_protn <- user(0.2)                #protection TBI+ TODO incorporate

## array rates:
CDR_raw[, , , , , , , ] <- user() #
dim(CDR_raw) <- c(lttp, nage, 2, nnat, nrisk, npost, nstrain, nprot)
CDR_array[, , , , , , ] <- interpolate(ttp, CDR_raw, "linear")

## --- FOI calculations
## individual component inputs
BETAage[, ] <- user()
dim(BETAage) <- c(nage, nage)
BETAsex[, ] <- user()
dim(BETAsex) <- c(2, 2)
BETAnat[, ] <- user()
dim(BETAnat) <- c(nnat, nnat)
BETArisk[, ] <- user()
dim(BETArisk) <- c(nrisk, nrisk)
## BETApost[, ] <- user()
## dim(BETApost) <- c(npost, npost)
BETAstrain[, ] <- user()
dim(BETAstrain) <- c(nstrain, nstrain)
## BETAprot[, ] <- user()
## dim(BETAprot) <- c(nprot, nprot)
## prevalence across relevant indices: NOTE this denominator so that all 1s in BETA matrices equals random mixing
asymp_relinf <- user(1) #relative infectiousness of asymptomatic
PI5[1:nage, 1:2, 1:nnat, 1:nrisk, 1:nstrain] <- (asymp_relinf * sum(Asymp[i, j, k, l, , i5, ]) + sum(Symp[i, j, k, l, , i5, ])) / (sum(totalpops) + tol)
dim(PI5) <- c(nage, 2, nnat, nrisk, nstrain)
## treated as tensor product:
Ha0[1:nage, 1:nage, 1:2, 1:nnat, 1:nrisk, 1:nstrain] <- BETAage[i, j] * PI5[j, k, l, i5, i6]
dim(Ha0) <- c(nage, nage, 2, nnat, nrisk, nstrain)
Ha[1:nage, 1:2, 1:nnat, 1:nrisk, 1:nstrain] <- sum(Ha0[i,,j,k,l,i5]) #matrix product with BETAage
dim(Ha) <- c(nage, 2, nnat, nrisk, nstrain)
Hs0[1:nage, 1:2, 1:2, 1:nnat, 1:nrisk, 1:nstrain] <- BETAsex[j, k] * Ha[i, k, l, i5, i6]
dim(Hs0) <- c(nage, 2, 2, nnat, nrisk, nstrain)
Hs[1:nage, 1:2, 1:nnat, 1:nrisk, 1:nstrain] <- sum(Hs0[i, j, , k, l, i5]) # matrix product with BETAsex
dim(Hs) <- c(nage, 2, nnat, nrisk, nstrain)
Hn0[1:nage, 1:2, 1:nnat, 1:nnat, 1:nrisk, 1:nstrain] <- BETAnat[k, l] * Hs[i, j, l, i5, i6]
dim(Hn0) <- c(nage, 2, nnat, nnat, nrisk, nstrain)
Hn[1:nage, 1:2, 1:nnat, 1:nrisk, 1:nstrain] <- sum(Hn0[i, j, k, , l, i5]) # matrix product with BETAnat
dim(Hn) <- c(nage, 2, nnat, nrisk, nstrain)
Hr0[1:nage, 1:2, 1:nnat, 1:nrisk, 1:nrisk, 1:nstrain] <- BETArisk[l,i5] * Hn[i, j, k, i5, i6]
dim(Hr0) <- c(nage, 2, nnat, nrisk, nrisk, nstrain)
Hr[1:nage, 1:2, 1:nnat, 1:nrisk, 1:nstrain] <- sum(Hr0[i, j, k, l, , i5]) # matrix product with BETArisk
dim(Hr) <- c(nage, 2, nnat, nrisk, nstrain)
Ht0[1:nage, 1:2, 1:nnat, 1:nrisk, 1:nrisk, 1:nstrain] <- BETAstrain[i5, i6] * Hr[i, j, k, i5, i6]
dim(Ht0) <- c(nage, 2, nnat, nrisk, nrisk, nstrain)
Ht[1:nage, 1:2, 1:nnat, 1:nrisk, 1:nstrain] <- sum(Ht0[i, j, k, l, i5, ]) # matrix product with BETAstrain
## Ht[1:nage, 1:2, 1:nnat, 1:nrisk, 1:nstrain] <- foi #testing
dim(Ht) <- c(nage, 2, nnat, nrisk, nstrain)
## hazard of infection
HI[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- if (staticfoi > 0) foi else Ht[i, j, k, l, i6]
dim(HI) <- c(nage, 2, nnat, nrisk, npost, nstrain, nprot)


## TB outcomes
## a = 1 / d = rate out with no detection
## ATT : no-ATT = CDR : (1-CDR)
## total rate w/detection = a + b; CDR=b/(a+b) -> b = CDR/(1-CDR)* a -> a+b = 1/(1-CDR) / d
## 1 / ((1-CDR)*d) = rate out with detection
## notes ~ b * D = (CDR/(1-CDR)) * D / d
## -> non-notes ~  D / d
## deaths = cfr * D / d; self-cure = (1-cfr) * D / d

detect_symp[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- symptb_inversedurn * Symp[i,j,k,l,i5,i6,i7] * CDR_array[i,j,k,l,i5,i6,i7]/(1-CDR_array[i,j,k,l,i5,i6,i7]) #=detection
symp_tbend[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- symptb_inversedurn * Symp[i, j, k, l, i5, i6, i7] / (1 - CDR_array[i, j, k, l, i5, i6, i7]) # =selfcure + mortality + detection
dim(CDR_array) <-c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(detect_symp) <-c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(symp_tbend) <- c(nage, 2, nnat, nrisk, npost, nstrain, nprot)


## intervention interpolations:
## TODO detection (think costs over time?)
## vaccination/TPT
## treatment outcomes


## TODO think FPs
## TODO post-TB and relapse

## --- TB dynamics
## Uninfected
deriv(Uninfected[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- demogU[i,j,k,l,i5,i6,i7] - HI[i,j,k,l,i5,i6,i7] * Uninfected[i,j,k,l,i5,i6,i7]

## Early TBI
deriv(Learly[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot]) <- demogE[i, j, k, l, i5, i6, i7] + HI[i,j,k,l,i5,i6,i7] * Uninfected[i, j, k, l, i5, i6, i7] - stabilization * Learly[i, j, k, l, i5, i6, i7] - progn_fast * Learly[i, j, k, l, i5, i6, i7] + HI[i,j,k,l,i5,i6,i7] * tbi_protn * Llate[i, j, k, l, i5, i6, i7]

## Late TBI
deriv(Llate[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot]) <- demogL[i, j, k, l, i5, i6, i7] + stabilization * Learly[i, j, k, l, i5, i6, i7] - progn_slow * Llate[i, j, k, l, i5, i6, i7] + fromtreatmentL[i, j, k, l, i5, i6, i7] - relapsefrompost[i, j, k, l, i5, i6, i7] - HI[i,j,k,l,i5,i6,i7] * tbi_protn * Llate[i, j, k, l, i5, i6, i7] + (1 - symptb_CFR) * symptb_inversedurn * Symp[i, j, k, l, i5, i6, i7]
## TODO wiring to post TB for undetected?

## Asymptomatic TB disease
deriv(Asymp[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- demogA[i,j,k,l,i5,i6,i7] + progn_fast * Learly[i,j,k,l,i5,i6,i7] + progn_slow * Llate[i,j,k,l,i5,i6,i7] - progn_symp * Asymp[i,j,k,l,i5,i6,i7] - detect_asymp * Asymp[i,j,k,l,i5,i6,i7] + fromtreatmentA[i,j,k,l,i5,i6,i7] + relapsefrompost[i,j,k,l,i5,i6,i7]

## Symptomatic TB disease
deriv(Symp[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- demogS[i,j,k,l,i5,i6,i7] + progn_symp * Asymp[i,j,k,l,i5,i6,i7] - symp_tbend[i,j,k,l,i5,i6,i7]

## currently on ATT
deriv(Treat[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- demogT[i,j,k,l,i5,i6,i7] + treatmentstarts[i,j,k,l,i5,i6,i7] - treatmentends[i,j,k,l,i5,i6,i7] #TODO


##  l = risk group (optional)
## TB:
##  i5 = post-TB   (compulsory)
##  i6 = strain    (optional)
##  i7 = protection from TPT + vaccination (optional)



## --- monitored rates
## capture notifications
output(rate_Notification[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- 1e5 * treatmentstarts[i,j,k,l,i5,i6,i7] / (totalpops[i,j,k,l,i5,i6,i7] + tol)


## TODO check how it plays with current processing
rate_TBmortality[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- mortality_treated * treatmentends[i, j, k, l, i5, i6, i7] + symptb_CFR * symptb_inversedurn * Symp[i, j, k, l, i5, i6, i7]
output(rate_TBmortality) <- TRUE
output(rate_Incidence[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <-  1e5*(progn_fast * Learly[i,j,k,l,i5,i6,i7] + progn_slow * Llate[i,j,k,l,i5,i6,i7]) /(totalpops[i,j,k,l,i5,i6,i7]+tol)

dim(rate_Incidence) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(rate_Notification) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(rate_TBmortality) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)


## --- interim quantities
treatmentstarts[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- detect_asymp * Asymp[i,j,k,l,i5,i6,i7] +  detect_symp[i,j,k,l,i5,i6,i7]
totalpops[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- Uninfected[i,j,k,l,i5,i6,i7] + Learly[i,j,k,l,i5,i6,i7] + Llate[i,j,k,l,i5,i6,i7] + Asymp[i,j,k,l,i5,i6,i7] + Symp[i,j,k,l,i5,i6,i7] + Treat[i,j,k,l,i5,i6,i7]

## treatent ends & destinations
treatmentends[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- treatment_inversedurn * Treat[i,j,k,l,i5,i6,i7]
fromtreatmentL[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nprot,1:nprot] <- if(i5==2) sum(treatmentends[i,j,k,l,1:npost,i6,i7]) else if(npost==1) (1-relapse) * treatmentends[i,j,k,l,i5,i6,i7] / (1-mortality_treated) else 0 #if post-TB layers, put in
fromtreatmentA[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(npost==1) relapse * treatmentends[i,j,k,l,i5,i6,i7] / (1-mortality_treated) else 0 #if no post-TB layers, put here
relapsefrompost[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i5==2) relapse * Llate[i,j,k,l,i5,i6,i7] else 0

## NOTE on relapse (see above):
## if npost = 1, relapse will go from T -> A
## if npost > 1, relapse will go from L x (post == 1)
## note that relapse is treated as probability for npost==1 and as rate for npost>1

## TODO post-TB progression and relapse from post-TB
## TODO convertions CFRs and mortality hazards
## TODO tot up TB mortality and also introduce a correction to overall mortality


## dims
dim(treatmentstarts) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(totalpops) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(treatmentends) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(fromtreatmentL) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(fromtreatmentA) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(relapsefrompost) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)

## --- corrections to omega from migration and TB mortality:
## total population by stratum
dim(totalpop) <- c(nage, 2, nnat, nrisk, npost, nstrain, nprot)
totalpop[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- Uninfected[i, j, k, l, i5, i6, i7] + Learly[i, j, k, l, i5, i6, i7] + Llate[i, j, k, l, i5, i6, i7] + Asymp[i, j, k, l, i5, i6, i7] + Symp[i, j, k, l, i5, i6, i7] + Treat[i, j, k, l, i5, i6, i7]
## total exits by stratum
dim(totalexits) <- c(nage, 2, nnat, nrisk, npost, nstrain, nprot)
totalexits[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- OutR[i, j, k] * totalpop[i, j, k, l, i5, i6, i7]
## total/average by age/sex
dim(NAS) <- c(nage, 2)
NAS[1:nage, 1:2] <- sum(totalpop[i, j, , , , , ]) #total pop by age/sex
dim(ExitAS) <- c(nage, 2)
ExitAS[1:nage, 1:2] <- sum(totalexits[i, j, , , , , ]) # total emigration rate by age/sex
dim(TBdeathsAS) <- c(nage, 2)
TBdeathsAS[1:nage, 1:2] <- sum(rate_TBmortality[i, j, , , , , ])
## corrected omega
omegac[1:nage, 1:2] <- omega[i,j] + In[i,j] / NAS[i,j] - ExitAS[i,j] / NAS[i,j] - TBdeathsAS[i,j] / NAS[i,j]



## --- demographic dynamics summarizers
demogU[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- ageinU[i, j, k, l, i5, i6, i7] - omegac[i, j] * Uninfected[i, j, k, l, i5, i6, i7] + migrU[i, j, k, l, i5, i6, i7] - OutR[i, j, k] * Uninfected[i, j, k, l, i5, i6, i7] + migrageinU[i, j, k, l, i5, i6, i7] - migrageoutU[i, j, k, l, i5, i6, i7] + RiskChangeU[i, j, k, l, i5, i6, i7] + PostTBprognU[i, j, k, l, i5, i6, i7]
demogE[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- ageinE[i, j, k, l, i5, i6, i7] - omegac[i, j] * Learly[i, j, k, l, i5, i6, i7] + migrE[i, j, k, l, i5, i6, i7] - OutR[i, j, k] * Learly[i, j, k, l, i5, i6, i7] + migrageinE[i, j, k, l, i5, i6, i7] - migrageoutE[i, j, k, l, i5, i6, i7] + RiskChangeE[i, j, k, l, i5, i6, i7] + PostTBprognE[i, j, k, l, i5, i6, i7]
demogL[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- ageinL[i, j, k, l, i5, i6, i7] - omegac[i, j] * Llate[i, j, k, l, i5, i6, i7] + migrL[i, j, k, l, i5, i6, i7] - OutR[i, j, k] * Llate[i, j, k, l, i5, i6, i7] + migrageinL[i, j, k, l, i5, i6, i7] - migrageoutL[i, j, k, l, i5, i6, i7] + RiskChangeL[i, j, k, l, i5, i6, i7] + PostTBprognL[i, j, k, l, i5, i6, i7]
demogA[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- ageinA[i, j, k, l, i5, i6, i7] - omegac[i, j] * Asymp[i, j, k, l, i5, i6, i7] + migrA[i, j, k, l, i5, i6, i7] - OutR[i, j, k] * Asymp[i, j, k, l, i5, i6, i7] + migrageinA[i, j, k, l, i5, i6, i7] - migrageoutA[i, j, k, l, i5, i6, i7] + RiskChangeA[i, j, k, l, i5, i6, i7] + PostTBprognA[i, j, k, l, i5, i6, i7]
demogS[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- ageinS[i, j, k, l, i5, i6, i7] - omegac[i, j] * Symp[i, j, k, l, i5, i6, i7] + migrS[i, j, k, l, i5, i6, i7] - OutR[i, j, k] * Symp[i, j, k, l, i5, i6, i7] + migrageinS[i, j, k, l, i5, i6, i7] - migrageoutS[i, j, k, l, i5, i6, i7] + RiskChangeS[i, j, k, l, i5, i6, i7] + PostTBprognS[i, j, k, l, i5, i6, i7]
demogT[1:nage, 1:2, 1:nnat, 1:nrisk, 1:npost, 1:nstrain, 1:nprot] <- ageinT[i, j, k, l, i5, i6, i7] - omegac[i, j] * Treat[i, j, k, l, i5, i6, i7] + migrT[i, j, k, l, i5, i6, i7] - OutR[i, j, k] * Treat[i, j, k, l, i5, i6, i7] + migrageinT[i, j, k, l, i5, i6, i7] - migrageoutT[i, j, k, l, i5, i6, i7] + RiskChangeT[i, j, k, l, i5, i6, i7] + PostTBprognT[i, j, k, l, i5, i6, i7]

dim(demogU) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(demogE) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(demogL) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(demogA) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(demogS) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(demogT) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)

dim(Uninfected) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(Learly) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(Llate) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(Asymp) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(Symp) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(Treat) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)




## == dims for demography
dim(ttp) <- user()                    #note need this before length() use
lttp <- length(ttp)
dim(popdat) <- c(lttp,nage,2)
dim(BB) <- c(lttp,2)
dim(omega) <- c(nage, 2)
dim(omegac) <- c(nage, 2) #corrected omega
dim(bz) <- c(2)
dim(native) <- nnat
dim(birthrisk) <- nrisk
dim(r) <- nage
dim(tbbottom) <- c(npost,nstrain,nprot)
dim(progn_posttb) <- npost

dim(ageinU) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(ageinE) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(ageinL) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(ageinA) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(ageinS) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(ageinT) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)

dim(popinitU) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(popinitE) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(popinitL) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(popinitA) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(popinitS) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(popinitT) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)

## == specific additional aspects:

## -- migration dynamics
## outside of model need to correct omega -> omega + Ipc where Ipc is In migration per capita
migrage[] <- user() #migration ageing rate
In[,] <- interpolate(ttp,immigration,"linear")
OutR[, , ] <- interpolate(ttp, exmigrate, "linear")

## where to migrants flow into
Pmigr_risk[,] <- user()
Pmigr_post[,] <- user()
Pmigr_strain[,] <- user()
Pmigr_prot[,] <- user()
immigration[,,] <- user()
exmigrate[,,,] <- user()
migr_TBD[,] <- user()
migr_TBI[,] <- user()

## where to migrants flow into
Pmigr_nat[1:nnat] <- if(i==2) 1 else 0

## TODO may wish to adjust splits A/S and E/L
migrU[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- (1-migr_TBI[i,j]-migr_TBD[i,j]) * In[i,j] * Pmigr_nat[k] * Pmigr_risk[j,l] * Pmigr_post[j,i5] * Pmigr_strain[j,i6] * Pmigr_prot[j,i7]
migrE[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- 0.2 * migr_TBI[i,j] * In[i,j] * Pmigr_nat[k] * Pmigr_risk[j,l] * Pmigr_post[j,i5] * Pmigr_strain[j,i6] * Pmigr_prot[j,i7]
migrL[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- 0.8 * migr_TBI[i,j] * In[i,j] * Pmigr_nat[k] * Pmigr_risk[j,l] * Pmigr_post[j,i5] * Pmigr_strain[j,i6] * Pmigr_prot[j,i7]
migrA[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- 0.5 * migr_TBD[i,j] * In[i,j] * Pmigr_nat[k] * Pmigr_risk[j,l] * Pmigr_post[j,i5] * Pmigr_strain[j,i6] * Pmigr_prot[j,i7]
migrS[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- 0.5 * migr_TBD[i,j] * In[i,j] * Pmigr_nat[k] * Pmigr_risk[j,l] * Pmigr_post[j,i5] * Pmigr_strain[j,i6] * Pmigr_prot[j,i7]
migrT[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- 0


migrageinU[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>2) Uninfected[i,j,k-1,l,i5,i6,i7] * migrage[k-1] else 0
migrageoutU[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>1 && k<nnat) Uninfected[i,j,k,l,i5,i6,i7] * migrage[k] else 0
migrageinE[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>2) Learly[i,j,k-1,l,i5,i6,i7] * migrage[k-1] else 0
migrageoutE[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>1 && k<nnat) Learly[i,j,k,l,i5,i6,i7] * migrage[k] else 0
migrageinL[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>2) Llate[i,j,k-1,l,i5,i6,i7] * migrage[k-1] else 0
migrageoutL[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>1 && k<nnat) Llate[i,j,k,l,i5,i6,i7] * migrage[k] else 0
migrageinA[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>2) Asymp[i,j,k-1,l,i5,i6,i7] * migrage[k-1] else 0
migrageoutA[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>1 && k<nnat) Asymp[i,j,k,l,i5,i6,i7] * migrage[k] else 0
migrageinS[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>2) Symp[i,j,k-1,l,i5,i6,i7] * migrage[k-1] else 0
migrageoutS[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>1 && k<nnat) Symp[i,j,k,l,i5,i6,i7] * migrage[k] else 0
migrageinT[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>2) Treat[i,j,k-1,l,i5,i6,i7] * migrage[k-1] else 0
migrageoutT[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>1 && k<nnat) Treat[i,j,k,l,i5,i6,i7] * migrage[k] else 0


## dimensions for migration dynamics
dim(migrage) <- nnat
dim(Pmigr_nat) <- nnat
dim(Pmigr_risk) <- c(2,nrisk)
dim(Pmigr_post) <- c(2,npost)
dim(Pmigr_strain) <- c(2,nstrain)
dim(Pmigr_prot) <- c(2,nprot)
dim(immigration) <- c(lttp, nage, 2)
dim(exmigrate) <- c(lttp, nage, 2, nnat)
dim(In) <- c(nage,2)
dim(OutR) <- c(nage, 2, nnat)
dim(migr_TBD) <- c(nage,2)
dim(migr_TBI) <- c(nage,2)



dim(migrU) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrE) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrL) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrA) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrS) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrT) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)


dim(migrageinU) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageoutU) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageinE) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageoutE) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageinL) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageoutL) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageinA) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageoutA) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageinS) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageoutS) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageinT) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageoutT) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)


## -- risk strata dynamics
RiskHazardData[,,,] <- user()
RiskHazard[,,] <- interpolate(ttp,RiskHazardData,"linear")
dim(RiskHazardData) <- c(lttp,nage,2,nrisk)
dim(RiskHazard) <-  c(nage,2,nrisk)

RiskChangeU[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(l>1) RiskHazard[i,j,l-1] * Uninfected[i,j,k,l-1,i5,i6,i7]-RiskHazard[i,j,l] * Uninfected[i,j,k,l,i5,i6,i7] else -RiskHazard[i,j,l] * Uninfected[i,j,k,l,i5,i6,i7]
RiskChangeE[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(l>1) RiskHazard[i,j,l-1] * Learly[i,j,k,l-1,i5,i6,i7]-RiskHazard[i,j,l] * Learly[i,j,k,l,i5,i6,i7] else -RiskHazard[i,j,l] * Learly[i,j,k,l,i5,i6,i7]
RiskChangeL[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(l>1) RiskHazard[i,j,l-1] * Llate[i,j,k,l-1,i5,i6,i7]-RiskHazard[i,j,l] * Llate[i,j,k,l,i5,i6,i7] else -RiskHazard[i,j,l] * Llate[i,j,k,l,i5,i6,i7]
RiskChangeA[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(l>1) RiskHazard[i,j,l-1] * Asymp[i,j,k,l-1,i5,i6,i7]-RiskHazard[i,j,l] * Asymp[i,j,k,l,i5,i6,i7] else -RiskHazard[i,j,l] * Asymp[i,j,k,l,i5,i6,i7]
RiskChangeS[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(l>1) RiskHazard[i,j,l-1] * Symp[i,j,k,l-1,i5,i6,i7]-RiskHazard[i,j,l] * Symp[i,j,k,l,i5,i6,i7] else -RiskHazard[i,j,l] * Symp[i,j,k,l,i5,i6,i7]
RiskChangeT[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(l>1) RiskHazard[i,j,l-1] * Treat[i,j,k,l-1,i5,i6,i7]-RiskHazard[i,j,l] * Treat[i,j,k,l,i5,i6,i7] else -RiskHazard[i,j,l] * Treat[i,j,k,l,i5,i6,i7]

dim(RiskChangeU) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(RiskChangeE) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(RiskChangeL) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(RiskChangeA) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(RiskChangeS) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(RiskChangeT) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)

## -- posttb layer progression
## NOTE ensure that progn_posttb[npost] == 0 and progn_posttb[1]==0
PostTBprognU[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i5>1) progn_posttb[i5-1] * Uninfected[i,j,k,l,i5-1,i6,i7]-progn_posttb[i5] * Uninfected[i,j,k,l,i5,i6,i7] else 0
PostTBprognE[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i5>1) progn_posttb[i5-1] * Learly[i,j,k,l,i5-1,i6,i7]-progn_posttb[i5] * Learly[i,j,k,l,i5,i6,i7] else 0
PostTBprognL[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i5>1) progn_posttb[i5-1] * Llate[i,j,k,l,i5-1,i6,i7]-progn_posttb[i5] * Llate[i,j,k,l,i5,i6,i7] else 0
PostTBprognA[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i5>1) progn_posttb[i5-1] * Asymp[i,j,k,l,i5-1,i6,i7]-progn_posttb[i5] * Asymp[i,j,k,l,i5,i6,i7] else 0
PostTBprognS[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i5>1) progn_posttb[i5-1] * Symp[i,j,k,l,i5-1,i6,i7]-progn_posttb[i5] * Symp[i,j,k,l,i5,i6,i7] else 0
PostTBprognT[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i5>1) progn_posttb[i5-1] * Treat[i,j,k,l,i5-1,i6,i7]-progn_posttb[i5] * Treat[i,j,k,l,i5,i6,i7] else 0

dim(PostTBprognU) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(PostTBprognE) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(PostTBprognL) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(PostTBprognA) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(PostTBprognS) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(PostTBprognT) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)

