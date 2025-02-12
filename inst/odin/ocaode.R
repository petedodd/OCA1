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
popinit[,,,,,,] <- user() #initial population
popdat[,,] <- user()      #population dynamics
BB[,] <- user()           #birth rate
ttp[] <- user()           #time points for data
## interpolation
bz[] <- interpolate(ttp,BB,"linear")
omega[,] <- interpolate(ttp,popdat,"linear")

## useful masks:
native[1] <- 1
native[2:nnat] <- 0
tbbottom[1:npost,1:nstrain,1:nprot] <- 0
tbbottom[1,1,1] <- 1 #CHECK overwrite works : only 1 for tb indices = 1
agein[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(i>1) r[i-1] * N[i-1,j,k,l,i5,i6,i7] else bz[j] * native[k] * birthrisk[l] * tbbottom[i5,i6,i7]

## NOTE once TB states are introduced, will need to handle U differently for strains
## == initial state
initial(N[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinit[i,j,k,l,i5,i6,i7]

## == model dynamics
deriv(N[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- agein[i,j,k,l,i5,i6,i7] - omega[i,j] * N[i,j,k,l,i5,i6,i7] + migr[i,j,k,l,i5,i6,i7] + migragein[i,j,k,l,i5,i6,i7] - migrageout[i,j,k,l,i5,i6,i7] + RiskChange[i,j,k,l,i5,i6,i7]


## == dims for demography
dim(ttp) <- user()                    #note need this before length() use
lttp <- length(ttp)
dim(popdat) <- c(lttp,nage,2)
dim(popinit) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(BB) <- c(lttp,2)
dim(omega) <- c(nage,2)
dim(bz) <- c(2)
dim(native) <- nnat
dim(birthrisk) <- nrisk
dim(N) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(r) <- nage
dim(tbbottom) <- c(npost,nstrain,nprot)
dim(agein) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)

## == specific additional aspects:

## -- migration dynamics
## outside of model need to correct omega -> omega + Ipc where Ipc is In migration per capita
migrage[] <- user() #migration ageing rate
In[,] <- interpolate(ttp,immigration,"linear")

## where to migrants flow into
Pmigr_risk[,] <- user()
Pmigr_post[,] <- user()
Pmigr_strain[,] <- user()
Pmigr_prot[,] <- user()
immigration[,,] <- user()

## where to migrants flow into
Pmigr_nat[1:nnat] <- if(i==2) 1 else 0
migr[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- In[i,j] * Pmigr_nat[k] * Pmigr_risk[j,l] * Pmigr_post[j,i5] * Pmigr_strain[j,i6] * Pmigr_prot[j,i7]

migragein[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>2) N[i,j,k-1,l,i5,i6,i7] * migrage[k-1] else 0
migrageout[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(k>1 && k<nnat) N[i,j,k,l,i5,i6,i7] * migrage[k] else 0

## dimensions for migration dynamics
dim(migrage) <- nnat
dim(Pmigr_nat) <- nnat
dim(Pmigr_risk) <- c(2,nrisk)
dim(Pmigr_post) <- c(2,npost)
dim(Pmigr_strain) <- c(2,nstrain)
dim(Pmigr_prot) <- c(2,nprot)
dim(immigration) <- c(lttp,nage,2)
dim(In) <- c(nage,2)
dim(migr) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migragein) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(migrageout) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)


## -- risk strata dynamics
RiskHazardData[,,,] <- user()
RiskHazard[,,] <- interpolate(ttp,RiskHazardData,"linear")
RiskChange[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(l>1) RiskHazard[i,j,l-1] * N[i,j,k,l-1,i5,i6,i7]-RiskHazard[i,j,l] * N[i,j,k,l,i5,i6,i7] else -RiskHazard[i,j,l] * N[i,j,k,l,i5,i6,i7]
dim(RiskHazardData) <- c(lttp,nage,2,nrisk)
dim(RiskHazard) <-  c(nage,2,nrisk)
dim(RiskChange) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)

