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
popdatF[,] <- user()
popdatM[,] <- user()
## initial state
popinitM[,,,,,] <- user()
popinitF[,,,,,] <- user()
## birth rates
BF[] <- user()
BM[] <- user()
ttp[] <- user()
## interpolation
omegaF[] <- interpolate(ttp,popdatF,"linear")
omegaM[] <- interpolate(ttp,popdatM,"linear")
bzf <- interpolate(ttp,BF,"linear")
bzm <- interpolate(ttp,BM,"linear")
## useful masks:
native[1] <- 1
native[2:nnat] <- 0
tbbottom[1:npost,1:nstrain,1:nprot] <- 0
tbbottom[1,1,1] <- 1 #CHECK overwrite works : only 1 for tb indices = 1

## NOTE once TB states are introduced, will need to handle U differently for strains
## == initial state
initial(N[1:nage,1,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinitM[i,k,l,i5,i6,i7]
initial(N[1:nage,2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinitF[i,k,l,i5,i6,i7]

## == model dynamics
## male
deriv(N[1,1,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- bzm * native[k] * birthrisk[l] * tbbottom[i5,i6,i7] - omegaM[1] * N[1,1,k,l,i5,i6,i7] + migrM[1,k,l,i5,i6,i7] + migrMagein[1,k,l,i5,i6,i7] - migrMageout[1,k,l,i5,i6,i7]
deriv(N[2:nage,1,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- r[i-1] * N[i-1,1,k,l,i5,i6,i7] - omegaM[i] * N[i,1,k,l,i5,i6,i7] + migrM[i,k,l,i5,i6,i7] + migrMagein[i,k,l,i5,i6,i7] - migrMageout[i,k,l,i5,i6,i7] + RiskChange[i,1,k,l,i5,i6,i7]
## female
deriv(N[1,2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- bzf * native[k] * birthrisk[l] * tbbottom[i5,i6,i7] - omegaF[1] * N[1,2,k,l,i5,i6,i7] + migrF[1,k,l,i5,i6,i7] + migrFagein[1,k,l,i5,i6,i7] - migrFageout[1,k,l,i5,i6,i7]
deriv(N[2:nage,2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- r[i-1] * N[i-1,2,k,l,i5,i6,i7] - omegaF[i] * N[i,2,k,l,i5,i6,i7] + migrF[i,k,l,i5,i6,i7] + migrFagein[i,k,l,i5,i6,i7] - migrFageout[i,k,l,i5,i6,i7] + RiskChange[i,2,k,l,i5,i6,i7]


## == dims for demography
dim(ttp) <- user()                    #note need this before length() use
lttp <- length(ttp)
dim(popdatF) <- c(lttp,nage)
dim(popdatM) <- c(lttp,nage)
dim(popinitF) <- c(nage,nnat,nrisk,npost,nstrain,nprot)
dim(popinitM) <- c(nage,nnat,nrisk,npost,nstrain,nprot)
dim(BF) <- lttp
dim(BM) <- lttp
dim(omegaF) <- nage
dim(omegaM) <- nage
## dim(firstage) <- nage
dim(native) <- nnat
## dim(lorisk) <- nrisk
dim(birthrisk) <- nrisk
dim(N) <- c(nage,2,nnat,nrisk,npost,nstrain,nprot)
dim(r) <- nage
dim(tbbottom) <- c(npost,nstrain,nprot)

## == specific additional aspects:

## -- migration dynamics
## outside of model need to correct omega -> omega + Ipc where Ipc is In migration per capita
migrage[] <- user() #migration ageing rate
InM[] <- interpolate(ttp,immigration_male,"linear")
InF[] <- interpolate(ttp,immigration_female,"linear")
## where to migrants flow into
## M
PmigrM_risk[] <- user()
PmigrM_post[] <- user()
PmigrM_strain[] <- user()
PmigrM_prot[] <- user()
immigration_male[,] <- user()
## F
PmigrF_risk[] <- user()
PmigrF_post[] <- user()
PmigrF_strain[] <- user()
PmigrF_prot[] <- user()
immigration_female[,] <- user()

## where to migrants flow into
Pmigr_nat[1:nnat] <- if(i==2) 1 else 0
migrM[1:nage,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- InM[i] * Pmigr_nat[j] * PmigrM_risk[k] * PmigrM_post[l] * PmigrM_strain[i5] * PmigrM_prot[i6]
migrF[1:nage,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- InF[i] * Pmigr_nat[j] * PmigrF_risk[k] * PmigrF_post[l] * PmigrF_strain[i5] * PmigrF_prot[i6]

## migration ageing
migrMagein[1:nage,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(j>2) N[i,1,j-1,k,l,i5,i6] * migrage[j-1] else 0
migrMageout[1:nage,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(j>1 && j<nnat) N[i,1,j,k,l,i5,i6] * migrage[j] else 0
migrFagein[1:nage,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(j>2) N[i,2,j-1,k,l,i5,i6] * migrage[j-1] else 0
migrFageout[1:nage,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(j>1 && j<nnat) N[i,2,j,k,l,i5,i6] * migrage[j] else 0

## dimensions for migration dynamics
dim(migrage) <- nnat
dim(Pmigr_nat) <- nnat
## M
dim(PmigrM_risk) <- nrisk
dim(PmigrM_post) <- npost
dim(PmigrM_strain) <- nstrain
dim(PmigrM_prot) <- nprot
dim(immigration_male) <- c(lttp,nage)
dim(InM) <- nage
dim(migrM) <- c(nage,nnat,nrisk,npost,nstrain,nprot)
dim(migrMagein) <- c(nage,nnat,nrisk,npost,nstrain,nprot)
dim(migrMageout) <- c(nage,nnat,nrisk,npost,nstrain,nprot)
## F
dim(PmigrF_risk) <- nrisk
dim(PmigrF_post) <- npost
dim(PmigrF_strain) <- nstrain
dim(PmigrF_prot) <- nprot
dim(immigration_female) <- c(lttp,nage)
dim(InF) <- nage
dim(migrF) <- c(nage,nnat,nrisk,npost,nstrain,nprot)
dim(migrFagein) <- c(nage,nnat,nrisk,npost,nstrain,nprot)
dim(migrFageout) <- c(nage,nnat,nrisk,npost,nstrain,nprot)

## -- risk strata dynamics
RiskHazardData[,,,] <- user()
RiskHazard[,,] <- interpolate(ttp,RiskHazardData,"linear")
RiskChange[1:nage,1:2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot] <- if(l>1) RiskHazard[i,j,l-1] * N[i,j,k,l-1,i5,i6,i7]-RiskHazard[i,j,l] * N[i,j,k,l,i5,i6,i7] else -RiskHazard[i,j,l] * N[i,j,k,l,i5,i6,i7]
dim(RiskHazardData) <- c(lttp,nage,2,nrisk)
dim(RiskHazard) <-  c(nage,2,nrisk)
dim(RiskChange) <-  c(nage,2,nnat,nrisk,npost,nstrain,nprot)

