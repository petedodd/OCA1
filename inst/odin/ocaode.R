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

## == inputs & interpolation
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
omegaF[] <- interpolate(ttp,popdatF,'linear')
omegaM[] <- interpolate(ttp,popdatM,'linear')
bzf <- interpolate(ttp,BF,'linear')
bzm <- interpolate(ttp,BM,'linear')
## useful masks:
native[1] <- 1
native[2:nnat] <- 0
tbbottom[1:npost,1:nstrain,1:nprot] <- 0
tbbottom[1,1,1] <- 1 #CHECK overwrite works : only 1 for tb indices = 1

## NOTE once TB states are introduced, will need to handle U differently for strains
## == initial state
initial(N[1:nage,1,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinitM[i,k,l,i5,i6,i7]
initial(N[1:nage,2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- popinitF[i,k,l,i5,i6,i7]

## == dynamics
## male
deriv(N[1,1,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- bzm * native[k] * birthrisk[l] * tbbottom[i5,i6,i7] - omegaM[1] * N[1,1,k,l,i5,i6,i7]
deriv(N[2:nage,1,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- r[i-1] * N[i-1,1,k,l,i5,i6,i7] - omegaM[i] * N[i,1,k,l,i5,i6,i7]
## female
deriv(N[1,2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- bzf * native[k] * birthrisk[l] * tbbottom[i5,i6,i7] - omegaF[1] * N[1,2,k,l,i5,i6,i7]
deriv(N[2:nage,2,1:nnat,1:nrisk,1:npost,1:nstrain,1:nprot]) <- r[i-1] * N[i-1,2,k,l,i5,i6,i7] - omegaF[i] * N[i,2,k,l,i5,i6,i7]

## == dims
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
