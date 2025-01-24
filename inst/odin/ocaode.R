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
birthrisk[] <- user()                       #which risk at birth?
r[] <- user()                             #aging rate
## demographic aging/mortality/net migration
popdatF[,] <- user()
popdatM[,] <- user()
## initial state
popinitM[,,] <- user()
popinitF[,,] <- user()
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
## firstage[1] <- 1
## firstage[2:nage] <- 0
native[1] <- 1
native[2:nnat] <- 0
## lorisk[1] <- 1
## lorisk[2:nrisk] <- 0

## == initial state
initial(N[1:nage,1,1:nnat,1:nrisk]) <- popinitM[i,k,l]
initial(N[1:nage,2,1:nnat,1:nrisk]) <- popinitF[i,k,l]

## == dynamics
## male
deriv(N[1,1,1:nnat,1:nrisk]) <- bzm * native[k] * birthrisk[l] - omegaM[1] * N[1,1,k,l]
deriv(N[2:nage,1,1:nnat,1:nrisk]) <- r[i-1] * N[i-1,1,k,l] - omegaM[i] * N[i,1,k,l]
## female
deriv(N[1,2,1:nnat,1:nrisk]) <- bzf * native[k] * birthrisk[l] - omegaF[1] * N[1,2,k,l]
deriv(N[2:nage,2,1:nnat,1:nrisk]) <- r[i-1] * N[i-1,2,k,l] - omegaF[i] * N[i,2,k,l]

## == dims
dim(ttp) <- user()                    #note need this before length() use
lttp <- length(ttp)
dim(popdatF) <- c(lttp,nage)
dim(popdatM) <- c(lttp,nage)
dim(popinitF) <- c(nage,nnat,nrisk)
dim(popinitM) <- c(nage,nnat,nrisk)
dim(BF) <- lttp
dim(BM) <- lttp
dim(omegaF) <- nage
dim(omegaM) <- nage
## dim(firstage) <- nage
dim(native) <- nnat
## dim(lorisk) <- nrisk
dim(birthrisk) <- nrisk
dim(N) <- c(nage,2,nnat,nrisk)
dim(r) <- nage
