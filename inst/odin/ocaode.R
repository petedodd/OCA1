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
## nrisk <- user()                           #number of risk cats
r[] <- user()                             #aging rate
## demographic aging/mortality/net migration
popdatF[,] <- user()
popdatM[,] <- user()
## initial state
popinitM[,] <- user()
popinitF[,] <- user()
## birth rates
BF[] <- user()
BM[] <- user()
ttp[] <- user()
## interpolation
omegaF[] <- interpolate(ttp,popdatF,'linear')
omegaM[] <- interpolate(ttp,popdatM,'linear')
bzf <- interpolate(ttp,BF,'linear')
bzm <- interpolate(ttp,BM,'linear')
## useful qties
## firstage[1] <- 1
## firstage[2:nage] <- 0
native[1] <- 1
native[2:nnat] <- 0

## == initial state
initial(N[1:nage,1,1:nnat]) <- popinitM[i,k]
initial(N[1:nage,2,1:nnat]) <- popinitF[i,k]

## == dynamics
## male
deriv(N[1,1,1:nnat]) <- bzm * native[k] - omegaM[1] * N[1,1,k]
deriv(N[2:nage,1,1:nnat]) <- r[i-1] * N[i-1,1,k] - omegaM[i] * N[i,1,k]
## female
deriv(N[1,2,1:nnat]) <- bzf * native[k] - omegaF[1] * N[1,2,k]
deriv(N[2:nage,2,1:nnat]) <- r[i-1] * N[i-1,2,k] - omegaF[i] * N[i,2,k]

## == dims
dim(ttp) <- user()                    #note need this before length() use
lttp <- length(ttp)
dim(popdatF) <- c(lttp,nage)
dim(popdatM) <- c(lttp,nage)
dim(popinitF) <- c(nage,nnat)
dim(popinitM) <- c(nage,nnat)
dim(BF) <- lttp
dim(BM) <- lttp
dim(omegaF) <- nage
dim(omegaM) <- nage
## dim(firstage) <- nage
dim(native) <- nnat
dim(N) <- c(nage,2,nnat)
dim(r) <- nage
