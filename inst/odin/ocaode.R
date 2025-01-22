## ODEs for model
## interpolation
nage <- user()                            #no age cats
r[] <- user()
popdatF[,] <- user()
popdatM[,] <- user()
popinitM[] <- user()
popinitF[] <- user()
BF[] <- user()
BM[] <- user()
ttp[] <- user()
omegaF[] <- interpolate(ttp,popdatF,'linear')
omegaM[] <- interpolate(ttp,popdatM,'linear')
bzf <- interpolate(ttp,BF,'linear')
bzm <- interpolate(ttp,BM,'linear')
## initial state
initial(N[1:nage,1]) <- popinitM[i]
initial(N[1:nage,2]) <- popinitF[i]
## == dynamics
## male
deriv(N[1,1]) <- bzm - omegaM[1]*N[1,1]
deriv(N[2:nage,1]) <- r[i-1]*N[i-1,1] - omegaM[i]*N[i,1]
## female
deriv(N[1,2]) <- bzf - omegaF[1]*N[1,2]
deriv(N[2:nage,2]) <- r[i-1]*N[i-1,2] - omegaF[i]*N[i,2]
## dims
dim(ttp) <- user()                    #note need this before length() use
lttp <- length(ttp)
dim(popdatF) <- c(lttp,nage)
dim(popdatM) <- c(lttp,nage)
dim(popinitF) <- c(nage)
dim(popinitM) <- c(nage)
dim(BF) <- lttp
dim(BM) <- lttp
dim(omegaF) <- nage
dim(omegaM) <- nage
dim(N) <- c(nage,2)
dim(r) <- nage
