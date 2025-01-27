## internal package function aimed at simplifying additional layers of parameter customization
create_demographic_baseparms <- function(tc = 1970:2020) {
  r <- OCA1::UKdemo$r
  ## men
  omegaL <- OCA1::UKdemo$omega[Year %in% tc, .(Year, AgeGrp, omegaM)]
  omegaW <- data.table::dcast(omegaL, Year ~ AgeGrp, value.var = "omegaM")
  omega <- as.matrix(omegaW)
  omega <- omega[, -1] #
  row.names(omega) <- tc
  omegaM <- omega # NOTE
  bzm <- OCA1::UKdemo$B[Year %in% tc, BirthsMale] # NOTE
  pinitM <- OCA1::UKdemo$N[Year == min(tc), PopMale] # NOTE
  ## women
  omegaL <- OCA1::UKdemo$omega[Year %in% tc, .(Year, AgeGrp, omegaF)]
  omegaW <- data.table::dcast(omegaL, Year ~ AgeGrp, value.var = "omegaF")
  omega <- as.matrix(omegaW)
  omega <- omega[, -1] #
  row.names(omega) <- tc
  omegaF <- omega # NOTE
  bzf <- OCA1::UKdemo$B[Year %in% tc, BirthsFemale] # NOTE
  pinitF <- OCA1::UKdemo$N[Year == min(tc), PopFemale] # NOTE
  ## make parameter object
  list(
    nage = length(r),
    r = r,
    ttp = tc,
    omegaF = omegaF,
    omegaM = omegaM,
    bzf = bzf, bzm = bzm,
    popinitM = pinitM,
    popinitF = pinitF
  )
}




##' .. content for \description{} (no empty lines) ..TODO
##'
##' .. content for \details{} ..TODO
##' @title TODO
##' @param tc TODO
##' @param propinitnat vector of length nnat with initial proportions
##' @param propinitrisk vector of length nrisk with initial proportions
##' @param birthrisk where to births go in risk?
##' @param propinitpost initial post-TB proportions
##' @param propinitstrain initial TB strain proportions
##' @param propinitprot initial TB protection proportions
##' @param migrationdata list of migration data see details
##' @param verbose give more feedback
##' @return list of parameters to run odin model
##' @author Pete Dodd
##' @import data.table
##' @export
create_demographic_parms <- function(tc = 1970:2020,
                                     propinitnat = 1,
                                     propinitrisk = 1,
                                     birthrisk = 1,
                                     propinitpost = 1,
                                     propinitstrain = 1,
                                     propinitprot = 1,
                                     migrationdata,
                                     verbose=FALSE) {
  ## --- checks TODO write some functions to simplify these:
  if (abs(sum(propinitnat)-1)>1e-15) stop("sum(propinitnat) must be 1!")
  if (any(propinitnat < 0) | any(propinitnat > 1)) stop("propinitnat elements must be valid probabilities!")
  if (abs(sum(propinitrisk) - 1) > 1e-15) stop("sum(propinitrisk) must be 1!")
  if (any(propinitrisk < 0) | any(propinitrisk > 1)) stop("propinitrisk elements must be valid probabilities!")
  if (abs(sum(propinitpost) - 1) > 1e-15) stop("sum(propinitpost) must be 1!")
  if (any(propinitpost < 0) | any(propinitpost > 1)) stop("propinitpost elements must be valid probabilities!")
  if (abs(sum(propinitstrain) - 1) > 1e-15) stop("sum(propinitstrain) must be 1!")
  if (any(propinitstrain < 0) | any(propinitstrain > 1)) stop("propinitstrain elements must be valid probabilities!")
  if (abs(sum(propinitprot) - 1) > 1e-15) stop("sum(propinitprot) must be 1!")
  if (any(propinitprot < 0) | any(propinitprot > 1)) stop("propinitprot elements must be valid probabilities!")
  if (abs(sum(birthrisk) - 1) > 1e-15) stop("sum(birthrisk) must be 1!")
  if (any(birthrisk < 0) | any(birthrisk > 1)) stop("birthrisk elements must be valid probabilities!")
  if(length(birthrisk)!=length(propinitrisk)){
    cat("Creating a birthrisk with the same length as propinitisk!\n")
    birthrisk <- rep(0, length(propinitrisk))
    birthrisk[1] <- 1
  }

  ## --- create the base parameters:
  list2env(create_demographic_baseparms(tc), envir = environment())

  ## --- build on base parameters:
  ## now promote the initial states to arrays:
  nnat <- length(propinitnat) #number of nativity classes
  nrisk <- length(propinitrisk) # number of nativity classes
  if (verbose) cat("Using ", nnat, " nativity classes...\n")
  if (verbose) cat("Using ", nrisk, " risk classes...\n")

  ## TB
  npost <- length(propinitpost) #number of post TB
  nstrain <- length(propinitstrain) #number of TB strains
  nprot <- length(propinitprot)     #number of TB protection strata
  if (verbose) cat("Using ", npost, " post-TB classes (1=none)...\n")
  if (verbose) cat("Using ", nstrain, " TB strains...\n")
  if (verbose) cat("Using ", nprot, " TB protection classes (1=none)...\n")

  ## promote to array
  popinitF <- array(popinitF,
                    dim = c(length(OCA1::agz), nnat, nrisk, npost, nstrain, nprot),
                    dimnames = list(
                      acat = OCA1::agz,
                      nativity = 1:nnat,
                      risk = 1:nrisk,
                      post = 1:npost,
                      strain = 1:nstrain,
                      protn = 1:nprot
                    )
                    )
  popinitM <- array(popinitM,
                    dim = c(length(OCA1::agz), nnat, nrisk, npost, nstrain, nprot),
                    dimnames = list(
                      acat = OCA1::agz,
                      nativity = 1:nnat,
                      risk = 1:nrisk,
                      post = 1:npost,
                      strain = 1:nstrain,
                      protn = 1:nprot
                    )
    )

  if (verbose) cat("*** NOTE: this will use ", 2*prod(dim(popinitM)), " strata! ***\n")
  if (verbose) cat("The total number of ODEs will be (the number of strata) x (the number of TB states).\n")

  ## multiply nativity/risk by proportions (done like this to avoid nested loops)
  for (k in 1:nnat) {
    popinitM[, k, , , , ] <- popinitM[, k, , , , ] * propinitnat[k]
    popinitF[, k, , , , ] <- popinitF[, k, , , , ] * propinitnat[k]
  }
  for (l in 1:nrisk) {
    popinitM[, , l, , , ] <- popinitM[, , l, , , ] * propinitrisk[l]
    popinitF[, , l, , , ] <- popinitF[, , l, , , ] * propinitrisk[l]
  }
  for (i5 in 1:npost) {
    popinitM[, , , i5, , ] <- popinitM[, , , i5, , ] * propinitpost[i5]
    popinitF[, , , i5, , ] <- popinitF[, , , i5, , ] * propinitpost[i5]
  }
  for (i6 in 1:nstrain) {
    popinitM[, , , , i6, ] <- popinitM[, , , , i6, ] * propinitstrain[i6]
    popinitF[, , , , i6, ] <- popinitF[, , , , i6, ] * propinitstrain[i6]
  }
  for (i7 in 1:nprot) {
    popinitM[, , , , , i7] <- popinitM[, , , , , i7] * propinitprot[i7]
    popinitF[, , , , , i7] <- popinitF[, , , , , i7] * propinitprot[i7]
  }

  ## migration data
  if(missing(migrationdata)){
    if (verbose & nnat>1) cat("Multiple nativity classes, but no migration dynamics data supplied: using static defaults.\n")
    migrage <- rep(0,nnat) #rates of moving between categories
    ## plumbing for where new migrants go in strata:
    ## M & F
    PmigrF_risk <- PmigrM_risk <- rep(0, nrisk)
    PmigrF_post <- PmigrM_post <- rep(0, npost)
    PmigrF_strain <- PmigrM_strain <- rep(0, nstrain)
    PmigrF_prot <- PmigrM_prot <- rep(0, nprot)
    ## default to bottom:
    PmigrF_risk[1] <- PmigrM_risk[1] <- 1
    PmigrF_post[1] <- PmigrM_post[1] <- 1
    PmigrF_strain[1] <- PmigrM_strain[1] <- 1
    PmigrF_prot[1] <- PmigrM_prot[1] <- 1
    ## migration flow
    immigration_female <- immigration_male <- matrix(0, nrow = length(tc), ncol = length(OCA1::agz))
  } else {
    ## TODO checks
    ## relevant data in migrationdata list
    ## dims all consistent
    ## probabilities all valid
    ## migration data >=0
    list2env(migrationdata, envir = environment())

    ## correct omegaF and omegaM:
    ## TODO create IpcM[age] = IM[age] / N[M,age] & for F
    ## omegaM -> omegaM + IpcM & for F

  }

  ## make parameter object
  list(
    nage = nage,
    nnat = nnat,
    nrisk = nrisk,
    npost = npost,
    nstrain = nstrain,
    nprot = nprot,
    birthrisk = birthrisk,
    r = r,
    ttp = tc,
    popdatF = omegaF,
    popdatM = omegaM,
    BF = bzf, BM = bzm,
    popinitM = popinitM,
    popinitF = popinitF,
    ## migration data:
    migrage = migrage,
    PmigrF_risk = PmigrF_risk,
    PmigrM_risk = PmigrM_risk,
    PmigrF_post = PmigrF_post,
    PmigrM_post = PmigrM_post,
    PmigrF_strain = PmigrF_strain,
    PmigrM_strain = PmigrM_strain,
    PmigrF_prot = PmigrF_prot,
    PmigrM_prot = PmigrM_prot,
    immigration_female = immigration_female,
    immigration_male = immigration_male
  )
}

