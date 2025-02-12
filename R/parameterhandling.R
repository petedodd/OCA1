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
##' @param riskdata list of risk data see details
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
                                     riskdata,
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
  ntimes <- length(ttp)         #number of data time points
  nage <- length(OCA1::agz)     #number of ages
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
  popinit <- array(0,
    dim = c(nage, 2, nnat, nrisk, npost, nstrain, nprot),
    dimnames = list(
      acat = OCA1::agz,
      sex = c("M", "F"),
      nativity = 1:nnat,
      risk = 1:nrisk,
      post = 1:npost,
      strain = 1:nstrain,
      protn = 1:nprot
    )
  )
  popinit[, 1, , , , , ] <- popinitM
  popinit[, 2, , , , , ] <- popinitF

  ## male/female births
  BB <- cbind(bzm,bzf)

  ## combine ageing data
  popdat <- array(0,
    dim = c(ntimes, nage, 2),
    dimnames = list(
      tindex = 1:ntimes,
      acat = OCA1::agz,
      sex = c("M", "F")
    )
  )
  popdat[, , 1] <- omegaM
  popdat[, , 2] <- omegaF

  if (verbose) cat("*** NOTE: this will use ", 2*prod(dim(popinit)), " strata! ***\n")
  if (verbose) cat("The total number of ODEs will be (the number of strata) x (the number of TB states).\n")

  ## multiply nativity/risk by proportions (done like this to avoid nested loops)
  for (k in 1:nnat) {
    popinit[, , k, , , , ] <- popinit[, , k, , , , ] * propinitnat[k]
  }
  for (l in 1:nrisk) {
    popinit[, , , l, , , ] <- popinit[, , , l, , , ] * propinitrisk[l]
  }
  for (i5 in 1:npost) {
    popinit[, , , , i5, , ] <- popinit[, , , , i5, , ] * propinitpost[i5]
  }
  for (i6 in 1:nstrain) {
    popinit[, , , , , i6, ] <- popinit[, , , , , i6, ] * propinitstrain[i6]
  }
  for (i7 in 1:nprot) {
    popinit[, , , , , , i7] <- popinit[, , , , , , i7] * propinitprot[i7]
  }

  ## migration data
  if(missing(migrationdata)){
    if (verbose & nnat>1) cat("Multiple nativity classes, but no migration dynamics data supplied: using static defaults.\n")
    migrage <- rep(0,nnat) #rates of moving between categories
    ## plumbing for where new migrants go in strata:
    Pmigr_risk <- matrix(0, nrow = 2, ncol = nrisk)
    Pmigr_post <- matrix(0, nrow = 2, ncol = npost)
    Pmigr_strain <- matrix(0, nrow = 2, ncol = nstrain)
    Pmigr_prot <- matrix(0, nrow = 2, ncol = nprot)
    ## default to bottom:
    Pmigr_risk[, 1] <- 1
    Pmigr_post[, 1] <- 1
    Pmigr_strain[, 1] <- 1
    Pmigr_prot[, 1] <- 1
    ## migration flow
    immigration <- array(0,
      dim = c(ntimes, nage, 2),
      dimnames = list(
        tindex = 1:ntimes,
        acat = OCA1::agz,
        sex = c("M", "F")
      )
    )
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

  if (missing(riskdata)) {
    if (verbose & nrisk > 1) cat("Multiple risk classes, but no dynamics data supplied: using static defaults.\n")
    ## zeros all the way:
    RiskHazardData <- array(0,
      dim = c(ntimes, nage, 2, nrisk),
      dimnames = list(
        tindex = 1:ntimes,
        acat = OCA1::agz,
        sex = c("M", "F"),
        risk = 1:nrisk
      )
    )

  } else {
    ## TODO checks
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
    popdat = popdat,
    BB = BB,
    popinit = popinit,
    ## migration data:
    migrage = migrage,
    Pmigr_risk = Pmigr_risk,
    Pmigr_post = Pmigr_post,
    Pmigr_strain = Pmigr_strain,
    Pmigr_prot = Pmigr_prot,
    immigration = immigration,
    ## risk dynamics
    RiskHazardData = RiskHazardData
  )
}

