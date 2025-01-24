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
##' @param verbose give more feedback
##' @return list of parameters to run odin model
##' @author Pete Dodd
##' @import data.table
##' @export
create_demographic_parms <- function(tc = 1970:2020,
                                     propinitnat=1,
                                     propinitrisk=1,
                                     birthrisk=1,
                                     verbose=FALSE) {
  ## --- checks TODO write some functions to simplify these:
  if (abs(sum(propinitnat)-1)>1e-15) stop("sum(propinitnat) must be 1!")
  if (any(propinitnat < 0) | any(propinitnat > 1)) stop("propinitnat elements must be valid probabilities!")
  if (abs(sum(propinitrisk) - 1) > 1e-15) stop("sum(propinitrisk) must be 1!")
  if (any(propinitrisk < 0) | any(propinitrisk > 1)) stop("propinitrisk elements must be valid probabilities!")
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

  if(verbose) cat("Using ",nnat," nativity classes...\n")
  if (verbose) cat("Using ", nrisk, " risk classes...\n")
  ## promote to array
  popinitF <- array(popinitF,
    dim = c(length(OCA1::agz), nnat, nrisk),
    dimnames = list(acat = OCA1::agz, nativity = 1:length(propinitnat), risk = 1:nrisk)
  )
  ## promote to array
  popinitM <- array(popinitM,
    dim = c(length(OCA1::agz), nnat, nrisk),
    dimnames = list(acat = OCA1::agz, nativity = 1:length(propinitnat), risk = 1:nrisk)
  )
  ## multiply nativity/risk by proportions
  for (j in 1:length(propinitnat)) {
    for (k in 1:length(propinitrisk)) {
      popinitM[, j, k] <- popinitM[, j, k] * propinitnat[j] * propinitrisk[k]
      popinitF[, j, k] <- popinitF[, j, k] * propinitnat[j] * propinitrisk[k]
    }
  }
  ## make parameter object
  list(
    nage = nage,
    nnat = nnat,
    nrisk = nrisk,
    birthrisk = birthrisk,
    r = r,
    ttp = tc,
    popdatF = omegaF,
    popdatM = omegaM,
    BF = bzf, BM = bzm,
    popinitM = popinitM,
    popinitF = popinitF
  )
}

