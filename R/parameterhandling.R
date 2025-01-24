##' .. content for \description{} (no empty lines) ..TODO
##'
##' .. content for \details{} ..TODO
##' @title TODO
##' @param tc TODO
##' @param propinitnat vector of length nnat with initial proportions
##' @param verbose give more feedback
##' @return list of parameters to run odin model
##' @author Pete Dodd
##' @import data.table
##' @export
create_demographic_parms <- function(tc = 1970:2020,propinitnat=1,verbose=FALSE) {
  if (abs(sum(propinitnat)-1)>1e-15) stop("sum(propinitnat) must be 1!")
  if (any(propinitnat < 0) | any(propinitnat > 1)) stop("propinitnat elements must be valid probabilities!")
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
  ## now promote the initial states to arrays:
  nnat <- length(propinitnat) #number of nativity classes
  if(verbose) cat("Using ",nnat," nativity classes...\n")
  pinitM <- array(pinitM,
                  dim = c(length(OCA1::agz), nnat),
                  dimnames = list(acat = OCA1::agz, nativity = 1:nnat)
                  )
  pinitF <- array(pinitF,
                  dim = c(length(OCA1::agz), nnat),
                  dimnames = list(acat = OCA1::agz, nativity = 1:nnat)
                  )
  ## multiply nativity by proportions
  for (j in 1:length(propinitnat)) {
    pinitM[, j] <- pinitM[, j] * propinitnat[j]
    pinitF[, j] <- pinitF[, j] * propinitnat[j]
  }
  ## make parameter object
  list(
    nage = length(r),
    nnat = nnat,
    r = r,
    ttp = tc,
    popdatF = omegaF,
    popdatM = omegaM,
    BF = bzf, BM = bzm,
    popinitM = pinitM,
    popinitF = pinitF
  )
}


