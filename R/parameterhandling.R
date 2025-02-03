## internal package function aimed at simplifying additional layers of parameter customization
create_demographic_baseparms <- function(tc = 1970:2020) {
  r <- OCA1::UKdemo$r
  
  if(min(tc) < min(OCA1::UKdemo$omega$Year) | max(tc) > max(OCA1::UKdemo$omega$Year)) stop("Range of tc must between 1950 and 2100")
  
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



check_probabilities <- function(params) {

  # 1. Check for numeric type:
  if (!all(is.numeric(params))) {
    message("All parameters must be numeric.")
    return(FALSE)
  }
  
  # 2. Check for values between 0 and 1 (inclusive):
  if (!all(params >= 0 & params <= 1)) {
    message("All parameters must be probabilities between 0 and 1.")
    return(FALSE)
  }
  
  # 3. Check if the sum is (approximately) equal to 1 (important for discrete distributions):
  if (abs(sum(params) - 1) > .Machine$double.eps^0.5) { # Using tolerance for floating point comparison
    message("The sum of parameters must be approximately equal to 1.")
    return(FALSE)
  }
  
  return(TRUE) # Return TRUE if all checks pass (implicitly returns NULL if stop is called)
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
  param_list <- list("propinitnat"=propinitnat,
                     "propinitnat"=propinitrisk,
                     "birthrisk"=birthrisk,
                     "propinitpost"=propinitpost,
                     "propinitstrain"=propinitstrain,
                     "propinitprot"=propinitprot)
    
  checks <- sapply(param_list,check_probabilities)

  if(all(checks)){
    if(verbose) message("All parameters containing probabilities were correct\n")
  }  else {
    message("Parameters with problems:", paste(names(param_list)[!checks]))
  }
  
  if(length(birthrisk)!=length(propinitrisk)){
    message("Creating a birthrisk with the same length as propinitisk!\n")
    birthrisk <- rep(0, length(propinitrisk))
    birthrisk[1] <- 1
  }

  ## --- create the base parameters:
  list2env(create_demographic_baseparms(tc), envir = environment())

  ## --- build on base parameters:
  ## now promote the initial states to arrays:
  nnat <- length(propinitnat) #number of nativity classes
  nrisk <- length(propinitrisk) # number of nativity classes
  if (verbose) message("Using ", nnat, " nativity classes...\n")
  if (verbose) message("Using ", nrisk, " risk classes...\n")

  ## TB
  npost <- length(propinitpost) #number of post TB
  nstrain <- length(propinitstrain) #number of TB strains
  nprot <- length(propinitprot)     #number of TB protection strata
  if (verbose) message("Using ", npost, " post-TB classes (1=none)...\n")
  if (verbose) message("Using ", nstrain, " TB strains...\n")
  if (verbose) message("Using ", nprot, " TB protection classes (1=none)...\n")

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

  if (verbose) message("*** NOTE: this will use ", 2*prod(dim(popinitM)), " strata! ***\n")
  if (verbose) message("The total number of ODEs will be (the number of strata) x (the number of TB states).\n")

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
    if (verbose & nnat>1) message("Multiple nativity classes, but no migration dynamics data supplied: using static defaults.\n")
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

  if (missing(riskdata)) {
    if (verbose & nrisk > 1) cat("Multiple risk classes, but no dynamics data supplied: using static defaults.\n")
    ## zeros all the way:
    RiskHazardData <- array(0,
      dim = c(length(ttp), length(OCA1::agz), 2, nrisk),
      dimnames = list(
        tindex = 1:length(ttp),
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
    immigration_male = immigration_male,
    ## risk dynamics
    RiskHazardData = RiskHazardData
  )
}

