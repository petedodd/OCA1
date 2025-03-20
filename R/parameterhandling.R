## internal package function aimed at simplifying additional layers of parameter customization
create_demographic_baseparms <- function(tc = 1970:2020) {
  r <- OCA1::UKdemo$r
  if(any(is.null(tc) | is.na(tc))) stop("tc argument must not contain any NA values")
  if(!(all(is.numeric(tc)))) stop("tc argument must only contain numeric values")
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


## a utility to check probabilities are valid
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
  if(is.vector(params)){
  # 3. Check if the sum is (approximately) equal to 1
    if (abs(sum(params) - 1) > .Machine$double.eps^0.5) {
      message("The sum of parameters must be approximately equal to 1.")
      return(FALSE)
    }
  }
  else if(is.matrix(params)){
    test <- apply(params, 1, function(x) abs(sum(x) - 1) > .Machine$double.eps^0.5)
    if(any(test)) return(FALSE)
  }
  return(TRUE) # Return TRUE if all checks pass
}

## utility: length if vector, dim[2] if array
lenordim2 <- function(X) ifelse(is.null(dim(X)), length(X), dim(X)[2])


## utility to check lengths
check_dims <- function(parlist, dms){
  ## dms = c(ntimes, nnat, nrisk, npost, nstrain, nprot)
  parnames <- names(parlist)
  ans <- list()
  for(pname in parnames){
    ans[[pname]] <- FALSE
    if(pname %in% c("migrage","propinitnat")){
      ans[[pname]] <- (length(parlist[[pname]])==dms[2]) #nnat
    }
    if(pname %in% c("Pmigr_risk","propinitrisk","birthrisk")){
      ans[[pname]] <- (lenordim2(parlist[[pname]])==dms[3]) #nrisk
    }
    if(pname %in% c("Pmigr_post","propinitpost","progn_posttb")){
      ans[[pname]] <- (lenordim2(parlist[[pname]])==dms[4]) #npost
    }
    if(pname %in% c("Pmigr_strain","propinitstrain")){
      ans[[pname]] <- (lenordim2(parlist[[pname]])==dms[5]) #nstrain
    }
    if(pname %in% c("Pmigr_prot","propinitprot")){
      ans[[pname]] <- (lenordim2(parlist[[pname]])==dms[6]) #nprotn
    }
    ## now test dim 1s:
    if(pname %in% c("Pmigr_risk","Pmigr_post","Pmigr_strain","Pmigr_prot")){
      if (ans[[pname]]) {                                       #passed on dim 2
        ans[[pname]] <- (dim(parlist[[pname]])[1] == 2) # nsex
      }
    }
    if(pname %in% c("immigration")){
      ans[[pname]] <- all(dim(parlist[[pname]]) == c(dms[1], length(OCA1::agz), 2))
    }
    if (pname %in% c("exmigrate")) {
      ans[[pname]] <- all(dim(parlist[[pname]]) == c(dms[1], length(OCA1::agz), 2, dms[2]))
    }
    if(pname=="RiskHazardData"){
      ans[[pname]] <- all(dim(parlist[[pname]])==c(dms[1], length(OCA1::agz), 2, dms[3]))
    }
    if(pname=="CDR_raw"){
      ans[[pname]] <- all(dim(parlist[[pname]]) == c(dms[1], length(OCA1::agz), 2, dms[2:6]))
    }
  } #end loop over list names
  unlist(ans)
}

## utility for defaults: [1,0,0,...] or just 1
hotone <- function(n){
  ans <- rep(0,n)
  ans[1] <- 1
  ans
}

## a utility to construct default values for unsupplied parameters/data
default_parameters <- function(parname, dms, verbose=FALSE){
  ## dms = c(ntimes, nnat, nrisk, npost, nstrain, nprot)
  if(verbose) message("Using default value for ",parname,"...")
  ans <- NA
  if(parname %in% c("migrage")){
    ans <- rep(0,dms[2]) #nnat
  }
  if(parname %in% c("propinitnat")){
    ans <- hotone(dms[2]) #nnat
  }
  if(parname %in% c("Pmigr_risk","propinitrisk","birthrisk")){
    ans <- hotone(dms[3]) #nrisk
  }
  if(parname %in% c("Pmigr_post","propinitpost")){
    ans <- hotone(dms[4]) #npost
  }
  if(parname %in% c("Pmigr_strain","propinitstrain")){
    ans <- hotone(dms[5]) #nstrain
  }
  if(parname %in% c("Pmigr_prot","propinitprot")){
    ans <- hotone(dms[6]) #nprotn
  }
  ## these need bumping up to duplicate by sex
  if(parname %in% c("Pmigr_risk","Pmigr_post","Pmigr_strain","Pmigr_prot")){
    ans <- rbind(ans,ans) #one for each sex
  }
  if(parname %in% c("immigration")){
    ans <- array(0,
      dim = c(dms[1], length(OCA1::agz), 2),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F")
      )
      )
  }
  if (parname %in% c("exmigrate")) {
    ans <- array(0,
      dim = c(dms[1], length(OCA1::agz), 2, dms[2]),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F"),
        nativity = 1:dms[2]
      )
    )
  }
  if(parname == "RiskHazardData"){
    ans <- array(0,
      dim = c(dms[1], length(OCA1::agz), 2, dms[3]),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F"),
        risk = 1:dms[3]
      )
    )
  }
  if (parname == "CDR_raw") {
    ans <- array(0.7,
      dim = c(dms[1], length(OCA1::agz), 2, dms[2:6]),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F"),
        nativity = 1:dms[2],
        risk = 1:dms[3],
        post = 1:dms[4],
        strain = 1:dms[5],
        protn = 1:dms[6]
      )
    )
  }
  if (parname == "progn_posttb") {
    ans <- rep(0,dms[4])
  }
  if(parname %in% names(OCA1::parms)){ #basic TB parameters
    ans <- OCA1::parms[[parname]]
  }
  if(is.na(sum(ans))) stop("No default available for ",parname,"!\n") #sum to handle arrays
  ans
}

## loops over to add missing parameters as default
add_defaults_if_missing <- function(L, parnames, dms, verbose){
  for(pname in parnames){
    if (!pname %in% names(L)) {
      L[[pname]] <- default_parameters(pname, dms, verbose)
    } else {
      known_parameters(pname,quiet=TRUE) #test exists
      if (verbose) message("Using supplied value for ", pname, "...")
    }
  }
  L
}


##' @title TODO
##' @details
##' Additional details...
##' @description
##' A short description...
##' 
##' @param tc TODO
##' @param nnat TODO
##' @param nrisk TODO
##' @param npost TODO
##' @param nstrain TODO
##' @param nprot TODO
##' @param migrationdata list of migration data see details
##' @param riskdata list of risk data see details
##' @param postdata TODO
##' @param straindata TODO
##' @param protdata TODO
##' @param verbose give more feedback
##' @return list of parameters to run odin model
##' @examples
##' ## example without nativity class used
##' pms <- create_demographic_parms() #create UK parameters
##' ## version with 2 static nativity classes
##' pms <- create_demographic_parms(nnat = 2, migrationdata = list(propinitnat = c(0.9,0.1)))
##' ## version with 2 static nativity classes and 2 static risk classes
##' pms <- create_demographic_parms(nnat = 2, nrisk = 2,
##'                                 migrationdata = list(propinitnat = c(0.9,0.1)),
##'                                 riskdata = list(propinitrisk = c(0.9,0.1)))
##' @author Pete Dodd
##' @import data.table
##' @export
create_demographic_parms <- function(tc = 1970:2020,
                                     nnat=1,nrisk=1,npost=1,nstrain=1,nprot=1,
                                     migrationdata=list(),
                                     riskdata=list(),
                                     postdata=list(),
                                     straindata=list(),
                                     protdata=list(),
                                     verbose=FALSE) {
  ## === initial feedback:
  nage <- length(OCA1::agz) # number of ages
  if (verbose) message("Using ", nage, " age classes...")
  if (verbose) message("Using ", 2, " sex classes...")
  if (verbose) message("Using ", nnat, " nativity classes...")
  if (verbose) message("Using ", nrisk, " risk classes...")
  if (verbose) message("Using ", npost, " post-TB classes (1=none)...")
  if (verbose) message("Using ", nstrain, " TB strains...")
  if (verbose) message("Using ", nprot, " TB protection classes (1=none)...")
  if (verbose) message("*** NOTE: this will use ", 2*nage*nnat*nrisk*npost*nstrain*nprot, " strata! ***")
  if (verbose) message("The total number of ODEs will be (the number of strata) x (the number of TB states).\n")
  ntimes <- length(tc)                                 #number of time data points
  dms <- c(ntimes, nnat, nrisk, npost, nstrain, nprot) #dimensions, bar age/sex which are always fixed

  ## === complete & load supplied data:
  ## ---migration data
  migr_parnames <- c(
    "propinitnat",
    "immigration",
    "exmigrate",
    "migrage",
    ## migration x other strata:
    "Pmigr_risk",
    "Pmigr_post",
    "Pmigr_strain",
    "Pmigr_prot"
  )
  migrationdata <- add_defaults_if_missing(migrationdata, migr_parnames, dms, verbose)
  list2env(migrationdata, envir = environment()) #load complete list

  ## --- risk data
  risk_parnames <- c("propinitrisk", "birthrisk", "RiskHazardData")
  riskdata <- add_defaults_if_missing(riskdata, risk_parnames, dms, verbose)
  list2env(riskdata, envir = environment()) #load complete list

  ## --- post data
  post_parnames <- c("propinitpost", "progn_posttb")
  postdata <- add_defaults_if_missing(postdata, post_parnames, dms, verbose)
  list2env(postdata, envir = environment()) #load complete list

  ## --- strain data
  strain_parnames <- c("propinitstrain")
  straindata <- add_defaults_if_missing(straindata, strain_parnames, dms, verbose)
  list2env(straindata, envir = environment()) #load complete list

  ## --- protn data
  protn_parnames <- c("propinitprot")
  protdata <- add_defaults_if_missing(protdata, protn_parnames, dms, verbose)
  list2env(protdata, envir = environment()) #load complete list


  ## === CHECK s
  ## TODO consider whether easier to work with riskdata etc lists above, picking out probs
  ## check probabilities
  if(verbose) message("\n")
  param_list <- list("propinitnat"=propinitnat,
                     "propinitrisk"=propinitrisk,
                     "birthrisk"=birthrisk,
                     "propinitpost"=propinitpost,
                     "propinitstrain"=propinitstrain,
                     "propinitprot"=propinitprot,
                     "Pmigr_risk"=Pmigr_risk,
                     "Pmigr_post"=Pmigr_post,
                     "Pmigr_strain"=Pmigr_strain,
                     "Pmigr_prot"=Pmigr_prot)
  checks <- sapply(param_list,check_probabilities)
  if(all(checks)){
    if(verbose) message("All parameters containing probabilities were correct\n")
  }  else {
    stop("Parameters with problems:", paste(names(param_list)[!checks],collapse = ", "))
  }

  ## check dimension
  param_list <- c(param_list,
                  list("immigration"=immigration,
                       "RiskHazardData"=RiskHazardData)
                  )
  checks <- check_dims(param_list,dms)
  if(all(checks)){
    if(verbose) message("All input parameters dimensions were correct\n")
  }  else {
    message("Parameters with dimension problems:", paste(names(param_list)[!checks]))
  }

  ## check that the last layer of the risk progression hazard is zero
  if (any(RiskHazardData[, , , nrisk] > 0)) stop("Non-zero RiskHazardData in the top risk layer will result in a population leak!")

  ## check post progression
  if (any(progn_posttb < 0)) stop("Negative progression for post-TB!\n")
  if (progn_posttb[1] > .Machine$double.eps^0.5 ) stop("First element positive in progn_posttb will generate unwanted behaviour!\n")
  if (progn_posttb[npost] > .Machine$double.eps^0.5) stop("Last element positive in progn_posttb will generate unwanted behaviour!\n")

  ## === create the base parameters:
  list2env(create_demographic_baseparms(tc), envir = environment())

  ## corrections to omega in light of migration
  ## NOTE immigration is ntimes x nage x 2
  ## create Narray as pop in this format:
  Narray <- array(1e-6,
                  dim=c(ntimes,length(OCA1::agz), 2),
                  dimnames = list(tindex = 1:dms[1],
                                  acat = OCA1::agz,
                                  sex = c("M", "F")))
  Narray[,,1] <- OCA1::UKdemo$N[Year %in% tc, PopMale]
  Narray[,,2] <- OCA1::UKdemo$N[Year %in% tc, PopFemale]

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

  ## TB splits etc:
  ## TODO temporary split:
  popinitU <- popinit
  popinitE <- popinit * 0
  popinitL <- popinit * 0
  popinitA <- popinit * 0
  popinitS <- popinit * 0
  popinitT <- popinit * 0


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
    popinitU = popinitU,
    popinitE = popinitE,
    popinitL = popinitL,
    popinitA = popinitA,
    popinitS = popinitS,
    popinitT = popinitT,
    ## migration data:
    migrage = migrage,
    Pmigr_risk = Pmigr_risk,
    Pmigr_post = Pmigr_post,
    Pmigr_strain = Pmigr_strain,
    Pmigr_prot = Pmigr_prot,
    immigration = immigration,
    exmigrate = exmigrate,
    ## risk dynamics
    RiskHazardData = RiskHazardData,
    ## post-TB progression
    progn_posttb = progn_posttb
  )
}


##' .. content for \description{} (no empty lines) ..
##'
##' .. content for \details{} ..
##'
##' NOTE this extends the demographic parameters to be more explicit about TB parameters
##'
##' @title Creates parameters for model
##' @param tc TODO
##' @param nnat TODO
##' @param nrisk TODO
##' @param npost TODO
##' @param nstrain TODO
##' @param nprot TODO
##' @param migrationdata list of migration data see details
##' @param riskdata list of risk data see details
##' @param postdata TODO
##' @param straindata TODO
##' @param protdata TODO
##' @param tbparms TODO
##' @param verbose give more feedback
##' @return list of parameter for model
##' @author Pete Dodd
##' @export
create_parms <- function(tc = 1970:2020,
                         nnat = 1, nrisk = 1, npost = 1, nstrain = 1, nprot = 1,
                         migrationdata = list(),
                         riskdata = list(),
                         postdata = list(),
                         straindata = list(),
                         protdata = list(),
                         tbparms = list(),
                         verbose = FALSE) {

  ## === key dims
  nage <- length(OCA1::agz) # number of ages
  ntimes <- length(tc) # number of time data points
  dms <- c(ntimes, nnat, nrisk, npost, nstrain, nprot) # dimensions, bar age/sex which are always fixed

  ## === create demographic parameters
  P <- create_demographic_parms(
    tc = tc,
    nnat = nnat, nrisk = nrisk, npost = npost, nstrain = nstrain, nprot = nprot,
    migrationdata = migrationdata,
    riskdata = riskdata,
    postdata = postdata,
    straindata = straindata,
    protdata = protdata,
    verbose = verbose
  )

  ## === TB specific parameters following similar pattern to above
  ## TODO
  ## probably want a new function for default_tb_parameters (like default_parameters)
  ## perhaps use the same check_dims function
  ## pjd to start with migration x TB
  ## das to start with time/dependent CDR/hazard - NOTE let me know if this impacts speed

  ## tb parms
  tbparnames <- names(OCA1::parms)
  tbparnames <- c(tbparnames, "CDR_raw")
  ## defaults:
  tbparms <- add_defaults_if_missing(tbparms, tbparnames, dms, verbose)

  ## === TB parm checks
  ## prob checks
  if (verbose) message("\n")
  param_list <- list(
    "CDR_raw" = tbparms$CDR_raw
  )
  checks <- sapply(param_list, check_probabilities)
  if (all(checks)) {
    if (verbose) message("All TB parameters containing probabilities were correct\n")
  } else {
    stop("TB parameters with problems:", paste(names(param_list)[!checks], collapse = ", "))
  }

  ## check dimension
  checks <- check_dims(param_list, dms)
  if (all(checks)) {
    if (verbose) message("All TB input parameters dimensions were correct\n")
  } else {
    message("TB parameters with dimension problems:", paste(names(param_list)[!checks]))
  }

  ## === return combined demographic & TB parameters
  c(P, tbparms)
}

## CHECK
# pms <-create_parms()



## a utility to construct default values for unsupplied parameters/data
default_parameters <- function(parname, dms, verbose=FALSE){
  ## dms = c(ntimes, nnat, nrisk, npost, nstrain, nprot)
  if(verbose) message("Using default value for ",parname,"...")
  ans <- NA
  if(parname %in% c("migrage")){
    ans <- rep(0,dms[2]) #nnat
  }
  if(parname %in% c("propinitnat")){
    ans <- hotone(dms[2]) #nnat
  }
  if(parname %in% c("Pmigr_risk","propinitrisk","birthrisk")){
    ans <- hotone(dms[3]) #nrisk
  }
  if(parname %in% c("Pmigr_post","propinitpost")){
    ans <- hotone(dms[4]) #npost
  }
  if(parname %in% c("Pmigr_strain","propinitstrain")){
    ans <- hotone(dms[5]) #nstrain
  }
  if(parname %in% c("Pmigr_prot","propinitprot")){
    ans <- hotone(dms[6]) #nprotn
  }
  ## these need bumping up to duplicate by sex
  if(parname %in% c("Pmigr_risk","Pmigr_post","Pmigr_strain","Pmigr_prot")){
    ans <- rbind(ans,ans) #one for each sex
  }
  if(parname %in% c("immigration")){
    ans <- array(0,
      dim = c(dms[1], length(OCA1::agz), 2),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F")
      )
      )
  }
  if (parname %in% c("exmigrate")) {
    ans <- array(0,
      dim = c(dms[1], length(OCA1::agz), 2, dms[2]),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F"),
        nativity = 1:dms[2]
      )
    )
  }
  if(parname == "RiskHazardData"){
    ans <- array(0,
      dim = c(dms[1], length(OCA1::agz), 2, dms[3]),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F"),
        risk = 1:dms[3]
      )
    )
  }
  if (parname == "CDR_raw") {
    ans <- array(0.7,
      dim = c(dms[1], length(OCA1::agz), 2, dms[2:6]),
      dimnames = list(
        tindex = 1:dms[1],
        acat = OCA1::agz,
        sex = c("M", "F"),
        nativity = 1:dms[2],
        risk = 1:dms[3],
        post = 1:dms[4],
        strain = 1:dms[5],
        protn = 1:dms[6]
      )
    )
  }
  if (parname == "progn_posttb") {
    ans <- rep(0,dms[4])
  }
  if(parname %in% names(OCA1::parms)){ #basic TB parameters
    ans <- OCA1::parms[[parname]]
  }
  if(is.na(sum(ans))) stop("No default available for ",parname,"!\n") #sum to handle arrays
  ans
}




##' A helper function
##'
##' See `known_parameters`
##'
##' @title A helper function for `known_parameters`
##' @param parname a specific parameter name (Default NULL)
##' @param parmlist one of the lists of parameters
##' @return no return only printing
##' @author Pete Dodd
##' @importFrom rlang as_label ensym
print_helper <- function(parname=NULL,parmlist){
  parmlistname <- rlang::as_label(rlang::ensym(parmlist))
  print_here <- ifelse(is.null(parname),TRUE,parname %in% names(parmlist))
  if(print_here){
    cat("\n")
    cat(parmlistname,":\n")
    if(length(parmlist)>0){
      if(is.null(parname)){
        for(nm in names(parmlist)){
          cat("\t",nm,": ",parmlist[[nm]][[1]],"[",parmlist[[nm]][[2]],"]\n")
        }
      } else{
        cat("\t",parname,": ",parmlist[[parname]][[1]],"[",parmlist[[parname]][[2]],"]\n")
      }
    }
  }
}

##' Print info or test known parameters
##'
##' This has no return value but can be used to either print info on known parameters (all, or specific parameters),
##' and will test for unknown parameters.
##'
##' Info is returned organised by the parameter list they should be part of, and a short description and the expected
##' dimensions are printed.
##'
##' @title Print/test known parameters
##' @param parname if supplied consider only this parameter name; otherwise (default) print info on all parameters
##' @param quiet if TRUE (default) do not print info, only test whether parameter name is known
##' @return no return value
##' @author Pete Dodd
##' @examples
##' known_parameters()                     ##print all info
##' known_parameters("nonsense")           ##throws error as name not known
##' known_parameters("CDR_raw")            ##knows this parameter, prints info on it
##' known_parameters("CDR_raw",quiet=TRUE) ##knows this parameter, does not print info
##' @export
known_parameters <- function(parname=NULL,quiet=FALSE){

  ## relevant data on parameters
  ## will be docstring + dim string
  migrationdata <- list(
    migrage=list("",c()),
    propinitnat=list("",c()),
    Pmigr_risk=list("",c()),
    propinitrisk=list("",c()),
    birthrisk=list("",c()),
    Pmigr_post=list("",c()),
    propinitpost=list("",c()),
    Pmigr_strain=list("",c()),
    propinitstrain=list("",c()),
    Pmigr_prot=list("",c()),
    propinitprot=list("",c()),
    Pmigr_risk=list("",c()),
    Pmigr_post=list("",c()),
    Pmigr_strain=list("",c()),
    Pmigr_prot=list("",c()),
    immigration=list("",c()),
    exmigrate=list("",c())
  )
  riskdata <- list(
    RiskHazardData=list("",c())
  )
  postdata <- list(
    progn_posttb=list("",c())
  )
  straindata <- list()
  protdata <- list()
  tbparms <- list(
    CDR_raw=list("",c())
  )
  for(nm in names(OCA1::hyperparms)){
    tbparms[[nm]] <- list(hyperparms[[nm]][[3]],"scalar")
  }
  nmzall <- c(
    names(migrationdata),
    names(riskdata),
    names(postdata),
    names(straindata),
    names(protdata),
    names(tbparms)
  )

  ## test if recognized:
  if(!is.null(parname)){
    if(!parname %in% nmzall){
      ## NOTE TODO could try to suggest nearest parameter for case of typos
      stop("Input parameter '", parname,"' not recognized!")
    }
  }

  ## print things
  if(!quiet){
    if(is.null(parname)){ #no argument given: print mode
      cat("(No argument given to test, printing parameter info)\n")
      cat("\n")
    }
    print_helper(parname,migrationdata)
    print_helper(parname,riskdata)
    print_helper(parname,postdata)
    print_helper(parname,straindata)
    print_helper(parname,protdata)
    print_helper(parname,tbparms)
    cat("\n")
  }
}


## known_parameters()
## known_parameters("nonsense")
## known_parameters("CDR_raw")
## known_parameters("CDR_raw",quiet=TRUE)
