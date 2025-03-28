


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

  ## tb parms
  tbparnames <- names(OCA1::parms)
  tbparnames <- c(tbparnames,
                  "CDR_raw", "migr_TBD", "migr_TBI",
                  "BETAage", "BETAsex", "BETAnat", "BETArisk", "BETAstrain",
                  "propinitE","propinitL","propinitA","propinitS","propinitT")
  ## defaults:
  tbparms <- add_defaults_if_missing(tbparms, tbparnames, dms, verbose)

  ## === TB parm checks
  ## prob checks
  if (verbose) message("\n")
  ## for probability checks
  param_list <- list(
    "CDR_raw" = tbparms$CDR_raw
  )
  checks <- sapply(param_list, check_probabilities)
  ## 0 <= x <= 1 checks; same but without checking sums are near 1
  param_list <- list(
    "migr_TBD" = tbparms$migr_TBD, "migr_TBI" = tbparms$migr_TBI,
    "propinitE" = tbparms$propinitE, "propinitL" = tbparms$propinitL, "propinitA" = tbparms$propinitA,
    "propinitS" = tbparms$propinitS, "propinitT" = tbparms$propinitT
  )
  checks01 <- sapply(param_list, check_probabilities, checksum = FALSE)
  ## respond to all
  checks <- c(checks, checks01)
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

  ## --- complete TB initial states
  ## create new split pops
  tbparms$popinitU <- P$popinit * (1-tbparms$propinitE-tbparms$propinitL-
                                   tbparms$propinitA-tbparms$propinitS-tbparms$propinitT)
  tbparms$popinitU[tbparms$popinitU<0] <- 0 #safety
  tbparms$popinitE <- P$popinit * tbparms$propinitE
  tbparms$popinitL <- P$popinit * tbparms$propinitL
  tbparms$popinitA <- P$popinit * tbparms$propinitA
  tbparms$popinitS <- P$popinit * tbparms$propinitS
  tbparms$popinitT <- P$popinit * tbparms$propinitT
  ## remove unnecessary data:
  P$popinit <- NULL
  tbparms$propinitE <- tbparms$propinitL <- tbparms$propinitA <-
    tbparms$propinitS <- tbparms$propinitT <- NULL

  ## === return combined demographic & TB parameters
  c(P, tbparms)
}

## CHECK
# pms <-create_parms()
