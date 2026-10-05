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
##' @param tbparms list of TB parameters; any not supplied take defaults (see \code{known_parameters()}). Time-varying entries (one row per \code{tc}) include \code{CDR_raw}, \code{migr_TBD_raw}, \code{migr_TBI_raw} and \code{betat_raw} (a length \code{length(tc)} multiplier on the force of infection, default all ones). \code{IRRnat} (length \code{nnat}, default all ones) multiplies progression from both latent states by nativity class. \code{progt_slow_raw} and \code{progt_fast_raw} (dimensions \code{length(tc)}, \code{nage}, \code{nnat}; default all ones) are time-varying multipliers on slow (reactivation) and fast progression by age and nativity class
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
  xtra_tbparms <- c(
    "CDR_raw", "migr_TBD_raw", "migr_TBI_raw", "betat_raw",
    "BETAage", "BETAsex", "BETAnat", "BETArisk", "BETAstrain",
    "propinitE", "propinitL", "propinitA", "propinitS", "propinitT",
    "IRRstrain", "IRRprotn", "IRRnat", "progt_slow_raw", "progt_fast_raw"
  )
  tbparnames <- c(tbparnames, xtra_tbparms)
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
    "migr_TBD_raw" = tbparms$migr_TBD_raw, "migr_TBI_raw" = tbparms$migr_TBI_raw,
    "propinitE" = tbparms$propinitE, "propinitL" = tbparms$propinitL, "propinitA" = tbparms$propinitA,
    "propinitS" = tbparms$propinitS, "propinitT" = tbparms$propinitT
  )
  checks01 <- sapply(param_list, check_probabilities, checksum = FALSE)
  ## non-negativity for the transmission and progression multipliers
  checks_nn <- c(
    "betat_raw" = is.numeric(tbparms$betat_raw) &&
      all(tbparms$betat_raw >= 0),
    "IRRnat" = is.numeric(tbparms$IRRnat) &&
      all(tbparms$IRRnat >= 0),
    "progt_slow_raw" = is.numeric(tbparms$progt_slow_raw) &&
      all(tbparms$progt_slow_raw >= 0),
    "progt_fast_raw" = is.numeric(tbparms$progt_fast_raw) &&
      all(tbparms$progt_fast_raw >= 0)
  )
  if (!all(checks_nn)) {
    message(
      paste(names(checks_nn)[!checks_nn], collapse = ", "),
      " must be numeric and non-negative."
    )
  }
  ## respond to all
  checks <- c(checks, checks01, checks_nn)
  if (all(checks)) {
    if (verbose) message("All TB parameters containing probabilities were correct\n")
  } else {
    stop("TB parameters with problems:", paste(names(checks)[!checks], collapse = ", "))
  }

  ## check dimension
  ## add in extra parms for dim checks:
  for (nm in xtra_tbparms) {
    param_list[[nm]] <- tbparms[[nm]]
  }
  checks <- check_dims(param_list, dms)
  if (all(checks)) {
    if (verbose) message("All TB input parameters dimensions were correct\n")
  } else {
    message("TB parameters with dimension problems:", paste(names(param_list)[!checks], collapse = ", "))
  }

  ## --- complete TB initial states
  ## create new split pops
  tbparms$popinitU <- P$popinit * (1 - tbparms$propinitE - tbparms$propinitL -
    tbparms$propinitA - tbparms$propinitS - tbparms$propinitT)
  tbparms$popinitU[tbparms$popinitU < 0] <- 0 # safety
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
