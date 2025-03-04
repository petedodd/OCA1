## code for running models

##' A Simple Wrapper to Run Model
##'
##' content for details here
##'
##'
##' @title runmodel
##' @param p parameter list
##' @param times vector of times to run the model over - if not given, will construct from p
##' @param raw return the raw matrix ODE output or a data.table? (default FALSE)
##' @param singleout (default FALSE) return a single output, or a list of data.tables split by variable type (only if raw=FALSE)
##' @return a matrix of timeseries
##' @author Pete Dodd
##' @useDynLib OCA1, .registration = TRUE
##' @import data.table
##' @export
runmodel <- function(p, times, raw = FALSE, singleout = FALSE) {
  if (missing(times)) {
    times <- seq(from = min(p$ttp), to = max(p$ttp), by = 0.1) # default times if not given
  }
  ## create model
  mdl <- ocaode$new(user=p)
  ## run
  ans <- mdl$run(times)
  if (!raw) { # convert to data.table
    if (singleout) {
      ans <- raw2datatable(ans)
    } else {
      ans <- typesplit_raw2datatable(ans)
    }
  }
  ans # return
}


## utility to convert raw array output in to format data.table
raw2datatable <- function(ans) {
    ans <- data.table::as.data.table(ans)
    ans <- data.table::melt(ans, id = "t")
    ans[, state := gsub("\\[|\\]|,|[[:digit:]]+", "", variable)]
    ans[, vars := gsub("_|\\[|\\]|[[:alpha:]]+", "", variable)]
    ans[, c("astring", "sexstring", "natstring","rskstring","poststring","strainstring","protstring") :=
            data.table::tstrsplit(vars, split = ",")]
    ## suppressing warnings to allow graceful conversion to NA where missing
    ans[, c("AgeGrp", "sex", "natcat","risk","post","strain","prot") := .(
            suppressWarnings(OCA1::agz[as.integer(astring)]),
            suppressWarnings(c("M", "F")[as.integer(sexstring)]),
            suppressWarnings(as.integer(natstring)),
            suppressWarnings(as.integer(rskstring)),
            suppressWarnings(as.integer(poststring)),
            suppressWarnings(as.integer(strainstring)),
            suppressWarnings(as.integer(protstring))
          )]
    ans[, c("astring", "sexstring", "natstring", "rskstring", "vars", "variable",
            "poststring", "strainstring", "protstring") := NULL]
    ## as before
    ans$sex <- factor(ans$sex,levels=c("M","F"))
    ans$AgeGrp <- factor(ans$AgeGrp,levels=OCA1::agz)
    ans
}

## for outputting a list of data.tables by output type (var): determined by colnames of type var_nm
typesplit_raw2datatable <- function(ans) {
  ## find prefixes
  colnmz <- colnames(ans)
  colnmz <- gsub("\\[|\\]|,|[[:digit:]]+", "", colnmz)
  colnmz <- unique(colnmz)
  PF <- strsplit(colnmz, split = "_")
  prefixes <- list()
  for (i in 1:length(PF)) {
    prefixes[[i]] <- ifelse(length(PF[[i]]) == 1, "state", PF[[i]][1])
  }
  prefixes <- unique(unlist(prefixes))
  ## answer list
  statecols <- colnames(ans)[!grepl("_", colnames(ans))]
  ANS <- list(state = raw2datatable(ans[, statecols]))
  prefixes <- prefixes[-1] # the rest if there
  for (p in prefixes) {
    pcols <- colnames(ans)[grepl(paste0(p, "_"), colnames(ans))]
    pcols <- c("t", pcols)
    ANS[[p]] <- raw2datatable(ans[, pcols])
  }
  ANS
}
