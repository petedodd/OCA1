##' Transform a single unit-interval value to a parameter draw
##'
##' Applies the inverse-CDF (quantile function) implied by a
##' hyperparameter specification `L` (a named list whose first
##' element's name selects the distribution family: `meanlog`/`sdlog`
##' for log-normal, `shape1`/`shape2` for Beta, `mean`/`sd` for
##' Normal, `shape`/`scale` for Gamma) to a value `u` on (0,1). A
##' specification without a recognized first name (e.g. `fixed`) is
##' treated as already being on the natural parameter scale and
##' returned unchanged.
##'
##' @title qfun
##' @param u a value (or vector) on the unit interval
##' @param L a hyperparameter specification list, see `hyperparms`
##' @return the transformed parameter value(s)
##' @author Pete Dodd
##' @export
qfun <- function(u, L) {
  x <- NULL
  if (names(L)[1] == "meanlog") x <- qlnorm(u, L[[1]], L[[2]])
  if (names(L)[1] == "shape1") x <- qbeta(u, L[[1]], L[[2]])
  if (names(L)[1] == "mean") x <- qnorm(u, L[[1]], L[[2]])
  if (names(L)[1] == "shape") x <- qgamma(u, L[[1]], scale = L[[2]])
  if (is.null(x)) x <- u[[1]] # not formatted numbers differently
  x
}

##' Transform a unit-cube vector to a named parameter list
##'
##' Maps a vector `u` of unit-interval values (e.g. a draw from a
##' sampler operating on the unit cube) to natural-scale
##' parameter values via `qfun()`, using the distribution family
##' specified for each entry of `HP` (typically `hyperparms`).
##' Because this is exactly the standard
##' inverse-CDF/probability-integral-transform, sampling `u` from
##' Uniform(0,1) and evaluating a likelihood at `uv2ps(u, HP)` induces
##' an exact posterior proportional to `prior(x) * likelihood(x)`:
##' no separate prior density needs to be evaluated by the sampler.
##'
##' @title uv2ps
##' @param u a vector of unit-interval values, one per entry of `HP`
##' @param HP a named list of hyperparameter specifications, see
##'   `hyperparms`
##' @param returnlist if `TRUE` (default) return a named list,
##'   otherwise a plain (unnamed) vector
##' @return named list (or vector) of parameter values
##' @author Pete Dodd
##' @export
uv2ps <- function(u, HP, returnlist = TRUE) {
  for (i in seq_along(HP)) {
    if (is.list(HP[[i]])) {
      u[i] <- qfun(u[i], HP[[i]])
    } else { # fixed value
      u[i] <- HP[[i]]
    }
  }
  if (returnlist) {
    u <- as.list(u)
    names(u) <- names(HP)
  }
  u
}
