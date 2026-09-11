##' Age group labels
##'
##' 5-year age band labels used throughout the package (17 bands,
##' 0-4 to 80+).
##' @format A character vector of length 17.
"agz"

##' Prior hyperparameter specifications for TB natural-history and
##' migration-split parameters
##'
##' A named list, one entry per parameter, each either a
##' distribution specification (log-normal via `meanlog`/`sdlog`,
##' Beta via `shape1`/`shape2`) or a `fixed` value. Built by
##' `data-raw/parameters.R`; see `uv2ps()` for how these are used to
##' transform unit-cube draws into parameter values.
##' @format A named list.
"hyperparms"

##' Default TB natural-history and migration-split parameters
##'
##' The point estimate at the median (`u=0.5`) of every prior in
##' `hyperparms`, i.e. `uv2ps(rep(0.5, length(hyperparms)),
##' hyperparms)`. Built by `data-raw/parameters.R`.
##' @format A named list.
"parms"
