## uk_calibrated_example.R
## Builds inst/extdata/uk_calibrated_example.rds, used by the vignette's
## "single run at calibrated values" example. It needs the UKTB_fitting
## project (https://github.com/petedodd/UKTB_fitting, local path in
## UKTB_FITTING, default ~/Documents/UKTB_fitting) with its gitignored
## data/ inputs and a MAP estimate in tmpdata/.
##
## Contents (a list):
##  parms    the complete OCA1 parameter list at the MAP, 1950-2024 (from
##           UKTB_fitting's build_oca1_parms(); runs with runmodel())
##  targets  UKHSA England notifications by place of birth, 2000-2024
##           (TB in England 2025, Supplementary Table 9) with the share
##           of notifications with known place of birth (Table 1)
##  adjust   by year, the observation terms UKTB_fitting applies outside
##           OCA1: England / UK population scale, the shared secular
##           multiplier, and the HIV-associated TB component added to
##           non-UK born
##  fitted   the fitted parameter values (natural scale) and the MAP's
##           negative log likelihood
##  sim      the MAP's own expected counts, to check the vignette
##           reproduces them
##   Rscript data-raw/uk_calibrated_example.R

uktb <- path.expand(Sys.getenv("UKTB_FITTING", "~/Documents/UKTB_fitting"))
map_file <- Sys.getenv("UKTB_MAP", "map_estimate.rds")
oca1 <- getwd()
setwd(uktb)
suppressMessages(source(file.path(uktb, "R", "model", "likelihood.R")))

tc <- 1950:2024
yrs <- 2000:2024
r <- readRDS(file.path(uktb, "tmpdata", map_file))
u <- complete_u(r$u_map)
vals <- u_to_oca1_parms(u)
parms <- build_oca1_parms(u, tc)

targets <- get_fitting_targets()[, .(
  year, nativity, n_notified,
  known_share
)]
adjust <- data.table::data.table(
  year = yrs,
  england_scale = england_scale(yrs),
  secular_mult = secular_multiplier(yrs, vals$extra$secular_decline),
  hiv_nonuk = hiv_component(yrs)
)
sim <- run_oca1_notifications(u, years = yrs)[
  , .(year, nativity, n_notified)
]


ex <- list(
  parms = parms, targets = targets, adjust = adjust,
  fitted = list(
    tbparms = vals$tbparms, extra = vals$extra,
    negloglik = r$opt$value
  ),
  sim = sim,
  source = sprintf(
    "UKTB_fitting %s, tmpdata/%s, built %s",
    system("git rev-parse --short HEAD", intern = TRUE), map_file,
    format(Sys.Date())
  )
)

setwd(oca1)
dir.create("inst/extdata", showWarnings = FALSE, recursive = TRUE)
saveRDS(ex, "inst/extdata/uk_calibrated_example.rds", compress = "xz")
cat(
  "wrote inst/extdata/uk_calibrated_example.rds (",
  round(file.size("inst/extdata/uk_calibrated_example.rds") / 1024),
  "KB ) from", ex$source, "\n"
)
