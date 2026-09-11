## data-raw/parameters.R
## Regenerates data/agz.rda, data/hyperparms.rda and data/parms.rda.
## Run with: Rscript -e 'devtools::load_all(); source("data-raw/parameters.R")'
## (load_all() needed so uv2ps()/qfun() from R/hyperparms.R are
## available without a full reinstall first)
##
## Previously this content lived in inst/extdata/parameters.R, which
## was never actually sourced anywhere -- data/hyperparms.rda and
## data/parms.rda were static, hand-saved files that had drifted out
## of sync with that source. This script is now the single source of
## truth; re-run it (then devtools::document() + reinstall) whenever
## hyperparms changes.

agz <- paste0(seq(from = 0, by = 5, len = 17), "-", seq(from = 4, by = 5, len = 17))
agz[length(agz)] <- "80+"

## === TB hyperparms
hyperparms <- list(
  staticfoi = list(fixed=-1,"fixed","Static or dynamic based on >0 or not"),
  asymp_relinf = list(fixed=1,"fixed","Relative infectiousness of asymptomatic vs symptomatic TB"),
  ## --------------------------------------------------- transmission
  ## bet=list(meanlog=log(10),sdlog=0.75),         #bet,      #beta
  ##psi:protn Andrews
  tbi_protn=list(shape1=20.7,shape2=77.9,"Protection against reinfection-disease from TB infection"),
  ## previous version:  foi=list(meanlog=log(1e-2),sdlog=0.5)
  ##ari0: to give 20% (10% to 30%) LTBI over 20 years
  foi=list(meanlog=-4.495672,sdlog=0.2717918,
           "A force-of-infection for the initial state (per year)"),
  ## --------------------------------------------------- progression
  ##kappa:arig Ragonnet
  stabilization=list(meanlog=0.62, sdlog=0.068,"Stabilization rate Fast->Slow latent (per year)"),
  progn_fast=list(meanlog=-2.837,sdlog=0.32,
                  "Fast latent progression rate to TB disease (per year)"),         #eps: pp Ragonnet
  progn_slow=list(meanlog=-6.89,sdlog=0.58,
                  "Slow latent progression rate to TB disease (per year)"),         #nu: Ragonnet
  relapse=list(meanlog=-3.95,sdlog=0.27,"Relapse rate (per year)"),         #omega: relapse Crampin NOTE x-ref
  ## --------------------------------------------------- detection
  ## CDRa=list(shape1=41.80,shape2=5.22),        #UK: CDR based on WHO data & Laura Anderson data
  ## --------------------------------------------------- timescales
  ## TODO update and ensure rate
  symptb_inversedurn=list(meanlog=1.1,sdlog=0.2,"Inverse duration for symptomatic TB (years)"),
  ## --------------------------------------------------- CFRs
  mortality_treated=list(shape1=157+5,shape2= 3426-157-5,"CFR for treated TB"), #UK data:
  ## https://www.gov.uk/government/publications/tuberculosis-in-england-2023-report-data-up-to-end-of-2022/tb-treatment-and-outcomes-england-2022
  symptb_CFR=list(shape1=25.48, shape2= 33.78,"CFR for asymptomatic untreated TB"),
  ## --------------------------------------------------- other
  ## m=list(meanlog=log(0.94),sdlog=0.1), #multiplier for population transmission = R
  ## tptHR = list(meanlog=-1.772,sdlog=0.089), #HR TPT protection in TBI+, Martinez et al 0.17 (0.14-0.2)
  ## tpt_drn = 20,       #durn of TPT: Salazar-Austin
  ##durn AS TB D from getLNparms(0.5,1) = Frascella half of TB SC + 1ydrn
  progn_symp = list(meanlog=-0.693,sdlog=0.97,
                    "Progression rate Asymp->Symp TB (per year)"),
  ## mHR = list(meanlog=0.131,sdlog=0.071),             #post-TB mortality HR
  treatment_inversedurn = list(fixed=2,"fixed","Inverse ATT duration (per year)"),
  ## --------------------------------------------------- migration
  ## previously hardcoded (0.2/0.8, 0.5/0.5) in ocaode.R; neither
  ## split is well pinned down by available UK surveillance data
  ## (time-since-arrival speaks to time since immigration, not time
  ## since infection or symptom status on arrival), so both are
  ## free/fitted parameters with weakly-informative priors centred
  ## on the previous defaults
  migr_splitEL=list(shape1=4,shape2=16,
    "Fraction of migrant LTBI that is early/fast-progression risk"),
  migr_splitAS=list(shape1=5,shape2=5,
    "Fraction of migrant active TB that is asymptomatic at arrival")
  ## late_post_time=2,    #duration defining early post-TB
  ## mort=0.02,            #mortality rate
  ## hrqolptb=list(meanlog=-3.324,sdlog=0.486), # HRQoL decrement while post TB
  ## hrqol = list(shape1 = 21.15177, shape2 = 42.36706) # GBD decrement 0.333 (0.224-0.454)
  )

## === tb parms: point estimate at the median of every prior
parms <- uv2ps(rep(0.5, length(hyperparms)), hyperparms) # natural history

usethis::use_data(agz, overwrite = TRUE)
usethis::use_data(hyperparms, overwrite = TRUE)
usethis::use_data(parms, overwrite = TRUE)
