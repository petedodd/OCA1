[![R Package Tests](https://github.com/petedodd/OCA1/actions/workflows/r.yml/badge.svg)](https://github.com/petedodd/OCA1/actions/workflows/r.yml)

# OCA1 <img src="man/figures/logo.png" align="right" height="139" alt="" />
UK TB transmission model

This uses `odin` vn 1 as this seems to work


## Work in progress:

2. introduce TB-related index layers
3. decide on and start to implement TB model structure

C. End project steps
1. more realistic input/output handling



## Updating

I'm using `usethis`, `devtools`, and `odin` to update package changes, like:
```r
library(here)
library(devtools)
library(usethis)

odin::odin_package('.')
devtools::document()

```

## Installing

This may work depending on config:
```r
devtools::install_github('petedodd/OCA1')
```

If not - clone the repo and install from local copy:

```r
devtools::install('my.local.path/OCA1')
```


## Using

NOTE need to redo with explicit migration at some point in any case

```r
##load
library(OCA1)

## example without nativity class used
pms <- create_demographic_parms()       #create UK parameters
out <- runmodel(pms)                    #run model with these, keep variable types separate
out$state                               #inspect
out$rate

## visualize
plt_DemoGrowth(out)               #total population over time
plt_DemoSnapshots(out)            #snapshots

plt_TBSnapshots(out, by_layer = "natcat")
plt_TBSnapshots(out, by_layer = "risk") # pyramid
plt_TBSnapshots(out, by_layer = "post") # pyramid

plt_TB_rates(out,rate_type = "incidence",by_layer = "natcat")
plt_TB_rates(out,rate_type = "notification",by_layer = "risk")
plt_TB_rates(out,rate_type = "mortality",by_layer = "post")



## version with 2 static nativity classes
pms <- create_parms(nnat = 2, migrationdata = list(propinitnat = c(0.9,0.1)))
pms$cdr_hz_nat    <- c(1, 1.05) # two cdr hz required here as we have two nativity classes

out <- runmodel(pms,raw=FALSE)
out

plt_DemoGrowth(out)
plt_DemoSnapshots(out)

## version with 2 static nativity classes and 2 static risk classes
pms <- create_parms(nnat = 2, nrisk = 2,
                    migrationdata = list(propinitnat = c(0.9,0.1)),
                    riskdata = list(propinitrisk = c(0.9,0.1)))
                    
## here we have to specify  two cdr hzr for both nat and risk groups. 
## Other  hz ratio params are at their default values of 1                  
pms$cdr_hz_nat    <- c(1, 1.05)    
pms$cdr_hz_risk    <- c(1, 1.05) 

out <- runmodel(pms)
out

plt_DemoGrowth(out)

## go big version with all strata to some degree:
pms <- create_parms(
  nnat = 2, nrisk = 2, npost = 2, nstrain = 2, nprot = 2,
  migrationdata = list(propinitnat = c(0.9,0.1)),
  riskdata = list(propinitrisk = c(0.9,0.1)),
  straindata = list(propinitstrain = c(0.9,0.1)),
  protdata = list(propinitprot = c(0.9,0.1)),
  verbose=TRUE)

## NOTE needs to work harder:
out <- runmodel(pms)
out

plt_DemoGrowth(out) #still stable

## Looking at TB snapshots


```

## TODO
- How to incorporate this info- the most common countries of birth for those born outside of the UK were India, Pakistan, Romania, Bangladesh and Eritrea
- Next version should be about using flexible approach to put truncation on haz ratio not to exceed 1 for CDR

## License

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

