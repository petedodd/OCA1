# OCA1
UK TB transmission model

This uses `odin` vn 1 as tjos seems to work


## Work in progress:

Currently, this is just simulating 17 age cat 2 sex demography for UK

A. Immediate next steps:

1. work on additional indices

B. Medium term next steps:
1. introduce indices relating to migration and in/out migration
2. introduce TB-related index layers
3. decide on and start to implement TB model structure

C. End project steps
1. more realistic input/output handling
2. fitting utilities?


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
pms <- create_demographic_parms() #create UK parameters
out <- runmodel(pms)              #run model with these
out                               #inspect

## visualize
plt_DemoGrowth(out)               #total population over time
plt_DemoSnapshots(out)            #snapshots

## version with 2 static nativity classes
pms <- create_demographic_parms(propinitnat = c(0.9,0.1))
out <- runmodel(pms,raw=FALSE)
out

plt_DemoGrowth(out)
plt_DemoSnapshots(out)

## version with 2 static nativity classes and 2 static risk classes
pms <- create_demographic_parms(propinitnat = c(0.9,0.1),propinitrisk = c(0.9,0.1))
out <- runmodel(pms)
out

plt_DemoGrowth(out)

```




## License

[![License: CC BY 4.0](https://img.shields.io/badge/License-CC_BY_4.0-lightgrey.svg)](https://creativecommons.org/licenses/by/4.0/)

