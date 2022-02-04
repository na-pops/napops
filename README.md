# napops <img src="man/figures/logo.png" align="right"/>

This package provides an R interface for users to download results from the NA-POPS database housed on Github. Users can access estimates of effective detection radius, cue rate, and detection probability for more than 300 species of North American landbirds.

## Installation

``` {.r}
# To install the development version from GitHub:
install.packages("devtools")
library(devtools)
devtools::install_github("na-pops/napops")
```
## Accessing Results
When you first install 'napops', you will need to fetch the data from the Github results repository. This can be done simply by calling
```{.r}
fetch_data()
```
which will download the latest version of the results from Github and assemble the
database on an app-specific directory on your computer.

NA-POPS regularly comes out with updated results as we get more data sent to us. Thus, it can be useful to check for updated results. You can do this by running
```{.r}
check_for_updates()
```
which will check to see if your local results are older than the most recent results on Github. If new results exist, you can download the new results with `fetch_data`. COMING SOON: The package will eventually automatically check for new updates upon attachment.

## Exploring Data
You can use the package to explore which species are modelled with NA-POPS by calling
```{.r}
napops_species <- list_species()
```
which will return a data frame with the 4 letter banding code, common name,
scientific name, family, and whether it was modelled with removal modelling, distance
modelling, or both. 

As this package is still in its infancy, the best way to explore the NA-POPS results through visualization would be through the [NA-POPS Results Dashboard](https://cons.carleton.ca/napops-dashboard).

## Generating Detectability Estimates
There are a few different components of detectability that are modelled in NA-POPS and are therefore available in the 'napops' package. I would recommend that you give [Solymos et al. 2013](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12106) a read to brush up *cue rate*, *effective detection radius*, *availability*, and *perceptibility*.

For all of these metrics, the user has access to results from the full suite of models used in NA-POPS. For the removal modelling process, NA-POPS considers a suite of 9 models consisting of different combinations of time since sunrise (TSSR), ordinal day (OD), and their quadratic terms (TSSR2 and OD2, respectively). The models are:

1. NULL Model
2. OD
3. OD + OD2
4. TSSR
5. TSSR + TSSR2
6. TSSR + OD
7. TSSR + OD + OD2
8. TSSR + TSSR2 + OD
9. TSSR + TSSR2 + OD + OD2

For the distance modelling process, NA-POPS considers a suite of 5 models consisting of covariates of roadside status (roadside), forest coverage (forest), and interactions. The models are:

1. NULL Model
2. Roadside
3. Forest
4. Roadside + Forest
5. Roadside X Forest (Interaction)

In the following metric function definitions, these models can be referred to by the corresponding number (i.e., 1, 4, 8, etc.). You can also choose "best", corresponding to the best model for the species as chosen by AIC for removal and distance.

### Cue Rate and Availability
Cue rate is a function of ordinal day and time since sunrise, depending on which model to use. If we wanted the cue rate of American Robin on the 153rd day of the year, 1 hour after sunrise, for the best model as chosen by AIC, we can use
```{.r}
amro_cr <- cue_rate(species = "AMRO", od = 153, tssr = 1, model = "best")
```
Alternatively, if we wanted to specifically get cue rate from model 6, we can use
```{.r}
amro_cr <- cue_rate(species = "AMRO", od = 153, tssr = 1, model = 6)
```
You can also get the availability using the same arguments as above. We also need to specify a maximum survey distance. For example, using the arguments above, for a 5 minute survey we can get availability with
```{.r}
amro_avail <- avail(species = "AMRO",od = 153, tssr = 1, model = "best", time = 5)
```

### Effective Detection Radius and Perceptibility
Effective detection radius is a function of survey roadside status and forest coverage, depending on which model to use. If we wanted the effective detection radius of American Robin on a roadside survey under 50% forest coverage, for the best model as chosen by AIC, we can use
```{.r}
amro_edr <- edr(species = "AMRO", roadside = TRUE, forest = 0.50, model = "best")
```
Alternatively, if we wanted to specifically get effective detection radius from model 4, we can use
```{.r}
amro_edr <- edr(species = "AMRO", roadside = TRUE, forest = 0.50, model = 4)
```
You can also get the perceptibility using the same arguments as above. We also need to specify a maximum survey radius. For example, using the arguments above, a bird 100 m
away, we can get perceptibility with
```{.r}
amro_percept <- percept(species = "AMRO", roadside = TRUE, forest = 0.50, model = "best", distance = 100)
```
