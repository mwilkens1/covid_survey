# TRANSFORM AND LOAD
# While reading this file, comments will be created for all variables.
# The comments for values will be stored as attributes (attr) as well.
 
#setwd("C:/Users/mwi/OneDrive - Eurofound/covid_survey/")

library(dplyr)
library(rgdal)
library(car)
library(survey)
library(weights)
library(anesrake)
library(sjstats)
library(rlist)

#Loading the raw data from the extract script
load("data/ds_raw_wave2.Rda")

### ----------------------- LABELING AND RECODING ------------------------------ ###

source("wave2/label_and_recode.R")
ds <- label_and_recode(ds)

### ----------------------- CLEANING ------------------------------ ###

#Cleaning the data
source("wave2/Cleaning_simple.R", local = TRUE)

ds <- ds %>%
  left_join(ds_clean[c("CASE","clean")], by="CASE")

ds$clean[is.na(ds$clean)] <- FALSE

table(ds$clean)

### ----------------------- WEIGHTING ------------------------------ ###

source("wave2/weighting_by_country.R",local=TRUE)

# Running the weight 3 times to make sure its identical
# If insufficient iterations or too low convergence criterion the algorithm
# might converge at different points. 
weights <- lapply(1:3, function(i) {
  
  weights <- ds %>% 
    filter(clean==TRUE) %>%
    weigh_data(minimum_weight = 0.05,
               trim_lower = 0.16,
               trim_upper = 6)
  
})

stopifnot(weights[[1]]$w_country == weights[[2]]$w_country)
stopifnot(weights[[2]]$w_country == weights[[3]]$w_country)

weights <- weights[[1]]

ds <- weights %>%
  select(CASE, w, w_trimmed, w_gross, w_gross_trim) %>%
  right_join(ds, by="CASE")

### ----------------------- SAVING FILES FOR ANALYSIS ------------------------------ ###
#note: these include unclean interviews and unweighted data.

save(ds, file="data/ds_wave2_full.Rda")

save(weights, file="data/weights_wave2.Rda")

library(foreign)
write.dta(ds, "data/ds_wave2_full.dta")

library(haven)
write_sav(ds, "data/ds_wave2_full.sav")
