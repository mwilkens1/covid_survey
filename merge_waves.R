### ------------------ MERGE WAVES ---------------------- ###

library(dplyr)
library(rlist)
library(foreign)
library(haven)

#Loading in the wave 1 and wave 2 data
load('data/ds_raw_wave1.Rda')
ds_full_wave1 <- ds
ds_full_wave1$wave <- 1

load('data/ds_raw_wave2.Rda')
ds_full_wave2 <- ds
ds_full_wave2$wave <- 2

rm(ds)

# BECAUSE WAVE 2 IS NOT REALLY WAVE 2 AT THIS MOMENT I AM FAKING A FEW 
# NEW VARIABLES AND REMOVING TWO FROM WAVE 1
# THIS CAN BE REMOVED LATER
ds_full_wave2 <- ds_full_wave2 %>%
  select(-C002_01, -D004_01) %>%#removing 2 variables
  mutate(random_numeric = runif(nrow(ds_full_wave2)), #numeric
         random_factor = as.factor(runif(nrow(ds_full_wave2))<0.5))

# REMOVING OBSERVATIONS FROM WAVE 1 AND WAVE 2
ds_full_wave1 <- ds_full_wave1[sample(nrow(ds_full_wave1), 85000), ]
ds_full_wave2 <- ds_full_wave2[sample(nrow(ds_full_wave2), 85000), ]

# The datasets differ have overlapping and non overlapping variables. 
# so the mergeing should look like the table below where X are variables
# unique to wave1, y are variables that occur in both and z are variables
# unique to wave 2 and 'wave' is the variable showing the wave and case 
# is the case number

# CASE - wave - x1 - x2 - x3 - y1 - y2 - y3 - z1 - z2 - z3
#  1   -   1  -  1 -  1 -  1 -  1 -  1 -  1 - NA - NA - NA
#  2   -   1  -  1 -  1 -  1 -  1 -  1 -  1 - NA - NA - NA 
#  3   -   1  -  1 -  1 -  1 -  1 -  1 -  1 - NA - NA - NA 
#  4   -   2  - NA - NA - NA -  1 -  1 -  1 - 1  -  1 -  1
#  1   -   2  - NA - NA - NA -  1 -  1 -  1 - 1  -  1 -  1 
#  5   -   2  - NA - NA - NA -  1 -  1 -  1 - 1  -  1 -  1 

# Note that case number 1 appears in wave 1 and 2. This would be 
# someone who gave their email for wave 1 and filled out the survey again. 
# For case 1, variables y1, y2 and y3 can be compared over time.


### RECODE VARIABLE NAMES!!!

###

ds_merged_full <- full_join(ds_full_wave1, ds_full_wave2)

# the number of rows should be equal to the sum of the number of rows in both dataframes
stopifnot(nrow(ds_full_wave1) + nrow(ds_full_wave2) ==  nrow(ds_merged_full))

unique_in_wave1 <- setdiff(colnames(ds_full_wave1), colnames(ds_full_wave2))
unique_in_wave2 <- setdiff(colnames(ds_full_wave2), colnames(ds_full_wave1))
overlapping <- intersect(colnames(ds_full_wave2), colnames(ds_full_wave1))
 
# the number of columns sohuld be equal to unique in wave1 + unique in wave2 + interseting
stopifnot(length(unique_in_wave1) + length(unique_in_wave2) + length(overlapping) == ncol(ds_merged_full))

sprintf("There are %s unique variables in wave 1: %s", length(unique_in_wave1), paste(unique_in_wave1, collapse=", "))
sprintf("There are %s unique variables in wave 2: %s", length(unique_in_wave2), paste(unique_in_wave2, collapse=", "))
sprintf("There are %s intersecting variables: %s", length(overlapping), paste(overlapping, collapse=", "))

### ---------------------- LABEL AND RECODE ----------------------------- ###

source("label_and_recode.R", local=TRUE)

ds_merged_full <- label_and_recode(ds_merged_full)


### ---------------------- CLEANING ----------------------------- ###

#Loading both screening scripts
source("wave1/cleaning.R", local = TRUE)
source("wave2/cleaning.R", local = TRUE)

#Running the cleaning for each wave seperately because they are based on 
#different rules
ds_clean1 <- cleaning_wave1(ds_merged_full[ds_merged_full$wave==1,])
ds_clean1$wave <- 1
ds_clean2 <- cleaning_wave2(ds_merged_full[ds_merged_full$wave==2,])
ds_clean2$wave <- 2

ds_clean <- rbind(ds_clean1,ds_clean2)

ds_merged_full <- ds_merged_full %>%
  left_join(ds_clean[c("CASE","clean","wave")], by=c("CASE","wave"))

ds_merged_full$clean[is.na(ds_merged_full$clean)] <- FALSE

table(ds_merged_full$clean)

### ----------------------- WEIGHTING ------------------------------------ ###

# weighting is done for each wave seperately
source("weighting_by_country.R", local=TRUE)

#This runs the weight 3 times
weights_wave1 <- lapply(1:3, function(i) {
  
  weights <- ds_merged_full[ds_merged_full$wave==1,] %>% 
    filter(clean==TRUE) %>%
    weigh_data(minimum_weight = 0.05,
               trim_lower = 0.16,
               trim_upper = 6)
  
})

#This checks if the weights are identical in each of the three runs
stopifnot(weights_wave1[[1]]$w_country == weights_wave1[[2]]$w_country)
stopifnot(weights_wave1[[2]]$w_country == weights_wave1[[3]]$w_country)

#THis selects the weights of the first run
weights_wave1 <- weights_wave1[[1]]
weights_wave1$wave <- 1

#Same for wave 2
weights_wave2 <- lapply(1:3, function(i) {
  
  weights <- ds_merged_full[ds_merged_full$wave==2,] %>% 
    filter(clean==TRUE) %>%
    weigh_data(minimum_weight = 0.05,
               trim_lower = 0.16,
               trim_upper = 6)
  
})

#This checks if the weights are identical in each of the three runs
stopifnot(weights_wave2[[1]]$w_country == weights_wave2[[2]]$w_country)
stopifnot(weights_wave2[[2]]$w_country == weights_wave2[[3]]$w_country)

#This selects the weights of the first run
weights_wave2 <- weights_wave2[[1]]
weights_wave2$wave <- 2

#Merging the two
weights <- rbind(weights_wave1, weights_wave2)[,c("CASE","wave","w", "w_trimmed", "w_gross", "w_gross_trim")]

#Saving the weights
save(weights, file="data/weights.rda")

#adding the weights to the data
ds_merged_full <- left_join(ds_merged_full, weights, by=c("CASE","wave"))

#saving the data
save(ds_merged_full, file="data/ds_merged_full.rda")
write.dta(ds_merged_full, "data/ds_merged_full.dta")
write_sav(ds_merged_full, "data/ds_merged_full.sav")

### ----------------------- CREATING REFERENCE LIST ------------------------------ ###

source("create_reference_list.R", local=TRUE, encoding="utf-8")

save(varinfo, file="data/varinfo.rda")

### ------------------ DROPPING VARIABLES --------------------------- ###

# variable that are in the section 'other' in the varinfo file are dropped
to_drop <- names(list.filter(varinfo, section=='other'))

#Removing unclean and unweighted cases
ds <- ds_merged_full %>%
  filter(clean==TRUE, !is.na(w)) %>%
  select(-one_of(to_drop)) %>%
  droplevels()

### ----------------------- SAVING MERGED FILE ------------------------------ ###

save(ds, file="data/ds_merged.Rda")

### ----------------------- PREPARE MAP ------------------------------ ###

#Read in shapefile and select only relevant countries
shp_20 <- readOGR("shapefiles","CNTR_RG_20M_2016_4326") %>% 
  subset(NAME_ENGL %in% levels(ds$B001)) 

shp_20$NAME_ENGL <- droplevels(shp_20$NAME_ENGL)
shp_20$Country <- shp_20$NAME_ENGL

save(shp_20, file = "data/shp_20.rda")

