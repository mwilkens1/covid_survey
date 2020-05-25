### ------------------ MERGE WAVES ---------------------- ###

#Loading in the wave 1 and wave 2 data
load('data/ds_0105_full.Rda')
ds_full_wave1 <- ds
ds_full_wave1$wave <- 1

load('data/ds_wave2_full.Rda')
ds_full_wave2 <- ds
ds_full_wave2$wave <- 2

rm(ds)

# BECAUSE WAVE 2 IS NOT REALLY WAVE 2 AT THIS MOMENT I AM FAKING A FEW 
# NEW VARIABLES AND REMOVING TWO FROM WAVE 1

ds_full_wave2 <- ds_full_wave2 %>%
  select(-C002_01, -D004_01) %>%#removing 2 variables
  mutate(random_numeric = runif(nrow(ds_full_wave2)), #numeric
         random_factor = as.factor(runif(nrow(ds_full_wave2))<0.5))

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

save(ds_merged_full, file="data/merged_full.rda")

### ----------------------- CREATING REFERENCE LIST ------------------------------ ###

source("create_reference_list.R", local=TRUE)

### ------------------ DROPPING VARIABLES --------------------------- ###

to_drop <- names(list.filter(varinfo, section=='other'))

#Removing unclean and unweighted cases
ds <- ds_merged_full %>%
  filter(clean==TRUE, !is.na(w)) %>%
  select(-one_of(to_drop)) %>%
  droplevels()

### ----------------------- SAVING FILES FOR APPS ------------------------------ ###

save(ds_full_wave1, file="data/ds_0105.Rda")
save(ds_full_wave2, file="data/ds_wave2.Rda")
save(ds, file="data/ds_merged.Rda")

save(varinfo, file="data/varinfo.rda")

### ----------------------- PREPARE MAP ------------------------------ ###

#Read in shapefile and select only relevant countries
shp_20 <- readOGR("shapefiles","CNTR_RG_20M_2016_4326") %>% 
  subset(NAME_ENGL %in% levels(ds$B001)) 

shp_20$NAME_ENGL <- droplevels(shp_20$NAME_ENGL)
shp_20$Country <- shp_20$NAME_ENGL

save(shp_20, file = "data/shp_20.rda")
