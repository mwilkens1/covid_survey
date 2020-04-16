##Dropouts
##Rule: Valid interview if the respondent did not drop out before page 27 (household questions). 

ds_clean <- ds[!(ds$LASTPAGE<27),]

##Item nonresponse
##Rule: Valid interview if there are less than 50% item nonresponse throughout the survey

ds_clean$item_na_count <- apply(is.na(ds_clean), 1, sum)
ds_clean <- ds_clean[!(ds_clean$item_na_count > 44),]

##Time spent

ds_clean <- ds_clean[!(ds_clean$TIME_SUM<180),]

## Age

ds_clean <- ds_clean[!(ds_clean$B003_01>98 & ds_clean$STARTED<"2020-04-10 17:00:00"),]

##Duplicates
#Email

email_d <- duplicated(ds_clean["F021"])
email_none <- is.na(ds_clean$F021)

ds_clean$email_d <- (email_none=="FALSE"& email_d=="TRUE")

ds_clean$clean <- TRUE
