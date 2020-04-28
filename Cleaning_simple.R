#Correcting for unauthorised routing

ds$D003[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" | is.na(ds$D001)] <- NA
ds$D004_01[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" | is.na(ds$D001)] <- NA
ds$D004_02[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" | is.na(ds$D001)] <- NA
ds$D004_03[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" | is.na(ds$D001)] <- NA
ds$D004_04[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" | is.na(ds$D001)] <- NA
ds$D004_05[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" | is.na(ds$D001)] <- NA
ds$D005_01[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" | is.na(ds$D001)] <- NA
ds$D006_01[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" | is.na(ds$D001)] <- NA
ds$D007_01[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" | is.na(ds$D001)] <- NA
ds$D008_01[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" | is.na(ds$D001)] <- NA

ds$D002[ds$D001!="Employee" & ds$D001!="Self-employed with employees" & ds$D001!="Self-employed without employees" & ds$D001!="Unemployed" | is.na(ds$D001)] <- NA

#Setting all D007_01 in Slovenia to NA because of a translation issue
ds$D007_01[ds$B001=="Slovenia"] <- NA

##Dropouts
##Rule: Valid interview if the respondent did not drop out before page 27 (household questions). 
ds_clean <- ds[!(ds$LASTPAGE<27),]

##Item nonresponse
##Rule: Valid interview if there are less than 50% item nonresponse throughout the survey
# This only refers to the survey questions, not the demographics and paradata

questions <- names(list.filter(varinfo, section %in% c('Quality of life','Work and teleworking','Financial situation')))

ds_clean$item_na_count <- apply(is.na(ds_clean[questions]), 1, sum)

#If less then half of the questions have been answered the row will be removed
#Employees, self-employed and unemployed had more questions then other people (50 vs 40) so 
#we need to account for that.
ds_clean$long <- ds_clean$D001=="Employee" | ds_clean$D001=="Self-employed with employees" |
                 ds_clean$D001=="Self-employed without employees" 
ds_clean$long[is.na(ds_clean$D001)] <- FALSE

ds_clean$items[ds_clean$long==TRUE] <- 50
ds_clean$items[ds_clean$long==FALSE] <- 40

ds_clean$perc_na <- ds_clean$item_na_count / ds_clean$items

ds_clean <- ds_clean[ds_clean$perc_na<0.5,]


##Time spent

ds_clean <- ds_clean[!(ds_clean$TIME_SUM<180),]

## Age

ds_clean <- ds_clean[!(ds_clean$B003_01>98),]

##Tertiary education

primary <- (ds_clean$F004=="Primary")
secondary <- (ds_clean$F004=="Secondary")
tertiary <- (ds_clean$F004=="Tertiary")
edu_na <- is.na(ds_clean$F004)
under21 <- (ds_clean$B003_01<21)
over21 <- (ds_clean$B003_01>20)

ds_clean$youngtertiary <- case_when (tertiary & under21 ~ 1,
                               primary ~ 0,
                               secondary ~ 0,
                               edu_na ~ 0,
                               over21 ~ 0)
ds_clean <- mutate(ds_clean, F004 = ifelse(youngtertiary == 1, 2, F004))
ds_clean$F004 <- factor(ds_clean$F004,
                  levels = c(1,2,3),
                  labels = c("Primary", "Secondary", "Tertiary"))
drops <- "youngtertiary"
ds_clean <- ds_clean[, ! names(ds_clean) %in% drops, drop = F]

##Contradiction

contradiction_who5 <-sum ((ds_clean$C005_01=="All of the time" & ds_clean$C006_03=="All of the time"), na.rm=TRUE)
contradiction_who5

contradiction_who5_2 <-sum ((ds_clean$C005_01=="Most of the time" & ds_clean$C006_03=="Most of the time"), na.rm=TRUE)
contradiction_who5_2

##Duplicates
#Email

email_d <- duplicated(ds_clean["F021"])
email_none <- is.na(ds_clean$F021)

ds_clean$email_d <- (email_none=="FALSE"& email_d=="TRUE")

ds_clean$clean <- TRUE
