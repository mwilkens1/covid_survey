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
load("data/ds_raw_2804.Rda")

# Variable und Value Labels
ds$EU27 = ds$B001>0 & ds$B001<28
ds$B001 = factor(ds$B001, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","43","44","45","46","47","48","49","50","51","52","53","54","55","56","57","58","59","60"), labels=c("Austria","Belgium","Bulgaria","Croatia","Cyprus","Czechia","Denmark","Estonia","Finland","France","Germany","Greece","Hungary","Ireland","Italy","Latvia","Lithuania","Luxembourg","Malta","Netherlands","Poland","Portugal","Romania","Slovakia","Slovenia","Spain","Sweden","Albania","Bosnia and Herzegovina","Brazil","Canada","China","Colombia","Ecuador","Egypt","India","Indonesia","Iran","Japan","Mexico","Montenegro","Morocco","Netherlands Antilles","Nigeria","North Macedonia","Pakistan","Philippines","Russia","Serbia","South Korea","Switzerland","Suriname","Syria","Thailand","Turkey","Ukraine","United Kingdom","United States","Vietnam","Other country"), ordered=FALSE)
levels(ds$B001)[c(28:56,58:60)] <- "Other country"
ds$B002 = factor(ds$B002, levels=c("1","2","3"), labels=c("Male","Female","In another way"), ordered=FALSE)
ds$C008 = factor(ds$C008, levels=c("1","2","3","4"), labels=c("The open countryside","A village/small town","A medium to large town","A city or city suburb"), ordered=FALSE)
ds$D001 = factor(ds$D001, levels=c("1","2","3","4","5","6","7","8"), labels=c("Employee","Self-employed with employees","Self-employed without employees","Unemployed","Unable to work due to long-term illness or disability","Retired","Full-time homemaker/fulfilling domestic tasks","Student"), ordered=FALSE)
ds$D002 = factor(ds$D002, levels=c("1","2","3"), labels=c("Yes, permanently","Yes, temporarily","No"), ordered=FALSE)
ds$D003 = factor(ds$D003, levels=c("1","2","3","4","5"), labels=c("Decreased a lot","Decreased a little","Stayed the same","Increased a little","Increased a lot"), ordered=FALSE)
ds$E004 = factor(ds$E004, levels=c("1","2","3"), labels=c("Better","The same","Worse"), ordered=TRUE)
ds$E005 = factor(ds$E005, levels=c("1","2","3"), labels=c("Better","The same","Worse"), ordered=TRUE)
ds$E006 = factor(ds$E006, levels=c("5","1","2","3","4"), labels=c("No savings","Less than 3 months","From 3 up to 6 months","From 6 up to 12 months","12 or more months"), ordered=TRUE)
ds$F004 = factor(ds$F004, levels=c("1","2","3"), labels=c("Primary","Secondary","Tertiary"), ordered=TRUE)
ds$F005 = factor(ds$F005, levels=c("1","2","3"), labels=c("East Austria","South Austria","West Austria"), ordered=FALSE)
ds$F006 = factor(ds$F006, levels=c("1","2","3"), labels=c("Brussels Capital Region","Flemish Region","Walloon Region"), ordered=FALSE)
ds$F007 = factor(ds$F007, levels=c("1","2"), labels=c("Northern and Eastern Bulgaria","South-Western and South-Central Bulgaria"), ordered=FALSE)
ds$F008 = factor(ds$F008, levels=c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), labels=c("Baden-Württemberg","Bavaria","Berlin","Brandenburg","Bremen","Hamburg","Hessen","Mecklenburg-Vorpommern","Lower Saxony","North Rhine-Westphalia","Rhineland-Palatinate","Saarland","Saxony","Saxony-Anhalt","Schleswig-Holstein","Thuringia"), ordered=FALSE)
ds$F009 = factor(ds$F009, levels=c("1","2","3","4"), labels=c("Attica","Nisia Aigaiou, Kriti","Voreia Ellada","Kentriki Ellada"), ordered=FALSE)
ds$F010 = factor(ds$F010, levels=c("1","2","3","4","5","6","7"), labels=c("North West","North East","Community of Madrid","Centre","East","South","Canary Islands"), ordered=FALSE)
ds$F011 = factor(ds$F011, levels=c("1","2"), labels=c("Mainland Finland","Åland"), ordered=FALSE)
ds$F012 = factor(ds$F012, levels=c("1","2","3","4","5","6","7","8","9"), labels=c("Région parisienne","Bassin parisien","Nord","Est","Ouest","Sud-Ouest","Centre-Est (Auvergne-Rhône-Alpes)","Méditerranée","Départements d\'Outre-Mer"), ordered=FALSE)
ds$F013 = factor(ds$F013, levels=c("1","2","3"), labels=c("Central Hungary","Transdanubia","Great Plain and North"), ordered=FALSE)
ds$F014 = factor(ds$F014, levels=c("1","2","3","4","5"), labels=c("North West","North East","Centre","South","Islands"), ordered=FALSE)
ds$F015 = factor(ds$F015, levels=c("1","2","3","4"), labels=c("North Netherlands","East Netherlands","West Netherlands","South Netherlands"), ordered=FALSE)
ds$F016 = factor(ds$F016, levels=c("1","2","3","4","5","6"), labels=c("Central Region","South Region","East Region","Northwest Region","Southwest Region","North Region"), ordered=FALSE)
ds$F017 = factor(ds$F017, levels=c("1","2","3"), labels=c("Mainland Portugal","Azores","Madeira"), ordered=FALSE)
ds$F018 = factor(ds$F018, levels=c("1","2","3","4"), labels=c("Nord-Vest, Centru","Nord-Est, Sud-Est","Sud – Muntenia, Bucuresti – Ilfov","Sud-Vest Oltenia, Vest"), ordered=FALSE)
ds$F019 = factor(ds$F019, levels=c("1","2","3"), labels=c("East Sweden","South Sweden","North Sweden"), ordered=FALSE)
ds$F020 = factor(ds$F020, levels=c("1","2","3","4","5","6","7","8","9","10","11","12"), labels=c("North East","North West","Yorkshire and the Humber","East Midlands","West Midlands","East of England","Greater London","South East","South West","Wales","Scotland","Northern Ireland"), ordered=FALSE)

attr(ds$C001_01,"1") = "1 Very dissatisfied"
attr(ds$C001_01,"2") = "2"
attr(ds$C001_01,"3") = "3"
attr(ds$C001_01,"4") = "4"
attr(ds$C001_01,"5") = "5"
attr(ds$C001_01,"6") = "6"
attr(ds$C001_01,"7") = "7"
attr(ds$C001_01,"8") = "8"
attr(ds$C001_01,"9") = "9"
attr(ds$C001_01,"10") = "10 Very satisfied"
attr(ds$C001_01,"-1") = "Don\'t know/ Prefer not to answer"

attr(ds$C002_01,"1") = "1 Very unhappy"
attr(ds$C002_01,"2") = "2"
attr(ds$C002_01,"3") = "3"
attr(ds$C002_01,"4") = "4"
attr(ds$C002_01,"5") = "5"
attr(ds$C002_01,"6") = "6"
attr(ds$C002_01,"7") = "7"
attr(ds$C002_01,"8") = "8"
attr(ds$C002_01,"9") = "9"
attr(ds$C002_01,"10") = "10 Very happy"
attr(ds$C002_01,"-1") = "Don\'t know/ Prefer not to answer"

for (var in c("C003_01", "C003_02", "C003_03", "C003_04")) {
  
  ds[[var]] <- factor(ds[[var]], levels = c(1,2,3,4,5), 
                      labels=c("Strongly agree", "Agree", "Neither agree nor disagree", "Disagree", "Strongly disagree"),
                      ordered=TRUE)
  
}

ds$C004_01 <- factor(ds$C004_01, levels=c(1,2,3,4,5),
                                 labels=c("Very good","Good","Fair","Bad","Very bad"))

for (var in c("C005_01","C005_02","C005_03","C005_04","C005_05")) {
  
  ds[[var]] <- factor(ds[[var]], levels = c(1,2,3,4,5,6), 
                      labels=c("At no time", "Some of the time",  "Less than half of the time","More than half of the time", "Most of the time","All of the time"),
                      ordered=TRUE)
  
}

for (var in c("C006_01","C006_02","C006_03")) {
  
  ds[[var]] <- factor(ds[[var]], levels = c(1,2,3,4,5,6), 
                      labels=c("All of the time","Most of the time","More than half of the time","Less than half of the time","Some of the time", "At no time"),
                      ordered=TRUE)
  
}

attr(ds$C007_01,"1") = "1 Do not trust at all"
attr(ds$C007_01,"2") = "2"
attr(ds$C007_01,"3") = "3"
attr(ds$C007_01,"4") = "4"
attr(ds$C007_01,"5") = "5"
attr(ds$C007_01,"6") = "6"
attr(ds$C007_01,"7") = "7"
attr(ds$C007_01,"8") = "8"
attr(ds$C007_01,"9") = "9"
attr(ds$C007_01,"10") = "10 Trust completely"
attr(ds$C007_01,"-1") = "Don\'t know/ Prefer not to answer"
attr(ds$C007_02,"1") = "1 Do not trust at all"
attr(ds$C007_02,"2") = "2"
attr(ds$C007_02,"3") = "3"
attr(ds$C007_02,"4") = "4"
attr(ds$C007_02,"5") = "5"
attr(ds$C007_02,"6") = "6"
attr(ds$C007_02,"7") = "7"
attr(ds$C007_02,"8") = "8"
attr(ds$C007_02,"9") = "9"
attr(ds$C007_02,"10") = "10 Trust completely"
attr(ds$C007_02,"-1") = "Don\'t know/ Prefer not to answer"
attr(ds$C007_03,"1") = "1 Do not trust at all"
attr(ds$C007_03,"2") = "2"
attr(ds$C007_03,"3") = "3"
attr(ds$C007_03,"4") = "4"
attr(ds$C007_03,"5") = "5"
attr(ds$C007_03,"6") = "6"
attr(ds$C007_03,"7") = "7"
attr(ds$C007_03,"8") = "8"
attr(ds$C007_03,"9") = "9"
attr(ds$C007_03,"10") = "10 Trust completely"
attr(ds$C007_03,"-1") = "Don\'t know/ Prefer not to answer"
attr(ds$C007_04,"1") = "1 Do not trust at all"
attr(ds$C007_04,"2") = "2"
attr(ds$C007_04,"3") = "3"
attr(ds$C007_04,"4") = "4"
attr(ds$C007_04,"5") = "5"
attr(ds$C007_04,"6") = "6"
attr(ds$C007_04,"7") = "7"
attr(ds$C007_04,"8") = "8"
attr(ds$C007_04,"9") = "9"
attr(ds$C007_04,"10") = "10 Trust completely"
attr(ds$C007_04,"-1") = "Don\'t know/ Prefer not to answer"
attr(ds$C007_05,"1") = "1 Do not trust at all"
attr(ds$C007_05,"2") = "2"
attr(ds$C007_05,"3") = "3"
attr(ds$C007_05,"4") = "4"
attr(ds$C007_05,"5") = "5"
attr(ds$C007_05,"6") = "6"
attr(ds$C007_05,"7") = "7"
attr(ds$C007_05,"8") = "8"
attr(ds$C007_05,"9") = "9"
attr(ds$C007_05,"10") = "10 Trust completely"
attr(ds$C007_05,"-1") = "Don\'t know/ Prefer not to answer"

for (var in c("D004_01","D004_02","D004_03","D004_04","D004_05")) {
  
  ds[[var]] <- factor(ds[[var]], levels = c(1,2,3,4,5), 
                      labels=c("Always","Most of the time","Sometimes","Rarely","Never"),
                      ordered=TRUE)
  
}

ds$D005_01 <- factor(ds$D005_01, levels=c(1,2,3,4,5),
                                 labels=c("Every day","Every other day","Once or twice a week","Less often","Never"),
                     ordered=TRUE)

ds$D006_01 <- factor(ds$D006_01, levels=c(1,2,3,4,5),
                     labels=c("Daily","Several times a week","Several times a month","Less often","Never"),
                     ordered=TRUE)

ds$D007_01 <- factor(ds$D007_01, levels=c(1,2), labels=c("No","Yes"))

for (var in c("D008_01","E007_01")) {

  ds[[var]] <- factor(ds[[var]], levels=c(1,2,3,4,5), 
                                   labels=c("Very likely","Rather likely","Neither likely nor unlikely","Rather unlikely","Very unlikely"),
                       ordered=TRUE)

}

ds$E001_01 <- factor(ds$E001_01, levels=c(1,2,3,4,5,6), 
                     labels=c("With great difficulty","With difficulty","With some difficulty","Fairly easily","Easily","Very easily"),
                     ordered=TRUE)

for (var in c("E002_01","E002_02","E003_01","E003_02","E003_03","E003_04","E003_05","E003_06","F002_01","F022_01")) {
  
  ds[[var]] <- factor(ds[[var]], levels = c(1,2), 
                      labels=c("Yes","No"))
  
}

for (var in c("E008_01","E008_02","E008_03","E008_04","E008_05","E008_06")) {
  
  ds[[var]] <- factor(ds[[var]], levels=c(1,2,3,4), 
                      labels=c("A member of your family/relative",
                               "A friend, neighbour, or someone else",
                               "A service provider, institution or organisation",
                               "Nobody"))
}


attr(ds$F001_01a,"F") = "Not checked"
attr(ds$F001_01a,"T") = "Checked"
attr(ds$F003_01a,"F") = "Not checked"
attr(ds$F003_01a,"T") = "Checked"
attr(ds$F003_02a,"F") = "Not checked"
attr(ds$F003_02a,"T") = "Checked"

attr(ds$FINISHED,"F") = "Canceled"
attr(ds$FINISHED,"T") = "Finished"
attr(ds$Q_VIEWER,"F") = "Respondent"
attr(ds$Q_VIEWER,"T") = "Spectator"

comment(ds$CASE) = "Case number"
comment(ds$SERIAL) = "Serial number (if provided)"
comment(ds$REF) = "Reference (if provided in link)"
comment(ds$QUESTNNR) = "Questionnaire that has been used in the interview"
comment(ds$MODE) = "Interview mode"
comment(ds$LANGUAGE) = "Language"
comment(ds$STARTED) = "Time the interview has started (Europe/Berlin)"
comment(ds$B001) = "Country"
comment(ds$B002) = "Gender"
comment(ds$B003_01) = "Age"
comment(ds$C001_01) = "Life satisfaction"
comment(ds$C002_01) = "Percieved happiness"
comment(ds$C003_01) = "I am optimistic about my future"
comment(ds$C003_02) = "I am optimistic about my children's or grandchildren's future"
comment(ds$C003_03) = "I find it difficult to deal with important problems that come up in my life"
comment(ds$C003_04) = "When things go wrong in my life, it generally takes me a long time to get back to normal"
comment(ds$C004_01) = "Perceived health status"
comment(ds$C005_01) = "I have felt cheerful and in good spirits"
comment(ds$C005_02) = "I have felt calm and relaxed"
comment(ds$C005_03) = "I have felt active and vigorous"
comment(ds$C005_04) = "I woke up feeling fresh and rested"
comment(ds$C005_05) = "My daily life has been filled with things that interest me"
comment(ds$C006_01) = "I have felt particularly tense"
comment(ds$C006_02) = "I have felt lonely"
comment(ds$C006_03) = "I have felt downhearted and depressed"
comment(ds$C007_01) = "The news media"
comment(ds$C007_02) = "The police"
comment(ds$C007_03) = "Your country’s government"
comment(ds$C007_04) = "The European Union"
comment(ds$C007_05) = "The healthcare system"
comment(ds$C008) = "Urbanisation"
comment(ds$D001) = "Employment status"
comment(ds$D002) = "Lost your job(s) or contract(s)"
comment(ds$D003) = "Any change in working hours"
comment(ds$D004_01) = "Kept worrying about work when you were not working"
comment(ds$D004_02) = "Felt too tired after work to do some of the household jobs which need to be done"
comment(ds$D004_03) = "Found that your job prevented you from giving the time you wanted to your family"
comment(ds$D004_04) = "Found it difficult to concentrate on your job because of your family responsibilities"
comment(ds$D004_05) = "Found that your family responsibilities prevented you from giving the time you should to your job"
comment(ds$D005_01) = "Over the last 2 weeks, how often have you worked in your free time to meet work demands?"
comment(ds$D006_01) = "Frequency of working from home before the outbreak"
comment(ds$D007_01) = "Started to work from home as a result of the situation"
comment(ds$D008_01) = "Do you think you might lose your job in the next 3 months?"
comment(ds$E001_01) = "Is your household able to make ends meet?"
comment(ds$E002_01) = "Gone without fresh fruit and vegetables"
comment(ds$E002_02) = "Bought cheaper cuts of meat or bought less than wanted"
comment(ds$E003_01) = "Rent or mortgage payments for accommodation"
comment(ds$E003_02) = "Utility bills, such as electricity, water, gas"
comment(ds$E003_03) = "Payments related to consumer loans, including credit card overdrafts"
comment(ds$E003_04) = "Telephone, mobile or internet connection bills"
comment(ds$E003_05) = "Payments related to informal loans from friends or relatives not living in your household"
comment(ds$E003_06) = "Payments for healthcare or health insurance"
comment(ds$E004) = "Financial situation of household now compared to 3 months ago"
comment(ds$E005) = "Expected financial situation of household in 3 months"
comment(ds$E006) = "Without income, length of time household could maintain the same standard of living using savings"
comment(ds$E007_01) = "Think you will need to leave your accommodation within the next 6 months because you can no longer afford it"
comment(ds$E008_01) = "If you needed help around the house when ill"
comment(ds$E008_02) = "If you needed advice about a serious personal or family matter"
comment(ds$E008_03) = "If you needed help when looking for a job"
comment(ds$E008_04) = "If you were feeling a bit depressed and wanting someone to talk to"
comment(ds$E008_05) = "If you needed help in looking after your children"
comment(ds$E008_06) = "If you needed help with shopping"
comment(ds$F001_01) = "Household size"
comment(ds$F001_01a) = "Household size: Don\'t know/Prefer not to answer"
comment(ds$F002_01) = "Partner"
comment(ds$F003_01) = "Number of children: Age 0-11"
comment(ds$F003_01a) = "Number of children: Age 0-11: Don\'t know/Prefer not to answer"
comment(ds$F003_02) = "Number of children: Age 12-17"
comment(ds$F003_02a) = "Number of children: Age 12-17: Don\'t know/Prefer not to answer"
comment(ds$F004) = "Education"
comment(ds$F005) = "NUTS 1 AT"
comment(ds$F006) = "NUTS 1 BE"
comment(ds$F007) = "NUTS 1 BG"
comment(ds$F008) = "NUTS 1 DE"
comment(ds$F009) = "NUTS 1 EL"
comment(ds$F010) = "NUTS 1 ES"
comment(ds$F011) = "NUTS 1 FI"
comment(ds$F012) = "NUTS 1 FR"
comment(ds$F013) = "NUTS 1 HU"
comment(ds$F014) = "NUTS 1 IT"
comment(ds$F015) = "NUTS 1 NL"
comment(ds$F016) = "NUTS 1 PL"
comment(ds$F017) = "NUTS 1 PT"
comment(ds$F018) = "NUTS 1 RO"
comment(ds$F019) = "NUTS 1 SE"
comment(ds$F020) = "NUTS 1 UK"
comment(ds$F021) = "Person ID (SERIAL)"
comment(ds$F022_01) = "Reporting: [No Description] 01"
comment(ds$TIME001) = "Time spent on page 1"
comment(ds$TIME002) = "Time spent on page 2"
comment(ds$TIME003) = "Time spent on page 3"
comment(ds$TIME004) = "Time spent on page 4"
comment(ds$TIME005) = "Time spent on page 5"
comment(ds$TIME006) = "Time spent on page 6"
comment(ds$TIME007) = "Time spent on page 7"
comment(ds$TIME008) = "Time spent on page 8"
comment(ds$TIME009) = "Time spent on page 9"
comment(ds$TIME010) = "Time spent on page 10"
comment(ds$TIME011) = "Time spent on page 11"
comment(ds$TIME012) = "Time spent on page 12"
comment(ds$TIME013) = "Time spent on page 13"
comment(ds$TIME014) = "Time spent on page 14"
comment(ds$TIME015) = "Time spent on page 15"
comment(ds$TIME016) = "Time spent on page 16"
comment(ds$TIME017) = "Time spent on page 17"
comment(ds$TIME018) = "Time spent on page 18"
comment(ds$TIME019) = "Time spent on page 19"
comment(ds$TIME020) = "Time spent on page 20"
comment(ds$TIME021) = "Time spent on page 21"
comment(ds$TIME022) = "Time spent on page 22"
comment(ds$TIME023) = "Time spent on page 23"
comment(ds$TIME024) = "Time spent on page 24"
comment(ds$TIME025) = "Time spent on page 25"
comment(ds$TIME026) = "Time spent on page 26"
comment(ds$TIME027) = "Time spent on page 27"
comment(ds$TIME028) = "Time spent on page 28"
comment(ds$TIME_SUM) = "Time spent overall (except outliers)"
comment(ds$MAILSENT) = "Time when the invitation mailing was sent (personally identifiable recipients, only)"
comment(ds$LASTDATA) = "Time when the data was most recently updated"
comment(ds$FINISHED) = "Has the interview been finished (reached last page)?"
comment(ds$Q_VIEWER) = "Did the respondent only view the questionnaire, omitting mandatory questions?"
comment(ds$LASTPAGE) = "Last page that the participant has handled in the questionnaire"
comment(ds$MAXPAGE) = "Hindmost page handled by the participant"

#Getting list of all numeric variables
nums <- unlist(lapply(ds, is.numeric))  
num_vars <- names(nums[nums==TRUE])

#Replacing all -1 and -9 values of numeric variables with NA
for (var in num_vars) {
  
  ds[var][ds[var]==-1 | ds[var]==-9] <- NA
  
}


### ----------------------- RECODES ------------------------------ ###

#Create age groups
ds$age_group[ds$B003_01<35] <- "18-34"
ds$age_group[ds$B003_01>=35 & ds$B003_01<50] <- "35-49"
ds$age_group[ds$B003_01>=50] <- "50+"
ds$age_group <- factor(ds$age_group, levels=c("18-34","35-49","50+"), ordered=TRUE)

#Recode employment status
ds$emp_stat  <- recode_factor(ds$D001,
                              "Employee"  = "Employee",
                              "Self-employed with employees" = "Self-employed",
                              "Self-employed without employees" = "Self-employed",
                              "Unemployed" = "Unemployed",
                              "Retired" = "Retired",
                              "Unable to work due to long-term illness or disability" = "Other",
                              "Full-time homemaker/fulfilling domestic tasks" = "Other",
                              "Student" = "Other") 


### ----------------------- CREATING REFERENCE LIST ------------------------------ ###


##Here a list is created with all sorts of info per variable

varinfo <- lapply(colnames(ds), function(x) { 
  
  list( # section
    section = {if (startsWith(x, "C0")) {"Quality of life"} 
      else if (startsWith(x, "D0")) {"Work and teleworking"}
      else if (startsWith(x, "E0")) {"Financial situation"}
      else {"other"}},
    
    # label will appear in the selection box
    label = comment(ds[[x]]),
    
    # smaller subtext after the label
    subtext = "",
    
    # Full question wording
    question = NULL,
    
    # Extra explanatory text
    extra_text = NULL,
    
    # class of the variable
    class = class(ds[[x]]),
    
    # any levels in case its a categorical variable
    levels = levels(ds[[x]]),
  
    # default selected levels is initated
    default_levels = NULL,
    
    # Special axis range for numerical variables
    range = NULL
    
    )
  
})

names(varinfo) <- colnames(ds)

#Define extra descriptions
varinfo[["C001_01"]]$extra_text <-"Life satistfaction is measured on a scale of 1 to 10, 
  where 1 means very dissatisfied and 10 means very satisfied." 
varinfo[["C002_01"]]$extra_text <-"Happiness is measured on a scale of 1 to 10, 
  where 1 means very unhappy and 10 means very happy." 


for (var in c("C005_01","C005_02","C005_03","C005_04","C005_05")) {
  
  varinfo[[var]]$extra_text <- "WHO-5 is the World Health Organization’s Mental Well-being Index. 
    On a scale from 0 to 100, people with a WHO-5 score of 50 or 
    lower are considered at risk of depression."
  
}

for (var in c("C007_01","C007_02","C007_03","C007_04","C007_05")) {
  
  varinfo[[var]]$extra_text <- "Trust is measured on a scale of 1 to 10, 
    where 1 means that you do not trust at all, 
    and 10 means that you trust completely."
  
}


#Full questions
varinfo[["C001_01"]]$question <- "All things considered, how satisfied would you say you are with your life these days?"
varinfo[["C002_01"]]$question <- "Taking all things together on a scale of 1 to 10, how happy would you say you are?"
varinfo[["C003_01"]]$question <- "To what extent do you agree or disagree with the following statement? I am optimistic about my future."
varinfo[["C003_02"]]$question <- "To what extent do you agree or disagree with the following statement? I am optimistic about my children’s or grandchildren’s future."
varinfo[["C003_03"]]$question <- "To what extent do you agree or disagree with the following statement? I find it difficult to deal with important problems that come up in my life."
varinfo[["C003_04"]]$question <- "To what extent do you agree or disagree with the following statement? When things go wrong in my life, it generally takes me a long time to get back to normal."
varinfo[["C004_01"]]$question <- "In general, how is your health?"
varinfo[["C005_01"]]$question <- "How have you been feeling over the last two weeks? I have felt cheerful and in good spirits."
varinfo[["C005_02"]]$question <- "How have you been feeling over the last two weeks? I have felt calm and relaxed."
varinfo[["C005_03"]]$question <- "How have you been feeling over the last two weeks? I have felt active and vigorous."
varinfo[["C005_04"]]$question <- "How have you been feeling over the last two weeks? I woke up feeling fresh and rested."
varinfo[["C005_05"]]$question <- "How have you been feeling over the last two weeks? My daily life has been filled with things that interest me."
varinfo[["C006_01"]]$question <- "How have you been feeling over the last two weeks? I have felt particularly tense."
varinfo[["C006_02"]]$question <- "How have you been feeling over the last two weeks? I have felt lonely."
varinfo[["C006_03"]]$question <- "How have you been feeling over the last two weeks? I have felt downhearted and depressed."
varinfo[["C007_01"]]$question <- "Please tell me how much you personally trust each of the following institutions? The news media."
varinfo[["C007_02"]]$question <- "Please tell me how much you personally trust each of the following institutions? The police."
varinfo[["C007_03"]]$question <- "Please tell me how much you personally trust each of the following institutions? Your country's government."
varinfo[["C007_04"]]$question <- "Please tell me how much you personally trust each of the following institutions? The European Union."
varinfo[["C007_05"]]$question <- "Please tell me how much you personally trust each of the following institutions? The healthcare system."
varinfo[["D002"]]$question    <- "During the COVID-19 pandemic have you lost your job(s)/contract(s)?"
varinfo[["D003"]]$question    <- "During the COVID-19 pandemic have your working hours...?"
varinfo[["D004_01"]]$question <- "How often in the last 2 weeks, have you kept worrying about work when you were not working?" 
varinfo[["D004_02"]]$question <- "How often in the last 2 weeks, have you felt too tired after work to do some of the household jobs which need to be done."
varinfo[["D004_03"]]$question <- "How often in the last 2 weeks, have you found that your job prevented you from giving the time you wanted to your family?"
varinfo[["D004_04"]]$question <- "How often in the last 2 weeks, have you found it difficult to concentrate on your job because of your family responsibilities?"
varinfo[["D004_05"]]$question <- "How often in the last 2 weeks, have you found that your family responsibilities prevented you from giving the time you should to your job?"
varinfo[["D007_01"]]$question <- "Have you started to work from home as a result of the COVID-19 situation?"
varinfo[["D008_01"]]$question <- "How likely or unlikely do you think it is that you might lose your job in the next 3 months?"
varinfo[["E001_01"]]$question <- "A household may have different sources of income and more than one household member may contribute to it. Thinking of your household’s total monthly income: is your household able to make ends meet?" 
varinfo[["E002_01"]]$question <- "Thinking about food, over the last two weeks did you or someone else in your household change your diet because money was needed for other essentials? Gone without fresh fruit and vegetables."
varinfo[["E002_02"]]$question <- "Thinking about food, over the last two weeks did you or someone else in your household change your diet because money was needed for other essentials? Bought cheaper cuts of meat or bought less than wanted."
varinfo[["E003_01"]]$question <- "Has your household been in arrears at any time during the past 3 months, that is, unable to pay as scheduled... Rent or mortgage payments for accommodation?"
varinfo[["E003_02"]]$question <- "Has your household been in arrears at any time during the past 3 months, that is, unable to pay as scheduled... Utility bills, such as electricity, water, gas?"
varinfo[["E003_03"]]$question <- "Has your household been in arrears at any time during the past 3 months, that is, unable to pay as scheduled... Payments related to consumer loans, including credit card overdrafts (to buy electrical appliances, a car, furniture, etc.)?"
varinfo[["E003_04"]]$question <- "Has your household been in arrears at any time during the past 3 months, that is, unable to pay as scheduled... Telephone, mobile or internet connection bills?"
varinfo[["E003_05"]]$question <- "Has your household been in arrears at any time during the past 3 months, that is, unable to pay as scheduled... Payments related to informal loans from friends or relatives not living in your household?"
varinfo[["E003_06"]]$question <- "Has your household been in arrears at any time during the past 3 months, that is, unable to pay as scheduled... Payments for healthcare or health insurance?"
varinfo[["E004"]]$question    <- "When you compare the financial situation of your household 3 months ago and now would you say it has become better, worse or remained the same?"
varinfo[["E005"]]$question    <- "Thinking of the financial situation of your household in 3 months’ time do you think it will become better, worse or remain the same?"
varinfo[["E006"]]$question    <- "If your household would not receive any income, how long would your household be able to maintain the same standard of living using savings?"
varinfo[["E007_01"]]$question <- "How likely or unlikely do you think it is that you will need to leave your accommodation within the next 6 months because you can no longer afford it?"
varinfo[["E008_01"]]$question <- "From whom would you get support if you needed help around the house when ill?"
varinfo[["E008_02"]]$question <- "From whom would you get support if you needed advice about a serious personal or family matter?"
varinfo[["E008_03"]]$question <- "From whom would you get support if you needed help when looking for a job?"
varinfo[["E008_04"]]$question <- "From whom would you get support if you were feeling a bit depressed and wanting someone to talk to?"
varinfo[["E008_05"]]$question <- "From whom would you get support if you needed help in looking after your children?"
varinfo[["E008_06"]]$question <- "From whom would you get support if you needed help with shopping?"

#Define subtexts
for (var in c("C003_01","C003_02","C003_03","C003_04")) {
  
  varinfo[[var]]$subtext <- "Optimism and resiliance"
  
}

for (var in c("C005_01","C005_02","C005_03","C005_04","C005_05")) {
  
  varinfo[[var]]$subtext <- "WHO-5"
  
}

for (var in c("C006_01","C006_02","C006_03")) {
  
  varinfo[[var]]$subtext <- "Negative affect"
  
}

for (var in c("C007_01","C007_02","C007_03","C007_04","C007_05")) {
  
  varinfo[[var]]$subtext <- "Trust"
  
}

for (var in c("D002","D003","D006_01","D007_01")) {
  
  varinfo[[var]]$subtext <- "COVID-19"
  
}

for (var in c("D004_01","D004_02","D004_03","D004_04","D004_05","D005_01")) {
  
  varinfo[[var]]$subtext <- "Work-life balance"
  
}


varinfo[["D008_01"]]$subtext <- "Job security"

varinfo[["E001_01"]]$subtext <- "Living standards"

for (var in c("E002_01","E002_02")) {
  
  varinfo[[var]]$subtext <- "Economising"
  
}

for (var in c("E003_01","E003_02","E003_03","E003_04","E003_05","E003_06")) {
  
  varinfo[[var]]$subtext <- "Arrears"
  
}

for (var in c("E004","E005","E006","E007_01")) {
  
  varinfo[[var]]$subtext <- "Depriviation"
  
}

for (var in c("E008_01","E008_02","E008_03","E008_04","E008_05","E008_06")) {
  
  varinfo[[var]]$subtext <- "Support"
  
}



#Specifying special axis ranges
for (var in c("C001_01","C002_01","C007_01","C007_02",
             "C007_03","C007_04","C007_05")) {
  
  varinfo[[var]]$range <- c(1,10)
  
}

#defining the breakdown variables
varinfo[["B001"]]$section <- "breakdown"
varinfo[["B002"]]$section <- "breakdown"
varinfo[["F004"]]$section <- "breakdown"
varinfo[["emp_stat"]]$section <- "breakdown"
varinfo[["age_group"]]$section <- "breakdown"

#This is to make sure we keep the case number and whether someone has left an email address
#when we subset the data later on
varinfo[["CASE"]]$section <- "other_needed"
varinfo[["F021"]]$section <- "other_needed"
varinfo[["EU27"]]$section <- "other_needed"

# For every factor variable a set of default selected categories (in the the plot)
# is defined and added to the list
for (var in c("C003_01","C003_02","C003_03","C003_04")) {
  
  varinfo[[var]]$default_levels <- c("Strongly agree","Agree")
  
}

varinfo[["C004_01"]]$default_levels <- c("Very good","Good")

for (var in c("C005_01","C005_02","C005_03","C005_04","C005_05")) {
  
  varinfo[[var]]$default_levels <- c("All of the time","Most of the time")
  
}

for (var in c("C006_01","C006_02","C006_03")) {
  
  varinfo[[var]]$default_levels <- c("All of the time","Most of the time")
  
}

varinfo[["D002"]]$default_levels <- c("Yes, permanently","Yes, temporarily")
varinfo[["D003"]]$default_levels <- c("Decreased a lot","Decreased a little")

for (var in c("D004_01","D004_02","D004_03","D004_04","D004_05")) {
  
  varinfo[[var]]$default_levels <- c("Always","Most of the time")
  
}

varinfo[["D005_01"]]$default_levels <- c("Every day","Every other day")
varinfo[["D006_01"]]$default_levels <- c("Daily","Several times a week")
varinfo[["D007_01"]]$default_levels <- c("Yes")
varinfo[["D008_01"]]$default_levels <- c("Very likely","Rather likely")
varinfo[["E001_01"]]$default_levels <- c("With great difficulty","With difficulty")

for (var in c("E002_01","E002_02","E003_01","E003_02","E003_03","E003_04","E003_05","E003_06")) {
  
  varinfo[[var]]$default_levels <- "Yes"
  
}

varinfo[["E004"]]$default_levels <- "Worse"
varinfo[["E005"]]$default_levels <- "Worse"
varinfo[["E006"]]$default_levels <- c("No savings","Less than 3 months")
varinfo[["E007_01"]]$default_levels <- c("Very likely","Rather likely")


for (var in c("E008_01","E008_02","E008_03","E008_04","E008_05","E008_06")) {
  
  varinfo[[var]]$default_levels <- "Nobody"
  
}

# Some manual changes
varinfo$C008$section <- "other"
varinfo$D001$section <- "other"

#Set an arbitrary string for numerical variables
for (var in num_vars) {
  
  varinfo[[var]]$levels <- "None"
  varinfo[[var]]$default_levels <- "None"

}

### ----------------------- CLEANING ------------------------------ ###

#Cleaning the data
source("Cleaning_simple.R", local = TRUE)

ds <- ds %>%
  left_join(ds_clean[c("CASE","clean")], by="CASE")

ds$clean[is.na(ds$clean)] <- FALSE

table(ds$clean)

### ----------------------- WEIGHTING ------------------------------ ###

source("weighting_by_country.R",local=TRUE)

weights <- ds %>% 
           filter(clean==TRUE) %>%
           weigh_data(minimum_weight = 0.05,
                      trim_lower = 0.16,
                      trim_upper = 6)

ds <- weights %>%
  select(CASE, w, w_trimmed, w_gross, w_gross_trim) %>%
  right_join(ds, by="CASE")

### ----------------------- SAVING FILES FOR ANALYSIS ------------------------------ ###
#note: these include unclean interviews and unweighted data.

save(ds, file="data/ds_2804_full.Rda")

library(foreign)
write.dta(ds, "data/ds_2804_full.dta")

library(haven)
write_sav(ds, "data/ds_2804_full.sav")

### ------------------ DROPPING VARIABLES --------------------------- ###

to_drop <- names(list.filter(varinfo, section=='other'))

#Removing unclean and unweighted cases
ds <- ds %>%
  select(-one_of(to_drop)) %>%
  filter(clean==TRUE, !is.na(w)) %>%
  droplevels()

### ----------------------- SAVING FILES FOR APPS ------------------------------ ###

save(weights, file="data/weights_2804.rda")
save(varinfo, file="app_benchmark/data/varinfo.rda")
save(ds, file="app_benchmark/data/ds_2804.Rda")

save(ds, file="app_web/data/ds_2804.Rda")
save(varinfo, file="app_web/data/varinfo.rda")

save(varinfo, file="app_response/varinfo.rda")

### ----------------------- PREPARE MAP ------------------------------ ###

#Read in shapefile and select only relevant countries
shp_20 <- readOGR("shapefiles","CNTR_RG_20M_2016_4326") %>% 
  subset(NAME_ENGL %in% levels(ds$B001)) 

shp_20$NAME_ENGL <- droplevels(shp_20$NAME_ENGL)
shp_20$Country <- shp_20$NAME_ENGL

save(shp_20, file = "app_web/data/shp_20.rda")