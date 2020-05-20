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
load("data/ds_raw_0105.Rda")

### ----------------------- LABELING AND RECODING ------------------------------ ###

source("app_web/label_and_recode.R")
ds <- label_and_recode(ds)

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
varinfo[["C001_01"]]$extra_text <-"Life satisfaction is measured on a scale of 1 to 10, 
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

varinfo[["D007_01"]]$extra_text <- "Slovenia is excluded from the data for this question because of a translation issue."

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
varinfo[["D005_01"]]$question <- "Over the last 2 weeks, how often have you worked in your free time to meet work demands?"
varinfo[["D006_01"]]$question <- "How frequently did you work from home before the outbreak of COVID-19?"
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
  
  varinfo[[var]]$subtext <- "Optimism and resilience"
  
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
  
  varinfo[[var]]$subtext <- "Deprivation"
  
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
varinfo[["STARTED"]]$section <- "other_needed"

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

#Getting list of all numeric variables
nums <- unlist(lapply(ds, is.numeric))  
num_vars <- names(nums[nums==TRUE])

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

save(ds, file="data/ds_0105_full.Rda")

library(foreign)
write.dta(ds, "data/ds_0105_full.dta")

library(haven)
write_sav(ds, "data/ds_0105_full.sav")

### ------------------ DROPPING VARIABLES --------------------------- ###

to_drop <- names(list.filter(varinfo, section=='other'))

#Removing unclean and unweighted cases
ds <- ds %>%
  select(-one_of(to_drop)) %>%
  filter(clean==TRUE, !is.na(w)) %>%
  droplevels()

### ----------------------- SAVING FILES FOR APPS ------------------------------ ###

save(weights, file="data/weights_0105.rda")
save(varinfo, file="app_benchmark/data/varinfo.rda")
save(ds, file="app_benchmark/data/ds_0105.Rda")

save(ds, file="app_web/data/ds_0105.Rda")
save(varinfo, file="app_web/data/varinfo.rda")

save(varinfo, file="app_response/varinfo.rda")

### ----------------------- PREPARE MAP ------------------------------ ###

#Read in shapefile and select only relevant countries
shp_20 <- readOGR("shapefiles","CNTR_RG_20M_2016_4326") %>% 
  subset(NAME_ENGL %in% levels(ds$B001)) 

shp_20$NAME_ENGL <- droplevels(shp_20$NAME_ENGL)
shp_20$Country <- shp_20$NAME_ENGL

save(shp_20, file = "app_web/data/shp_20.rda")
