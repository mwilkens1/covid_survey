##Here a list is created with all sorts of info per variable
varinfo <- lapply(colnames(ds_merged_full), function(x) { 
  
  list( # section
    section = {if (startsWith(x, "C0")) {"Quality of life"} 
      else if (startsWith(x, "D0")) {"Work and teleworking"}
      else if (startsWith(x, "E0")) {"Financial situation"}
      else {"other"}},
    
    # label will appear in the selection box
    label = NULL,
    
    # smaller subtext after the label
    subtext = "",
    
    # Full question wording
    question = NULL,
    
    # Extra explanatory text
    extra_text = NULL,
    
    # class of the variable
    class = class(ds_merged_full[[x]]),
    
    # any levels in case its a categorical variable
    levels = levels(ds_merged_full[[x]]),
    
    # default selected levels is initated
    default_levels = NULL,
    
    # Special axis range for numerical variables
    range = NULL
    
  )
  
})

names(varinfo) <- colnames(ds_merged_full)



###FAKE VARIABLES ###

varinfo[["random_factor"]]$section <- "Work and teleworking"
varinfo[["random_factor"]]$subtext <- "Fake"
varinfo[["random_factor"]]$question <- "What is the fake random factor?"
varinfo[["random_factor"]]$default_levels <- "TRUE"
varinfo[["random_factor"]]$label <- "Random factor variable"

varinfo[["random_numeric"]]$section <- "Work and teleworking"
varinfo[["random_numeric"]]$subtext <- "Fake"
varinfo[["random_numeric"]]$question <- "What is the fake random number?"
varinfo[["random_numeric"]]$label <- "Random numeric variable"


# Define labels

varinfo[["CASE"]]$label = "Case number"
varinfo[["SERIAL"]]$label = "Serial number (if provided)"
varinfo[["REF"]]$label = "Reference (if provided in link)"
varinfo[["QUESTNNR"]]$label = "Questionnaire that has been used in the interview"
varinfo[["MODE"]]$label = "Interview mode"
varinfo[["LANGUAGE"]]$label = "Language"
varinfo[["STARTED"]]$label = "Time the interview has started (Europe/Berlin)"
varinfo[["B001"]]$label = "Country"
varinfo[["B002"]]$label = "Gender"
varinfo[["B003_01"]]$label = "Age"
varinfo[["C001_01"]]$label = "Life satisfaction"
varinfo[["C002_01"]]$label = "Happiness"
varinfo[["C003_01"]]$label = "I am optimistic about my future"
varinfo[["C003_02"]]$label = "I am optimistic about my children's or grandchildren's future"
varinfo[["C003_03"]]$label = "I find it difficult to deal with important problems that come up in my life"
varinfo[["C003_04"]]$label = "When things go wrong in my life, it generally takes me a long time to get back to normal"
varinfo[["C004_01"]]$label = "Perceived health status"
varinfo[["C005_01"]]$label = "I have felt cheerful and in good spirits"
varinfo[["C005_02"]]$label = "I have felt calm and relaxed"
varinfo[["C005_03"]]$label = "I have felt active and vigorous"
varinfo[["C005_04"]]$label = "I woke up feeling fresh and rested"
varinfo[["C005_05"]]$label = "My daily life has been filled with things that interest me"
varinfo[["C006_01"]]$label = "I have felt particularly tense"
varinfo[["C006_02"]]$label = "I have felt lonely"
varinfo[["C006_03"]]$label = "I have felt downhearted and depressed"
varinfo[["C007_01"]]$label = "The news media"
varinfo[["C007_02"]]$label = "The police"
varinfo[["C007_03"]]$label = "Your country’s government"
varinfo[["C007_04"]]$label = "The European Union"
varinfo[["C007_05"]]$label = "The healthcare system"
varinfo[["C008"]]$label = "Urbanisation"
varinfo[["D001"]]$label = "Employment status"
varinfo[["D002"]]$label = "Lost your job(s) or contract(s)"
varinfo[["D003"]]$label = "Any change in working hours"
varinfo[["D004_01"]]$label = "Kept worrying about work when you were not working"
varinfo[["D004_02"]]$label = "Felt too tired after work to do some of the household jobs which need to be done"
varinfo[["D004_03"]]$label = "Found that your job prevented you from giving the time you wanted to your family"
varinfo[["D004_04"]]$label = "Found it difficult to concentrate on your job because of your family responsibilities"
varinfo[["D004_05"]]$label = "Found that your family responsibilities prevented you from giving the time you should to your job"
varinfo[["D005_01"]]$label = "Over the last 2 weeks, how often have you worked in your free time to meet work demands?"
varinfo[["D006_01"]]$label = "Frequency of working from home before the outbreak"
varinfo[["D007_01"]]$label = "Started to work from home as a result of the situation"
varinfo[["D008_01"]]$label = "Do you think you might lose your job in the next 3 months?"
varinfo[["E001_01"]]$label = "Is your household able to make ends meet?"
varinfo[["E002_01"]]$label = "Gone without fresh fruit and vegetables"
varinfo[["E002_02"]]$label = "Bought cheaper cuts of meat or bought less than wanted"
varinfo[["E003_01"]]$label = "Rent or mortgage payments for accommodation"
varinfo[["E003_02"]]$label = "Utility bills, such as electricity, water, gas"
varinfo[["E003_03"]]$label = "Payments related to consumer loans, including credit card overdrafts"
varinfo[["E003_04"]]$label = "Telephone, mobile or internet connection bills"
varinfo[["E003_05"]]$label = "Payments related to informal loans from friends or relatives not living in your household"
varinfo[["E003_06"]]$label = "Payments for healthcare or health insurance"
varinfo[["E004"]]$label = "Financial situation of household now compared to 3 months ago"
varinfo[["E005"]]$label = "Expected financial situation of household in 3 months"
varinfo[["E006"]]$label = "Without income, length of time household could maintain the same standard of living using savings"
varinfo[["E007_01"]]$label = "Think you will need to leave your accommodation within the next 6 months because you can no longer afford it"
varinfo[["E008_01"]]$label = "If you needed help around the house when ill"
varinfo[["E008_02"]]$label = "If you needed advice about a serious personal or family matter"
varinfo[["E008_03"]]$label = "If you needed help when looking for a job"
varinfo[["E008_04"]]$label = "If you were feeling a bit depressed and wanting someone to talk to"
varinfo[["E008_05"]]$label = "If you needed help in looking after your children"
varinfo[["E008_06"]]$label = "If you needed help with shopping"
varinfo[["F001_01"]]$label = "Household size"
varinfo[["F001_01a"]]$label = "Household size: Don\'t know/Prefer not to answer"
varinfo[["F002_01"]]$label = "Partner"
varinfo[["F003_01"]]$label = "Number of children: Age 0-11"
varinfo[["F003_01a"]]$label = "Number of children: Age 0-11: Don\'t know/Prefer not to answer"
varinfo[["F003_02"]]$label = "Number of children: Age 12-17"
varinfo[["F003_02a"]]$label = "Number of children: Age 12-17: Don\'t know/Prefer not to answer"
varinfo[["F004"]]$label = "Education"
varinfo[["F005"]]$label = "NUTS 1 AT"
varinfo[["F006"]]$label = "NUTS 1 BE"
varinfo[["F007"]]$label = "NUTS 1 BG"
varinfo[["F008"]]$label = "NUTS 1 DE"
varinfo[["F009"]]$label = "NUTS 1 EL"
varinfo[["F010"]]$label = "NUTS 1 ES"
varinfo[["F011"]]$label = "NUTS 1 FI"
varinfo[["F012"]]$label = "NUTS 1 FR"
varinfo[["F013"]]$label = "NUTS 1 HU"
varinfo[["F014"]]$label = "NUTS 1 IT"
varinfo[["F015"]]$label = "NUTS 1 NL"
varinfo[["F016"]]$label = "NUTS 1 PL"
varinfo[["F017"]]$label = "NUTS 1 PT"
varinfo[["F018"]]$label = "NUTS 1 RO"
varinfo[["F019"]]$label = "NUTS 1 SE"
varinfo[["F020"]]$label = "NUTS 1 UK"
varinfo[["F021"]]$label = "Person ID (SERIAL)"
varinfo[["F022_01"]]$label = "Reporting: [No Description] 01"
varinfo[["TIME001"]]$label = "Time spent on page 1"
varinfo[["TIME002"]]$label = "Time spent on page 2"
varinfo[["TIME003"]]$label = "Time spent on page 3"
varinfo[["TIME004"]]$label = "Time spent on page 4"
varinfo[["TIME005"]]$label = "Time spent on page 5"
varinfo[["TIME006"]]$label = "Time spent on page 6"
varinfo[["TIME007"]]$label = "Time spent on page 7"
varinfo[["TIME008"]]$label = "Time spent on page 8"
varinfo[["TIME009"]]$label = "Time spent on page 9"
varinfo[["TIME010"]]$label = "Time spent on page 10"
varinfo[["TIME011"]]$label = "Time spent on page 11"
varinfo[["TIME012"]]$label = "Time spent on page 12"
varinfo[["TIME013"]]$label = "Time spent on page 13"
varinfo[["TIME014"]]$label = "Time spent on page 14"
varinfo[["TIME015"]]$label = "Time spent on page 15"
varinfo[["TIME016"]]$label = "Time spent on page 16"
varinfo[["TIME017"]]$label = "Time spent on page 17"
varinfo[["TIME018"]]$label = "Time spent on page 18"
varinfo[["TIME019"]]$label = "Time spent on page 19"
varinfo[["TIME020"]]$label = "Time spent on page 20"
varinfo[["TIME021"]]$label = "Time spent on page 21"
varinfo[["TIME022"]]$label = "Time spent on page 22"
varinfo[["TIME023"]]$label = "Time spent on page 23"
varinfo[["TIME024"]]$label = "Time spent on page 24"
varinfo[["TIME025"]]$label = "Time spent on page 25"
varinfo[["TIME026"]]$label = "Time spent on page 26"
varinfo[["TIME027"]]$label = "Time spent on page 27"
varinfo[["TIME028"]]$label = "Time spent on page 28"
varinfo[["TIME_SUM"]]$label = "Time spent overall (except outliers)"
varinfo[["MAILSENT"]]$label = "Time when the invitation mailing was sent (personally identifiable recipients, only)"
varinfo[["LASTDATA"]]$label = "Time when the data was most recently updated"
varinfo[["FINISHED"]]$label = "Has the interview been finished (reached last page)?"
varinfo[["Q_VIEWER"]]$label = "Did the respondent only view the questionnaire, omitting mandatory questions?"
varinfo[["LASTPAGE"]]$label = "Last page that the participant has handled in the questionnaire"
varinfo[["MAXPAGE"]]$label = "Hindmost page handled by the participant"

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
varinfo[["w"]]$section <- "other_needed"
varinfo[["w_trimmed"]]$section <- "other_needed"
varinfo[["w_gross"]]$section <- "other_needed"
varinfo[["w_gross_trim"]]$section <- "other_needed"
varinfo[["wave"]]$section <- "other_needed"

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
nums <- unlist(lapply(ds_merged_full, is.numeric))  
num_vars <- names(nums[nums==TRUE])

#Set an arbitrary string for numerical variables
for (var in num_vars) {
  
  varinfo[[var]]$levels <- "None"
  varinfo[[var]]$default_levels <- "None"
  
}