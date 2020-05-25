# Function to label and recode the data pulled from the sosci API (ds)
# Assumes a certain set of questions so needs to be adjusted to the dataset of choice.

label_and_recode <- function(ds) {

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

  
  return(ds)
    
}