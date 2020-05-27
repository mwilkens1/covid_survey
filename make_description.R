# Function for the description under the plot
make_description <- function(category, inputvar) {
  
  # Getting the class of the input variable
  class <- class(ds[[inputvar]])[length(class(ds[[inputvar]]))]
  
  if (class=="factor") {
    
    text <- "The figure shows "
    
    #Loop over categories if more than 1 selected
    if (length(category)>1) {
      
      for (c in category) {
        
        #Only for those with more than 2 categories selected
        # If not the final two categories
        if ((length(category)>2) & (c %in% category[1:(length(category)-2)])) {

          text <- paste0(text," '",tolower(c),"',")
          
        }    
        
        # If the category before the last, use 'and'
        if (c==category[length(category)-1]) {

          text <- paste0(text," '",tolower(c),"' and")
          
          # If it is the final category, dont add comma
        } else if (c==category[length(category)]) {

          text <- paste0(text," '",tolower(c),"' ")
          
        }
        
      }
      
      # If only one category seleted
    } else {
      
      text <- paste0(text," '",category,"' ")
      
    }
    
    return(text)
    
    # If numeric variable
  } else {
    
    return("The figure shows the mean ")
    
  }
  
}


make_filter_description <- function(country_filter, gender_filter, age_filter,empstat_filter,education_filter) {
  

  ###------------------------gender-----------------------------------####
  
  if (gender_filter!="All") {
    
    gender_list <- paste0(" for ",tolower(gender_filter)," respondents")
    
  } else {gender_list <- " for respondents"}
  
  ###------------------------age-----------------------------------####
  
  if (length(age_filter)==1) {
    
    age_list <- paste0("of age group '", age_filter,"'") %>% tolower()
    
  } else if (any(!(levels(ds$age_group) %in% age_filter))) {
    
    age_list <- paste0("of age group '", 
                       age_filter[1],"' and '",
                       age_filter[2],"'") %>% tolower()
    
  } else {
    
    age_list <- NULL
    
  }
  
  ###------------------------employment status-----------------------------------####
  
  
  if (length(empstat_filter)==1) {
    
    empstat_list <- paste0(empstat_filter,";")
    empstat_list <- paste0("with employment status ", empstat_list) %>%
      tolower()
    
  } else if (any(!(levels(ds$emp_stat) %in% empstat_filter))) {
    
    empstat_list <- "'"
    
    for (group in empstat_filter) {
      
      if (group==empstat_filter[length(empstat_filter)-1]) {
        
        empstat_list <- paste0(empstat_list, group, "' and '")
        
      } else {
        
        empstat_list <- paste0(empstat_list, group, "', '")
        
      }
      
    }
    
    empstat_list <- substr(empstat_list,1,nchar(empstat_list)-3) 
    empstat_list <- paste0("with employment status ", empstat_list) %>%
      tolower()
    
  } else {
    
    empstat_list <- NULL
    
  }
  
  ###------------------------education-----------------------------------####
  
  if (length(education_filter)==1) {
    
    education_list <- paste0("with education ",education_filter) %>% tolower()
    
  } else if (any(!(levels(ds$F004) %in% education_filter))) {
    
    education_list <- paste0("with education '", 
                             education_filter[1],"' and '",
                             education_filter[2],"' ") %>% tolower()
    
  } else {
    
    education_list <- NULL
    
  }
  
  
  ###------------------------Country-----------------------------------####
  
  #If its only one country, just take that...
  if (length(country_filter)==1) {
    
    countries_list <- country_filter
    
    #If any of the default levels are missing from the countries selected...
  } else if (any(!(levels(ds$B001)[1:27] %in% country_filter))) {
    
    # ... make a list of countries
    countries_list <- ""
    
    for (country in country_filter) {
      
      if (country==country_filter[length(country_filter)-1]) {
        
        countries_list <- paste0(countries_list, country, " and ")
        
      } else {
        
        countries_list <- paste0(countries_list, country, ", ")
      }
      
    }
    
    countries_list <- substr(countries_list,1,nchar(countries_list)-2)
    countries_list <- paste0("in ",countries_list)
    
    # If none of the default levels are missing from the countries selected
    # but the UK is also among the selection
  } else if ("United Kingdom" %in% country_filter) {
    
    countries_list <- "in the EU27 and the United Kingdom"
    
    # if UK is not among the selections it must be EU27  
  } else {
    
    countries_list <- "in the EU27"
    
  }
  

  ###------------------------combining-----------------------------------####

  full_text <- paste(gender_list, age_list, empstat_list, education_list, countries_list)
  
}


make_question_description <- function(inputvar) {
  
  question <- paste0(" when asked: ",HTML("<b>",varinfo[[inputvar]]$question,"</b>"))  
  
}




make_excluded_text <- function(data) {

  # Variable of the number of time periods 
  # The exclusions can be differen tin each wave of the survey
  periods <- length(data)  
  
  
  
  if (periods==1) {
  
    #If only one period, the excluded categories are the third element of the first list
    excluded <- data[[1]][[3]]  
    
    text <- paste("Excluded due to insufficient data:",paste(excluded, collapse=", "))
    text <- paste0(text,".")
    if (length(excluded)==0) {text <- NULL}

  } else {
    
    excluded_1 <- data[[1]][[3]]  
    excluded_2 <- data[[2]][[3]]  

    excluded_both  <- intersect(excluded_1, excluded_2)
    excluded_1only <- setdiff(excluded_1, excluded_2)
    excluded_2only <- setdiff(excluded_2, excluded_1)
    
    text <- ""
    
    if (length(excluded_both)>0) {
      text <- paste(text,"Excluded due to insufficient data in both periods:",paste(excluded_both, collapse=", "))
      text <- paste0(text,".")
    }
    
    if (length(excluded_1only)>0) {
      
      text <- paste(text, "Excluded due to insufficient data in April/May:", paste(excluded_1only, collapse=", "))
      text <- paste0(text,".")
      
    }
    
    if (length(excluded_2only)>0) {
      
      text <- paste(text, "Excluded due to insufficient data in June/July:", paste(excluded_2only, collapse=", "))
      text <- paste0(text,".")
      
    }
    
    if (length(excluded_both)==0 & length(excluded_1only)==0 & length(excluded_2only)==0) {
      
      text <- NULL  
      
    } 
  
  }
  
  return(text)
  
}




make_low_reliability_description <- function(data) {
  
  # Variable of the number of time periods 
  # The exclusions can be different in each wave of the survey
  periods <- length(data)
  
  if (periods==1) {
    
    list <- data[[1]][[6]]
    
    if (length(list)!=0) {
      
      if (length(list)==1) {
        
        text <- paste0(list,".")
        
      } else {
        
        text <- ""
        
        for (cat in list) {
          
          if (cat==list[length(list)-1]) {
            
            text <- paste0(text,cat," and ")
            
          } else {
            
            text <- paste0(text,cat,", ")
            
          }
          
        }
        
        substr(text,nchar(text)-1,nchar(text)) <- "."
        
      }
      
      text <- paste("Low reliability (*): ",text)
      
    } else {
      
      text <- NULL
      
    }
    
  } else {
    
    low_rel_1 <- data[[1]][[6]]
    low_rel_2 <- data[[2]][[6]]
    
    lr_both  <- intersect(low_rel_1, low_rel_2)
    lr_1only <- setdiff(low_rel_1, low_rel_2)
    lr_2only <- setdiff(low_rel_2, low_rel_1)
    
    text <- ""
    
    if (length(lr_both)>0) {
      
      text <- paste(text,"Low reliability (*) in both periods:",paste(lr_both,collapse=", "))
      text <- paste0(text,".")
       
    }
    
    if (length(lr_1only)>0) {

      text <- paste(text,"Low reliability (*) in April/May:",paste(lr_1only,collapse=", "))
      text <- paste0(text,".")
      
    }
    
    if (length(lr_2only)>0) {
      
      text <- paste(text,"Low reliability (*) in June/July:",paste(lr_2only,collapse=", "))
      text <- paste0(text,".")
      
    }
    
    if (length(lr_both)==0 & length(lr_1only)==0 & length(lr_2only)==0) {
      
      text <- NULL
      
    }
    
    return(text)
    
  }
  
  
  
  
}
