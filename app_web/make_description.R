# Function for the description under the plot
make_description <- function(category, inputvar) {
  
  # Getting the class of the input variable
  class <- class(ds[[inputvar]])[length(class(ds[[inputvar]]))]
  
  if (class=="factor") {
    
    text <- "The figure shows the the percentage that answered:"
    
    #Loop over categories if more than 1 selected
    if (length(category)>1) {
      
      for (c in category) {
        
        #Only for those with more than 2 categories selected
        # If not the final two categories
        if ((length(category)>2) & (c %in% category[1:(length(category)-2)])) {

          text <- paste0(text," '",c,"',")
          
        }    
        
        # If the category before the last, use 'or'
        if (c==category[length(category)-1]) {

          text <- paste0(text," '",c,"' or")
          
          # If it is the final category, dont add comma
        } else if (c==category[length(category)]) {

          text <- paste0(text," '",c,"' ")
          
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

make_excluded_text <- function(data) {
  
  excluded <- data[[3]]
  
  text <- "Excluded due to insufficient data: "
  
  for (c in excluded) {
    
    text <- paste0(text, c, ", ")
    
  }
  
  text <- paste0(substr(text,1,nchar(text)-2),".")
  
  if (length(excluded)==0) text <- NULL
  
  return(text)
  
}

make_filter_description <- function(country_filter, gender_filter, age_filter,empstat_filter,education_filter) {
    
  
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
      
      substr(countries_list,nchar(countries_list)-1,nchar(countries_list)) <- ";"
      countries_list <- paste0("in ",countries_list)
      
    # If none of the default levels are missing from the countries selected
    # but the UK is also among the selection
  } else if ("United Kingdom" %in% country_filter) {
      
      countries_list <- "for the EU27 and the United Kingdom;"
    
  # if UK is not among the selections it must be EU27  
  } else {
      
    countries_list <- "for the EU27;"
      
  }
  
  ###------------------------gender-----------------------------------####
  
  if (gender_filter!="All") {
    
    gender_list <- paste0("with gender: ", tolower(gender_filter),";")
    
  } else {gender_list <- NULL}
  
  ###------------------------age-----------------------------------####
  
  if (length(age_filter)==1) {
    
    age_list <- paste0("with age: ", age_filter,";") %>% tolower()
    
  } else if (any(!(levels(ds$age_group) %in% age_filter))) {
    
    age_list <- paste0("with age: ", 
                             age_filter[1]," and ",
                             age_filter[2],"; ") %>% tolower()
    
  } else {
    
    age_list <- NULL
    
  }

  ###------------------------employment status-----------------------------------####
  
  
  if (length(empstat_filter)==1) {
    
    empstat_list <- paste0(empstat_filter,";")
    empstat_list <- paste0("with employment status: ", empstat_list) %>%
      tolower()
    
  } else if (any(!(levels(ds$emp_stat) %in% empstat_filter))) {
    
      empstat_list <- NULL
      
        for (group in empstat_filter) {
          
          if (group==empstat_filter[length(empstat_filter)-1]) {
            
            empstat_list <- paste0(empstat_list, group, " and ")
            
          } else {
            
            empstat_list <- paste0(empstat_list, group, ", ")
            
          }
          
        }
      
      substr(empstat_list,nchar(empstat_list)-1,nchar(empstat_list)) <- ";"
      empstat_list <- paste0("with employment status: ", empstat_list) %>%
        tolower()
  
  } else {
    
    empstat_list <- NULL
    
  }
  
  ###------------------------education-----------------------------------####
  
  if (length(education_filter)==1) {
    
    education_list <- paste0("with education: ",education_filter,";") %>% tolower()
    
  } else if (any(!(levels(ds$F004) %in% education_filter))) {
    
      education_list <- paste0("with education: ", 
                         education_filter[1]," and ",
                         education_filter[2],"; ") %>% tolower()
      
  } else {

    education_list <- NULL
    
  }
  
  ###------------------------combining-----------------------------------####
  
  full_text <- paste(countries_list, gender_list, age_list, empstat_list, education_list)
  
  #Replace final ';' with a period
  full_text <- gsub('^(.*);.*$', '\\1.', full_text)
  
}


make_low_reliability_description <- function(data) {
  
  list <- data[[6]]
  
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
  
}
