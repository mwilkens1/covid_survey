# Function for creating the data
make_data <- function(inputvar, breakdown, category,
                      gender_filter,
                      age_filter,
                      education_filter,
                      country_filter,
                      empstat_filter,
                      threshold) {
  
  # catforprint <- ""
  # for (c in category) {
  #   
  #   catforprint <- paste0(catforprint, c, " ")
  # }
  # 
  # print(paste0("data: ",inputvar,", cats: ",catforprint))
  
  #Series of messages in case a user selects too little inputs
  validate(
    need(
      (!is.null(category) | "numeric" %in% varinfo[[inputvar]]$class), 
      "Please select at least one category")
  )
  
  validate(
    need(
      (!is.null(age_filter)), 
      "Please select at least one age group")
  )
  
  validate(
    need(
      (!is.null(education_filter)), 
      "Please select at least one educational level")
  )
  
  validate(
    need(
      (!is.null(empstat_filter)), 
      "Please select at least one employment status")
  )
  
  validate(
    need(
      ((country_filter %in% levels(shp_20$NAME_ENGL)) > 0 ), 
      "Select at least one country")
  )
  
  #Retrieving the class of the variable to determine whether its numeric or factor
  class <- varinfo[[inputvar]]$class[length(varinfo[[inputvar]]$class)]
  #Get the label of the breakdown variable
  label_breakdown <- names(breakdown_list)[match(breakdown,breakdown_list)]
  #Count the number of selected categories
  numcats <- length(category)
  
  #Function for creating the data for each category of the variable
  #In case of a numeric variable this function is only run once.
  calc_data <- function(data, inputvar_inner, cname) {
  
      #Preparing the data
      df <- data %>%
        #Select needed variables
        select(inputvar_inner, breakdown, B002, age_group, F004, B001, emp_stat, w, EU27) %>%
        #Filter out NA's in inputvar and breakdown
        filter(!is.na(!!sym(inputvar_inner)), !is.na(!!sym(breakdown)), !is.na(w)) %>%
        #Applying gender age, education and employment status filters
        {if (gender_filter!="All") filter(., B002 %in% gender_filter) else .} %>%
        {if (length(age_filter)!=length(levels(.$age_group))) 
          filter(., .$age_group %in% age_filter) else .} %>%  
        {if (length(education_filter)!=length(levels(.$F004))) 
          filter(., .$F004 %in% education_filter) else .} %>%  
        {if (length(empstat_filter)!=length(levels(.$emp_stat))) 
          filter(., .$emp_stat %in% empstat_filter) else .} %>%  
        droplevels()
    
      # Calculating the total 
      # This is always EU27, but with any other filter applied (gender, age etc)
      # This means the EU27 total will appear in the bar chart if all filters have been unselected
      df_total <- df %>% 
        filter(EU27==TRUE) 
      mean_total <- weighted.mean(df_total[[inputvar_inner]], df_total$w)    
      
      df <- df %>%
        #Applying the country filter now
        filter(B001 %in% country_filter) %>%
        droplevels()
  
        #Storing the n for this particular selection
        count <- nrow(df)
        
        #Calculating effective sample size
        eff_n <- df %>%
          summarise(eff_n = (sum(w)^2) / sum(w^2)) 
        
        #This tests whether the overall dataset (all categories combined) is large enough
        validate(
          need(
            (eff_n$eff_n>threshold), 
            "Insufficient data, please change filters")
        )
        
      #Even if the total number is large enough, some categories of the breakdown may not be
      #These are then excluded from the data (not from the totals)
  
      # Counting the n per category of the breakdown    
      # As well as effective sample size
      df_count <- df %>%
        group_by(!!sym(breakdown)) %>% 
        summarise(n_eff = (sum(w)^2) / sum(w^2)) 

      df_threshold_1 <- df_count %>%
        filter(n_eff < threshold)
      
      #Getting the categories that are excluded
      cats_below_threshold <- as.vector(unique(df_threshold_1[[1]]))
      all_cats <- as.vector(unique(df[[breakdown]]))
      #and those that should remain
      not_excluded <- setdiff(all_cats,cats_below_threshold)
    
      #Filter out the ones that are excluded
      df <- df %>%
        filter(!!sym(breakdown) %in% not_excluded)
      #Not dropping the empty levels of the breakdown here so in case of a map the 
      #countries will appear as NA.
      
      #Recalculating overall effective sample size again
      eff_n <- df %>%
        summarise(eff_n = (sum(w)^2) / sum(w^2)) 
      
      #General check on the total numbers again 
      validate(
        need(
          (eff_n$eff_n>threshold), 
          "Insufficient data, please change filters")
      )
      
      #Flagging those with low effective sample size
      df_threshold_2 <- df_count %>%
        filter(n_eff >= threshold & n_eff <= threshold_flag)
      
      #Getting the categories that are flagged
      cats_flagged <- as.vector(unique(df_threshold_2[[1]]))
      
      # Actual calculation
      # By the chosen breakdown
      df <- df %>%
        group_by(!!sym(breakdown)) %>% 
        # Calculate the weighted mean
        mutate(!!cname := weighted.mean(!!sym(inputvar_inner), w),
               # Change the label of the breakdown variable
               !!label_breakdown := !!sym(breakdown)) %>%
        ungroup() %>%
        # Keep only 2 columns: breakdown and the outcome
        select(!!label_breakdown, !!cname) %>%
        distinct() %>%
        # Add the total
        add_row(!!label_breakdown := "Total (EU27)", !!cname := mean_total)
    
      #Adding the flags
      newlevels <- NULL
      for (level in levels(df[[label_breakdown]])) {
        
        if (level %in% cats_flagged) {
          
          newlevels <- c(newlevels,paste0(level,"*"))
          
        } else {
          
          newlevels <- c(newlevels,level)
          
        }
        
      }
      
      df[[label_breakdown]] <- factor(df[[label_breakdown]], 
                                      levels = levels(df[[label_breakdown]]), 
                                      labels=newlevels)
      
      out <- list(df, cats_below_threshold, count, cats_flagged)
      
      return(out)
      
  }
  
  # If its a factor we need to add columns per selected category. 
  # The seletion is made by the user and captured in 'category'
  # The function above is run multiple times, each time adding a column to the data
  if (class=="factor") {

    # Apply the calc data function to each selected category
    df <- map(category, function(c) {
      
      #First create a variable with the name of the category
      df <- ds %>%
        mutate(!!c := as.numeric(!!sym(inputvar) == c) * 100) %>%
        #Then pass that dataframe to the function
        calc_data(c,c)
      
      return(df[[1]])
      
      }) 
    
    # If more than one category selected...
    if (numcats>1) {
      
      # ... reduce the list of dataframes to 1 dataframe with different columns
      df <- df %>% reduce(left_join, by=label_breakdown)    
      
      # Calculate 'total' column
      
      df <- df %>% mutate(Total = rowSums(.[2:ncol(df)]))
      
    # If only 1 category
    } else {
      
      #Just pick the only dataframe in hte list
      df <- df[[1]] %>% mutate(Total = rowSums(.[2:ncol(df[[1]])]))
      
    }

    #Get the excluded countries by running the function once and
    #saving the second element in the list
    run <- calc_data({ds %>%
                            mutate(!!category[1] := as.numeric(!!sym(inputvar) == category[1]) * 100)},
                            category[1],category[1])
    excluded <- run[[2]]
    count <- run[[3]]
    flagged <- run[[4]]
    
  #If a numeric variable then just run the function once  
  } else {
    
    var <- inputvar
    
    df <- calc_data(data=ds, inputvar_inner=var, cname="Mean")
    
    excluded <- df[[2]]
    count <- df[[3]]
    flagged <- df[[4]]
    df <- df[[1]]
    
  }    
  
  #Get plot axis range
  range <- varinfo[[inputvar]]$range
  
  #Finally, store in a list:
  # - dataframe
  # - class of the inputvariable
  # - vector of excluded breakdown categories
  # - any special range for plot axis
  # - the n
  # - flagged for unreliability
  df <- list(df, class, excluded, range, count, flagged)

  return(df)

}