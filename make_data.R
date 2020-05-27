# Function for creating the data
make_data <- function(inputvar, breakdown, category,
                      gender_filter,
                      age_filter,
                      education_filter,
                      country_filter,
                      empstat_filter,
                      threshold,
                      time) {

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
  
  validate(
    need(
     ((as.numeric(time) %in% unique(ds[!is.na(ds[[inputvar]]),]$wave)) > 0 | time=="0"),
     "No data available for this time period. Please select a different time period."
    )
  )
  
  
  #Retrieving the class of the variable to determine whether its numeric or factor
  class <- varinfo[[inputvar]]$class[length(varinfo[[inputvar]]$class)]
  #Get the label of the breakdown variable
  label_breakdown <- names(breakdown_list)[match(breakdown,breakdown_list)]
  #Count the number of selected categories
  numcats <- length(category)
  
  #Creating a variable of the periods that need to be covered
  if (time=="0") {
    
    periods <- unique(ds$wave)
    
  } else {
    
    periods <- as.numeric(time)
    
  }
    
  
  #Function for creating the data for each category of the variable
  #In case of a numeric variable this function is only run once.
  calc_data <- function(data, inputvar_inner, cname) {
  
      #Preparing the data
      df <- data %>%
        #Select needed variables
        select(inputvar_inner, breakdown, B002, age_group, F004, B001, emp_stat, w, EU27, wave) %>%
        #Filter by wave
        {if (time!="0") filter(., wave==as.numeric(time)) else . } %>%
        #Filter out NA's in inputvar
        filter(!is.na(!!sym(inputvar_inner)), !is.na(w)) %>%
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
          #Applying the country filter now as well as a filter for missing values in the breakdown
          filter(B001 %in% country_filter,
                 !is.na(!!sym(breakdown))) %>%
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
    
      
  }
  
  # If its a factor we need to add columns per selected category. 
  # The seletion is made by the user and captured in 'category'
  # The function above is run multiple times, each time adding a column to the data
  if (class=="factor") {
    
    list_per_time <- map(periods, function(t) {
      
      ds <- filter(ds, wave==t)
      
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
      range <- varinfo[[inputvar]]$range
      period <- ifelse(t==1,"April/May","June/July")
      
      list(df, class, excluded, range, count, flagged, period)
      
    })
    
  #If a numeric variable then just run the function once  
  } else {
    
    var <- inputvar
    
    # Apply the calc_data function to each time period selected
    # This could be just 1 time period
    # The output is a list of lists
  
    list_per_time <- map(periods, function(t) {
      
      #filter year
      df <- filter(ds, wave==t) %>%
              calc_data(inputvar_inner=var, cname="Mean")
     
      excluded <- df[[2]]
      count <- df[[3]]
      flagged <- df[[4]]
      df <- df[[1]]
      
      #Get plot axis range
      range <- varinfo[[var]]$range
      
      #Finally, store in a list (for each time period):
      # - dataframe
      # - class of the inputvariable
      # - vector of excluded breakdown categories
      # - any special range for plot axis
      # - the n
      # - flagged for unreliability
      # - wave of the survey
      
      period <- ifelse(t==1,"April/May","June/July")
      
      list(df, class, excluded, range, count, flagged, period)
      
    })
    
  }    
  
  #List per time is a list of lists that contain the items shown above
  
  return(list_per_time)

}