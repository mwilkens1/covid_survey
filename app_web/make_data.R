# Function for creating the data
make_data <- function(inputvar, breakdown, category,
                      gender_filter,
                      age_filter,
                      education_filter,
                      country_filter,
                      empstat_filter) {

  #Retrieving the class of the variable to determine whether its numeric or factor
  class <- class(ds[[inputvar]])[length(class(ds[[inputvar]]))]
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
      filter(!is.na(!!sym(inputvar_inner)), !is.na(!!sym(breakdown)), !is.na(w))
    
    #Calculating the total
    df_total <- df %>% 
      # The totals are always EU27
      filter(EU27==TRUE) 
    mean_total <- weighted.mean(df_total[[inputvar_inner]], df_total$w)    
    
    #Apply any filters that have been activated by the user
    df <- df %>%
      filter(B002 %in% gender_filter) %>%
      filter(age_group %in% age_filter) %>%
      filter(F004 %in% education_filter) %>%
      filter(emp_stat %in% empstat_filter) %>%
      filter(B001 %in% country_filter) %>%
      droplevels() %>%
      # Actual calculation
      # By the chosen breakdown
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
  
    return(df)
    
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

      }) 
    
    # If more than one category selected...
    if (numcats>1) {
      
      # ... reduce the list of dataframes to 1 dataframe with different columns
      df <- df %>% reduce(left_join, by=label_breakdown)    
      
    # If only 1 category
    } else {
      
      #Just pick the only dataframe in hte list
      df <- df[[1]]
      
    }
    
  #If a numeric variable then just run the function once  
  } else {
    
    var <- inputvar
    
    df <- calc_data(data=ds, inputvar_inner=var, cname="Mean")
    
  }    
  
  #Finally, store in a list with first the dataframe and then the class of the inputvariable
  df <- list(df, class)
  
  return(df)

}