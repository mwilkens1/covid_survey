# Function for creating the data
make_data <- function(inputvar, breakdown, category,
                      gender_filter,
                      age_filter,
                      education_filter,
                      country_filter,
                      empstat_filter) {

  class <- class(ds[[inputvar]])[length(class(ds[[inputvar]]))]
  label_breakdown <- names(breakdown_list)[match(breakdown,breakdown_list)]
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
      #Apply any filters that have been activated by the user
      {if (gender_filter!="All") filter(., B002==gender_filter) else .} %>%
      {if (age_filter!="All") filter(., age_group==age_filter) else .} %>%
      {if (education_filter!="All") filter(., F004==education_filter) else .} %>%
      {if (empstat_filter!="All") filter(., emp_stat==empstat_filter) else .} %>%
      #Dropping unused levels
      droplevels() 
    
    df_total <- df %>% 
      # If country is the breakdown, the EU27 is always the total
      # If country is not the breakdown, the filter is still by default EU27
      # But the user could select 'all' or a single country. The total would reflect that then.
      {if (breakdown=="B001") filter(., EU27==TRUE) else .} 
    mean_total <- weighted.mean(df_total[[inputvar_inner]], df_total$w)
    
    # Creating a variable label
    if (breakdown=="B001") label_total <- "EU27" else label_total <- "Total"
    
    # Here the country filter is activated because we have the totals we need
    df <- df %>%
      {if (country_filter!="All") 
      {if (country_filter=="EU27") filter(., EU27==TRUE) else 
        filter(., B001==country_filter)} else .} %>%
      droplevels()
    
    # Actual calculation
    df <- df %>%
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
      add_row(!!label_breakdown := label_total, !!cname := mean_total)
  
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