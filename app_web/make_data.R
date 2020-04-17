# Function for creating the data
make_data <- function(inputvar, breakdown, category,
                      gender_filter,
                      age_filter,
                      education_filter,
                      country_filter,
                      empstat_filter) {
  
  class <- class(ds[[inputvar]])[length(class(ds[[inputvar]]))]
  label_breakdown <- names(breakdown_list)[match(breakdown,breakdown_list)]
 
  
  #Preparing the data
  df <- ds %>%
    #Select needed variables
    select(inputvar, breakdown, B002, age_group, F004, B001, emp_stat, w, EU27) %>%
    #Filter out NA's in inputvar and breakdown
    filter(!is.na(!!sym(inputvar)), !is.na(!!sym(breakdown)), !is.na(w)) %>%
    #Apply any filters that have been activated by the user
    {if (gender_filter!="All") filter(., B002==gender_filter) else .} %>%
    {if (age_filter!="All") filter(., age_group==age_filter) else .} %>%
    {if (education_filter!="All") filter(., F004==education_filter) else .} %>%
    {if (empstat_filter!="All") filter(., emp_stat==empstat_filter) else .} %>%
    #Dropping unused levels
    droplevels() 
  
  # If its a factor we need to select only one of the categories. 
  # The seletion is made by the user and captured in 'category'
  if (class=="factor") {
    
    #Little workaround to avoid a warning
    if(is.null(category)) {category <- "_"}
    
    #Changing the variable to a 0 or 100 variable (for percentages)
    df <- df %>%
      mutate(!!inputvar := as.numeric(!!sym(inputvar) == category) * 100) 
  }    
  
  # Calculating the total
  df_total <- df %>% 
    # If country is the breakdown, the EU27 is always the total
    # If country is not the breakdown, the filter is still by default EU27
    # But the user could select 'all' or a single country. The total would reflect that then.
    {if (breakdown=="B001") filter(., EU27==TRUE) else .} 
  mean_total <- weighted.mean(df_total[[inputvar]], df_total$w)
  
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
    mutate(Mean := weighted.mean(!!sym(inputvar), w),
           # Change the label of the breakdown variable
           !!label_breakdown := !!sym(breakdown)) %>%
    ungroup() %>%
    # Keep only 2 columns: breakdown and the outcome
    select(!!label_breakdown, Mean) %>%
    distinct() %>%
    # Add the total
    add_row(!!label_breakdown := label_total, Mean := mean_total)
  
  df <- list(df, class)
  
  return(df)

}