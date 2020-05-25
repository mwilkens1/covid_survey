library(survey)
library(weights)
library(anesrake)

weigh_data <- function(ds, minimum_weight, trim_lower, trim_upper) {

    ##Randomise third gender category and don't knows
    
    #Replace with NA
    ds$gender_w <-ds$B002
    na_gender <- ds %>%
      select(gender_w) %>%
      mutate(gender_w = na_if(gender_w, "In another way"))
    ds$gender_w <-na_gender
    
    #Recode to numeric
    male <- (ds$B002=="Male")
    female <- (ds$B002=="Female")
    ds$gender_w <- case_when (male ~1, female ~2)
    
    #Randomise
    ds$gender_w[is.na(ds$gender_w)] <- sample(ds$gender_w[!is.na(ds$gender_w)], sum(is.na(ds$gender_w)), replace=F)
    #Back to labels
    ds$gender_w <- factor(ds$gender_w,
                          levels = c(1,2),
                          labels = c("Male", "Female"))
    
    ds$country <- ds$B001
    ds <- ds[!is.na(ds$country),]
    ds <- ds[ds$country!="Other country",] %>% droplevels()
    
    ##Recode age gender categories
    
    ds$agecat <- cut(ds$B003_01,
                     breaks=c(-Inf, 24, 34, 44, 54, 64, Inf),
                     labels=c("18-24","25-34","35-44", "45-54", "55-64", "65+"))
    
    ds$age_gender <-"NA"
    
    male <- (ds$gender_w=="Male")
    female <- (ds$gender_w=="Female")
    age_18_24 <- (ds$agecat=="18-24")
    age_25_34 <- (ds$agecat=="25-34")
    age_35_44 <- (ds$agecat=="35-44")
    age_45_54 <- (ds$agecat=="45-54")
    age_55_64 <- (ds$agecat=="55-64")
    age_65_over <- (ds$agecat=="65+")
    
    ds$age_gender <- case_when(
      male & age_18_24 ~ 1,
      male & age_25_34 ~ 2,
      male & age_35_44 ~ 3,
      male & age_45_54 ~ 4,
      male & age_55_64 ~ 5,
      male & age_65_over ~ 6,
      female & age_18_24 ~ 7,
      female & age_25_34 ~ 8,
      female & age_35_44 ~ 9,
      female & age_45_54 ~ 10,
      female & age_55_64 ~ 11,
      female & age_65_over ~ 12)
    
    ##Recode urbanisation to 1=rural, 2=urban
    
    ds$urb <-"NA"
    ds$urb <- case_when(
      ds$C008=="The open countryside" ~ 1,
      ds$C008=="A village/small town" ~ 1,
      ds$C008=="A medium to large town" ~ 2,
      ds$C008=="A city or city suburb" ~ 2)
    
    ##Randomise NAs for urbanisation
    
    ds$urb[is.na(ds$urb)] <- sample(ds$urb[!is.na(ds$urb)], sum(is.na(ds$urb)), replace=F)
    
    ##recode education to 1=non-tertiary, 2=tertiary
    
    ds$education <- "NA"
    ds$education <- case_when(
      ds$F004=="Primary" ~ 1,
      ds$F004=="Secondary" ~ 1,
      ds$F004=="Tertiary" ~ 2)
    
    ##randomise NAs for education
    
    ds$education[is.na(ds$education)] <- sample(ds$education[!is.na(ds$education)], sum(is.na(ds$education)), replace=F)
    
    ##necessary variables only
    vars <- c("CASE", "country","age_gender","urb","education")
    ds_w <- ds[vars]
    
    
    #Creating a list of country files
    country_names <- as.vector(unique(ds_w$country))
    
    country_data <- lapply(country_names, function(country) {
      
      ds_w[ds_w$country == country,]
      
    })
    names(country_data) <- country_names
    
    
    ##reading target data

    targets <-read.csv("weighting_data/target_all_clean_alt.csv")
  
    country_targets <- lapply(country_names, function(country) {
      
      targets[targets$country == country,]
      
    })
    names(country_targets) <- country_names
    
    set.seed(1)
    
    #### Applying raking function to each country ###
    weights <- lapply(country_names, function(country) {
      
      w_target <- with(country_targets[[country]], list(
        age_gender = wpct(age_gender, population),
        urb = wpct(urbanisation, population),
        education = wpct(education, population)
      ))
      
      print(paste("<<--------------",country,"-------------->>"))
      
      cap <- 4
      convergence <- FALSE
      while (convergence==FALSE) {
        
        raking <-     anesrake(w_target,
                               country_data[[country]],
                               country_data[[country]]$CASE,
                               cap=cap,
                               choosemethod = "total",
                               type = "pctlim",
                               pctlim = 0.05,
                               maxit = 5000,
                               convcrit = 0.001
         )
      
        if (raking$converge=="Complete convergence was achieved" &
            min(raking$weightvec)>minimum_weight) {
          
          convergence <- TRUE
          print(paste("<<-------------- Convergence at cap: ", cap," -------------->>"))
          
        } else if (raking$converge=="Complete convergence was achieved") {
          
          print("<<-------------- Convergence, but too small weights -------------->>")
          cap <- cap + 1
          print(paste("<<-------------- Cap increased to: ", cap," -------------->>"))
          
        } else {
          
          cap <- cap + 1
          print(paste("<<-------------- Cap increased to: ", cap," -------------->>"))
          
        }
      
      }
     
      country_data[[country]]$w <- raking$weightvec
      
      country_data[[country]]$cap <- cap
      
      country_data[[country]]$trimmed <- "OK"
      country_data[[country]]$trimmed[country_data[[country]]$w<0.16] <- "Too low"
      country_data[[country]]$trimmed[country_data[[country]]$w>6] <- "Too high"
      
      #Adding the targets
      country_data[[country]] <- country_data[[country]] %>%
        rename(urbanisation = urb) %>%
        left_join(targets,by=c("country","age_gender","urbanisation","education"))
      
      #Trimming the weights
      ds_svy <- svydesign(id=~CASE, 
                          strata=~strata, 
                          weights=~w, 
                          data=country_data[[country]], 
                          fpc=~population)
      
      #trim weights
      ds_trim <-trimWeights(ds_svy, lower=trim_lower, upper=trim_upper, strict=TRUE)
      
      #store trimmed weights
      country_data[[country]]$w_trimmed <- weights(ds_trim)
      
      #rescaling weights to population 18+ total=424,755,108
      country_data[[country]] <- country_data[[country]] %>%
        mutate(w_country = sum(w)) %>%
        mutate(w_gross = pop_country / w_country * w) %>%
        mutate(w_country_trim = sum(w_trimmed)) %>%
        mutate(w_gross_trim = pop_country / w_country_trim * w_trimmed)
        
      #returns the dataset with extra weight variable
      return(country_data[[country]])
      
    }) 
    names(weights) <- country_names
    
    
    #print the final cap used
    lapply(country_names, function(country) {
      
      print(paste(country,": ",max(weights[[country]]$cap)))
      print(table(weights[[country]]$trimmed))
      
    })
    
    
    #Merging into one dataframe
    weights <- do.call("rbind",weights)
    
    #Population: 
    sum(weights$w_gross)
    sum(weights$w_gross_trim)
    
    #Rescaling the weights to a mean of 1
    weights$w_gross <- weights$w_gross / mean(weights$w_gross)
    weights$w_gross_trim <- weights$w_gross_trim / mean(weights$w_gross_trim)
    
    #Showing the distributions of the weights by country
    for (country in country_names) {
      
      print(country)
      
      weights %>%
        filter(country == !!country) %>%
        select(w, w_trimmed, w_gross, w_gross_trim) %>%
        summary() %>%
        print()
      
    }
    
    return(weights)

}