library(tidyverse)
library(dplyr)
library(readr)
library(MMWRweek)

path_hub <- "../covid19-forecast-hub-de/"

truth <- read.csv(paste0(path_hub, "/data-truth/RKI/truth_RKI-", "Cumulative Deaths", "_", "Germany", '.csv'),
                  colClasses = c(location="character", date ="Date")) %>% 
  select(-location_name)


load_truth <- function(target='Cumulative Deaths', country="Germany", as_of){
  if(missing(as_of)){
    truth <- read.csv(paste0(path_hub, "/data-truth/RKI/truth_RKI-", target, "_", country, '.csv'),
                      colClasses = c(location="character", date ="Date")) %>% 
      select(-location_name)
    
    if(str_detect(target, 'Incident')){
      truth$epiyear <- MMWRweek(truth$date)$MMWRyear
      truth$epiweek <- MMWRweek(truth$date)$MMWRweek
      
      truth <- truth %>% 
        group_by(location, epiyear, epiweek) %>%
        arrange(date) %>%
        summarise(date=max(date), value = sum(value)) %>%
        ungroup() %>%
        select(c(date, location, value))
    }
  }
  # else{
  #   target <- str_replace(tolower(target), " ", '_')
  #   truth <- read.csv(paste0("data/JHU/", target, '/truth_jhu_', target, '_',
  #                            as_of, ".csv"), 
  #                     colClasses = c(location="character", date ="Date"))
  # }
  
  return(truth)
}

add_truth <- function(df, as_of){
  target <- unique(str_sub(df$target, 2))
  if (str_detect(target, 'cum death')){
      target <- 'Cumulative Deaths'
    } else if (str_detect(target, 'inc death')){
      target <- 'Incident Deaths'
    } else if (str_detect(target, 'cum case')){
      target <- 'Cumulative Cases'
    } else if (str_detect(target, 'inc case')){
      target <- 'Incident Cases'
    }
  
  if (str_detect(unique(df$location)[1], 'GM')){
    country <- "Germany"
  } else {
    country <- "Poland"
  }
   
  truth <- load_truth(target, country, as_of) %>%
    rename(truth = value) 
  
  df <- df %>%
    left_join(truth, by=c("target_end_date"="date", "location"="location"))
  
  return(df)
}

remove_revisions <- function(df, window_size=4){
  revisions <- read.csv("data/revisions.csv", colClasses = c(first_valid_test_date ="Date"))
  
  # infer horizon from target column
  horizon <- as.numeric(substr(unique(df$target), 1, 1))
  
  # to exclude forecasts trained on unrevised data
  time_delta <- window_size*7 + (horizon - 1)*7
  
  df <- df %>%
    rowwise() %>%
    filter(all(!(location == revisions[, 1] & target_end_date < revisions[, 3] + time_delta))) %>%
    data.frame()
  
  return(df)
}

add_location_names <- function(df){
  if(!("location_name" %in% colnames(df))){
    locations <- read.csv('data/locations.csv')
    locations <- locations %>%
      select(-c(abbreviation, population))
    
    df <- left_join(df, locations, by="location")
  }
  return(df)
}


get_all_models <- function(path_hub="../covid19-forecast-hub-de/"){
  all_models <- list.dirs(path = file.path(path_hub, 'data-processed'), 
                          full.names = FALSE, recursive = FALSE)
  return(all_models)
}

# all_models <- get_all_models()

get_filenames <- function(model){
  model_files <- list.files(path=paste0(path_hub, "data-processed/", model), 
                            pattern=".csv$", full.names = TRUE)
  return(model_files)
}

fs <- get_filenames('FIAS_FZJ-Epi1Ger')

str_subset(fs, 'Germany')
str_detect("1 wk ahead inc death", 'case', negate=TRUE)
str_subset(fs, "case", negate=TRUE)

load_forecasts <- function(models, country="Germany", targets=paste(1:4, "wk ahead inc death"),
                           start_date="2019-01-01", end_date="3000-01-01", intersect_dates=FALSE,
                           add_truth=FALSE, remove_revisions=FALSE, add_location_names=FALSE){
  
  if(missing(models)){
    models <- get_all_models()
  }
  
  df <- data.frame()
  
  for (m in models){
    print(m)
    
    # all files in model folder
    model_files <- get_filenames(m)
    
    # filter files
    model_files <- str_subset(model_files, country)
    
    if (any((str_detect(targets, "case")))){
      model_files <- str_subset(model_files, "case")
    } else {
      model_files <- str_subset(model_files, "case", negate=TRUE)
    }
    
    if (length(model_files) == 0) {
      print(paste0("No forecast files found for: ", m, "."))
      next
    }
    
    df_temp <- model_files %>% 
      lapply(read_csv, 
             col_types = cols_only(
               forecast_date = col_date(format = ""),
               target = col_character(),
               target_end_date = col_date(format = ""),
               location = col_character(),
               # location_name = col_character(),
               type = col_character(),
               quantile = col_double(),
               value = col_double())) %>%
      bind_rows
    

    # filter forecasts
    df_temp <- df_temp %>%
      filter(target %in% targets,
             type == 'quantile',
             target_end_date >= start_date,
             target_end_date <= end_date)
    
    if (nrow(df_temp) == 0) {
      print(paste0("No relevant forecasts available for: ", m))
      next
    }    
    
    # remove multiple forecasts for same target (only keep newest one)
    df_temp <- df_temp %>%
      group_by(target_end_date, location, target, quantile) %>%
      slice(which.max(forecast_date)) %>%
      as.data.frame()
    
    # check if all quantiles are available
    df_temp <- df_temp %>%
      group_by(target, target_end_date, location) %>%
      mutate(n_quantiles = n()) %>%
      group_by(target) %>%
      mutate(n_quantiles = min(n_quantiles)) %>%
      filter(n_quantiles == 23 | (str_detect(target, 'inc case') & n_quantiles == 7)) %>%
      select(-n_quantiles)
    
    if (nrow(df_temp) == 0) {
      print(paste0("Not all required quantiles available for: ", m))
      next
    }  
    
    df_temp$model <- m
    df <- bind_rows(df, df_temp)
  }
  
  if(intersect_dates){
    df <- df %>%
      group_by(target, target_end_date) %>%
      filter(length(unique(model)) == length(models)) %>%
      as.data.frame()
  }
  
  if(remove_revisions){
    df <- remove_revisions(df, window_size=4)
  }
  
  if(add_truth){
    df <- add_truth(df)
  }
  
  if(add_location_names){
    df <- add_location_names(df)
  }
  
  
  return(df)
}

# df <- load_forecasts(c("YYG-ParamSearch", "IBF-TimeSeries"), start_date="2020-05-23")
# df <- load_forecasts()


load_ensembles <- function(filename, add_truth=FALSE, add_baseline=FALSE,
                           remove_revisions=FALSE, add_location_names=TRUE, in_sample=FALSE){
  if (!in_sample){
    df <- read_csv(filename, 
                 col_types = cols_only(
                   target = col_character(),
                   target_end_date = col_date(format = ""),
                   location = col_character(),
                   quantile = col_double(),
                   value = col_double(),
                   window_size = col_factor(c("1", "2", "3","4")),
                   model = col_factor(c('EWA', 'MED', 'INV', 'INVA', 'V2', 'V3', 'V4', 'GQRA2', 
                                        'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4', 'QNA3', 'V3_iter'))
                   )
  ) %>% as.data.frame()
  }
  else{
    df <- read_csv(filename, 
                   col_types = cols_only(
                     target = col_character(),
                     target_end_date = col_date(format = ""),
                     location = col_character(),
                     quantile = col_double(),
                     value = col_double(),
                     window_size = col_factor(c("1", "2", "3","4")),
                     model = col_factor(c('EWA', 'MED', 'INV', 'INVA', 'V2', 'V3', 'V4', 'GQRA2', 
                                          'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4', 'QNA3', 'V3_iter')),
                     id_date = col_date(format = "")
                     )
    ) %>% as.data.frame()
  }
  
  
  if(add_baseline){
    df_baseline <- load_forecasts(models = c("COVIDhub-baseline"), targets = unique(df$target),
                                  exclude_locations = c("11", "60", "66", "69", "72", "74", "78"), 
                                  start_date = min(df$target_end_date), 
                                  end_date = max(df$target_end_date),
                                  add_location_names = FALSE)%>% 
      select (-c(forecast_date, type))
    
    df_baseline$model <- 'Baseline'
    df_baseline <- bind_rows(replicate(4, df_baseline, simplify = FALSE))
    df_baseline$window_size <- factor(rep(1:4, each = nrow(df_baseline)/4))
    
    df <- bind_rows(df, df_baseline)
    df$model <- fct_relevel(df$model, c('EWA', 'MED', 'INV', 'INVA', 'V2', 'V3', 'V4', 
                                        'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4', 
                                        'QNA3', 'V3_iter', 'Baseline'))
  }

  if(remove_revisions){
    df <- remove_revisions(df)
  }
  
  if(add_truth){
    df <- add_truth(df)
  }
  
  if(add_location_names){
    df <- add_location_names(df)
  }
  
  return(df)
}

# df <- load_ensembles("data/ensemble_forecasts/df_ensembles_1wk_2020-11-13.csv", add_baseline = TRUE)


load_scores <- function(filename, scores=c('ae', 'wis', 'wis_decomposition'), 
                        add_truth=FALSE, remove_revisions=FALSE, 
                        add_location_names=TRUE, long_format=FALSE, in_sample=FALSE){
  
  if('wis_decomposition' %in% scores){
    scores <- scores[scores != 'wis_decomposition']
    scores <- c(scores, 'wgt_pen_u', 'wgt_iw', 'wgt_pen_l')
  }
  
  if(!in_sample){
    df <- read_csv(filename, 
                   col_types = cols(
                     target = col_character(),
                     target_end_date = col_date(format = ""),
                     location = col_character()
                   )
    ) %>%
      pivot_longer(cols=-any_of(c("target_end_date", "location", "location_name", "target",  "model",  "window_size",  "truth")),
                   names_to="score") %>%
      filter(score %in% scores) %>%
      as.data.frame()
  }
  else{
    df <- read_csv(filename, 
                   col_types = cols(
                     target = col_character(),
                     target_end_date = col_character(),
                     location = col_character(),
                     id_date = col_date(format = "")
                   )
    ) %>%
      pivot_longer(cols=-any_of(c("target_end_date", "location", "location_name", "target",  "model",  "window_size",  "truth", 
                                  "id_date")),
                   names_to="score") %>%
      filter(score %in% scores) %>%
      as.data.frame()
  }

  
  if('window_size' %in% colnames(df)){
    df$window_size <- factor(df$window_size, levels=c("1", "2", "3","4"))
  }
  
  # fix order of ensemble model names
  # if(str_detect(filename, 'ensemble')){
  #   df$model <- factor(df$model, levels=intersect(c('EWA', 'MED', 'INV', 'INVA', 'V2', 'V3', 'V4', 
  #                                                   'GQRA2', 'GQRA3', 'GQRA4', 'QRA2', 'QRA3', 'QRA4', 'QNA3', 'V3_iter',
  #                                                   'Baseline', 'COVIDhub-ensemble'),
  #                                                 unique(df$model)))
  # }
  
  if(remove_revisions){
    df <- remove_revisions(df)
  }
  
  if(add_truth){
    df <- add_truth(df)
  }
  
  if(add_location_names){
    df <- add_location_names(df)
  }
  
  if(!long_format){
    df <- df %>% pivot_wider(names_from='score', values_from='value')
  }
  
  return(df)
}

# df <- load_scores("scores/ensemble_scores_1wk.csv")

