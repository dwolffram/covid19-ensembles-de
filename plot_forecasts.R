setwd("covid19-ensembles-de")

source("data_loading.R")
library(gridExtra)

Sys.setlocale("LC_TIME", "C")
Sys.setlocale("LC_ALL", "C")

# 2021-01-11and2020-03-29

targets=c(paste(1:4, "wk ahead inc death"), paste(1:4, "wk ahead inc case"))

any(str_detect(targets, 'case'))
df1 <- load_forecasts(start_date = "2021-01-16", end_date = "2021-04-24",
                     targets=paste(1:2, "wk ahead inc death"))
df2 <- load_forecasts(start_date = "2021-01-16", end_date = "2021-04-24",
                     targets=paste(1:2, "wk ahead inc case"))


df <- df %>%
  group_by(model) %>%
  mutate(n = n_distinct(target_end_date)) %>%
  filter(n == 15) %>%
  select(-n)

df <- add_truth(df)

df1 <- df1 %>%
  group_by(model) %>%
  mutate(n = n_distinct(target_end_date)) %>%
  filter(n == 15) %>%
  select(-n)

df1 <- add_truth(df1)

df2 <- df2 %>%
  group_by(model) %>%
  mutate(n = n_distinct(target_end_date)) %>%
  filter(n == 15) %>%
  select(-n)

df2 <- add_truth(df2)

df_all <- bind_rows(df1, df2)

df_all <- df
df1 <- subset(df, target == "1 wk ahead inc death")

plot_forecast <- function(df, models, locations, window_sizes,
                          facet, facet_row=location_name, facet_col=model,
                          incidence=FALSE, center=FALSE, title=NULL, xlab="Date", ylab="Deaths",
                          start_date='1900-01-01', end_date='3000-01-01',
                          ncol=4, dir='v', scales='fixed', strip.position="top", 
                          base_size=18){
  cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1] # length(levels_coverage) + 1
  facet <- ensym(facet)
  
  df <- pivot_wider(df, names_from=quantile, names_prefix="value.", values_from=value)
  
  if(missing(models)){
    models <- unique(df$model)
  }  
  
  
  # try(df$model <- factor(df$model, labels=c("EWA", "MED", "INV", "V[2]", "V[3]", "V[4]", "GQRA[2]", "GQRA[3]", "GQRA[4]",
  #                                           "QRA[2]", "QRA[3]", "QRA[4]", "Baseline")),
  #     silent=TRUE)
  
  if(missing(locations)){
    locations <- unique(df$location)
  }
  
  if(missing(window_sizes) & 'window_size' %in% colnames(df)){
    window_sizes <- unique(df$window_size)
  }
  
  # filter forecasts
  df <- df %>%
    filter(model %in% models,
           location %in%  locations,
           target_end_date >= start_date,
           target_end_date <= end_date
    )
  
  if('window_size'  %in% colnames(df)){
    df <- df %>%
      filter(window_size %in% window_sizes)
  }
  
  if (incidence){
    if (missing(facet)){
      grouping_vars <- quos(model, location)
    }
    else{
      grouping_vars <- facet
    }
    
    horizon = as.numeric(substr(unique(df$target), 1, 1))
    
    df <- df %>% 
      group_by(!!!grouping_vars) %>%
      mutate(value.0.5 = value.0.5 - lag(truth, horizon)) %>%
      mutate(value.0.05 = value.0.05 - lag(truth, horizon)) %>%
      mutate(value.0.95 = value.0.95 - lag(truth, horizon)) %>%
      mutate(value.0.25 = value.0.25 - lag(truth, horizon)) %>%
      mutate(value.0.75 = value.0.75 - lag(truth, horizon)) %>%
      mutate(truth=c(NA, diff(truth))) %>%
      drop_na()
  }
  
  if (center){
    if (missing(facet)){
      grouping_vars <- quos(model, location)
    }
    else{
      grouping_vars <- facet
    }
    
    df <- df %>% 
      group_by(!!!grouping_vars) %>%
      mutate(value.0.5 = value.0.5 - truth) %>%
      mutate(value.0.05 = value.0.05 - truth) %>%
      mutate(value.0.95 = value.0.95 - truth) %>%
      mutate(value.0.25 = value.0.25 - truth) %>%
      mutate(value.0.75 = value.0.75 - truth) %>%
      mutate(truth=0) %>%
      drop_na()
  }
  
  # default title
  if(missing(title)){
    horizon = substr(unique(df$target), 1, 1)
    title = paste(horizon, "wk ahead forecasts with 50% and 90% prediction intervals")
  }

  
  ggplot(df, aes(x=target_end_date, y=truth)) +
    {if(!missing(facet)) facet_wrap(facet, ncol=ncol, dir=dir, scales=scales, strip.position=strip.position)} +
    {if(missing(facet)) facet_grid(rows=enquos(facet_row), cols=enquos(facet_col), scales=scales)} +
    #{if(missing(facet)) facet_wrap(model ~ location_name, ncol=ncol, scales=scales, labeller=label_parsed)} +
    #{if(missing(facet)) facet_grid(location_name~model, scales=scales, labeller=label_parsed)} +
    geom_smooth(aes(y = value.0.5, ymin = value.0.05, ymax = value.0.95), 
                linetype=3, size=0.2, colour="white", fill=cols[2], alpha=1, stat = "identity") +
    geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
                linetype=3, size=0.2, colour="white", fill=cols[1], alpha=1, stat = "identity") +
    geom_line() +
    geom_point(pch = 4, size=0.7) +
    geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white", size=0.7) +
    theme_bw() +
    #scale_x_date(date_breaks="1 months", date_labels = "%B") +
    labs(title=title,
         x = xlab,
         y = ylab) +
    theme_grey(base_size=base_size) #+
    # theme(axis.text.x=element_text(angle=90,hjust=1, vjust=0.5))
}

ms <- unique(df1$model)
dput(ms)
ms <- c("epiforecasts-EpiExpert", "epiforecasts-EpiNow2", 
        "FIAS_FZJ-Epi1Ger", "itwm-dSEIR", "ITWW-county_repro", "Karlen-pypm", 
        "KIT-baseline", "KITCOVIDhub-median_ensemble", "LANL-GrowthRate", "LeipzigIMISE-SECIR", 
        "MIT_CovidAnalytics-DELPHI", "USC-SIkJalpha")

plot_forecast(df, models=ms, locations='GM', facet_row=model, facet_col=target, incidence=FALSE, ncol=1, center=FALSE, 
              strip.position="left", base_size = 8, xlab=NULL, title=NULL, scales='free_y')

ggsave('plots/inc_death_gm_freey.png', width=15, height=24, dpi=600, unit='cm', device='png')


plot_forecast(df1, models=ms, locations='GM', facet=model, incidence=FALSE, ncol=1, center=FALSE, 
              strip.position="left", base_size = 8, xlab=NULL)



ggsave('plots/inc_death_gm.png', width=15.7/2, height=24, dpi=600, unit='cm', device='png')


ggsave('plots/inc_death_gm_all.png', width=5, height=24, dpi=600, unit='cm', device='png')






cols <- colorRampPalette(c("deepskyblue4", "lightgrey"))(2 + 1)[-1] # length(levels_coverage) + 1

df <- df_all
df <- pivot_wider(df, names_from=quantile, names_prefix="value.", values_from=value)

# filter forecasts
df <- df %>%
  filter(model %in% ms, location=='GM')

df$model = str_replace(df$model, "KITCOVIDhub-median_ensemble", "KIT-median_ens.")
df$model = str_replace(df$model, "MIT_CovidAnalytics-DELPHI", "MIT_CA-DELPHI")
df$model = str_replace(df$model, "-", "- ")
#df$model = str_replace(df$model, "_ ", "_")



ggplot(df, aes(x=target_end_date, y=truth)) +
  facet_grid(rows=vars(model), cols=vars(target), scales="free", 
             labeller = label_wrap_gen(10), # switch="y"
             ) +
  geom_smooth(aes(y = value.0.5, ymin = value.0.05, ymax = value.0.95), 
              linetype=3, size=0.2, colour="white", fill=cols[2], alpha=1, stat = "identity") +
  geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
              linetype=3, size=0.2, colour="white", fill=cols[1], alpha=1, stat = "identity") +
  geom_line() +
  geom_point(pch = 4, size=0.7) +
  geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white", size=0.7) +
  theme_bw() +
  labs(title=NULL,
       x = NULL,
       y = "Deaths") +
  theme_grey(base_size=10) +
  theme(strip.text.y = element_text(size = 5.5)) # +
  # scale_y_continuous(position="right")

ggsave('plots/inc_death_gm_freey.png', width=20, height=24, dpi=800, unit='cm', device='png')

g1 <- ggplot(subset(df, target %in% c("1 wk ahead inc death", "2 wk ahead inc death")), aes(x=target_end_date, y=truth)) +
  facet_grid(rows=vars(model), cols=vars(target), scales="free_y", 
             labeller = labeller(model=label_wrap_gen(10)), # switch="y"
  ) +
  geom_smooth(aes(y = value.0.5, ymin = value.0.05, ymax = value.0.95), 
              linetype=3, size=0.2, colour="white", fill=cols[2], alpha=1, stat = "identity") +
  geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
              linetype=3, size=0.2, colour="white", fill=cols[1], alpha=1, stat = "identity") +
  geom_line() +
  geom_point(pch = 4, size=0.7) +
  geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white", size=0.7) +
  theme_bw() +
  labs(title=NULL,
       x = NULL,
       y = "Deaths") +
  theme_grey(base_size=10) +
  theme(strip.text.y = element_text(size = 7.5))


g2 <- ggplot(subset(df, target %in% c("1 wk ahead inc case", "2 wk ahead inc case")), aes(x=target_end_date, y=truth)) +
  facet_grid(rows=vars(model), cols=vars(target), scales="free_y", 
             labeller = labeller(model=label_wrap_gen(10)), # switch="y"
  ) +
  geom_smooth(aes(y = value.0.5, ymin = value.0.05, ymax = value.0.95), 
              linetype=3, size=0.2, colour="white", fill=cols[2], alpha=1, stat = "identity") +
  geom_smooth(aes(y = value.0.5, ymin = value.0.25, ymax = value.0.75),
              linetype=3, size=0.2, colour="white", fill=cols[1], alpha=1, stat = "identity") +
  geom_line() +
  geom_point(pch = 4, size=0.7) +
  geom_point(aes(y = value.0.5), pch = 21, col = "black", bg = "white", size=0.7) +
  theme_bw() +
  labs(title=NULL,
       x = NULL,
       y = "Cases") +
  theme_grey(base_size=10) +
  theme(strip.text.y = element_text(size = 7.5))

#grid.arrange(g1, g2, ncol = 2)

g <- arrangeGrob(g1, g2, ncol=2)


ggsave('plots/inc_death_gm_both.png', g,  width=20, height=24, dpi=800, unit='cm', device='png')

