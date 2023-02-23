# VISUALISE TRADE BETWEEN COUNTRIES:

# 0 - Define paths, vectors, functions
# 1 - Check missing values
# 2 - Visualise trading partners
# 3 - Visualise ranking of products


rm(list = ls())
cat("\014")    
`%!in%` <- Negate(`%in%`)
options( digits = 4 ) 


library(tidyr)
library(dplyr)
library(reshape2)
library(readr)
library(readxl)
library(data.table)
library(sqldf)
library(magrittr)
library(ggplot2)


#
# 0 - Define paths, vectors, functions -----------------------------------------
rm(list = ls())
# Paths
wd <- 'V:/TRADE_BALANCE_DATA/BACKUP/BATIS/BPM6 2022/DATA'
setwd(wd)

load('outputs/step9/9_balanced_all.Rdata')
data <- bilaterals_9


# Vectors
leading_ec <- c("US", "CN", "DE", "GB", "FR", "NL", "IE", "JP", "SG", "IN", "BE", 
                "IT", "CH", "ES", "KR", "CA", "HK", "LU", "RU", "AE", "SE", "AU", 
                "DK", "TH", "AT", "BR", "TW", "SA", "PL", "NO")

big_ec <- c("MY", "IL", "TR", "MX", "PH", "FI", "ID", "GR", "PT", "QA")

historical <- c( "888",  "AN",  "CW",  "ME",  "RS",  "SX",  "YU")


items <- unique(data$indicator)
items <- items[!(items %in% c('SPX1'))]

# Functions
mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Palette for labels
meth.colors <- c(
  'E0' = '#104969', 'E1' = '#1C5B7F', 'E2'='#266488', 'E3'='#347195', 
  'E4'='#3E789B', 'E5'='#4A84A7', 'E6'='#3783B0', 'E7' = '#5F96B8', 'E8' = '#71A1BF', 
  'E8.0'  = '#44494C',   'E8.1'  = '#696E72',   'E8.2'  = '#8D969B',  
  'M1.1'= '#D13B04',    'M1.2'= '#FE4703',    'M1.3'= '#F78220',   'M1.4'= '#F39444', 'M1.5'= '#DD975C', 
  'M2.0'= '#BC3706', 'M2.1'= '#D13B04',     'M2.3'= '#F78220',   'M2.4'= '#F39444', 'M2.5'= '#DD975C', 
  'M8.0'= '#2CC1B8',    'M8.1'= '#5ED7D0',   'M8.2'= '#78D7D2',
  'R_EURO'= '#1BAE5E', 'R_EUEQ'= '#1BAE5E',  'R_NAT'= '#38B973', 
  'R_NAT.1'= '#38B973',  'R_OECD'= '#2F9064',  'R_UNSD' = '#5FC296',
  'W0' = '#D77892')

# 1 - Check missing values & methods ------------------------------------------

# Check if there are any missing values, except for countries that no longer exist
t <- data %>% 
  filter(is.na(value)) %>% 
  filter(!(partner %in% historical) & !(reporter %in% historical))

table(t$partner)
table(t$reporter)
table(t$year)
table(t$indicator)


# 2 - Visualise rankings and series by items -----------------------------------

cols <-c("reporter", "partner", "indicator", "flow", "year", "balanced_value", "meth")
data <- data[cols]


for(i in 1:length(items)){
  item <- items[i]
  
  print(item)
  
  df <- data %>% 
    drop_na() %>% 
    filter(indicator == item) %>% 
    filter(!(partner %in%  c('ROW','XCF'))) %>% 
    group_by(reporter, flow, year) %>% 
    arrange(desc(value)) %>%
    mutate(rank = 1:n()) %>% 
    group_by(reporter, partner, flow) %>% 
    mutate(mode_rank_over_time = mode(rank)) %>% 
    ungroup() %>% 
    arrange(reporter, flow, year) 
  
  
  x <-  df %>%   
    filter(flow == 'X') 
  
  m <-  df %>%   
    filter(flow == 'M')
  
  
  # 2.1. Leading economies --------------------------------------------------
  countries <- intersect(unique(x$reporter), leading_ec)
  
  
  # Exports
  plot_list = list()
  
  for(i in 1:length(countries)){
    p = df %>%   
      drop_na() %>% 
      filter(flow == 'X') %>% 
      filter(mode_rank_over_time <=9) %>% 
      filter(reporter == countries[i]) %>% 
      arrange(mode_rank_over_time) %>% 
      ggplot(aes(year,value, color = meth)) + 
      geom_point(size=1) +
      facet_wrap(~partner, scales = "free_y") +
      ggtitle(countries[i],item) +
      scale_color_manual(values=meth.colors,  limits = force)
    plot_list[[i]] = p
  }
  
  pdf(paste0("outputs/Step9/checks/plots/",item,"/1_Top_tradersX.pdf"))
  for (i in 1:length(countries)) {
    print(plot_list[[i]])
  }
  dev.off()
  
  countries <- intersect(unique(m$reporter), leading_ec)
  
  # Imports
  plot_list = list()
  for(i in 1:length(countries)){
    p = df %>%
      drop_na() %>%
      filter(flow == 'M') %>%
      filter(mode_rank_over_time <=9) %>%
      filter(reporter == countries[i]) %>%
      arrange(mode_rank_over_time) %>%
      ggplot(aes(year,value, color = meth)) +
      geom_point(size=1) +
      facet_wrap(~partner, scales = "free_y") +
      ggtitle(countries[i],item)+
      scale_color_manual(values=meth.colors,  limits = force)
    plot_list[[i]] = p
  }
  
  pdf(paste0("outputs/Step9/checks/plots/",item,"/1_Top_tradersM.pdf"))
  for (i in 1:length(countries)) {
    print(plot_list[[i]])
  }
  dev.off()
  
  
  # 2.2. Other traders --------------------------------------------------
  #rest <- setdiff(unique(df$reporter), leading_ec)
  #countries <- intersect(unique(df$reporter), rest)
  countries <- setdiff(unique(x$reporter), leading_ec)
  
  
  # Exports
  plot_list = list()
  for(i in 1:length(countries)){
    p = df %>%   
      drop_na() %>% 
      filter(flow == 'X') %>% 
      filter(mode_rank_over_time <=9) %>% 
      filter(reporter == countries[i]) %>% 
      arrange(mode_rank_over_time) %>% 
      ggplot(aes(year,value, color = meth)) + 
      geom_point(size=1) +
      facet_wrap(~partner, scales = "free_y") +
      ggtitle(countries[i],item)+
      scale_color_manual(values=meth.colors,  limits = force)
    plot_list[[i]] = p
  }
  
  pdf(paste0("outputs/Step9/checks/plots/",item,"/2_rest_X.pdf"))
  for (i in 1:length(countries)) {
    print(plot_list[[i]])
  }
  dev.off()
  
  countries <- setdiff(unique(m$reporter), leading_ec)
  
  # Imports
  plot_list = list()
  for(i in 1:length(countries)){
    p = df %>%   
      drop_na() %>% 
      filter(flow == 'M') %>% 
      filter(mode_rank_over_time <=9) %>% 
      filter(reporter == countries[i]) %>% 
      arrange(mode_rank_over_time) %>% 
      ggplot(aes(year,value, color = meth)) + 
      geom_point(size=1) +
      facet_wrap(~partner, scales = "free_y") +
      ggtitle(countries[i],item)+
      scale_color_manual(values=meth.colors,  limits = force)
    plot_list[[i]] = p
  }
  
  pdf(paste0("outputs/Step9/checks/plots/",item,"/2_rest_M.pdf"))
  for (i in 1:length(countries)) {
    print(plot_list[[i]])
  }
  dev.off()
  
  
  
}

# 3 - Visualise rankings of items ----------------------------------------------

df <- data %>% 
  filter(indicator !='S') %>%  
  #drop_na() %>% 
  group_by(reporter, flow, year) %>%
  mutate(tot = sum(value , na.rm = TRUE) )  %>% 
  group_by(reporter, flow, indicator, year) %>%
  summarize(tot_ind = sum(value, na.rm = TRUE), sh_ind = tot_ind/tot, .groups = 'drop', na.rm = TRUE ) %>% 
  distinct %>% 
  ungroup() %>% 
  arrange(reporter, indicator, flow, year)

df$year <- as.numeric(df$year)

x <-  df %>%   
  filter(flow == 'X') 

m <-  df %>%   
  filter(flow == 'M')


# Leading traders, exports
countries <- intersect(unique(x$reporter), leading_ec)

plot_list = list()
for(i in 1:length(countries)){
  p = df %>%   
    filter(flow == 'X') %>% 
    filter(reporter == countries[i]) %>% 
    ggplot(aes(year,sh_ind)) + 
    geom_line()  +
    facet_wrap(~indicator, scales = "fixed") +
    expand_limits( y = c(0, 1))+
    ggtitle(countries[i])
  plot_list[[i]] = p
}

pdf(paste0("outputs/Step9/checks/plots/1_items_leading_X.pdf"))
for (i in 1:length(countries)) {
  print(plot_list[[i]])
}
dev.off()

# Leading traders, imports
countries <- intersect(unique(m$reporter), leading_ec)

plot_list = list()
for(i in 1:length(countries)){
  p = df %>%   
    filter(flow == 'M') %>% 
    filter(reporter == countries[i]) %>% 
    ggplot(aes(year,sh_ind)) + 
    geom_line()   +
    facet_wrap(~indicator, scales = "fixed") +
    expand_limits( y = c(0, 1))+
    ggtitle(countries[i])
  plot_list[[i]] = p
}

pdf(paste0("outputs/Step9/checks/plots/1_items_leading_M.pdf"))
for (i in 1:length(countries)) {
  print(plot_list[[i]])
}
dev.off()


# The rest, exports
countries <- setdiff(unique(x$reporter), leading_ec)

plot_list = list()
for(i in 1:length(countries)){
  p = df %>%   
    filter(flow == 'X') %>% 
    filter(reporter == countries[i]) %>% 
    ggplot(aes(year,sh_ind)) + 
    geom_line()   +
    facet_wrap(~indicator, scales = "fixed") +
    expand_limits( y = c(0, 1))+
    ggtitle(countries[i])
  plot_list[[i]] = p
}

pdf(paste0("outputs/Step9/checks/plots/2_items_rest_X.pdf"))
for (i in 1:length(countries)) {
  print(plot_list[[i]])
}
dev.off()


# The rest, imports
countries <- setdiff(unique(m$reporter), leading_ec)

plot_list = list()
for(i in 1:length(countries)){
  p = df %>%   
    filter(flow == 'M') %>% 
    filter(reporter == countries[i]) %>% 
    ggplot(aes(year,sh_ind)) + 
    geom_line()  +
    facet_wrap(~indicator, scales = "fixed") +
    expand_limits( y = c(0, 1))+
    ggtitle(countries[i])
  plot_list[[i]] = p
}

pdf(paste0("outputs/Step9/checks/plots/2_items_rest_M.pdf"))
for (i in 1:length(countries)) {
  print(plot_list[[i]])
}
dev.off()


