library(parallel)
library(dplyr)
library(tidyverse)
library(rvest)
library(purrr)
library(stringr)
library(stringi)

cl <- makeCluster(2)
clusterEvalQ(cl, {require(rvest)})
md_url <- 'https://www.mockdraftable.com/search?position=WR&beginYear=1999&endYear=2020&sort=DESC&page='
md_links <- lapply(paste0(md_url, 1:45),
                   function(url1){
                     url1 %>% read_html %>%
                       html_nodes('a') %>%
                       html_attr('href')
                   })
md_links <- unlist(md_links)
player_links <- md_links[str_detect(md_links, '/player')]
player_links <- player_links[!str_detect(player_links, 'marlon-brown')]
df <- tibble(stat_links = player_links)
df <- df %>%
  mutate(url = paste0('https://www.mockdraftable.com/', stat_links))
get_page <- function(url){
  page <- read_html(url)
  Sys.sleep(sample(seq(.25,2.5,.25), 1))
  page
}
page_data <- map(df$url, get_page)
stopCluster(cl)
md_data <- map(page_data, html_table)
md_data <- pluck(md_data, 1) %>% 
  map2_df(df$stat_links, 
          ~as_tibble(.x) %>% 
            mutate(stat = .y)) %>%
  set_names(c(
    'Measurables',
    'Measurements',
    'Percentile',
    'Names'
  ))
md_data <- select(md_data, -3)
md_data$Names <- gsub('/player/', '', md_data$Names)
md_data$Names <- gsub('position=WR', '', md_data$Names)
md_data$Names <- gsub('-', ' ', md_data$Names)
md_data$Names <- gsub('[?]', '', md_data$Names)
md_data$Names <- toupper(md_data$Names)
md_data$Measurements <- gsub('[*]', '', md_data$Measurements)
md_data$Measurements <- gsub('>', '', gsub('<U\\+', '\\\\u', md_data$Measurements))
md_data$Measurements <- gsub('[¼]', '.250', md_data$Measurements)
md_data$Measurements <- gsub('[½]', '.500', md_data$Measurements)
md_data$Measurements <- gsub('[¾]', '.750', md_data$Measurements)
md_data$Measurements <- gsub('[⅛]', '.125', md_data$Measurements)
md_data$Measurements <- gsub('[⅜]', '.375', md_data$Measurements)
md_data$Measurements <- gsub('[⅝]', '.625', md_data$Measurements)
md_data$Measurements <- gsub('[⅞]', '.875', md_data$Measurements)
md_data$Measurements <- gsub('lbs', '', md_data$Measurements)
md_data$Measurements <- gsub('reps', '', md_data$Measurements)
md_data$Measurements <- gsub('lbs', '', md_data$Measurements)
md_data$Measurements <- gsub('s', '', md_data$Measurements)
md_pivot <- md_data %>% pivot_wider(names_from = 'Measurables', values_from = 'Measurements')
md_pivot$Height <- sapply(strsplit(md_pivot$Height, "'|\""),
                          function(x){12*as.numeric(x[1]) + as.numeric(x[2])
                          })
md_pivot$Wingspan <- gsub('["]', '', md_pivot$Wingspan)
md_pivot$`Arm Length` <- gsub('["]', '', md_pivot$`Arm Length`)
md_pivot$`Hand Size` <- gsub('["]', '', md_pivot$`Hand Size`)
md_pivot$`Vertical Jump` <- gsub('["]', '', md_pivot$`Vertical Jump`)
md_pivot$`Broad Jump` <- gsub('["]', '', md_pivot$`Broad Jump`)
md_pivot_WR <- md_pivot
md_pivot_WR <- WR_Combine_Data

cl <- makeCluster(2)
clusterEvalQ(cl, {require(rvest)})
md_url <- 'https://www.mockdraftable.com/search?position=RB&beginYear=1999&endYear=2020&sort=DESC&page='
md_links <- lapply(paste0(md_url, 1:36),
                   function(url1){
                     url1 %>% read_html %>%
                       html_nodes('a') %>%
                       html_attr('href')
                   })
md_links <- unlist(md_links)
player_links <- md_links[str_detect(md_links, '/player')]
player_links <- player_links[!str_detect(player_links, 'christopher-thompson')]
player_links <- player_links[!str_detect(player_links, 'spencer-ware')]
df <- tibble(stat_links = player_links)
df <- df %>%
  mutate(url = paste0('https://www.mockdraftable.com/', stat_links))
get_page <- function(url){
  page <- read_html(url)
  Sys.sleep(sample(seq(.25,2.5,.25), 1))
  page
}
page_data <- map(df$url, get_page)
stopCluster(cl)
md_data <- map(page_data, html_table)
md_data <- pluck(md_data, 1) %>% 
  map2_df(df$stat_links, 
          ~as_tibble(.x) %>% 
            mutate(stat = .y)) %>%
  set_names(c(
    'Measurables',
    'Measurements',
    'Percentile',
    'Names'
  ))
md_data <- select(md_data, -3)
md_data$Names <- gsub('/player/', '', md_data$Names)
md_data$Names <- gsub('position=RB', '', md_data$Names)
md_data$Names <- gsub('-', ' ', md_data$Names)
md_data$Names <- gsub('[?]', '', md_data$Names)
md_data$Names <- toupper(md_data$Names)
md_data$Measurements <- gsub('[*]', '', md_data$Measurements)
md_data$Measurements <- gsub('>', '', gsub('<U\\+', '\\\\u', md_data$Measurements))
md_data$Measurements <- gsub('[¼]', '.250', md_data$Measurements)
md_data$Measurements <- gsub('[½]', '.500', md_data$Measurements)
md_data$Measurements <- gsub('[¾]', '.750', md_data$Measurements)
md_data$Measurements <- gsub('[⅛]', '.125', md_data$Measurements)
md_data$Measurements <- gsub('[⅜]', '.375', md_data$Measurements)
md_data$Measurements <- gsub('[⅝]', '.625', md_data$Measurements)
md_data$Measurements <- gsub('[⅞]', '.875', md_data$Measurements)
md_data$Measurements <- gsub('lbs', '', md_data$Measurements)
md_data$Measurements <- gsub('reps', '', md_data$Measurements)
md_data$Measurements <- gsub('lbs', '', md_data$Measurements)
md_data$Measurements <- gsub('s', '', md_data$Measurements)
md_pivot <- md_data %>% pivot_wider(names_from = 'Measurables', values_from = 'Measurements')
md_pivot$Height <- sapply(strsplit(md_pivot$Height, "'|\""),
                          function(x){12*as.numeric(x[1]) + as.numeric(x[2])
                          })
md_pivot$Wingspan <- gsub('["]', '', md_pivot$Wingspan)
md_pivot$`Arm Length` <- gsub('["]', '', md_pivot$`Arm Length`)
md_pivot$`Hand Size` <- gsub('["]', '', md_pivot$`Hand Size`)
md_pivot$`Vertical Jump` <- gsub('["]', '', md_pivot$`Vertical Jump`)
md_pivot$`Broad Jump` <- gsub('["]', '', md_pivot$`Broad Jump`)
md_pivot_RB <- md_pivot
md_pivot_RB <- RB_Combine_Data

cl <- makeCluster(2)
clusterEvalQ(cl, {require(rvest)})
md_url <- 'https://www.mockdraftable.com/search?position=TE&beginYear=1999&endYear=2020&sort=DESC&page='
md_links <- lapply(paste0(md_url, 1:20),
                   function(url1){
                     url1 %>% read_html %>%
                       html_nodes('a') %>%
                       html_attr('href')
                   })
md_links <- unlist(md_links)
player_links <- md_links[str_detect(md_links, '/player')]
df <- tibble(stat_links = player_links)
df <- df %>%
  mutate(url = paste0('https://www.mockdraftable.com/', stat_links))
get_page <- function(url){
  page <- read_html(url)
  Sys.sleep(sample(seq(.25,2.5,.25), 1))
  page
}
page_data <- map(df$url, get_page)
stopCluster(cl)
md_data <- map(page_data, html_table)
md_data <- pluck(md_data, 1) %>% 
  map2_df(df$stat_links, 
          ~as_tibble(.x) %>% 
            mutate(stat = .y)) %>%
  set_names(c(
    'Measurables',
    'Measurements',
    'Percentile',
    'Names'
  ))
md_data <- select(md_data, -3)
md_data$Names <- gsub('/player/', '', md_data$Names)
md_data$Names <- gsub('position=TE', '', md_data$Names)
md_data$Names <- gsub('-', ' ', md_data$Names)
md_data$Names <- gsub('[?]', '', md_data$Names)
md_data$Names <- toupper(md_data$Names)
md_data$Measurements <- gsub('[*]', '', md_data$Measurements)
md_data$Measurements <- gsub('>', '', gsub('<U\\+', '\\\\u', md_data$Measurements))
md_data$Measurements <- gsub('[¼]', '.250', md_data$Measurements)
md_data$Measurements <- gsub('[½]', '.500', md_data$Measurements)
md_data$Measurements <- gsub('[¾]', '.750', md_data$Measurements)
md_data$Measurements <- gsub('[⅛]', '.125', md_data$Measurements)
md_data$Measurements <- gsub('[⅜]', '.375', md_data$Measurements)
md_data$Measurements <- gsub('[⅝]', '.625', md_data$Measurements)
md_data$Measurements <- gsub('[⅞]', '.875', md_data$Measurements)
md_data$Measurements <- gsub('lbs', '', md_data$Measurements)
md_data$Measurements <- gsub('reps', '', md_data$Measurements)
md_data$Measurements <- gsub('lbs', '', md_data$Measurements)
md_data$Measurements <- gsub('s', '', md_data$Measurements)
md_pivot <- md_data %>% pivot_wider(names_from = 'Measurables', values_from = 'Measurements')
md_pivot$Height <- sapply(strsplit(md_pivot$Height, "'|\""),
                          function(x){12*as.numeric(x[1]) + as.numeric(x[2])
                          })
md_pivot$Wingspan <- gsub('["]', '', md_pivot$Wingspan)
md_pivot$`Arm Length` <- gsub('["]', '', md_pivot$`Arm Length`)
md_pivot$`Hand Size` <- gsub('["]', '', md_pivot$`Hand Size`)
md_pivot$`Vertical Jump` <- gsub('["]', '', md_pivot$`Vertical Jump`)
md_pivot$`Broad Jump` <- gsub('["]', '', md_pivot$`Broad Jump`)
md_pivot_TE <- md_pivot
md_pivot_TE <- TE_Combine_Data

cl <- makeCluster(2)
clusterEvalQ(cl, {require(rvest)})
md_url <- 'https://www.mockdraftable.com/search?position=QB&beginYear=1999&endYear=2020&sort=DESC&page='
md_links <- lapply(paste0(md_url, 1:20),
                   function(url1){
                     url1 %>% read_html %>%
                       html_nodes('a') %>%
                       html_attr('href')
                   })
md_links <- unlist(md_links)
player_links <- md_links[str_detect(md_links, '/player')]
player_links <- player_links[!str_detect(player_links, 'sean-renfree')]
player_links <- player_links[!str_detect(player_links, 'matt-barkley')]
player_links <- player_links[!str_detect(player_links, 'jordan-lynch-2014-1')]
player_links <- player_links[!str_detect(player_links, 'jordan-lynch-2014-2')]
df <- tibble(stat_links = player_links)
df <- df %>%
  mutate(url = paste0('https://www.mockdraftable.com/', stat_links))
get_page <- function(url){
  page <- read_html(url)
  Sys.sleep(sample(seq(.25,2.5,.25), 1))
  page
}
page_data <- map(df$url, get_page)
stopCluster(cl)
md_data <- map(page_data, html_table)
md_data <- pluck(md_data, 1) %>% 
  map2_df(df$stat_links, 
          ~as_tibble(.x) %>% 
            mutate(stat = .y)) %>%
  set_names(c(
    'Measurables',
    'Measurements',
    'Percentile',
    'Names'
  ))
md_data <- select(md_data, -3)
md_data$Names <- gsub('/player/', '', md_data$Names)
md_data$Names <- gsub('position=QB', '', md_data$Names)
md_data$Names <- gsub('-', ' ', md_data$Names)
md_data$Names <- gsub('[?]', '', md_data$Names)
md_data$Names <- toupper(md_data$Names)
md_data$Measurements <- gsub('[*]', '', md_data$Measurements)
md_data$Measurements <- gsub('>', '', gsub('<U\\+', '\\\\u', md_data$Measurements))
md_data$Measurements <- gsub('[¼]', '.250', md_data$Measurements)
md_data$Measurements <- gsub('[½]', '.500', md_data$Measurements)
md_data$Measurements <- gsub('[¾]', '.750', md_data$Measurements)
md_data$Measurements <- gsub('[⅛]', '.125', md_data$Measurements)
md_data$Measurements <- gsub('[⅜]', '.375', md_data$Measurements)
md_data$Measurements <- gsub('[⅝]', '.625', md_data$Measurements)
md_data$Measurements <- gsub('[⅞]', '.875', md_data$Measurements)
md_data$Measurements <- gsub('lbs', '', md_data$Measurements)
md_data$Measurements <- gsub('reps', '', md_data$Measurements)
md_data$Measurements <- gsub('lbs', '', md_data$Measurements)
md_data$Measurements <- gsub('s', '', md_data$Measurements)
md_pivot <- md_data %>% pivot_wider(names_from = 'Measurables', values_from = 'Measurements')
md_pivot$Height <- sapply(strsplit(md_pivot$Height, "'|\""),
                          function(x){12*as.numeric(x[1]) + as.numeric(x[2])
                          })
md_pivot$Wingspan <- gsub('["]', '', md_pivot$Wingspan)
md_pivot$`Arm Length` <- gsub('["]', '', md_pivot$`Arm Length`)
md_pivot$`Hand Size` <- gsub('["]', '', md_pivot$`Hand Size`)
md_pivot$`Vertical Jump` <- gsub('["]', '', md_pivot$`Vertical Jump`)
md_pivot$`Broad Jump` <- gsub('["]', '', md_pivot$`Broad Jump`)
md_pivot_QB <- md_pivot
md_pivot_QB <- QB_Combine_Data

WR_Draft_Data <- read_csv("https://docs.google.com/spreadsheets/d/1Jzk0z6_uHLmda6EujHUDI-1ZvMm--JnOytez8g9tp0I/edit?usp=sharing")
WR_Draft_Data$Player <- gsub('[\\]', '/', WR_Draft_Data$Player)
WR_Draft_Data$Player <- gsub('[/].*$', '\\1', WR_Draft_Data$Player)
WR_Draft_Data$Player <- gsub('[.]', '', WR_Draft_Data$Player)
WR_Draft_Data$Player <- gsub('-', '', WR_Draft_Data$Player)
WR_Draft_Data$Player <- gsub('[\']', '/', WR_Draft_Data$Player)
WR_Draft_Data$Player <- toupper(WR_Draft_Data$Player)

RB_Draft_Data <- read_csv("https://docs.google.com/spreadsheets/d/150dfxF0H9aWpE0Z5OTpYZ_iG1oRGusb0A5lE74uZexA/edit?usp=sharing")
RB_Draft_Data$Player <- gsub('[\\]', '/', RB_Draft_Data$Player)
RB_Draft_Data$Player <- gsub('[/].*$', '\\1', RB_Draft_Data$Player)
RB_Draft_Data$Player <- gsub('[.]', '', RB_Draft_Data$Player)
RB_Draft_Data$Player <- gsub('-', '', RB_Draft_Data$Player)
RB_Draft_Data$Player <- gsub('[\']', '', RB_Draft_Data$Player)
RB_Draft_Data$Player <- toupper(RB_Draft_Data$Player)

TE_Draft_Data <- read_csv("https://docs.google.com/spreadsheets/d/1GDEt3a-KnP4A8QJLTrldYEvd9jcT71BEQJWDZOTmRoE/edit?usp=sharing")
TE_Draft_Data$Player <- gsub('[\\]', '/', TE_Draft_Data$Player)
TE_Draft_Data$Player <- gsub('[/].*$', '\\1', TE_Draft_Data$Player)
TE_Draft_Data$Player <- gsub('[.]', '', TE_Draft_Data$Player)
TE_Draft_Data$Player <- gsub('-', '', TE_Draft_Data$Player)
TE_Draft_Data$Player <- gsub('[\']', '/', TE_Draft_Data$Player)
TE_Draft_Data$Player <- toupper(TE_Draft_Data$Player)

QB_Draft_Data <- read_csv("https://docs.google.com/spreadsheets/d/1o6UePMgtNw_a3xtvInnJTHM6jk8vQiXBsElDJ2e4_tU/edit?usp=sharing")
QB_Draft_Data$Player <- gsub('[\\]', '/', QB_Draft_Data$Player)
QB_Draft_Data$Player <- gsub('[/].*$', '\\1', QB_Draft_Data$Player)
QB_Draft_Data$Player <- gsub('[.]', '', QB_Draft_Data$Player)
QB_Draft_Data$Player <- gsub('-', '', QB_Draft_Data$Player)
QB_Draft_Data$Player <- gsub('[\']', '/', QB_Draft_Data$Player)
QB_Draft_Data$Player <- toupper(QB_Draft_Data$Player)

WR_Combine_Data_Updated <- read_csv("https://docs.google.com/spreadsheets/d/1qO9b19eaEI_YmgBMhWH8r2mTyEQ2YN9f8kGwmF_N5Jc/edit?usp=sharing")
WR_Total_Data <- merge(WR_Draft_Data, WR_Combine_Data_Updated, by.x = 'Player', by.y = 'Names', all.x = TRUE)

RB_Combine_Data_Updated <- read_csv("https://docs.google.com/spreadsheets/d/1t0-wM6kdxEZqI7dEyZhVlbm8ak-VtjPnD9129jejAXo/edit?usp=sharing")
RB_Total_Data <- merge(RB_Draft_Data, RB_Combine_Data_Updated, by.x = 'Player', by.y = 'Names', all.x = TRUE)

TE_Total_Data <- merge(TE_Draft_Data, TE_Combine_Data, by.x = 'Player', by.y = 'Names', all.x = TRUE)

QB_Combine_Data_Updated <- read_csv("https://docs.google.com/spreadsheets/d/1Wo1We2D_suVdjSR26WPt20JWrIblzV3DptmEsXDj7rE/edit?usp=sharing")
QB_Total_Data <- merge(QB_Draft_Data, QB_Combine_Data_Updated, by.x = 'Player', by.y = 'Names', all.x = TRUE)

FF_Data <- read_csv("https://docs.google.com/spreadsheets/d/1eOmYVLeHV2e1S1ANZbgP1nsHWOj8Ci54Pr_x1bzS65E/edit?usp=sharing")
FF_Data$Player <- gsub('[\\]', '/', FF_Data$Player)
FF_Data$Player <- gsub('[/].*$', '\\1', FF_Data$Player)
FF_Data$Player <- gsub('[.]', '', FF_Data$Player)
FF_Data$Player <- gsub('-', '', FF_Data$Player)
FF_Data$Player <- gsub('[\']', '', FF_Data$Player)
FF_Data$Player <- gsub('[*]', '', FF_Data$Player)
FF_Data$Player <- gsub('[+]', '', FF_Data$Player)
FF_Data$Player <- toupper(FF_Data$Player)

FF_Data <- read_csv("https://docs.google.com/spreadsheets/d/14CYs2Pq8Eg_gu7pE3v51wdoF-WYdca-Ni-Fiks1c5hg/edit?usp=sharing")

FF_WR_Subset <- filter(FF_Data, Position == 'WR')

FF_RB_Subset <- filter(FF_Data, Position == 'RB')

FF_TE_Subset <- filter(FF_Data, Position == 'TE')

FF_QB_Subset <- filter(FF_Data, Position == 'QB')

FF_WR_Data <- merge(FF_WR_Subset, WR_Total_Data, by.x = 'Player', by.y = 'Player', all.x = TRUE)

FF_RB_Data <- merge(FF_RB_Subset, RB_Total_Data, by.x = 'Player', by.y = 'Player', all.x = TRUE)

FF_TE_Data <- merge(FF_TE_Subset, TE_Total_Data, by.x = 'Player', by.y = 'Player', all.x = TRUE)

FF_QB_Data <- merge(FF_QB_Subset, QB_Total_Data, by.x = 'Player', by.y = 'Player', all.x = TRUE)

FF_WR_Data_Filled_In <- read_csv("https://docs.google.com/spreadsheets/d/1WjoBaEZs5ZFV3eiBB22jWMtJ9gZ9hGQAcDyjBjwTBpU/edit?usp=sharing")
FF_RB_Data_Filled_In <- read_csv("https://docs.google.com/spreadsheets/d/1Oev8zF1YV6yWOMq_5LKEKLY4F6efBKO2RxCz-uKsXXo/edit?usp=sharing")
FF_TE_Data_Filled_In <- read_csv("https://docs.google.com/spreadsheets/d/1M0bZb3b-ZPWTLXZtL5vBmbff-LypimENtukcU15DSnU/edit?usp=sharing")
FF_QB_Data_Filled_In <- read_csv("https://docs.google.com/spreadsheets/d/1vCI8hfCk7yZWRCgxyA7FegNwWKfZH5DhRLzF8Jj9ZgQ/edit?usp=sharing")

FF_All_Position_Data_Filled_In <- bind_rows(FF_WR_Data_Filled_In, FF_RB_Data_Filled_In, FF_TE_Data_Filled_In, FF_QB_Data_Filled_In)
FF_All_Position_Data_Filled_In$Pos <- NULL