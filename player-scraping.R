# Original author: https://github.com/seanjtaylor/learning-the-draft
# Edits by: https://github.com/akiehl2000

library(tidyverse)
library(rvest)
library(RCurl)
library(feather)

franchises <- c('ANA', 'PHX', 'BOS', 'BUF', 'CGY', 'CAR', 'CHI', 'COL', 'CBJ', 'DAL', 'DET', 'EDM', 'FLA', 'LAK', 'MIN', 'MTL', 
                'NSH', 'NJD', 'NYI', 'NYR', 'OTT', 'PHI', 'PIT', 'SJS', 'STL', 'TBL', 'TOR', 'VAN', 'VEG', 'WSH', 'WPG')

headers <- list()
headers[['skaters_playoffs']] <- c('Player', 'Pos', 'G', 'A', '+/-', 'TOI')
headers[['goalies_playoffs']] <- c('Player', 'SA', 'MIN')
headers[['teams']] <- c('Team', 'GP', 'W', 'OW', 'OL', 'G', 'GA')

url.extract <- function(tds) {
  results <- c()
  for(td in tds) {
    children <- html_children(td)
    if (length(children) == 0) {
      results <- c(results, NA)
    } else{
      results <- c(results, (html_attr(html_children(td), 'href')))
    }
  }
  results
}

parse_pfr_tables <- function(tables) {
  results = list()
  for (tbl in tables) {
    id <- html_attr(tbl, 'id')
    if (id %in% names(headers)) {
      
      df <- html_table(tbl) %>%
        head(-1) %>% tail(-1)
      
      if(ncol(df) == length(headers[[id]])) {
        colnames(df) <- headers[[id]]
      } else {
        next;
      }
      
      melted <- df %>%
        select(headers[[id]]) %>%
        gather(stat, value) %>%
        mutate(stat = as.character(stat)) %>%
        filter(value != '') %>%
        mutate(value = as.numeric(value),
               section = id)
      
      results[[id]] <- melted
    }
  }
  bind_rows(results)
}

# Want to index https://www.hockey-reference.com/playoffs/ by year
# Want to index https://www.hockey-reference.com/teams/ by franchise and then by year (conditional on playoff status)

if (!file.exists('data/playoffs.feather')) {
  playoffs_table <- data_frame(year = 1999:2019) %>%
    group_by(year) %>% do({
      url <- paste('https://www.hockey-reference.com/playoffs/NHL_', .$year, '.html', sep ='')
      doc <- read_html(url)
      html.table <- doc %>%
        html_nodes('table') %>%
        first
      urls <- html.table %>%
        html_nodes('tr td:nth-child(29)') %>%
        url.extract
      my.table <- html_table(html.table)
      my.table <- my.table %>%
        mutate(url = urls)
      my.table
    }) %>%
    ungroup
  write_feather(playoffs_table, 'data/playoffs.feather')
}

if (!file.exists('data/players.feather')) {
  players_table <- data_frame(year = 1999:2019, franchise = franchises) %>%
    group_by(franchise, year) %>% do({
      url <- paste('https://www.hockey-reference.com/teams/', .$franchise, '/', .$year, '.html', sep ='')
      html.table <- read_html(url) %>%
        html_nodes('table') %>%
        first
      urls <- html.table %>%
        html_nodes('tr td:nth-child(4)') %>%
        url.extract
      my.table <- html_table(html.table)
      my.table <- my.table %>%
        mutate(url = urls)
      my.table
    }) %>%
    ungroup
  write_feather(players_table, 'data/players.feather')
}

all.urls <- playoffs_table %>%
  select(url) %>%
  full_join(players_table %>% select(url)) %>%
  filter(!is.na(url))

counter <- 0
hockey_stats <- list()
for(url in all.urls$url){
  cat(match(url, all.urls$url), 'of', length(all.urls$url), ': URL = ', url, '\n')
  tryCatch(doc <- read_html(url), error = function(e){
    print("Error: unable to read html.")
    counter <- counter + 1})
  stats <- doc %>%
    html_nodes('table') %>%
    parse_pfr_tables
  if (nrow(stats) > 0){
    stats <- stats %>%
      group_by(section, stat) %>%
      summarise(value = sum(value))
  }
  hockey_stats[[url]] <- stats
}
hockey_stats <- bind_rows(hockey_stats, .id = 'url')

write_feather(hockey_stats, 'data/hockey_stats.feather')