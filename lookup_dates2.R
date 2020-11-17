require(tidyverse)
require(rvest)
require(tsibble)
require(lubridate)

site_date <- "https://www.nepalicalendar.com/index.php?ny=2082&nm=09&nd=02"
month_seq <- c("baisakh", "jestha", "ashar", "shrawan", "bhadra", "ashoj", 
               "kartik", "mangsir", "poush", "magh", "falgun", "chaitra")
month_seq_num <- seq_along(month_seq)

year_seq <- 2062:2077

site_calender <- vector("list", length = length(year_seq))
for (i in 1:length(year_seq)){
  year <- year_seq[i]
  for (j in 1:length(month_seq_num)) {
    month <- month_seq_num[j]
    site_calender[[i]][j] <- paste0('https://nepalicalendar.com/index.php?ny=', year,'&nm=', month,"&nd=02")
  }
} 
site_calender

######## scrape calendar from 2062 to 2077

site_calendar_scraped_2062_2077 <- site_calender %>% 
  map(function(x){
    x %>% 
      map(function(y){
        inner_month_html <- read_html(y)
        inner_month_date_cells <- inner_month_html %>% 
          html_nodes("div#day.left") %>% 
          html_attr("title")
        return(inner_month_date_cells)
      })
  })

# nepali calendar
nepali_calendar_2062_2077 <- site_calendar_scraped_2062_2077 %>% 
  map_df(~tibble(year_md = .x), .id = "nepali_year")

nepali_calendar_2062_2077 <- nepali_calendar_2062_2077 %>% 
  mutate(year_md = map(year_md, ~as.vector(na.omit(.x)))) %>% 
  group_by(nepali_year) %>% 
  mutate(nepali_month = row_number()) %>%
  unnest(year_md) %>% 
  ungroup()

nepali_calendar_2062_2077 <- nepali_calendar_2062_2077 %>% 
  separate("year_md", into = c("nepali_date", "english_date"), sep = "\\<\\>")

nepali_month_equivalent <- tibble("nepali_month" = 1:12, 
                                  "n_month" = month_seq)
nepali_year_equivalent <- tibble("nepali_year" = as.character(1:16), 
                                 "n_year" = 2062:2077)

nepali_english <- nepali_calendar_2062_2077 %>% 
  left_join(nepali_month_equivalent) %>% 
  left_join(nepali_year_equivalent) %>% 
  select(-nepali_month, -nepali_year) %>% 
  mutate(english_date = as_date(english_date))

# nepali_english %>% 
#   write_csv("./nepali_calendar_2062-2077.csv", "")
