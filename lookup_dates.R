require(tidyverse)
require(rvest)
require(tsibble)
require(lubridate)

site_date <- "https://nepalicalendar.rat32.com/index.php?year=2062&month=Shrawan"
month_seq <- c("baisakh", "jestha", "ashar", "shrawan", "bhadra", "ashoj", 
               "kartik", "mangsir", "poush", "magh", "falgun", "chaitra")

year_seq <- 2062:2077

site_calender <- vector("list", length = length(year_seq))
for (i in 1:length(year_seq)){
  year <- year_seq[i]
  for (j in 1:length(month_seq)) {
    month <- month_seq[j]
    site_calender[[i]][j] <- paste0('https://nepalicalendar.rat32.com/index.php?year=', year,'&month=',month) 
  }
} 
site_calender

######## scrape calendar from 2062 to 2071

site_calendar_scraped_2062_2071 <- site_calender[1:10] %>% 
  map(function(x){
    x %>% 
      map(function(y){
        inner_month_html <- read_html(y)
        inner_month_date_cells <- inner_month_html %>% 
          html_nodes("#main") %>%
          html_nodes("div.cells")
        
        english <- inner_month_date_cells %>% 
          html_nodes(xpath = '//*[@id="eday"]')
        nepali <- inner_month_date_cells %>% 
          html_nodes(xpath = '//*[@id="nday"]')
        return(list(english, nepali))
      })
  })

# nepali calendar
nepali_calendar_2062_2071 <- site_calendar_scraped_2062_2071 %>% 
  map(function(x) {
    x %>% 
      map(function(y) {
        y %>% 
          .[[2]] %>% 
          html_text()
      })
  })

nepali_calendar_2062_2071 <- nepali_calendar_2062_2071 %>% 
  map_df(~tibble(day = .x), .id = "year") %>% 
  mutate(day = map(day, ~str_remove_all(.x, "\\n") %>% 
                      str_squish() %>% 
                      as.numeric())) %>% 
  group_by(year) %>% 
  mutate(month = row_number()) %>%
  unnest(day)

nepali_calendar_2062_2071

# english calendar
english_calendar_2062_2071 <- site_calendar_scraped_2062_2071 %>% 
  map(function(x) {
    x %>% 
      map(function(y) {
        y %>% 
          .[[1]] %>% 
          html_text()
      })
  })

english_calendar_2062_2071 <- english_calendar_2062_2071 %>% 
  map_df(~tibble(day = .x), .id = "year") %>% 
  mutate(day = map(day, ~str_remove_all(.x, "\\n") %>% 
                     str_squish() %>% 
                     as.numeric())) %>% 
  group_by(year) %>% 
  mutate(month = row_number()) %>%
  unnest(day) %>% 
  ungroup()

english_calendar_2062_2071

nepali_month_equivalent <- tibble("nepali_month" = 1:12, 
                                  "n_month" = month_seq)
nepali_year_equivalent <- tibble("nepali_year" = 1:10, 
                                 "n_year" = 2062:2071)

bound_ne_2062_2071 <- nepali_calendar_2062_2071 %>% 
  rename("nepali_year" = year, 
         "nepali_day" = day, 
         "nepali_month" = month) %>% 
  bind_cols(english_calendar_2062_2071 %>% 
              select("english_day" = day))

bound_ne_2062_2071 <- bound_ne_2062_2071 %>% 
  mutate(nepali_year = as.integer(nepali_year)) %>% 
  filter(!is.na(nepali_day) & !is.na(english_day)) %>% 
  left_join(nepali_month_equivalent) %>% 
  left_join(nepali_year_equivalent) %>%
  ungroup() %>%
  arrange(nepali_year, nepali_month, nepali_day) %>%
  group_by(nepali_year, n_month) %>% 
  mutate(english_month = case_when(
    n_month == 'baisakh' & english_day == max(english_day) ~ "april",
    n_month == 'jestha' & english_day == max(english_day) ~ "may",
    n_month == 'ashar' & english_day == max(english_day) ~ "june",
    n_month == 'shrawan' & english_day == max(english_day) ~ "july",
    n_month == 'bhadra' & english_day == max(english_day) ~ "august",
    n_month == 'ashoj' & english_day == max(english_day) ~ "september",
    n_month == 'kartik' & english_day == max(english_day) ~ "october",
    n_month == 'mangsir' & english_day == max(english_day) ~ "november", 
    n_month == 'poush' & english_day == max(english_day) ~ "december",
    n_month == 'magh' & english_day == max(english_day) ~ "january",
    n_month == 'falgun' & english_day == max(english_day) ~ "february", 
    n_month == 'chaitra' & english_day == max(english_day) ~ "march"
  )) %>%
  tidyr::fill(english_month, .direction = "up") %>% 
  ungroup() %>% 
  tidyr::fill(english_month, .direction = "up")

bound_ne_2062_2071 # has problems

######## scrape calendar from 2072 to 2077

site_calendar_scraped_2072_2077 <- site_calender[11:16] %>% 
  map(function(x){
    x %>% 
      map(function(y){
        inner_month_html <- read_html(y)
        inner_month_date_cells <- inner_month_html %>% 
          html_nodes("#main") %>%
          html_nodes("div.cells")
        
        english <- inner_month_date_cells %>% 
          html_nodes(xpath = '//*[@id="eday"]')
        nepali <- inner_month_date_cells %>% 
          html_nodes(xpath = '//*[@id="nday"]')
        return(list(english, nepali))
      })
  })

# nepali calendar
nepali_calendar_2072_2077 <- site_calendar_scraped_2072_2077 %>% 
  map(function(x) {
    x %>% 
      map(function(y) {
        y %>% 
          .[[2]] %>% 
          html_text()
      })
  })

nepali_calendar_2072_2077 <- nepali_calendar_2072_2077 %>% 
  map_df(~tibble(day = .x), .id = "year") %>% 
  mutate(day = map(day, ~str_remove_all(.x, "\\n") %>% 
                     str_squish() %>% 
                     as.numeric())) %>% 
  group_by(year) %>% 
  mutate(month = row_number()) %>%
  unnest(day)

# english calendar
english_calendar_2072_2077 <- site_calendar_scraped_2072_2077 %>% 
  map(function(x) {
    x %>% 
      map(function(y) {
        y %>% 
          .[[1]] %>% 
          html_text()
      })
  })

english_calendar_2072_2077 <- english_calendar_2072_2077 %>% 
  map_df(~tibble(day = .x), .id = "year") %>% 
  mutate(day = map(day, ~str_remove_all(.x, "\\n") %>% 
                     str_squish() %>% 
                     as.numeric())) %>% 
  group_by(year) %>% 
  mutate(month = row_number()) %>%
  unnest(day) %>% 
  ungroup()

nepali_month_equivalent <- tibble("nepali_month" = 1:12, 
                                  "n_month" = month_seq)
nepali_year_equivalent <- tibble("nepali_year" = 1:6, 
                                 "n_year" = 2072:2077)

bound_ne_2072_2077 <- nepali_calendar_2072_2077 %>% 
  rename("nepali_year" = year, 
         "nepali_day" = day, 
         "nepali_month" = month) %>% 
  bind_cols(english_calendar_2072_2077 %>% 
              select("english_day" = day))

bound_ne_2072_2077 <- bound_ne_2072_2077 %>% 
  mutate(nepali_year = as.integer(nepali_year)) %>% 
  filter(!is.na(nepali_day) & !is.na(english_day)) %>% 
  left_join(nepali_month_equivalent) %>% 
  left_join(nepali_year_equivalent) %>%
  ungroup() %>%
  arrange(nepali_year, nepali_month, nepali_day) %>%
  group_by(nepali_year, n_month) %>% 
  mutate(english_month = case_when(
    n_month == 'baisakh' & english_day == max(english_day) ~ "april",
    n_month == 'jestha' & english_day == max(english_day) ~ "may",
    n_month == 'ashar' & english_day == max(english_day) ~ "june",
    n_month == 'shrawan' & english_day == max(english_day) ~ "july",
    n_month == 'bhadra' & english_day == max(english_day) ~ "august",
    n_month == 'ashoj' & english_day == max(english_day) ~ "september",
    n_month == 'kartik' & english_day == max(english_day) ~ "october",
    n_month == 'mangsir' & english_day == max(english_day) ~ "november", 
    n_month == 'poush' & english_day == max(english_day) ~ "december",
    n_month == 'magh' & english_day == max(english_day) ~ "january",
    n_month == 'falgun' & english_day == max(english_day) ~ "february", 
    n_month == 'chaitra' & english_day == max(english_day) ~ "march"
  )) %>%
  tidyr::fill(english_month, .direction = "up") %>% 
  ungroup() %>% 
  tidyr::fill(english_month, .direction = "up")

bound_ne_2072_2077 # has problems

bound_ne <- bound_ne_2062_2071 %>% 
  bind_rows(bound_ne_2072_2077) %>% 
  tidyr::replace_na(replace = list(english_month = "april"))

bound_ne_some <- bound_ne %>% 
  select(-nepali_year, -nepali_month) %>%
  mutate(year_md = seq(ymd("2005-04-14"), 
                     by = "day", length.out = nrow(.))) %>%
  as_tsibble(index = year_md) %>% 
  mutate(yearmon_auto = month(yearmonth(year_md), label = TRUE, abbr = FALSE), 
         english_month = str_to_title(english_month))

