# require packages
require(tidyverse)
require(lubridate)
require(tsibble)
require(feasts)
require(viridis)
theme_set(theme_bw())

kalimati_df <- readxl::read_xlsx("./data/data_crop_aggregated.xlsx", na = ".")
month_record_names <- outer(c("baisakh", "jestha", "ashar", "shrawan", "bhadra", "ashoj", 
                              "kartik", "mangsir", "poush", "magh", "falgun", "chaitra"), 
                            c("_min", "_max", "_average"), paste0) %>% 
  t() %>% 
  as.vector()

kalimati_df <- kalimati_df %>% 
  magrittr::set_colnames(c("sn", "year", "crop", "unit", month_record_names))

kalimati_df <- kalimati_df %>% 
  mutate(unit = case_when(
    unit == "100 Pcs" ~ "Rs/100 Pcs", 
    unit == "1 Pc" ~ "Rs/Pc", 
    unit == "Doz" ~ "Rs/Dozen", 
    unit == "Kg" ~ "Rs/Kg", 
    unit == "Per 100 pieces" ~ "Rs/100 Pcs", 
    unit == "Per 1 piece" ~ "Rs/Pc", 
    unit == "Per 1 Piece" ~ "Rs/Pc", 
    unit == "Rs/100" ~ "Rs/100 Pcs", 
    unit == "Rs/1 Pc" ~ "Rs/Pc", 
    unit == "Rs per Dozen" ~ "Rs/Dozen", 
    unit == "Rs/Doz" ~ "Rs/Dozen",
    unit == "Dozen" ~ "Rs/Dozen",
    TRUE ~ unit))

kalimati_df %>% 
  count(year)

# nepali_date_lookup <- read_csv("./data/nepali_calendar_2062-2077.csv", 
#                                col_types = cols(
#                                  nepali_date = col_character()
#                                ))

nepali_date_lookup <- nepali_date_lookup %>% 
  mutate(english_year = lubridate::year(english_date))

nepali_date_year_lookup <- nepali_date_lookup %>% 
  group_by(n_year, n_month) %>% 
  summarize(english_year = first(english_year)) %>% 
  ungroup()

nepali_english_month_base <- tribble(
  ~"n_month", ~"english_month",
  'baisakh', "april",
  'jestha', "may",
  'ashar', "june",
  'shrawan', "july",
  'bhadra', "august",
  'ashoj', "september",
  'kartik', "october",
  'mangsir', "november", 
  'poush', "december",
  'magh', "january",
  'falgun', "february", 
  'chaitra', "march")

kalimati_df <- kalimati_df %>% 
  mutate(crop = str_to_sentence(crop))

kalimati_onion <- kalimati_df %>% 
  mutate(onion_yn = str_detect(crop, regex("Onion", ignore_case = TRUE))) %>% 
  filter(onion_yn) %>% 
  select(-sn, -unit, -onion_yn) %>% 
  pivot_longer(cols = baisakh_min:chaitra_average, names_to = c("seasonality", "price_level"), names_sep = "_", values_to = "price") %>% 
  pivot_wider(names_from = price_level, values_from = price, names_prefix = "price_")

kalimati_onion_ts <- kalimati_onion %>% 
  rename(n_year = year, 
         n_month = seasonality) %>% 
  group_by(crop, n_year) %>% 
  mutate(n_month_num = row_number()) %>% 
  left_join(nepali_date_year_lookup) %>% 
  left_join(nepali_english_month_base) %>% 
  mutate(english_yearmon = yearmonth(paste0(english_year, " ", english_month))) %>% 
  select(-english_year, -english_month) %>% 
  as_tsibble(index = english_yearmon, key = crop, validate = TRUE, regular = TRUE) %>% 
  ungroup() %>% 
  filter(!(n_year == 2077 & n_month %in% c("kartik", "mangsir", "poush", "magh", "falgun", "chaitra"))) # filter out records after 2077 Ashoj (because no records were compiled)

kalimati_onion_ts_averagegg <- kalimati_onion_ts %>% 
  autoplot(.vars = price_average) +
  scale_color_viridis_d() +
  labs(x = "Date", y = "Average price")

kalimati_onion_ts_averagegg

# ggsave("./outputs/onion_wholesale_average_price.png", plot = kalimati_onion_ts_averagegg, device = "png", 
#          width = 6, height = 4, units = "in", dpi = 200)

cpi_nepal_2010 <- readxl::read_xlsx("./data/cpi_nepal.xlsx")

cpi_nepal_2010_prop <- cpi_nepal_2010 %>% 
  mutate(cpi_2010_100_prop = cpi_2010_100/100) %>% 
  select(-cpi_2010_100)

# obtain inflation adjusted price (real price) of onion by multiplying with CPI
# CPI value for the months of 2020 were obtained by linear interpolation
kalimati_onion_ts <- kalimati_onion_ts %>% 
  mutate(english_year = year(english_yearmon)) %>% 
  left_join(full_join(cpi_nepal_2010_prop %>% 
                        rename(english_year = year),
              lm(cpi_nepal_2010_prop, formula = cpi_2010_100_prop ~ year) %>% 
              broom::augment(newdata = add_row(cpi_nepal_2010_prop, year = 2020, cpi_2010_100_prop = NA)) %>% 
              filter(year == 2020) %>% 
              transmute(english_year = year, 
                        cpi_2010_100_prop = `.fitted`))
  ) %>% 
  mutate_at(vars(price_min:price_average), list(~.*cpi_2010_100_prop)) %>% 
  select(-cpi_2010_100_prop, -english_year)

# make a plot again
kalimati_onion_ts %>% 
  autoplot(.vars = price_average) +
  scale_color_viridis_d() +
  labs(x = "Date", y = "Average price") +
  scale_y_continuous(labels = scales::comma_format())
