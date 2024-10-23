library(dplyr)
library(tidyr)

#read data

data_max_consec <- read.csv("../drought_code_yvr/data/combine_max_consec.csv")
data_avgexc <- read.csv("../drought_code_yvr/data/combined_avgexc.csv")
data_EHF <- read.csv("../output/df_ehf_all.csv")

ehf_year_max <- data_EHF %>%
  group_by(Station, LOCAL_YEAR) %>%
  summarise(max_year_EHF95 = max(max_EHF_95, na.rm = TRUE))


ehf_ymax_wide <- ehf_year_max %>%
  select(Station, LOCAL_YEAR, max_year_EHF95) %>% # Select relevant columns
  pivot_wider(names_from = Station, values_from = max_year_EHF95) %>% # Reshape the data
  rename(year = LOCAL_YEAR,
         ab = Abbotsford, ke = Kelowna, pg = Prince_George, yv = YVR,
         fn = FortNelson) %>%
  arrange(year) %>%
  filter(year >= 1940 & year <= 2024)



avgexc_year_max <- data_avgexc %>%
  group_by(station, year) %>%
  summarise(ymax_avgexc = max(avgexc, na.rm = TRUE))


avgexc_ymax_wide <- avgexc_year_max %>%
  select(station, year, ymax_avgexc) %>% # Select relevant columns
  pivot_wider(names_from = station, values_from = ymax_avgexc) %>% # Reshape the data
  rename(year = year,
         ab = Abbotsford, ke = Kelowna, pg = Prince_George, yv = YVR,
         fn = FortNelson) %>%
  arrange(year) %>%
  filter(year >= 1940 & year <= 2024) %>%
  replace(is.na(.), 0)

#the na of avgexc_ymax_wide is replaced by 0


 