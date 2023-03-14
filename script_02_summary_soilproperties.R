
# Packages
library("tidyverse")
library("moments")

# Mounted disk for storing big files
mnt.dir <- "~/projects/mnt-ringtrial/"

soil <- read_csv(paste0(mnt.dir, "preprocessed/RT_wetchem_soildata.csv"))

summary.stats <- soil %>%
  select(source, clay_perc, pH_H20, carbon_org_perc, potassium_cmolkg) %>%
  rename_with(~str_replace_all(., pattern = "_", replacement = ".")) %>%
  group_by(source) %>%
  summarise(across(everything(),
                   .f = list(n = function(x, na.rm = T) {sum(!is.na(x), na.rm = na.rm)},
                             min = min, mean = mean, sd = sd,
                             median = median, iqr = IQR, max = max,
                             skewness = skewness, kurtosis = kurtosis),
            na.rm = TRUE)) %>%
  pivot_longer(-source, names_to = "statistics", values_to = "value") %>%
  separate(statistics, into = c("soil_property", "statistics"), sep = "_") %>%
  mutate(soil_property = str_replace_all(soil_property, "\\.", "_")) %>%
  pivot_wider(names_from = "statistics", values_from = "value") %>%
  bind_rows({
    soil %>%
      select(clay_perc, pH_H20, carbon_org_perc, potassium_cmolkg) %>%
      rename_with(~str_replace_all(., pattern = "_", replacement = ".")) %>%
      summarise(across(everything(),
                       .f = list(n = function(x, na.rm = T) {sum(!is.na(x), na.rm = na.rm)},
                                 min = min, mean = mean, sd = sd,
                                 median = median, iqr = IQR, max = max,
                                 skewness = skewness, kurtosis = kurtosis),
                       na.rm = TRUE)) %>%
      mutate(source = "ALL", .before = 1) %>% 
      pivot_longer(-source, names_to = "statistics", values_to = "value") %>%
      separate(statistics, into = c("soil_property", "statistics"), sep = "_") %>%
      mutate(soil_property = str_replace_all(soil_property, "\\.", "_")) %>%
      pivot_wider(names_from = "statistics", values_from = "value")
  }) %>%
  arrange(soil_property, source) %>%
  relocate(source, .after = soil_property) %>%
  mutate_if(is.numeric, round, 2)

summary.stats

summary.stats.log <- soil %>%
  select(source, carbon_org_perc, potassium_cmolkg) %>%
  mutate(carbon_org_perc = log(carbon_org_perc),
         potassium_cmolkg = log(potassium_cmolkg)) %>%
  rename_with(~str_replace_all(., pattern = "_", replacement = ".")) %>%
  group_by(source) %>%
  summarise(across(everything(),
                   .f = list(n = function(x, na.rm = T) {sum(!is.na(x), na.rm = na.rm)},
                             min = min, mean = mean, sd = sd,
                             median = median, iqr = IQR, max = max,
                             skewness = skewness, kurtosis = kurtosis),
                   na.rm = TRUE)) %>%
  pivot_longer(-source, names_to = "statistics", values_to = "value") %>%
  separate(statistics, into = c("soil_property", "statistics"), sep = "_") %>%
  mutate(soil_property = str_replace_all(soil_property, "\\.", "_")) %>%
  pivot_wider(names_from = "statistics", values_from = "value") %>%
  bind_rows({
    soil %>%
      select(carbon_org_perc, potassium_cmolkg) %>%
      mutate(carbon_org_perc = log(carbon_org_perc),
             potassium_cmolkg = log(potassium_cmolkg)) %>%
      rename_with(~str_replace_all(., pattern = "_", replacement = ".")) %>%
      summarise(across(everything(),
                       .f = list(n = function(x, na.rm = T) {sum(!is.na(x), na.rm = na.rm)},
                                 min = min, mean = mean, sd = sd,
                                 median = median, iqr = IQR, max = max,
                                 skewness = skewness, kurtosis = kurtosis),
                       na.rm = TRUE)) %>%
      mutate(source = "ALL", .before = 1) %>% 
      pivot_longer(-source, names_to = "statistics", values_to = "value") %>%
      separate(statistics, into = c("soil_property", "statistics"), sep = "_") %>%
      mutate(soil_property = str_replace_all(soil_property, "\\.", "_")) %>%
      pivot_wider(names_from = "statistics", values_from = "value")
  }) %>%
  arrange(soil_property, source) %>%
  relocate(source, .after = soil_property) %>%
  mutate_if(is.numeric, round, 2)

summary.stats.log

write_csv(summary.stats, "outputs/RT_wetchem_summary_beforeLog.csv")
write_csv(summary.stats.log, "outputs/RT_wetchem_summary_afterLog.csv")
