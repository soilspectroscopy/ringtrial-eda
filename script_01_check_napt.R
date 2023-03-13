
# Packages
library("tidyverse")
library("readxl")

# Mounted disk for storing big files
mnt.dir <- "~/projects/mnt-ringtrial/"

# Data
comparison <- read_xlsx(paste0(mnt.dir, "I2022USNL036_chem_data.xlsx"))
comparison

names(comparison)

metrics <- comparison %>%
  rename(TC_KSSL = `Total Carbon (%)`,
         TC_NAPT = `NAPT TC median`,
         OC_KSSL = `Estimated Organic Carbon (%)`,
         OC_NAPT = `NAPT OC median`) %>%
  summarise(TC_rsq = yardstick::rsq_vec(TC_KSSL, TC_NAPT),
            TC_rmse = yardstick::rmse_vec(TC_KSSL, TC_NAPT),
            OC_rsq = yardstick::rsq_vec(OC_KSSL, OC_NAPT),
            OC_rmse = yardstick::rmse_vec(OC_KSSL, OC_NAPT))

metrics

metrics <- metrics %>%
  mutate(across(c(TC_rsq, OC_rsq), function(x) ifelse(x >= 0.99, 0.99, x)))

metrics

metrics.plot <- metrics %>%
  pivot_longer(everything(), names_to = "soil_property", values_to = "value") %>%
  separate(soil_property, into = c("soil_property", "metric")) %>%
  pivot_wider(names_from = "metric", values_from = "value") %>%
  mutate(soil_property = recode(soil_property, "TC" = "TC (%)", "OC" = "OC (%)")) %>%
  mutate(rsq = round(rsq, 2),
         rmse = round(rmse, 2))

metrics.plot

p.comparison <- comparison %>%
  rename(sample_id = `NAPT Sample #`, 
         TC_KSSL = `Total Carbon (%)`,
         TC_NAPT = `NAPT TC median`,
         OC_KSSL = `Estimated Organic Carbon (%)`,
         OC_NAPT = `NAPT OC median`) %>%
  select(sample_id, TC_KSSL, TC_NAPT, OC_KSSL, OC_NAPT) %>%
  pivot_longer(-sample_id, names_to = "soil_property", values_to = "value") %>%
  separate(soil_property, into = c("soil_property", "source"), sep = "_") %>%
  pivot_wider(names_from = "source", values_from = "value") %>%
  mutate(soil_property = recode(soil_property, "TC" = "TC (%)", "OC" = "OC (%)")) %>%
  ggplot() +
  geom_point(aes(x = NAPT, y = KSSL)) +
  geom_abline(intercept = 0, slope = 1) +
  geom_text(data = metrics.plot, aes(x = -Inf, y = +Inf, label = paste0('~R^2>=', rsq, '')), parse = T, hjust = -0.25, vjust = 2, size = 4) +
  geom_text(data = metrics.plot, aes(x = -Inf, y = +Inf, label = paste0('~RMSE==', rmse, "*\'%\'")), parse = T, hjust = 0, vjust = 4, size = 4) +
  labs(x = "Reference (%, from NAPT)",
       y = "Estimated (%, from KSSL)") +
  facet_wrap(~soil_property, ncol = 2) +
  theme_light()

p.comparison

ggsave("outputs/plot_comparison_NAPT_KSSL.png",
       p.comparison, dpi = 300, width = 8, height = 4.5,
       units = "in", scale = 1)
