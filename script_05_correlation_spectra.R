
## Loading packages
library("tidyverse")
library("readxl")
library("purrr")
library("stringr")
library("prospectr")
library("rstatix")
library("corrplot")

## Folders
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")

dir.output <- "outputs/"
dir.figures <- paste0(dir.output, "correlation_spectra/")

## Reading organization codes
metadata <- read_xlsx(paste0(mnt.dir, "Spectrometers_Metadata.xlsx"), 1)

metadata <- metadata %>%
  filter(!is.na(code)) %>%
  select(code, folder_name, unique_name, country_iso)

new_codes <- metadata %>%
  pull(code)

names(new_codes) <- pull(metadata, folder_name)

organizations <- metadata %>%
  pull(folder_name)

codes <- metadata %>%
  pull(code)

## Correlation function
cor_fun <- function(df1, df2) {
  
  df2.std <- df2 %>%
    pivot_longer(-sample_id, names_to = "wavenumber", values_to = "absorbance2")
  
  df.cor <- df1 %>%
    pivot_longer(-sample_id, names_to = "wavenumber", values_to = "absorbance1") %>%
    left_join(df2.std, by = c("sample_id", "wavenumber"))
  
  df.cor %>%
    group_by(wavenumber) %>%
    summarise(cor = cor(absorbance1, absorbance2, method = "pearson")) %>%
    mutate(wavenumber = as.numeric(wavenumber)) %>%
    arrange(wavenumber)
  
}


## allMIRspectra raw

all.mirspectra.raw <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))

all.mirspectra.raw <- all.mirspectra.raw %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.raw

## Correlation with reference, i.e. KSSL

all.mirspectra.raw.kssl <- all.mirspectra.raw %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.raw.cor <- all.mirspectra.raw %>%
  nest(data1 = -organization) %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.raw.kssl)) %>%
  mutate(correlation = map2(.x = data1, .y = data2, .f = ~cor_fun(.x, .y))) %>%
  select(organization, correlation) %>%
  unnest(correlation)

# Visualization

p.cor.all <- all.mirspectra.raw.cor %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  ggplot(aes(x = wavenumber, y = cor, color = organization)) +
  geom_line(size = 0.25) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = "Pearson's correlation",
       title = "Correlation with reference instrument - raw spectra", color = "") +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.cor.all

ggsave(paste0(dir.figures, paste0("plot_correlation_spectra_raw_all.png")),
       p.cor.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra BOC

all.mirspectra.BOC <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_BOC.csv"))

all.mirspectra.BOC <- all.mirspectra.BOC %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.BOC

## Correlation with reference, i.e. KSSL

all.mirspectra.BOC.kssl <- all.mirspectra.BOC %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.BOC.cor <- all.mirspectra.BOC %>%
  nest(data1 = -organization) %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.BOC.kssl)) %>%
  mutate(correlation = map2(.x = data1, .y = data2, .f = ~cor_fun(.x, .y))) %>%
  select(organization, correlation) %>%
  unnest(correlation)

# Visualization

p.cor.all <- all.mirspectra.BOC.cor %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  ggplot(aes(x = wavenumber, y = cor, color = organization)) +
  geom_line(size = 0.25) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = "Pearson's correlation",
       title = "Correlation with reference instrument - BOC spectra", color = "") +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.cor.all

ggsave(paste0(dir.figures, paste0("plot_correlation_spectra_BOC_all.png")),
       p.cor.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra SG1stDer

all.mirspectra.SG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SG1stDer.csv"))

all.mirspectra.SG1stDer <- all.mirspectra.SG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SG1stDer

## Correlation with reference, i.e. KSSL

all.mirspectra.SG1stDer.kssl <- all.mirspectra.SG1stDer %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.SG1stDer.cor <- all.mirspectra.SG1stDer %>%
  nest(data1 = -organization) %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.SG1stDer.kssl)) %>%
  mutate(correlation = map2(.x = data1, .y = data2, .f = ~cor_fun(.x, .y))) %>%
  select(organization, correlation) %>%
  unnest(correlation)

# Visualization

p.cor.all <- all.mirspectra.SG1stDer.cor %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  ggplot(aes(x = wavenumber, y = cor, color = organization)) +
  geom_line(size = 0.25) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = "Pearson's correlation",
       title = "Correlation with reference instrument - SG1stDer spectra", color = "") +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.cor.all

ggsave(paste0(dir.figures, paste0("plot_correlation_spectra_SG1stDer_all.png")),
       p.cor.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra SNV

all.mirspectra.SNV <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

all.mirspectra.SNV <- all.mirspectra.SNV %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNV

## Correlation with reference, i.e. KSSL

all.mirspectra.SNV.kssl <- all.mirspectra.SNV %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.SNV.cor <- all.mirspectra.SNV %>%
  nest(data1 = -organization) %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.SNV.kssl)) %>%
  mutate(correlation = map2(.x = data1, .y = data2, .f = ~cor_fun(.x, .y))) %>%
  select(organization, correlation) %>%
  unnest(correlation)

# Visualization

p.cor.all <- all.mirspectra.SNV.cor %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  ggplot(aes(x = wavenumber, y = cor, color = organization)) +
  geom_line(size = 0.25) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = "Pearson's correlation",
       title = "Correlation with reference instrument - SNV spectra", color = "") +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.cor.all

ggsave(paste0(dir.figures, paste0("plot_correlation_spectra_SNV_all.png")),
       p.cor.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra SNVplusSG1stDer

all.mirspectra.SNVplusSG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNVplusSG1stDer.csv"))

all.mirspectra.SNVplusSG1stDer <- all.mirspectra.SNVplusSG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNVplusSG1stDer

## Correlation with reference, i.e. KSSL

all.mirspectra.SNVplusSG1stDer.kssl <- all.mirspectra.SNVplusSG1stDer %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.SNVplusSG1stDer.cor <- all.mirspectra.SNVplusSG1stDer %>%
  nest(data1 = -organization) %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.SNVplusSG1stDer.kssl)) %>%
  mutate(correlation = map2(.x = data1, .y = data2, .f = ~cor_fun(.x, .y))) %>%
  select(organization, correlation) %>%
  unnest(correlation)

# Visualization

p.cor.all <- all.mirspectra.SNVplusSG1stDer.cor %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  ggplot(aes(x = wavenumber, y = cor, color = organization)) +
  geom_line(size = 0.25) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = "Pearson's correlation",
       title = "Correlation with reference instrument - SNVplusSG1stDer spectra", color = "") +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.cor.all

ggsave(paste0(dir.figures, paste0("plot_correlation_spectra_SNVplusSG1stDer_all.png")),
       p.cor.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra wavelet

all.mirspectra.wavelet <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_wavelet.csv"))

all.mirspectra.wavelet <- all.mirspectra.wavelet %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  select(organization, sample_id, starts_with("H9_"))

all.mirspectra.wavelet.rescaled <- all.mirspectra.wavelet %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "haar_levels", values_to = "value") %>%
  separate(haar_levels, into = c("trend", "flux"), sep = "_") %>%
  mutate(flux = as.numeric(gsub("I", "", flux))) %>%
  group_by(sample_id, trend) %>%
  mutate(flux_scaled = round(flux/max(flux), 5)) %>%
  ungroup() %>%
  select(-trend, -flux) %>%
  pivot_wider(names_from = "flux_scaled", values_from = "value")

## Correlation with reference, i.e. KSSL

all.mirspectra.wavelet.kssl <- all.mirspectra.wavelet.rescaled %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.wavelet.cor <- all.mirspectra.wavelet.rescaled %>%
  nest(data1 = -organization) %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.wavelet.kssl)) %>%
  mutate(correlation = map2(.x = data1, .y = data2, .f = ~cor_fun(.x, .y))) %>%
  select(organization, correlation) %>%
  unnest(correlation)

# Visualization

p.cor.all <- all.mirspectra.wavelet.cor %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  ggplot(aes(x = wavenumber, y = cor, color = organization)) +
  geom_line(size = 0.25) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = "Pearson's correlation",
       title = "Correlation with reference instrument - wavelet spectra", color = "") +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     trans = "reverse") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.cor.all

ggsave(paste0(dir.figures, paste0("plot_correlation_spectra_wavelet_all.png")),
       p.cor.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## Composite plot

p1 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_spectra_raw_all.png"), scale = 1)
p2 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_spectra_BOC_all.png"), scale = 1)
p3 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_spectra_SG1stDer_all.png"), scale = 1)
p4 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_spectra_SNV_all.png"), scale = 1)
p5 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_spectra_SNVplusSG1stDer_all.png"), scale = 1)
p6 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_spectra_wavelet_all.png"), scale = 1)

final.plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, labels = "") +
  theme(plot.background = element_rect(fill = "white", colour = NA))

cowplot::ggsave2(paste0(dir.figures, paste0("plot_correlation_spectra_allGrid.png")),
                 final.plot, dpi = 200, width = 7, height = 8, units = "in", scale = 1)

