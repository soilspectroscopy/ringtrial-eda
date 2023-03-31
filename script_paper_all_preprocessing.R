
## Loading packages
library("tidyverse")
library("readr")
library("readxl")
library("cowplot")

## Folders
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")

dir.output <- "outputs/"
dir.figures <- paste0(dir.output, "check_spectra_paper/")

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

## Plotting raw
all.mirspectra.raw <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))

all.mirspectra.raw <- all.mirspectra.raw %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

p.export.raw <- all.mirspectra.raw %>%
  filter(organization == 8) %>%
  pivot_longer(-all_of(c("organization", "sample_id")), names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = sample_id)) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  geom_line(alpha = 0.25) +
  theme_light(); p.export.raw

## Plotting BOC
all.mirspectra.BOC <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_BOC.csv"))

all.mirspectra.BOC <- all.mirspectra.BOC %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

p.export.BOC <- all.mirspectra.BOC %>%
  filter(organization == 8) %>%
  pivot_longer(-all_of(c("organization", "sample_id")), names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = sample_id)) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  geom_line(alpha = 0.25) +
  theme_light(); p.export.BOC

## Plotting SG1stDer
all.mirspectra.SG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SG1stDer.csv"))

all.mirspectra.SG1stDer <- all.mirspectra.SG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

p.export.SG1stDer <- all.mirspectra.SG1stDer %>%
  filter(organization == 8) %>%
  pivot_longer(-all_of(c("organization", "sample_id")), names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = sample_id)) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  geom_line(alpha = 0.25) +
  theme_light(); p.export.SG1stDer

## Plotting SNV
all.mirspectra.SNV <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

all.mirspectra.SNV <- all.mirspectra.SNV %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

p.export.SNV <- all.mirspectra.SNV %>%
  filter(organization == 8) %>%
  pivot_longer(-all_of(c("organization", "sample_id")), names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = sample_id)) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  geom_line(alpha = 0.25) +
  theme_light(); p.export.SNV

## Plotting SNVplusSG1stDer
all.mirspectra.SNVplusSG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNVplusSG1stDer.csv"))

all.mirspectra.SNVplusSG1stDer <- all.mirspectra.SNVplusSG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

p.export.SNVplusSG1stDer <- all.mirspectra.SNVplusSG1stDer %>%
  filter(organization == 8) %>%
  pivot_longer(-all_of(c("organization", "sample_id")), names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = sample_id)) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  geom_line(alpha = 0.25) +
  theme_light(); p.export.SNVplusSG1stDer

## Composite plot

final.plot <- cowplot::plot_grid(p.export.raw, p.export.BOC,
                                 p.export.SG1stDer, p.export.SNV,
                                 p.export.SNVplusSG1stDer,
                                 ncol = 2, labels = "auto") +
    theme(plot.background = element_rect(fill = "white", colour = NA))
  
cowplot::ggsave2(paste0(dir.output, paste0("plot_paper_preprocessings_allGrid.png")),
                   final.plot, dpi = 300, width = 8, height = 8, units = "in", scale = 1.5)
