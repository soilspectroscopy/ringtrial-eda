
## Loading packages
library("tidyverse")
library("readr")
library("readxl")
library("cowplot")
library("qs")

## Folders
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")
dir.dissimilarity <- paste0(mnt.dir, "dissimilarity/")

dir.output <- "outputs/"
dir.figures <- paste0(dir.output, "instance_spectra/")

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


## Spectral visualization

selected.ids <- c("RT_19")

## allMIRspectra raw

all.mirspectra.raw <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))

all.mirspectra.raw <- all.mirspectra.raw %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  mutate(prep_spectra = "raw", .after = 1) %>%
  filter(sample_id %in% selected.ids)

all.mirspectra.raw

## allMIRspectra BOC

all.mirspectra.BOC <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_BOC.csv"))

all.mirspectra.BOC <- all.mirspectra.BOC %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  mutate(prep_spectra = "BOC", .after = 1) %>%
  filter(sample_id %in% selected.ids)

all.mirspectra.BOC

## allMIRspectra SG1stDer

all.mirspectra.SG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SG1stDer.csv"))

all.mirspectra.SG1stDer <- all.mirspectra.SG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  mutate(prep_spectra = "SG1stDer", .after = 1) %>%
  filter(sample_id %in% selected.ids)

all.mirspectra.SG1stDer

## allMIRspectra SNVplusSG1stDer

all.mirspectra.SNVplusSG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNVplusSG1stDer.csv"))

all.mirspectra.SNVplusSG1stDer <- all.mirspectra.SNVplusSG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  mutate(prep_spectra = "SNVplusSG1stDer", .after = 1) %>%
  filter(sample_id %in% selected.ids)

all.mirspectra.SNVplusSG1stDer

# All data

all.mirspectra <- bind_rows(all.mirspectra.raw, all.mirspectra.BOC,
                            all.mirspectra.SG1stDer, all.mirspectra.SNVplusSG1stDer)

# Missing values warning because BOC and SG1stDer have a lower range due to smoothing
# raw: 650-4000 cm-1, BOC/SG1stDer: 660-3990 cm-1

p.instance.all <- all.mirspectra %>%
  mutate(prep_spectra = factor(prep_spectra, levels = c("raw", "BOC", "SG1stDer", "SNVplusSG1stDer"))) %>%
  pivot_longer(-all_of(c("organization", "sample_id", "prep_spectra")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  mutate(label = ifelse(organization == 16, "reference", "replicates")) %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = organization)) +
  geom_line(linewidth = 0.25, alpha = 0.5, show.legend = F) +
  facet_wrap(~prep_spectra, ncol = 1, scale = "free_y") +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(color = "") +
  theme_light() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),); p.instance.all


## Spectral dissimilarity

ids.SG1stDer <- qread("outputs/RT_SG1stDer_ids.qs")
ids.test <- qread("outputs/RT_test_ids.qs")

all.mirspectra.raw.dissim <- read_csv(paste0(dir.dissimilarity, "dissim_euclidean_raw.csv"))

all.mirspectra.raw.dissim <- all.mirspectra.raw.dissim %>%
  filter(sample_id %in% ids.test) %>%
  mutate(prep_spectra = "raw", .after = 1)

all.mirspectra.BOC.dissim <- read_csv(paste0(dir.dissimilarity, "dissim_euclidean_BOC.csv"))

all.mirspectra.BOC.dissim <- all.mirspectra.BOC.dissim %>%
  filter(sample_id %in% ids.test) %>%
  mutate(prep_spectra = "BOC", .after = 1)

all.mirspectra.SG1stDer.dissim <- read_csv(paste0(dir.dissimilarity, "dissim_euclidean_SG1stDer.csv"))

all.mirspectra.SG1stDer.dissim <- all.mirspectra.SG1stDer.dissim %>%
  filter(sample_id %in% ids.test) %>%
  mutate(prep_spectra = "SG1stDer", .after = 1)

all.mirspectra.SNVplusSG1stDer.dissim <- read_csv(paste0(dir.dissimilarity, "dissim_euclidean_SNVplusSG1stDer.csv"))

all.mirspectra.SNVplusSG1stDer.dissim <- all.mirspectra.SNVplusSG1stDer.dissim %>%
  filter(sample_id %in% ids.test) %>%
  mutate(prep_spectra = "SNVplusSG1stDer", .after = 1)

all.dissim <- bind_rows(all.mirspectra.raw.dissim,
                        all.mirspectra.BOC.dissim,
                        all.mirspectra.SG1stDer.dissim,
                        all.mirspectra.SNVplusSG1stDer.dissim)

p.dissim <- all.dissim %>%
  mutate(prep_spectra = factor(prep_spectra, levels = c("raw", "BOC", "SG1stDer", "SNVplusSG1stDer"))) %>%
  mutate(organization = as.factor(organization)) %>%
  ggplot(aes(x = organization, y = distance, group = organization)) +
  geom_boxplot(size = 0.25, show.legend = F, outlier.size = 0.25) +
  facet_wrap(~prep_spectra, ncol = 1) +
  labs(x = "Instrument", y = "Euclidean distance") +
  theme_light() + ylim(0,3) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)); p.dissim

## Plotting together

# Without facet labels
p.together <- plot_grid(p.instance.all, p.dissim, ncol = 2, labels = "")
p.together

ggsave("outputs/plot_paper_spectra_and_dissim_supplement.png", p.together,
       width = 7, height = 7, dpi = 300, scale = 1.5, units = "in")
