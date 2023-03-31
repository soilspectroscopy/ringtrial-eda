
## Loading packages
library("tidyverse")
library("readr")
library("readxl")
library("cowplot")
library("qs")
library("plotly")

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

## allMIRspectra SNV

all.mirspectra.SNV <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

all.mirspectra.SNV <- all.mirspectra.SNV %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  mutate(prep_spectra = "SNV", .after = 1) %>%
  filter(sample_id %in% selected.ids)

all.mirspectra.SNV

## allMIRspectra SST

all.mirspectra.SST <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SST.csv"))

all.mirspectra.SST <- all.mirspectra.SST %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SST.kssl.afterSST <- all.mirspectra.SST %>%
  filter(organization == 16) %>%
  filter(ct_subset == "beforeSST") %>%
  mutate(ct_subset = "afterSST")

all.mirspectra.SST <- all.mirspectra.SST %>%
  bind_rows(all.mirspectra.SST.kssl.afterSST) %>%
  filter(ct_subset == "afterSST") %>%
  select(-ct_subset) %>%
  mutate(prep_spectra = "SST", .after = 1) %>%
  filter(sample_id %in% selected.ids)

all.mirspectra.SST

# All data

all.mirspectra <- bind_rows(all.mirspectra.raw, all.mirspectra.SNV, all.mirspectra.SST)

# Missing values because SNV and SST have a lower range due to smoothing
# raw: 650-4000 cm-1, SNV/SST: 660-3990 cm-1

p.instance.all <- all.mirspectra %>%
  pivot_longer(-all_of(c("organization", "sample_id", "prep_spectra")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  mutate(label = ifelse(organization == 16, "reference", "replicates")) %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = organization)) +
  geom_line(linewidth = 0.25, alpha = 0.5, show.legend = F) + facet_wrap(~prep_spectra, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(color = "") +
  theme_light() +
  theme(legend.position = "bottom",
        panel.grid.minor.x = element_blank(),
        panel.grid.minor.y = element_blank(),); p.instance.all

# Interactive visualization

# ggplotly({all.mirspectra.SNV %>%
#     pivot_longer(-all_of(c("organization", "sample_id", "prep_spectra")),
#                  names_to = "wavenumber", values_to = "absorbance") %>%
#     mutate(label = ifelse(organization == 16, "reference", "replicates")) %>%
#     ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = organization)) +
#     geom_line(linewidth = 0.25, alpha = 0.5, show.legend = F) +
#     scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
#                        trans = "reverse") +
#     theme_light() +
#     theme(legend.position = "bottom",
#           panel.grid.minor.x = element_blank(),
#           panel.grid.minor.y = element_blank())})

## Spectral dissimilarity

ids.sst <- qread("outputs/RT_sst_ids.qs")
ids.test <- qread("outputs/RT_test_ids.qs")

all.mirspectra.raw.dissim <- read_csv(paste0(dir.dissimilarity, "dissim_euclidean_raw.csv"))

all.mirspectra.raw.dissim <- all.mirspectra.raw.dissim %>%
  filter(sample_id %in% ids.test) %>%
  mutate(prep_spectra = "raw", .after = 1)

all.mirspectra.SNV.dissim <- read_csv(paste0(dir.dissimilarity, "dissim_euclidean_SNV.csv"))

all.mirspectra.SNV.dissim <- all.mirspectra.SNV.dissim %>%
  filter(sample_id %in% ids.test) %>%
  mutate(prep_spectra = "SNV", .after = 1)

all.mirspectra.SST.dissim <- read_csv(paste0(dir.dissimilarity, "dissim_euclidean_SST.csv"))

all.mirspectra.SST.dissim <- all.mirspectra.SST.dissim %>%
  filter(sample_id %in% ids.test) %>%
  select(-ct_subset) %>%
  mutate(prep_spectra = "SST", .after = 1)

all.dissim <- bind_rows(all.mirspectra.raw.dissim,
                        all.mirspectra.SNV.dissim,
                        all.mirspectra.SST.dissim)

p.dissim <- all.dissim %>%
  mutate(organization = as.factor(organization)) %>%
  ggplot(aes(x = organization, y = distance, group = organization)) +
  geom_boxplot(size = 0.25, show.legend = F, outlier.size = 0.25) +
  facet_wrap(~prep_spectra, ncol = 1) +
  labs(x = "Instrument", y = "Euclidean distance") +
  theme_light() + ylim(0,3) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)); p.dissim

# Interactive visualization
ggplotly({all.mirspectra.SST.dissim %>%
    mutate(organization = as.factor(organization)) %>%
    ggplot(aes(x = organization, y = distance,
               group = organization, label = sample_id)) +
    geom_boxplot(outlier.shape = NA) +
    geom_point()})


## Median decreases of dissim
all.dissim %>%
  group_by(prep_spectra) %>%
  summarise(median = median(distance))

## Median decrease % raw to SNV
(1.53/0.619-1)*100

## Median decrease % raw to SST
(1.53/0.44-1)*100

## Median decrease % raw to SST
(0.619/0.440-1)*100

## Variation of decrese SNV to SST
all.dissim %>%
  group_by(prep_spectra, organization) %>%
  summarise(median = median(distance)) %>%
  pivot_wider(names_from = "prep_spectra", values_from = "median") %>%
  select(organization, SNV, SST) %>%
  mutate(decrease_SNV_SST = (SNV/SST-1)*100)

all.dissim %>%
  group_by(prep_spectra, organization) %>%
  summarise(median = median(distance)) %>%
  pivot_wider(names_from = "prep_spectra", values_from = "median") %>%
  select(organization, SNV, SST) %>%
  mutate(decrease_SNV_SST = (SNV/SST-1)*100) %>%
  summary()

## Variation of decrese raw to SST
all.dissim %>%
  group_by(prep_spectra, organization) %>%
  summarise(median = median(distance)) %>%
  pivot_wider(names_from = "prep_spectra", values_from = "median") %>%
  select(organization, raw, SST) %>%
  mutate(decrease_raw_SST = (raw/SST-1)*100)
