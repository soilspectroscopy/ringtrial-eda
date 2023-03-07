
## Loading packages
library("tidyverse")
library("readr")
library("readxl")
library("cowplot")
library("resemble")

## Folders
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")
dir.dissimilarity <- paste0(mnt.dir, "dissimilarity/")

dir.output <- "outputs/"
dir.figures <- paste0(dir.output, "similarity/")

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

## Dissimilarity function
## Based on mahalanobis distance on PC space (99.99% cumvar), or
## Euclidean distance on original column-centered matrix

sim_fun <- function(df1, df2) {
  
  # all.mirspectra.raw <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))
  # df1 <- all.mirspectra.raw %>% filter(organization == "Agrocares") %>% select(-organization)
  # df2 <- all.mirspectra.raw %>% filter(organization == "KSSL") %>% select(-organization)
  
  df1.ids <- df1 %>% select(sample_id) %>% pull(sample_id)
  # set.seed(1993)
  # df1.ids <- sort(sample(df1.ids, 5))
  
  test <- df1 %>%
    filter(sample_id %in% df1.ids) %>%
    arrange(sample_id) %>%
    select(-sample_id) %>%
    as.matrix()
  
  reference <- df2 %>%
    filter(sample_id %in% df1.ids) %>%
    arrange(sample_id) %>%
    select(-sample_id) %>%
    as.matrix()
  
  # dissim <- dissimilarity(test, reference, diss_method = "pca",
  #                         pc_selection = list(method = "cumvar", value = 0.9999),
  #                         center = T, scale = T)
  
  dissim <- dissimilarity(test, reference, diss_method = "euclid",
                          center = T, scale = T)
    
  dissim <- diag(dissim$dissimilarity)
  
  tibble(reference = 16,
         sample_id = df1.ids,
         distance = dissim)
  
}


## allMIRspectra_raw

all.mirspectra.raw <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))

all.mirspectra.raw <- all.mirspectra.raw %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.raw

# Spectral dissimilarity with reference instrument
all.mirspectra.raw.reference <- all.mirspectra.raw %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.raw.dissim <- all.mirspectra.raw %>%
  nest(data1 = -organization) %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.raw.reference)) %>%
  mutate(similarity = map2(.x = data1, .y = data2, .f = ~sim_fun(.x, .y))) %>%
  select(organization, similarity) %>%
  unnest(similarity)

all.mirspectra.raw.dissim

write_csv(all.mirspectra.raw.dissim, paste0(dir.dissimilarity, "dissim_euclidean_raw.csv"))

# Visualization
p.dissim <- all.mirspectra.raw.dissim %>%
  ggplot(aes(x = organization, y = distance, color = organization)) +
  geom_boxplot(size = 0.5, show.legend = F) +
  labs(x = bquote("Instrument"), y = "Distance",
       title = "Dissimilarity with reference instrument (16) - raw spectra", color = "") +
  theme_light() + ylim(0,4) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)); p.dissim

ggsave(paste0(dir.figures, paste0("plot_dissimilarity_raw.png")),
       p.dissim, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra_BOC

all.mirspectra.BOC <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_BOC.csv"))

all.mirspectra.BOC <- all.mirspectra.BOC %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.BOC

# Spectral dissimilarity reference instrument
all.mirspectra.BOC.reference <- all.mirspectra.BOC %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.BOC.dissim <- all.mirspectra.BOC %>%
  nest(data1 = -organization) %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.BOC.reference)) %>%
  mutate(similarity = map2(.x = data1, .y = data2, .f = ~sim_fun(.x, .y))) %>%
  select(organization, similarity) %>%
  unnest(similarity)

all.mirspectra.BOC.dissim

write_csv(all.mirspectra.BOC.dissim, paste0(dir.dissimilarity, "dissim_euclidean_BOC.csv"))

# Visualization
p.dissim <- all.mirspectra.BOC.dissim %>%
  ggplot(aes(x = organization, y = distance, color = organization)) +
  geom_boxplot(size = 0.5, show.legend = F) +
  labs(x = bquote("Instrument"), y = "Distance",
       title = "Dissimilarity with reference instrument (16) - BOC spectra", color = "") +
  theme_light() + ylim(0,4) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)); p.dissim

ggsave(paste0(dir.figures, paste0("plot_dissimilarity_BOC.png")),
       p.dissim, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra_SG1stDer

all.mirspectra.SG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SG1stDer.csv"))

all.mirspectra.SG1stDer <- all.mirspectra.SG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SG1stDer

# Spectral dissimilarity reference instrument
all.mirspectra.SG1stDer.reference <- all.mirspectra.SG1stDer %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.SG1stDer.dissim <- all.mirspectra.SG1stDer %>%
  nest(data1 = -organization) %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.SG1stDer.reference)) %>%
  mutate(similarity = map2(.x = data1, .y = data2, .f = ~sim_fun(.x, .y))) %>%
  select(organization, similarity) %>%
  unnest(similarity)

all.mirspectra.SG1stDer.dissim

write_csv(all.mirspectra.SG1stDer.dissim, paste0(dir.dissimilarity, "dissim_euclidean_SG1stDer.csv"))

# Visualization
p.dissim <- all.mirspectra.SG1stDer.dissim %>%
  ggplot(aes(x = organization, y = distance, color = organization)) +
  geom_boxplot(size = 0.5, show.legend = F) +
  labs(x = bquote("Instrument"), y = "Distance",
       title = "Dissimilarity with reference instrument (16) - SG1stDer spectra", color = "") +
  theme_light() + ylim(0,4) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)); p.dissim

ggsave(paste0(dir.figures, paste0("plot_dissimilarity_SG1stDer.png")),
       p.dissim, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra_SNV

all.mirspectra.SNV <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

all.mirspectra.SNV <- all.mirspectra.SNV %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNV

# Spectral dissimilarity reference instrument
all.mirspectra.SNV.reference <- all.mirspectra.SNV %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.SNV.dissim <- all.mirspectra.SNV %>%
  nest(data1 = -organization) %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.SNV.reference)) %>%
  mutate(similarity = map2(.x = data1, .y = data2, .f = ~sim_fun(.x, .y))) %>%
  select(organization, similarity) %>%
  unnest(similarity)

all.mirspectra.SNV.dissim

write_csv(all.mirspectra.SNV.dissim, paste0(dir.dissimilarity, "dissim_euclidean_SNV.csv"))

# Visualization
p.dissim <- all.mirspectra.SNV.dissim %>%
  ggplot(aes(x = organization, y = distance, color = organization)) +
  geom_boxplot(size = 0.5, show.legend = F) +
  labs(x = bquote("Instrument"), y = "Distance",
       title = "Dissimilarity with reference instrument (16) - SNV spectra", color = "") +
  theme_light() + ylim(0,4) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)); p.dissim

ggsave(paste0(dir.figures, paste0("plot_dissimilarity_SNV.png")),
       p.dissim, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra_SNVplusSG1stDer

all.mirspectra.SNVplusSG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNVplusSG1stDer.csv"))

all.mirspectra.SNVplusSG1stDer <- all.mirspectra.SNVplusSG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNVplusSG1stDer

# Spectral dissimilarity reference instrument
all.mirspectra.SNVplusSG1stDer.reference <- all.mirspectra.SNVplusSG1stDer %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.SNVplusSG1stDer.dissim <- all.mirspectra.SNVplusSG1stDer %>%
  nest(data1 = -organization) %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.SNVplusSG1stDer.reference)) %>%
  mutate(similarity = map2(.x = data1, .y = data2, .f = ~sim_fun(.x, .y))) %>%
  select(organization, similarity) %>%
  unnest(similarity)

all.mirspectra.SNVplusSG1stDer.dissim

write_csv(all.mirspectra.SNVplusSG1stDer.dissim, paste0(dir.dissimilarity, "dissim_euclidean_SNVplusSG1stDer.csv"))

# Visualization
p.dissim <- all.mirspectra.SNVplusSG1stDer.dissim %>%
  ggplot(aes(x = organization, y = distance, color = organization)) +
  geom_boxplot(size = 0.5, show.legend = F) +
  labs(x = bquote("Instrument"), y = "Distance",
       title = "Dissimilarity with reference instrument (16) - SNVplusSG1stDer spectra", color = "") +
  theme_light() + ylim(0,4) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)); p.dissim

ggsave(paste0(dir.figures, paste0("plot_dissimilarity_SNVplusSG1stDer.png")),
       p.dissim, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra_wavelet

all.mirspectra.wavelet <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_wavelet.csv"))

all.mirspectra.wavelet <- all.mirspectra.wavelet %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  select(organization, sample_id, starts_with("H9_"))

all.mirspectra.wavelet

# Spectral dissimilarity reference instrument
all.mirspectra.wavelet.reference <- all.mirspectra.wavelet %>%
  filter(organization == 16) %>%
  select(-organization)

all.mirspectra.wavelet.dissim <- all.mirspectra.wavelet %>%
  nest(data1 = -organization) %>%
  filter(organization != 16) %>%
  mutate(data2 = list(all.mirspectra.wavelet.reference)) %>%
  mutate(similarity = map2(.x = data1, .y = data2, .f = ~sim_fun(.x, .y))) %>%
  select(organization, similarity) %>%
  unnest(similarity)

all.mirspectra.wavelet.dissim

write_csv(all.mirspectra.wavelet.dissim, paste0(dir.dissimilarity, "dissim_euclidean_wavelet.csv"))

# Visualization
p.dissim <- all.mirspectra.wavelet.dissim %>%
  ggplot(aes(x = organization, y = distance, color = organization)) +
  geom_boxplot(size = 0.5, show.legend = F) +
  labs(x = bquote("Instrument"), y = " distance",
       title = "Dissimilarity with reference instrument (16) - wavelet spectra", color = "") +
  theme_light() + ylim(0,4) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)); p.dissim

ggsave(paste0(dir.figures, paste0("plot_dissimilarity_wavelet.png")),
       p.dissim, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## Composite plot

p1 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_dissimilarity_raw.png"), scale = 1)
p2 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_dissimilarity_BOC.png"), scale = 1)
p3 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_dissimilarity_SG1stDer.png"), scale = 1)
p4 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_dissimilarity_SNV.png"), scale = 1)
p5 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_dissimilarity_SNVplusSG1stDer.png"), scale = 1)
p6 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_dissimilarity_wavelet.png"), scale = 1)

final.plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, labels = "") +
  theme(plot.background = element_rect(fill = "white", colour = NA))

cowplot::ggsave2(paste0(dir.figures, paste0("plot_dissimilarity_allGrid.png")),
                 final.plot, dpi = 200, width = 7, height = 8, units = "in", scale = 1)


## allMIRspectra SST

all.mirspectra.SST <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SST.csv"))

all.mirspectra.SST <- all.mirspectra.SST %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SST

# Spectral dissimilarity reference instrument
all.mirspectra.SST.reference <- all.mirspectra.SST %>%
  filter(organization == 16) %>%
  select(-all_of(c("organization", "ct_subset")))

all.mirspectra.SST.dissim.beforeSST <- all.mirspectra.SST %>%
  nest(data1 = -all_of(c("organization", "ct_subset"))) %>%
  filter(organization != 16) %>%
  filter(ct_subset == "beforeSST") %>%
  mutate(data2 = list(all.mirspectra.SST.reference)) %>%
  mutate(similarity = map2(.x = data1, .y = data2, .f = ~sim_fun(.x, .y))) %>%
  select(organization, ct_subset, similarity) %>%
  unnest(similarity)

all.mirspectra.SST.dissim.afterSST <- all.mirspectra.SST %>%
  nest(data1 = -all_of(c("organization", "ct_subset"))) %>%
  filter(organization != 16) %>%
  filter(ct_subset == "afterSST") %>%
  mutate(data2 = list(all.mirspectra.SST.reference)) %>%
  mutate(similarity = map2(.x = data1, .y = data2, .f = ~sim_fun(.x, .y))) %>%
  select(organization, ct_subset, similarity) %>%
  unnest(similarity)

all.mirspectra.SST.dissim <- bind_rows(all.mirspectra.SST.dissim.beforeSST,
                                       all.mirspectra.SST.dissim.afterSST) %>%
  mutate(ct_subset = factor(ct_subset, levels = c("beforeSST", "afterSST")))

write_csv(all.mirspectra.SST.dissim.afterSST, paste0(dir.dissimilarity, "dissim_euclidean_SST.csv"))

# Visualization
p.dissim <- all.mirspectra.SST.dissim %>%
  ggplot(aes(x = organization, y = distance, color = ct_subset)) +
  geom_boxplot(size = 0.5, show.legend = F) +
  labs(x = bquote("Instrument"), y = " distance",
       title = "Dissimilarity with reference instrument (16) - SST spectra", color = "") +
  theme_light() + ylim(0,4) +
  theme(legend.position = "bottom",
        axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 0.5)); p.dissim

ggsave(paste0(dir.figures, paste0("plot_dissimilarity_SST.png")),
       p.dissim, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)
