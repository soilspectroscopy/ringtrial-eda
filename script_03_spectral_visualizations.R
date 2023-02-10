
## Loading packages
library("tidyverse")
library("readr")
library("readxl")
library("cowplot")

## Folders
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")

dir.output <- "outputs/"
dir.figures <- paste0(dir.output, "check_spectra/")

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

# Clean old figures
do.call(file.remove, list(list.files(dir.figures, full.names = TRUE)))

## Plotting raw
all.mirspectra.raw <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))

all.mirspectra.raw <- all.mirspectra.raw %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.raw

for(i in 1:length(organizations)) {
  
  iorganization <- organizations[i]
  
  p.export <- all.mirspectra.raw %>%
    filter(organization == iorganization) %>%
    pivot_longer(-all_of(c("organization", "sample_id")), names_to = "wavenumber", values_to = "absorbance") %>%
    ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = sample_id)) +
    labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
    scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                       trans = "reverse") +
    geom_line(alpha = 0.25) +
    labs(title = paste0("MIR return for instrument ", iorganization, ", no preprocessing")) +
    theme_light()
  
  ggsave(paste0(dir.figures, paste0("plot_mir_instrument", iorganization, "_raw.png")),
         p.export, dpi = 200, width = 8, height = 6,
         units = "in", scale = 1)
  
  cat("Exported plot for", iorganization, "\n")
  
}

## Plotting BOC
all.mirspectra.BOC <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_BOC.csv"))

all.mirspectra.BOC <- all.mirspectra.BOC %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.BOC

for(i in 1:length(organizations)) {
  
  iorganization <- organizations[i]
  
  p.export <- all.mirspectra.BOC %>%
    filter(organization == iorganization) %>%
    pivot_longer(-all_of(c("organization", "sample_id")), names_to = "wavenumber", values_to = "absorbance") %>%
    ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = sample_id)) +
    labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
    scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                       trans = "reverse") +
    geom_line(alpha = 0.25) +
    labs(title = paste0("MIR return for instrument ", iorganization, ", BOC preprocessing")) +
    theme_light()
  
  ggsave(paste0(dir.figures, paste0("plot_mir_instrument", iorganization, "_BOC.png")),
         p.export, dpi = 200, width = 8, height = 6,
         units = "in", scale = 1)
  
  cat("Exported plot for", iorganization, "\n")
  
}


## Plotting SG1stDer
all.mirspectra.SG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SG1stDer.csv"))

all.mirspectra.SG1stDer <- all.mirspectra.SG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SG1stDer

for(i in 1:length(organizations)) {
  
  iorganization <- organizations[i]
  
  p.export <- all.mirspectra.SG1stDer %>%
    filter(organization == iorganization) %>%
    pivot_longer(-all_of(c("organization", "sample_id")), names_to = "wavenumber", values_to = "absorbance") %>%
    ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = sample_id)) +
    labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
    scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                       trans = "reverse") +
    geom_line(alpha = 0.25) +
    labs(title = paste0("MIR return for instrument ", iorganization, ", SG1stDer preprocessing")) +
    theme_light()
  
  ggsave(paste0(dir.figures, paste0("plot_mir_instrument", iorganization, "_SG1stDer.png")),
         p.export, dpi = 200, width = 8, height = 6,
         units = "in", scale = 1)
  
  cat("Exported plot for", iorganization, "\n")
  
}


## Plotting SNV
all.mirspectra.SNV <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

all.mirspectra.SNV <- all.mirspectra.SNV %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNV

for(i in 1:length(organizations)) {
  
  iorganization <- organizations[i]
  
  p.export <- all.mirspectra.SNV %>%
    filter(organization == iorganization) %>%
    pivot_longer(-all_of(c("organization", "sample_id")), names_to = "wavenumber", values_to = "absorbance") %>%
    ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = sample_id)) +
    labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
    scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                       trans = "reverse") +
    geom_line(alpha = 0.25) +
    labs(title = paste0("MIR return for instrument ", iorganization, ", SNV preprocessing")) +
    theme_light()
  
  ggsave(paste0(dir.figures, paste0("plot_mir_instrument", iorganization, "_SNV.png")),
         p.export, dpi = 200, width = 8, height = 6,
         units = "in", scale = 1)
  
  cat("Exported plot for", iorganization, "\n")
  
}


## Plotting SNVplusSG1stDer
all.mirspectra.SNVplusSG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNVplusSG1stDer.csv"))

all.mirspectra.SNVplusSG1stDer <- all.mirspectra.SNVplusSG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNVplusSG1stDer

for(i in 1:length(organizations)) {
  
  iorganization <- organizations[i]
  
  p.export <- all.mirspectra.SNVplusSG1stDer %>%
    filter(organization == iorganization) %>%
    pivot_longer(-all_of(c("organization", "sample_id")), names_to = "wavenumber", values_to = "absorbance") %>%
    ggplot(aes(x = as.numeric(wavenumber), y = absorbance, group = sample_id)) +
    labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
    scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                       trans = "reverse") +
    geom_line(alpha = 0.25) +
    labs(title = paste0("MIR return for instrument ", iorganization, ", SNVplusSG1stDer preprocessing")) +
    theme_light()
  
  ggsave(paste0(dir.figures, paste0("plot_mir_instrument", iorganization, "_SNVplusSG1stDer.png")),
         p.export, dpi = 200, width = 8, height = 6,
         units = "in", scale = 1)
  
  cat("Exported plot for", iorganization, "\n")
  
}


## Plotting wavelet
all.mirspectra.wavelet <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_wavelet.csv"))

all.mirspectra.wavelet <- all.mirspectra.wavelet %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  select(organization, sample_id, starts_with("H9_"))

all.mirspectra.wavelet

for(i in 1:length(organizations)) {
  
  iorganization <- organizations[i]
  
  plot.data.rescaled <- all.mirspectra.wavelet %>%
    filter(organization == iorganization) %>%
    pivot_longer(-all_of(c("organization", "sample_id")),
                 names_to = "haar_levels", values_to = "value") %>%
    separate(haar_levels, into = c("trend", "flux"), sep = "_") %>%
    mutate(trend = factor(trend, levels = paste0("H", seq(0, 11, 1)))) %>%
    mutate(flux = as.numeric(gsub("I", "", flux))) %>%
    group_by(sample_id, trend) %>%
    mutate(flux_scaled = flux/max(flux))
  
  p.export <- ggplot(plot.data.rescaled) +
    geom_line(aes(x = flux_scaled, y = value, group = sample_id), show.legend = F, alpha = 0.25) +
    geom_point(aes(x = flux_scaled, y = value, group = sample_id), show.legend = F, size = 0.1, alpha = 0.25) +
    labs(x = "Relative index", y = "Intensity") +
    scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                       trans = "reverse") +
    labs(title = paste0("MIR return for instrument ", iorganization, ", wavelet preprocessing (Haar level 9, n features = 256)")) +
    theme_light()
  
  ggsave(paste0(dir.figures, paste0("plot_mir_instrument", iorganization, "_wavelet.png")),
         p.export, dpi = 200, width = 8, height = 6,
         units = "in", scale = 1)
  
  cat("Exported plot for", iorganization, "\n")
  
}

## Composite plot

for(i in 1:length(organizations)) {
  
  iorganization <- organizations[i]
  
  icode <- codes[i]
  
  p1 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/check_spectra/plot_mir_instrument", icode, "_raw.png"), scale = 1)
  p2 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/check_spectra/plot_mir_instrument", icode, "_BOC.png"), scale = 1)
  p3 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/check_spectra/plot_mir_instrument", icode, "_SG1stDer.png"), scale = 1)
  p4 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/check_spectra/plot_mir_instrument", icode, "_SNV.png"), scale = 1)
  p5 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/check_spectra/plot_mir_instrument", icode, "_SNVplusSG1stDer.png"), scale = 1)
  p6 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/check_spectra/plot_mir_instrument", icode, "_wavelet.png"), scale = 1)
  
  final.plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, labels = "") +
    theme(plot.background = element_rect(fill = "white", colour = NA))
  
  cowplot::ggsave2(paste0(dir.figures, paste0("plot_mir_instrument", icode, "_allGrid.png")),
                   final.plot, dpi = 200, width = 7, height = 8, units = "in", scale = 1)
  
  cat("Exported plot for", iorganization, "\n")
  
}
