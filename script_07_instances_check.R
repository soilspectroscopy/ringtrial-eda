
## Loading packages
library("tidyverse")
library("readr")
library("readxl")
library("cowplot")

## Folders
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")

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

selected.ids <- c("RT_60", "RT_18", "RT_28")

## allMIRspectra raw

all.mirspectra.raw <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))

all.mirspectra.raw <- all.mirspectra.raw %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.raw

# All instruments

p.dissim.instance.raw.all <- all.mirspectra.raw %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - raw spectra", color = "") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.dissim.instance.raw.all

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_raw_all.png")),
       p.dissim.instance.raw.all, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)

# 3 instruments: 16 - KSSL vertex, 3 - Bruker vertex, and 12 - ThermoFisher Nicolet

p.dissim.instance.raw <- all.mirspectra.raw %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  filter(organization %in% c(16, 3, 12)) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - raw spectra", color = "") +
  scale_color_manual(values = c("gold", "red", "black")) +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)); p.dissim.instance.raw

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_raw_3orgs.png")),
       p.dissim.instance.raw, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)


## allMIRspectra BOC

all.mirspectra.BOC <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_BOC.csv"))

all.mirspectra.BOC <- all.mirspectra.BOC %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.BOC

# All instruments

p.dissim.instance.BOC.all <- all.mirspectra.BOC %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - BOC spectra", color = "") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.dissim.instance.BOC.all

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_BOC_all.png")),
       p.dissim.instance.BOC.all, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)

# 3 instruments: 16 - KSSL vertex, 3 - Bruker vertex, and 12 - ThermoFisher Nicolet

p.dissim.instance.BOC <- all.mirspectra.BOC %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  filter(organization %in% c(16, 3, 12)) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - BOC spectra", color = "") +
  scale_color_manual(values = c("gold", "red", "black")) +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)); p.dissim.instance.BOC

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_BOC_3orgs.png")),
       p.dissim.instance.BOC, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)


## allMIRspectra SG1stDer

all.mirspectra.SG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SG1stDer.csv"))

all.mirspectra.SG1stDer <- all.mirspectra.SG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SG1stDer

# All instruments

p.dissim.instance.SG1stDer.all <- all.mirspectra.SG1stDer %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - SG1stDer spectra", color = "") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.dissim.instance.SG1stDer.all

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_SG1stDer_all.png")),
       p.dissim.instance.SG1stDer.all, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)

# 3 instruments: 16 - KSSL vertex, 3 - Bruker vertex, and 12 - ThermoFisher Nicolet

p.dissim.instance.SG1stDer <- all.mirspectra.SG1stDer %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  filter(organization %in% c(16, 3, 12)) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - SG1stDer spectra", color = "") +
  scale_color_manual(values = c("gold", "red", "black")) +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)); p.dissim.instance.SG1stDer

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_SG1stDer_3orgs.png")),
       p.dissim.instance.SG1stDer, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)


## allMIRspectra SNV

all.mirspectra.SNV <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

all.mirspectra.SNV <- all.mirspectra.SNV %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNV

# All instruments

p.dissim.instance.SNV.all <- all.mirspectra.SNV %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - SNV spectra", color = "") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.dissim.instance.SNV.all

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_SNV_all.png")),
       p.dissim.instance.SNV.all, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)

# 3 instruments: 16 - KSSL vertex, 3 - Bruker vertex, and 12 - ThermoFisher Nicolet

p.dissim.instance.SNV <- all.mirspectra.SNV %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  filter(organization %in% c(16, 3, 12)) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - SNV spectra", color = "") +
  scale_color_manual(values = c("gold", "red", "black")) +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)); p.dissim.instance.SNV

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_SNV_3orgs.png")),
       p.dissim.instance.SNV, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)


## allMIRspectra SNVplusSG1stDer

all.mirspectra.SNVplusSG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNVplusSG1stDer.csv"))

all.mirspectra.SNVplusSG1stDer <- all.mirspectra.SNVplusSG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNVplusSG1stDer

# All instruments

p.dissim.instance.SNVplusSG1stDer.all <- all.mirspectra.SNVplusSG1stDer %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - SNVplusSG1stDer spectra", color = "") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.dissim.instance.SNVplusSG1stDer.all

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_SNVplusSG1stDer_all.png")),
       p.dissim.instance.SNVplusSG1stDer.all, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)

# 3 instruments: 16 - KSSL vertex, 3 - Bruker vertex, and 12 - ThermoFisher Nicolet

p.dissim.instance.SNVplusSG1stDer <- all.mirspectra.SNVplusSG1stDer %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  filter(organization %in% c(16, 3, 12)) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - SNVplusSG1stDer spectra", color = "") +
  scale_color_manual(values = c("gold", "red", "black")) +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)); p.dissim.instance.SNVplusSG1stDer

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_SNVplusSG1stDer_3orgs.png")),
       p.dissim.instance.SNVplusSG1stDer, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)


## allMIRspectra wavelet

all.mirspectra.wavelet <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_wavelet.csv"))

all.mirspectra.wavelet <- all.mirspectra.wavelet %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  select(organization, sample_id, starts_with("H9_"))

all.mirspectra.wavelet

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

# All instruments

p.dissim.instance.wavelet.all <- all.mirspectra.wavelet.rescaled %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     trans = "reverse") +
  labs(title = "Instance comparison - wavelet spectra", color = "") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.dissim.instance.wavelet.all

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_wavelet_all.png")),
       p.dissim.instance.wavelet.all, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)

# 3 instruments: 16 - KSSL vertex, 3 - Bruker vertex, and 12 - ThermoFisher Nicolet

p.dissim.instance.wavelet <- all.mirspectra.wavelet.rescaled %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = c("RT_60", "RT_18", "RT_28"))) %>%
  filter(organization %in% c(16, 3, 12)) %>%
  pivot_longer(-all_of(c("organization", "sample_id")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_wrap(~sample_id, ncol = 1) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(0, 0.25, 0.5, 0.75, 1),
                     trans = "reverse") +
  labs(title = "Instance comparison - wavelet spectra", color = "") +
  scale_color_manual(values = c("gold", "red", "black")) +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)); p.dissim.instance.wavelet

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_wavelet_3orgs.png")),
       p.dissim.instance.wavelet, dpi = 300, width = 8, height = 8,
       units = "in", scale = 1)


## Composite plot

p1 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_instance_comparison_raw_all.png"), scale = 1)
p2 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_instance_comparison_BOC_all.png"), scale = 1)
p3 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_instance_comparison_SG1stDer_all.png"), scale = 1)
p4 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_instance_comparison_SNV_all.png"), scale = 1)
p5 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_instance_comparison_SNVplusSG1stDer_all.png"), scale = 1)
p6 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_instance_comparison_wavelet_all.png"), scale = 1)

final.plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, labels = "") +
  theme(plot.background = element_rect(fill = "white", colour = NA))

cowplot::ggsave2(paste0(dir.figures, paste0("plot_instance_comparison_allGrid.png")),
                 final.plot, dpi = 200, width = 5.5, height = 8, units = "in", scale = 1)

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
  bind_rows(all.mirspectra.SST.kssl.afterSST)

selected.ids <- c("RT_06", "RT_19", "RT_46")

# All instruments

p.dissim.instance.SST.all <- all.mirspectra.SST %>%
  filter(sample_id %in% selected.ids) %>%
  mutate(sample_id = factor(sample_id, levels = selected.ids)) %>%
  mutate(ct_subset = factor(ct_subset, levels = c("beforeSST", "afterSST"))) %>%
  pivot_longer(-all_of(c("organization", "sample_id", "ct_subset")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_grid(sample_id~ct_subset) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - SST spectra", color = "") +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.dissim.instance.SST.all

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_SST_all.png")),
       p.dissim.instance.SST.all, dpi = 300, width = 8, height = 6,
       units = "in", scale = 1)

# 3 instruments: 16 - KSSL vertex, 3 - Bruker vertex, and 12 - ThermoFisher Nicolet

p.dissim.instance.SST <- all.mirspectra.SST %>%
  filter(sample_id %in% selected.ids) %>%
  filter(organization %in% c(16, 3, 12)) %>%
  mutate(sample_id = factor(sample_id, levels = selected.ids)) %>%
  mutate(ct_subset = factor(ct_subset, levels = c("beforeSST", "afterSST"))) %>%
  pivot_longer(-all_of(c("organization", "sample_id", "ct_subset")),
               names_to = "wavenumber", values_to = "absorbance") %>%
  ggplot(aes(x = as.numeric(wavenumber), y = absorbance, color = organization)) +
  geom_line(size = 0.5) + facet_grid(sample_id~ct_subset) +
  labs(x = bquote(Wavenumber~(cm^-1)), y = bquote(Absorbance~(log[10]~units))) +
  scale_x_continuous(breaks = c(650, 1200, 1800, 2400, 3000, 3600, 4000),
                     trans = "reverse") +
  labs(title = "Instance comparison - SST spectra", color = "") +
  scale_color_manual(values = c("gold", "red", "black")) +
  theme_light() +
  theme(legend.position = "bottom") +
  guides(color = guide_legend(nrow = 1, byrow = TRUE)); p.dissim.instance.SST

ggsave(paste0(dir.figures, paste0("plot_instance_comparison_SST_3orgs.png")),
       p.dissim.instance.SST, dpi = 300, width = 8, height = 6,
       units = "in", scale = 1)
