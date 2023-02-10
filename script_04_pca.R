
## Loading packages
library("tidyverse")
library("tidymodels")
library("readr")
library("readxl")
library("cowplot")

## Folders
mnt.dir <- "~/projects/mnt-ringtrial/"
dir.preprocessed <- paste0(mnt.dir, "preprocessed/")

dir.output <- "outputs/"
dir.figures <- paste0(dir.output, "pca_projection/")

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


# Clean files
# do.call(file.remove, list(list.files(dir.figures, full.names = TRUE)))


## allMIRspectra raw

all.mirspectra.raw <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))

all.mirspectra.raw <- all.mirspectra.raw %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.raw

# PCA of reference set, i.e. KSSL

train.spectra <- all.mirspectra.raw %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16)

test.spectra <- all.mirspectra.raw %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16)

pca.model <- train.spectra %>%
  recipe() %>%
  update_role(everything()) %>%
  update_role(all_of(c("organization", "sample_id")), new_role = "id") %>%
  step_normalize(all_predictors(), id = "normalization") %>% # Center and scale spectra
  step_pca(all_predictors(), num_comp = 4, id = "pca") %>% # Keep first 4 comps for visualization
  prep()

pca.variance <- tidy(pca.model, id = "pca", type = "variance")

pca.variance %>%
  distinct(terms)

pca.percents <- pca.variance %>%
  filter(terms == "percent variance") %>%
  filter(component <= 2) %>%
  mutate(value = round(value, 2))

# KSSL PC space

pca.scores.train <- juice(pca.model) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .))))

p.scores <- pca.scores.train %>%
  ggplot(aes(x = PC1, y = PC2, color = organization)) +
  geom_point(size = 0.5) +
  labs(x = paste0("PC1 (", pca.percents[[1, "value"]], "%)"),
       y = paste0("PC2 (", pca.percents[[2, "value"]], "%)"),
       color = "", title = "Scores of reference instrument - raw spectra") +
  theme_light() +
  theme(legend.position = "bottom")

# Loadings

pca.loadings.train <- tidy(pca.model, id = "pca", type = "coef") %>%
  select(-id) %>%
  filter(component %in% names(pca.scores.train)) %>%
  pivot_wider(values_from = "value", names_from = "component")

p.loading <- pca.loadings.train %>%
  pivot_longer(-terms, names_to = "PC", values_to = "loading") %>%
  ggplot(aes(x = as.numeric(terms), y = loading, group = PC)) +
  geom_line(size = 0.5) +
  facet_wrap(~PC, ncol = 1) +
  labs(x = bquote("Wavenumber"~cm^-1), y = "PCA loading",
       title = "Loadings of reference instrument - raw spectra") +
  scale_x_continuous(breaks = c(600, 1200, 1800, 2400, 3000, 3600, 4000)) +
  theme_light()

# ggsave(paste0(dir.figures, paste0("plot_pca_loadings_raw_kssl.png")),
#        p.loading, dpi = 200, width = 8, height = 6,
#        units = "in", scale = 0.75)

# Project other orgs onto reference PC space

pca.scores.test <- bake(pca.model, new_data = test.spectra) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .)))) %>%
  select(-organization, -sample_id) %>%
  bind_cols({test.spectra %>%
      select(organization, sample_id)}, .)

# p.scores.woodwell <- p.scores +
#   geom_point(data = {
#     pca.scores.test %>%
#       filter(organization == 8)},
#     aes(x = PC1, y = PC2, color = organization), size = 0.5) +
#   labs(title = "Projection of Woodwell Climate vertex - raw spectra"); p.scores.woodwell
# 
# ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_raw_woodwell.png")),
#        p.scores.woodwell, dpi = 200, width = 8, height = 8,
#        units = "in", scale = 0.75)

p.scores.all <- p.scores +
  geom_point(data = pca.scores.test,
             aes(x = PC1, y = PC2, color = organization), size = 0.5) +
  scale_colour_discrete(breaks = as.character(new_codes), labels = as.character(new_codes)) +
  labs(title = "Projection of all instruments - raw spectra") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.scores.all

ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_raw_all.png")),
       p.scores.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra BOC

all.mirspectra.BOC <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_BOC.csv"))

all.mirspectra.BOC <- all.mirspectra.BOC %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.BOC

# PCA of reference set, i.e. KSSL

train.spectra <- all.mirspectra.BOC %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16)

test.spectra <- all.mirspectra.BOC %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16)

pca.model <- train.spectra %>%
  recipe() %>%
  update_role(everything()) %>%
  update_role(all_of(c("organization", "sample_id")), new_role = "id") %>%
  step_normalize(all_predictors(), id = "normalization") %>% # Center and scale spectra
  step_pca(all_predictors(), num_comp = 4, id = "pca") %>% # Keep first 4 comps for visualization
  prep()

pca.variance <- tidy(pca.model, id = "pca", type = "variance")

pca.variance %>%
  distinct(terms)

pca.percents <- pca.variance %>%
  filter(terms == "percent variance") %>%
  filter(component <= 2) %>%
  mutate(value = round(value, 2))

# KSSL PC space

pca.scores.train <- juice(pca.model) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .))))

p.scores <- pca.scores.train %>%
  ggplot(aes(x = PC1, y = PC2, color = organization)) +
  geom_point(size = 0.5) +
  labs(x = paste0("PC1 (", pca.percents[[1, "value"]], "%)"),
       y = paste0("PC2 (", pca.percents[[2, "value"]], "%)"),
       color = "", title = "Scores of reference instrument - BOC spectra") +
  theme_light() +
  theme(legend.position = "bottom")

# Loadings

pca.loadings.train <- tidy(pca.model, id = "pca", type = "coef") %>%
  select(-id) %>%
  filter(component %in% names(pca.scores.train)) %>%
  pivot_wider(values_from = "value", names_from = "component")

p.loading <- pca.loadings.train %>%
  pivot_longer(-terms, names_to = "PC", values_to = "loading") %>%
  ggplot(aes(x = as.numeric(terms), y = loading, group = PC)) +
  geom_line(size = 0.5) +
  facet_wrap(~PC, ncol = 1) +
  labs(x = bquote("Wavenumber"~cm^-1), y = "PCA loading",
       title = "Loadings of reference instrument - BOC spectra") +
  scale_x_continuous(breaks = c(600, 1200, 1800, 2400, 3000, 3600, 4000)) +
  theme_light()

# ggsave(paste0(dir.figures, paste0("plot_pca_loadings_BOC_kssl.png")),
#        p.loading, dpi = 200, width = 8, height = 6,
#        units = "in", scale = 0.75)

# Project other orgs onto reference PC space

pca.scores.test <- bake(pca.model, new_data = test.spectra) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .)))) %>%
  select(-organization, -sample_id) %>%
  bind_cols({test.spectra %>%
      select(organization, sample_id)}, .)

# p.scores.woodwell <- p.scores +
#   geom_point(data = {
#     pca.scores.test %>%
#       filter(organization == 8)},
#     aes(x = PC1, y = PC2, color = organization), size = 0.5) +
#   labs(title = "Projection of Woodwell Climate vertex - BOC spectra"); p.scores.woodwell
# 
# ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_BOC_woodwell.png")),
#        p.scores.woodwell, dpi = 200, width = 8, height = 8,
#        units = "in", scale = 0.75)

p.scores.all <- p.scores +
  geom_point(data = pca.scores.test,
             aes(x = PC1, y = PC2, color = organization), size = 0.5) +
  scale_colour_discrete(breaks = as.character(new_codes), labels = as.character(new_codes)) +
  labs(title = "Projection of all instruments - BOC spectra") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.scores.all

ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_BOC_all.png")),
       p.scores.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra SG1stDer

all.mirspectra.SG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SG1stDer.csv"))

all.mirspectra.SG1stDer <- all.mirspectra.SG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SG1stDer

# PCA of reference set, i.e. KSSL

train.spectra <- all.mirspectra.SG1stDer %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16)

test.spectra <- all.mirspectra.SG1stDer %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16)

pca.model <- train.spectra %>%
  recipe() %>%
  update_role(everything()) %>%
  update_role(all_of(c("organization", "sample_id")), new_role = "id") %>%
  step_normalize(all_predictors(), id = "normalization") %>% # Center and scale spectra
  step_pca(all_predictors(), num_comp = 4, id = "pca") %>% # Keep first 4 comps for visualization
  prep()

pca.variance <- tidy(pca.model, id = "pca", type = "variance")

pca.variance %>%
  distinct(terms)

pca.percents <- pca.variance %>%
  filter(terms == "percent variance") %>%
  filter(component <= 2) %>%
  mutate(value = round(value, 2))

# KSSL PC space

pca.scores.train <- juice(pca.model) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .))))

p.scores <- pca.scores.train %>%
  ggplot(aes(x = PC1, y = PC2, color = organization)) +
  geom_point(size = 0.5) +
  labs(x = paste0("PC1 (", pca.percents[[1, "value"]], "%)"),
       y = paste0("PC2 (", pca.percents[[2, "value"]], "%)"),
       color = "", title = "Scores of reference instrument - SG1stDer spectra") +
  theme_light() +
  theme(legend.position = "bottom")

# Loadings

pca.loadings.train <- tidy(pca.model, id = "pca", type = "coef") %>%
  select(-id) %>%
  filter(component %in% names(pca.scores.train)) %>%
  pivot_wider(values_from = "value", names_from = "component")

p.loading <- pca.loadings.train %>%
  pivot_longer(-terms, names_to = "PC", values_to = "loading") %>%
  ggplot(aes(x = as.numeric(terms), y = loading, group = PC)) +
  geom_line(size = 0.5) +
  facet_wrap(~PC, ncol = 1) +
  labs(x = bquote("Wavenumber"~cm^-1), y = "PCA loading",
       title = "Loadings of reference instrument - SG1stDer spectra") +
  scale_x_continuous(breaks = c(600, 1200, 1800, 2400, 3000, 3600, 4000)) +
  theme_light()

# ggsave(paste0(dir.figures, paste0("plot_pca_loadings_SG1stDer_kssl.png")),
#        p.loading, dpi = 200, width = 8, height = 6,
#        units = "in", scale = 0.75)

# Project other orgs onto reference PC space

pca.scores.test <- bake(pca.model, new_data = test.spectra) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .)))) %>%
  select(-organization, -sample_id) %>%
  bind_cols({test.spectra %>%
      select(organization, sample_id)}, .)

# p.scores.woodwell <- p.scores +
#   geom_point(data = {
#     pca.scores.test %>%
#       filter(organization == 8)},
#     aes(x = PC1, y = PC2, color = organization), size = 0.5) +
#   labs(title = "Projection of Woodwell Climate vertex - SG1stDer spectra"); p.scores.woodwell
# 
# ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_SG1stDer_woodwell.png")),
#        p.scores.woodwell, dpi = 200, width = 8, height = 8,
#        units = "in", scale = 0.75)

p.scores.all <- p.scores +
  geom_point(data = pca.scores.test,
             aes(x = PC1, y = PC2, color = organization), size = 0.5) +
  scale_colour_discrete(breaks = as.character(new_codes), labels = as.character(new_codes)) +
  labs(title = "Projection of all instruments - SG1stDer spectra") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.scores.all

ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_SG1stDer_all.png")),
       p.scores.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra SNV

all.mirspectra.SNV <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

all.mirspectra.SNV <- all.mirspectra.SNV %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNV

# PCA of reference set, i.e. KSSL

train.spectra <- all.mirspectra.SNV %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16)

test.spectra <- all.mirspectra.SNV %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16)

pca.model <- train.spectra %>%
  recipe() %>%
  update_role(everything()) %>%
  update_role(all_of(c("organization", "sample_id")), new_role = "id") %>%
  step_normalize(all_predictors(), id = "normalization") %>% # Center and scale spectra
  step_pca(all_predictors(), num_comp = 4, id = "pca") %>% # Keep first 4 comps for visualization
  prep()

pca.variance <- tidy(pca.model, id = "pca", type = "variance")

pca.variance %>%
  distinct(terms)

pca.percents <- pca.variance %>%
  filter(terms == "percent variance") %>%
  filter(component <= 2) %>%
  mutate(value = round(value, 2))

# KSSL PC space

pca.scores.train <- juice(pca.model) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .))))

p.scores <- pca.scores.train %>%
  ggplot(aes(x = PC1, y = PC2, color = organization)) +
  geom_point(size = 0.5) +
  labs(x = paste0("PC1 (", pca.percents[[1, "value"]], "%)"),
       y = paste0("PC2 (", pca.percents[[2, "value"]], "%)"),
       color = "", title = "Scores of reference instrument - SNV spectra") +
  theme_light() +
  theme(legend.position = "bottom")

# Loadings

pca.loadings.train <- tidy(pca.model, id = "pca", type = "coef") %>%
  select(-id) %>%
  filter(component %in% names(pca.scores.train)) %>%
  pivot_wider(values_from = "value", names_from = "component")

p.loading <- pca.loadings.train %>%
  pivot_longer(-terms, names_to = "PC", values_to = "loading") %>%
  ggplot(aes(x = as.numeric(terms), y = loading, group = PC)) +
  geom_line(size = 0.5) +
  facet_wrap(~PC, ncol = 1) +
  labs(x = bquote("Wavenumber"~cm^-1), y = "PCA loading",
       title = "Loadings of reference instrument - SNV spectra") +
  scale_x_continuous(breaks = c(600, 1200, 1800, 2400, 3000, 3600, 4000)) +
  theme_light()

# ggsave(paste0(dir.figures, paste0("plot_pca_loadings_SNV_kssl.png")),
#        p.loading, dpi = 200, width = 8, height = 6,
#        units = "in", scale = 0.75)

# Project other orgs onto reference PC space

pca.scores.test <- bake(pca.model, new_data = test.spectra) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .)))) %>%
  select(-organization, -sample_id) %>%
  bind_cols({test.spectra %>%
      select(organization, sample_id)}, .)

# p.scores.woodwell <- p.scores +
#   geom_point(data = {
#     pca.scores.test %>%
#       filter(organization == 8)},
#     aes(x = PC1, y = PC2, color = organization), size = 0.5) +
#   labs(title = "Projection of Woodwell Climate vertex - SNV spectra"); p.scores.woodwell
# 
# ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_SNV_woodwell.png")),
#        p.scores.woodwell, dpi = 200, width = 8, height = 8,
#        units = "in", scale = 0.75)

p.scores.all <- p.scores +
  geom_point(data = pca.scores.test,
             aes(x = PC1, y = PC2, color = organization), size = 0.5) +
  scale_colour_discrete(breaks = as.character(new_codes), labels = as.character(new_codes)) +
  labs(title = "Projection of all instruments - SNV spectra") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.scores.all

ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_SNV_all.png")),
       p.scores.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra SNVplusSG1stDer

all.mirspectra.SNVplusSG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNVplusSG1stDer.csv"))

all.mirspectra.SNVplusSG1stDer <- all.mirspectra.SNVplusSG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNVplusSG1stDer

# PCA of reference set, i.e. KSSL

train.spectra <- all.mirspectra.SNVplusSG1stDer %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16)

test.spectra <- all.mirspectra.SNVplusSG1stDer %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16)

pca.model <- train.spectra %>%
  recipe() %>%
  update_role(everything()) %>%
  update_role(all_of(c("organization", "sample_id")), new_role = "id") %>%
  step_normalize(all_predictors(), id = "normalization") %>% # Center and scale spectra
  step_pca(all_predictors(), num_comp = 4, id = "pca") %>% # Keep first 4 comps for visualization
  prep()

pca.variance <- tidy(pca.model, id = "pca", type = "variance")

pca.variance %>%
  distinct(terms)

pca.percents <- pca.variance %>%
  filter(terms == "percent variance") %>%
  filter(component <= 2) %>%
  mutate(value = round(value, 2))

# KSSL PC space

pca.scores.train <- juice(pca.model) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .))))

p.scores <- pca.scores.train %>%
  ggplot(aes(x = PC1, y = PC2, color = organization)) +
  geom_point(size = 0.5) +
  labs(x = paste0("PC1 (", pca.percents[[1, "value"]], "%)"),
       y = paste0("PC2 (", pca.percents[[2, "value"]], "%)"),
       color = "", title = "Scores of reference instrument - SNVplusSG1stDer spectra") +
  theme_light() +
  theme(legend.position = "bottom")

# Loadings

pca.loadings.train <- tidy(pca.model, id = "pca", type = "coef") %>%
  select(-id) %>%
  filter(component %in% names(pca.scores.train)) %>%
  pivot_wider(values_from = "value", names_from = "component")

p.loading <- pca.loadings.train %>%
  pivot_longer(-terms, names_to = "PC", values_to = "loading") %>%
  ggplot(aes(x = as.numeric(terms), y = loading, group = PC)) +
  geom_line(size = 0.5) +
  facet_wrap(~PC, ncol = 1) +
  labs(x = bquote("Wavenumber"~cm^-1), y = "PCA loading",
       title = "Loadings of reference instrument - SNVplusSG1stDer spectra") +
  scale_x_continuous(breaks = c(600, 1200, 1800, 2400, 3000, 3600, 4000)) +
  theme_light()

# ggsave(paste0(dir.figures, paste0("plot_pca_loadings_SNVplusSG1stDer_kssl.png")),
#        p.loading, dpi = 200, width = 8, height = 6,
#        units = "in", scale = 0.75)

# Project other orgs onto reference PC space

pca.scores.test <- bake(pca.model, new_data = test.spectra) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .)))) %>%
  select(-organization, -sample_id) %>%
  bind_cols({test.spectra %>%
      select(organization, sample_id)}, .)

# p.scores.woodwell <- p.scores +
#   geom_point(data = {
#     pca.scores.test %>%
#       filter(organization == 8)},
#     aes(x = PC1, y = PC2, color = organization), size = 0.5) +
#   labs(title = "Projection of Woodwell Climate vertex - SNVplusSG1stDer spectra"); p.scores.woodwell
# 
# ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_SNVplusSG1stDer_woodwell.png")),
#        p.scores.woodwell, dpi = 200, width = 8, height = 8,
#        units = "in", scale = 0.75)

p.scores.all <- p.scores +
  geom_point(data = pca.scores.test,
             aes(x = PC1, y = PC2, color = organization), size = 0.5) +
  scale_colour_discrete(breaks = as.character(new_codes), labels = as.character(new_codes)) +
  labs(title = "Projection of all instruments - SNVplusSG1stDer spectra") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.scores.all

ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_SNVplusSG1stDer_all.png")),
       p.scores.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## allMIRspectra wavelet

all.mirspectra.wavelet <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_wavelet.csv"))

all.mirspectra.wavelet <- all.mirspectra.wavelet %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  select(organization, sample_id, starts_with("H9_"))

all.mirspectra.wavelet

# PCA of reference set, i.e. KSSL

train.spectra <- all.mirspectra.wavelet %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16)

test.spectra <- all.mirspectra.wavelet %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16)

pca.model <- train.spectra %>%
  recipe() %>%
  update_role(everything()) %>%
  update_role(all_of(c("organization", "sample_id")), new_role = "id") %>%
  step_normalize(all_predictors(), id = "normalization") %>% # Center and scale spectra
  step_pca(all_predictors(), num_comp = 4, id = "pca") %>% # Keep first 4 comps for visualization
  prep()

pca.variance <- tidy(pca.model, id = "pca", type = "variance")

pca.variance %>%
  distinct(terms)

pca.percents <- pca.variance %>%
  filter(terms == "percent variance") %>%
  filter(component <= 2) %>%
  mutate(value = round(value, 2))

# KSSL PC space

pca.scores.train <- juice(pca.model) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .))))

p.scores <- pca.scores.train %>%
  ggplot(aes(x = PC1, y = PC2, color = organization)) +
  geom_point(size = 0.5) +
  labs(x = paste0("PC1 (", pca.percents[[1, "value"]], "%)"),
       y = paste0("PC2 (", pca.percents[[2, "value"]], "%)"),
       color = "", title = "Scores of reference instrument - wavelet spectra") +
  theme_light() +
  theme(legend.position = "bottom")

# Loadings

pca.loadings.train <- tidy(pca.model, id = "pca", type = "coef") %>%
  select(-id) %>%
  filter(component %in% names(pca.scores.train)) %>%
  pivot_wider(values_from = "value", names_from = "component")

# p.loading <- pca.loadings.train %>%
#   pivot_longer(-terms, names_to = "PC", values_to = "loading") %>%
#   ggplot(aes(x = as.numeric(terms), y = loading, group = PC)) +
#   geom_line(size = 0.5) +
#   facet_wrap(~PC, ncol = 1) +
#   labs(x = bquote("Wavenumber"~cm^-1), y = "PCA loading",
#        title = "Loadings of reference instrument - wavelet spectra") +
#   scale_x_continuous(breaks = c(600, 1200, 1800, 2400, 3000, 3600, 4000)) +
#   theme_light()

# ggsave(paste0(dir.figures, paste0("plot_pca_loadings_wavelet_kssl.png")),
#        p.loading, dpi = 200, width = 8, height = 6,
#        units = "in", scale = 0.75)

# Project other orgs onto reference PC space

pca.scores.test <- bake(pca.model, new_data = test.spectra) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .)))) %>%
  select(-organization, -sample_id) %>%
  bind_cols({test.spectra %>%
      select(organization, sample_id)}, .)

# p.scores.woodwell <- p.scores +
#   geom_point(data = {
#     pca.scores.test %>%
#       filter(organization == 8)},
#     aes(x = PC1, y = PC2, color = organization), size = 0.5) +
#   labs(title = "Projection of Woodwell Climate vertex - wavelet spectra"); p.scores.woodwell
# 
# ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_wavelet_woodwell.png")),
#        p.scores.woodwell, dpi = 200, width = 8, height = 8,
#        units = "in", scale = 0.75)

p.scores.all <- p.scores +
  geom_point(data = pca.scores.test,
             aes(x = PC1, y = PC2, color = organization), size = 0.5) +
  scale_colour_discrete(breaks = as.character(new_codes), labels = as.character(new_codes)) +
  labs(title = "Projection of all instruments - wavelet spectra") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.scores.all

ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_wavelet_all.png")),
       p.scores.all, dpi = 200, width = 8, height = 6,
       units = "in", scale = 1)


## Composite plot

p1 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/pca_projection/plot_pca_scores_projection_raw_all.png"), scale = 1)
p2 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/pca_projection/plot_pca_scores_projection_BOC_all.png"), scale = 1)
p3 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/pca_projection/plot_pca_scores_projection_SG1stDer_all.png"), scale = 1)
p4 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/pca_projection/plot_pca_scores_projection_SNV_all.png"), scale = 1)
p5 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/pca_projection/plot_pca_scores_projection_SNVplusSG1stDer_all.png"), scale = 1)
p6 <- cowplot::ggdraw() + cowplot::draw_image(paste0("outputs/pca_projection/plot_pca_scores_projection_wavelet_all.png"), scale = 1)

final.plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, labels = "") +
  theme(plot.background = element_rect(fill = "white", colour = NA))

cowplot::ggsave2(paste0(dir.figures, paste0("plot_pca_scores_projection_allGrid.png")),
                 final.plot, dpi = 200, width = 7, height = 8, units = "in", scale = 1)


## allMIRspectra SST

all.mirspectra.SST <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SST.csv"))

all.mirspectra.SST <- all.mirspectra.SST %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SST

all.mirspectra.SNV <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

all.mirspectra.SNV <- all.mirspectra.SNV %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNV

# PCA of reference set, i.e. KSSL

train.spectra <- all.mirspectra.SNV %>%
  # filter(organization == "KSSL") %>%
  filter(organization == 16)

test.spectra <- all.mirspectra.SST %>%
  # filter(organization != "KSSL") %>%
  filter(organization != 16)

pca.model <- train.spectra %>%
  recipe() %>%
  update_role(everything()) %>%
  update_role(all_of(c("organization", "sample_id")), new_role = "id") %>%
  step_normalize(all_predictors(), id = "normalization") %>% # Center and scale spectra
  step_pca(all_predictors(), num_comp = 4, id = "pca") %>% # Keep first 4 comps for visualization
  prep()

pca.variance <- tidy(pca.model, id = "pca", type = "variance")

pca.variance %>%
  distinct(terms)

pca.percents <- pca.variance %>%
  filter(terms == "percent variance") %>%
  filter(component <= 2) %>%
  mutate(value = round(value, 2))

# KSSL PC space

pca.scores.train <- juice(pca.model) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .))))

p.scores <- pca.scores.train %>%
  ggplot(aes(x = PC1, y = PC2, color = organization)) +
  geom_point(size = 0.5) +
  labs(x = paste0("PC1 (", pca.percents[[1, "value"]], "%)"),
       y = paste0("PC2 (", pca.percents[[2, "value"]], "%)"),
       color = "", title = "Scores of reference instrument - SST spectra") +
  theme_light() +
  theme(legend.position = "bottom")

# Loadings

pca.loadings.train <- tidy(pca.model, id = "pca", type = "coef") %>%
  select(-id) %>%
  filter(component %in% names(pca.scores.train)) %>%
  pivot_wider(values_from = "value", names_from = "component")

# p.loading <- pca.loadings.train %>%
#   pivot_longer(-terms, names_to = "PC", values_to = "loading") %>%
#   ggplot(aes(x = as.numeric(terms), y = loading, group = PC)) +
#   geom_line(size = 0.5) +
#   facet_wrap(~PC, ncol = 1) +
#   labs(x = bquote("Wavenumber"~cm^-1), y = "PCA loading",
#        title = "Loadings of reference instrument - SST spectra") +
#   scale_x_continuous(breaks = c(600, 1200, 1800, 2400, 3000, 3600, 4000)) +
#   theme_light()

# ggsave(paste0(dir.figures, paste0("plot_pca_loadings_SST_kssl.png")),
#        p.loading, dpi = 200, width = 8, height = 6,
#        units = "in", scale = 0.75)

# Project other orgs onto reference PC space

pca.scores.test <- bake(pca.model, new_data = test.spectra) %>%
  rename_at(vars(starts_with("PC")), ~paste0("PC", as.numeric(gsub("PC", "", .)))) %>%
  select(-organization, -sample_id) %>%
  bind_cols({test.spectra %>%
      select(organization, sample_id, ct_subset)}, .) %>%
  mutate(ct_subset = factor(ct_subset, levels = c("beforeSST", "afterSST")))

# p.scores.woodwell <- p.scores +
#   geom_point(data = {
#     pca.scores.test %>%
#       filter(organization == 8)},
#     aes(x = PC1, y = PC2, color = organization), size = 0.5) +
#   labs(title = "Projection of Woodwell Climate vertex - SST spectra"); p.scores.woodwell
# 
# ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_SST_woodwell.png")),
#        p.scores.woodwell, dpi = 200, width = 8, height = 8,
#        units = "in", scale = 0.75)

p.scores.all <- p.scores +
  geom_point(data = pca.scores.test,
             aes(x = PC1, y = PC2, color = organization), size = 0.5) +
  scale_colour_discrete(breaks = as.character(new_codes), labels = as.character(new_codes)) +
  facet_wrap(~ct_subset, ncol = 1) +
  labs(title = "Projection of all instruments - SST") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)); p.scores.all

ggsave(paste0(dir.figures, paste0("plot_pca_scores_projection_SST_all.png")),
       p.scores.all, dpi = 300, width = 6, height = 8,
       units = "in", scale = 1)
