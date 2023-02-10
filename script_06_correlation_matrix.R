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
dir.figures <- paste0(dir.output, "correlation_matrix/")

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


## Correlation matrix

organizations.pairs <- as_tibble(data.frame(t(combn(codes, 2))))
organizations.pairs

mean_cor_fun <- function(df1, df2) {
  
  # df1 <- all.mirspectra.raw %>%
  #   filter(organization == organizations.pairs[[1,"X1"]])
  # 
  # df2 <- all.mirspectra.raw %>%
  #   filter(organization == organizations.pairs[[1,"X2"]])
  
  df2.std <- df2 %>%
    select(-organization) %>%
    pivot_longer(-all_of(c("sample_id")), names_to = "wavenumber", values_to = "absorbance2")
  
  df.cor <- df1 %>%
    select(-organization) %>%
    pivot_longer(-all_of(c("sample_id")), names_to = "wavenumber", values_to = "absorbance1") %>%
    left_join(df2.std, by = c("sample_id", "wavenumber"))
  
  df.cor %>%
    group_by(wavenumber) %>%
    summarise(cor = cor(absorbance1, absorbance2, method = "pearson")) %>%
    ungroup() %>%
    summarise(mean_cor = mean(cor, na.rm = T)) %>%
    mutate(X1 = unique(df1[["organization"]]),
           X2 = unique(df2[["organization"]]),
           .before = mean_cor)
  
}

## allMIRspectra raw

all.mirspectra.raw <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_raw.csv"))

all.mirspectra.raw <- all.mirspectra.raw %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.raw

result.raw.list <- list()

for(i in 1:nrow(organizations.pairs)) {
  
  df1 <- all.mirspectra.raw %>%
    filter(organization == organizations.pairs[[i,"X1"]])
  
  df2 <- all.mirspectra.raw %>%
    filter(organization == organizations.pairs[[i,"X2"]])
  
  result.raw.list[[i]] <- mean_cor_fun(df1, df2)
  
  cat(paste0("Run ", i, "/", nrow(organizations.pairs), "\n"))
  
}

mean.cor.result.raw <- Reduce(bind_rows, result.raw.list)

png(file = paste0(dir.figures, "plot_correlation_matrix_raw.png"),
    width = 10, height = 8, units = "in", res = 300)
mean.cor.result.raw %>%
  rename(var1 = X1, var2 = X2, cor = "mean_cor") %>%
  mutate(var1 = as.character(var1), var2 = as.character(var2)) %>%
  cor_spread(value = "cor") %>%
  column_to_rownames("rowname") %>%
  as.matrix() %>%
  corrplot(method = "shade", type = "upper",
           title = "Correlation between instruments - raw spectra",
           mar = c(0,0,1,0), number.cex = 0.85, number.digits = 2,
           tl.cex = 1, addCoef.col = "white", tl.col = "black", tl.srt = 45)
dev.off()


## allMIRspectra BOC

all.mirspectra.BOC <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_BOC.csv"))

all.mirspectra.BOC <- all.mirspectra.BOC %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.BOC

result.BOC.list <- list()

for(i in 1:nrow(organizations.pairs)) {
  
  df1 <- all.mirspectra.BOC %>%
    filter(organization == organizations.pairs[[i,"X1"]])
  
  df2 <- all.mirspectra.BOC %>%
    filter(organization == organizations.pairs[[i,"X2"]])
  
  result.BOC.list[[i]] <- mean_cor_fun(df1, df2)
  
  cat(paste0("Run ", i, "/", nrow(organizations.pairs), "\n"))
  
}

mean.cor.result.BOC <- Reduce(bind_rows, result.BOC.list)

png(file = paste0(dir.figures, "plot_correlation_matrix_BOC.png"),
    width = 10, height = 8, units = "in", res = 300)
mean.cor.result.BOC %>%
  rename(var1 = X1, var2 = X2, cor = "mean_cor") %>%
  mutate(var1 = as.character(var1), var2 = as.character(var2)) %>%
  cor_spread(value = "cor") %>%
  column_to_rownames("rowname") %>%
  as.matrix() %>%
  corrplot(method = "shade", type = "upper",
           title = "Correlation between instruments - BOC spectra",
           mar = c(0,0,1,0), number.cex = 0.85, number.digits = 2,
           tl.cex = 1, addCoef.col = "white", tl.col = "black", tl.srt = 45)
dev.off()


## allMIRspectra SG1stDer

all.mirspectra.SG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SG1stDer.csv"))

all.mirspectra.SG1stDer <- all.mirspectra.SG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SG1stDer

result.SG1stDer.list <- list()

for(i in 1:nrow(organizations.pairs)) {
  
  df1 <- all.mirspectra.SG1stDer %>%
    filter(organization == organizations.pairs[[i,"X1"]])
  
  df2 <- all.mirspectra.SG1stDer %>%
    filter(organization == organizations.pairs[[i,"X2"]])
  
  result.SG1stDer.list[[i]] <- mean_cor_fun(df1, df2)
  
  cat(paste0("Run ", i, "/", nrow(organizations.pairs), "\n"))
  
}

mean.cor.result.SG1stDer <- Reduce(bind_rows, result.SG1stDer.list)

png(file = paste0(dir.figures, "plot_correlation_matrix_SG1stDer.png"),
    width = 10, height = 8, units = "in", res = 300)
mean.cor.result.SG1stDer %>%
  rename(var1 = X1, var2 = X2, cor = "mean_cor") %>%
  mutate(var1 = as.character(var1), var2 = as.character(var2)) %>%
  cor_spread(value = "cor") %>%
  column_to_rownames("rowname") %>%
  as.matrix() %>%
  corrplot(method = "shade", type = "upper",
           title = "Correlation between instruments - SG1stDer spectra",
           mar = c(0,0,1,0), number.cex = 0.85, number.digits = 2,
           tl.cex = 1, addCoef.col = "white", tl.col = "black", tl.srt = 45)
dev.off()


## allMIRspectra SNV

all.mirspectra.SNV <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNV.csv"))

all.mirspectra.SNV <- all.mirspectra.SNV %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNV

result.SNV.list <- list()

for(i in 1:nrow(organizations.pairs)) {
  
  df1 <- all.mirspectra.SNV %>%
    filter(organization == organizations.pairs[[i,"X1"]])
  
  df2 <- all.mirspectra.SNV %>%
    filter(organization == organizations.pairs[[i,"X2"]])
  
  result.SNV.list[[i]] <- mean_cor_fun(df1, df2)
  
  cat(paste0("Run ", i, "/", nrow(organizations.pairs), "\n"))
  
}

mean.cor.result.SNV <- Reduce(bind_rows, result.SNV.list)

png(file = paste0(dir.figures, "plot_correlation_matrix_SNV.png"),
    width = 10, height = 8, units = "in", res = 300)
mean.cor.result.SNV %>%
  rename(var1 = X1, var2 = X2, cor = "mean_cor") %>%
  mutate(var1 = as.character(var1), var2 = as.character(var2)) %>%
  cor_spread(value = "cor") %>%
  column_to_rownames("rowname") %>%
  as.matrix() %>%
  corrplot(method = "shade", type = "upper",
           title = "Correlation between instruments - SNV spectra",
           mar = c(0,0,1,0), number.cex = 0.85, number.digits = 2,
           tl.cex = 1, addCoef.col = "white", tl.col = "black", tl.srt = 45)
dev.off()


## allMIRspectra SNVplusSG1stDer

all.mirspectra.SNVplusSG1stDer <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SNVplusSG1stDer.csv"))

all.mirspectra.SNVplusSG1stDer <- all.mirspectra.SNVplusSG1stDer %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SNVplusSG1stDer

result.SNVplusSG1stDer.list <- list()

for(i in 1:nrow(organizations.pairs)) {
  
  df1 <- all.mirspectra.SNVplusSG1stDer %>%
    filter(organization == organizations.pairs[[i,"X1"]])
  
  df2 <- all.mirspectra.SNVplusSG1stDer %>%
    filter(organization == organizations.pairs[[i,"X2"]])
  
  result.SNVplusSG1stDer.list[[i]] <- mean_cor_fun(df1, df2)
  
  cat(paste0("Run ", i, "/", nrow(organizations.pairs), "\n"))
  
}

mean.cor.result.SNVplusSG1stDer <- Reduce(bind_rows, result.SNVplusSG1stDer.list)

png(file = paste0(dir.figures, "plot_correlation_matrix_SNVplusSG1stDer.png"),
    width = 10, height = 8, units = "in", res = 300)
mean.cor.result.SNVplusSG1stDer %>%
  rename(var1 = X1, var2 = X2, cor = "mean_cor") %>%
  mutate(var1 = as.character(var1), var2 = as.character(var2)) %>%
  cor_spread(value = "cor") %>%
  column_to_rownames("rowname") %>%
  as.matrix() %>%
  corrplot(method = "shade", type = "upper",
           title = "Correlation between instruments - SNVplusSG1stDer spectra",
           mar = c(0,0,1,0), number.cex = 0.85, number.digits = 2,
           tl.cex = 1, addCoef.col = "white", tl.col = "black", tl.srt = 45)
dev.off()


## allMIRspectra wavelet

all.mirspectra.wavelet <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_wavelet.csv"))

all.mirspectra.wavelet <- all.mirspectra.wavelet %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes))) %>%
  select(organization, sample_id, starts_with("H9_"))

all.mirspectra.wavelet

result.wavelet.list <- list()

for(i in 1:nrow(organizations.pairs)) {
  
  df1 <- all.mirspectra.wavelet %>%
    filter(organization == organizations.pairs[[i,"X1"]])
  
  df2 <- all.mirspectra.wavelet %>%
    filter(organization == organizations.pairs[[i,"X2"]])
  
  result.wavelet.list[[i]] <- mean_cor_fun(df1, df2)
  
  cat(paste0("Run ", i, "/", nrow(organizations.pairs), "\n"))
  
}

mean.cor.result.wavelet <- Reduce(bind_rows, result.wavelet.list)

png(file = paste0(dir.figures, "plot_correlation_matrix_wavelet.png"),
    width = 10, height = 8, units = "in", res = 300)
mean.cor.result.wavelet %>%
  rename(var1 = X1, var2 = X2, cor = "mean_cor") %>%
  mutate(var1 = as.character(var1), var2 = as.character(var2)) %>%
  cor_spread(value = "cor") %>%
  column_to_rownames("rowname") %>%
  as.matrix() %>%
  corrplot(method = "shade", type = "upper",
           title = "Correlation between instruments - wavelet spectra",
           mar = c(0,0,1,0), number.cex = 0.85, number.digits = 2,
           tl.cex = 1, addCoef.col = "white", tl.col = "black", tl.srt = 45)
dev.off()

## Composite plot

p1 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_matrix_raw.png"), scale = 1)
p2 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_matrix_BOC.png"), scale = 1)
p3 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_matrix_SG1stDer.png"), scale = 1)
p4 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_matrix_SNV.png"), scale = 1)
p5 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_matrix_SNVplusSG1stDer.png"), scale = 1)
p6 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_matrix_wavelet.png"), scale = 1)

final.plot <- cowplot::plot_grid(p1, p2, p3, p4, p5, p6, ncol = 2, labels = "") +
  theme(plot.background = element_rect(fill = "white", colour = NA))

cowplot::ggsave2(paste0(dir.figures, paste0("plot_correlation_matrix_allGrid.png")),
                 final.plot, dpi = 200, width = 7, height = 8, units = "in", scale = 1)


## allMIRspectra SST

all.mirspectra.SST <- read_csv(paste0(dir.preprocessed, "RT_STD_allMIRspectra_SST.csv"))

all.mirspectra.SST <- all.mirspectra.SST %>%
  mutate(organization = recode(organization, !!!new_codes)) %>%
  mutate(organization = factor(organization, levels = as.character(new_codes)))

all.mirspectra.SST

# beforeSST

all.mirspectra.SST.beforeSST <- all.mirspectra.SST %>%
  filter(ct_subset == "beforeSST") %>%
  select(-ct_subset)

result.SST.list <- list()

for(i in 1:nrow(organizations.pairs)) {
  
  df1 <- all.mirspectra.SST.beforeSST %>%
    filter(organization == organizations.pairs[[i,"X1"]])
  
  df2 <- all.mirspectra.SST.beforeSST %>%
    filter(organization == organizations.pairs[[i,"X2"]])
  
  result.SST.list[[i]] <- mean_cor_fun(df1, df2)
  
  cat(paste0("Run ", i, "/", nrow(organizations.pairs), "\n"))
  
}

mean.cor.result.SST <- Reduce(bind_rows, result.SST.list)

png(file = paste0(dir.figures, "plot_correlation_matrix_beforeSST.png"),
    width = 10, height = 8, units = "in", res = 300)
mean.cor.result.SST %>%
  rename(var1 = X1, var2 = X2, cor = "mean_cor") %>%
  mutate(var1 = as.character(var1), var2 = as.character(var2)) %>%
  cor_spread(value = "cor") %>%
  column_to_rownames("rowname") %>%
  as.matrix() %>%
  corrplot(method = "shade", type = "upper",
           title = "Correlation between instruments - beforeSST spectra",
           mar = c(0,0,1,0), number.cex = 0.85, number.digits = 2,
           tl.cex = 1, addCoef.col = "white", tl.col = "black", tl.srt = 45)
dev.off()

# afterSST

all.mirspectra.SST.afterSST <- all.mirspectra.SST %>%
  filter(ct_subset == "afterSST") %>%
  select(-ct_subset) %>%
  bind_rows({all.mirspectra.SST.beforeSST %>%
      filter(organization == 16)})

result.SST.list <- list()

for(i in 1:nrow(organizations.pairs)) {
  
  df1 <- all.mirspectra.SST.afterSST %>%
    filter(organization == organizations.pairs[[i,"X1"]])
  
  df2 <- all.mirspectra.SST.afterSST %>%
    filter(organization == organizations.pairs[[i,"X2"]])
  
  result.SST.list[[i]] <- mean_cor_fun(df1, df2)
  
  cat(paste0("Run ", i, "/", nrow(organizations.pairs), "\n"))
  
}

mean.cor.result.SST <- Reduce(bind_rows, result.SST.list)

png(file = paste0(dir.figures, "plot_correlation_matrix_afterSST.png"),
    width = 10, height = 8, units = "in", res = 300)
mean.cor.result.SST %>%
  rename(var1 = X1, var2 = X2, cor = "mean_cor") %>%
  mutate(var1 = as.character(var1), var2 = as.character(var2)) %>%
  cor_spread(value = "cor") %>%
  column_to_rownames("rowname") %>%
  as.matrix() %>%
  corrplot(method = "shade", type = "upper",
           title = "Correlation between instruments - afterSST spectra",
           mar = c(0,0,1,0), number.cex = 0.85, number.digits = 2,
           tl.cex = 1, addCoef.col = "white", tl.col = "black", tl.srt = 45)
dev.off()

## Composite plot

p1 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_matrix_beforeSST.png"), scale = 1)
p2 <- cowplot::ggdraw() + cowplot::draw_image(paste0(dir.figures, "plot_correlation_matrix_afterSST.png"), scale = 1)

final.plot <- cowplot::plot_grid(p1, p2, ncol = 1, labels = "") +
  theme(plot.background = element_rect(fill = "white", colour = NA))

cowplot::ggsave2(paste0(dir.figures, paste0("plot_correlation_matrix_SST.png")),
                 final.plot, dpi = 300, width = 5, height = 8, units = "in", scale = 1)
