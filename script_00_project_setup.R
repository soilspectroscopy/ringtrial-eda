
## Loading packages
library("tidyverse")

## Creating input/output dirs
if(!dir.exists("outputs")){dir.create("outputs")}

## Mounted disck for storing big files
mnt.dir <- "~/projects/mnt-ringtrial/"

## Creating check_spectra dir
if(!dir.exists("outputs/check_spectra")){dir.create("outputs/check_spectra")}

## Creating pca_projection dir
if(!dir.exists("outputs/pca_projection")){dir.create("outputs/pca_projection")}

## Creating similarity dir
if(!dir.exists("outputs/similarity")){dir.create("outputs/similarity")}

## Creating instances dir
if(!dir.exists("outputs/instances")){dir.create("outputs/instances")}

## Creating check_spectra dir
if(!dir.exists(paste0(mnt.dir, "dissimilarity"))){dir.create(paste0(mnt.dir, "dissimilarity"))}
