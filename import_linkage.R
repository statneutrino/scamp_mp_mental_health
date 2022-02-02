library(tidyverse)
library(readxl)

### Load Psytools data files
psytools_bl_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/Psytools BL and FU/Psytools Baseline Wide Response 20210216.xlsx"
psytools_f1_path <- "D:/share/scamp/Data analysis/Alex/SCAMP Datasets/Psytools BL and FU/Psytools F1 Wide Response 20210216.xlsx"

psytools_bl <- read_excel(psytools_bl_path)
psytools_f1 <- read_excel(psytools_f1_path)


### Load analysis-relevant code book
codebook <- read.csv("./docs/codebook.csv")

### Select relevant column names:
colnames(psytools_bl)


