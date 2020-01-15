# Read Data
library(readr)
library(robCompositions)
library(rrcov)
library(compositions)
library(MASS)
library(HiDimDA)

df <- read_delim("Statistik/coda/data/apesteeth.csv", 
                 ";", escape_double = FALSE, trim_ws = TRUE)

