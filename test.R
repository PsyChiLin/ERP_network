## load (and install) packages and functions
library(pacman)
pacman::p_load(ERP, mnormt, fdrtool, tidyverse, gridExtra, crayon,
               boot, reshape2, ggthemes, devtools)
# install gganimate from github
# devtools::install_github("dgrtwo/gganimate")
require(gganimate)

dta <- read.csv("Data/Impulsivity.csv")
# coerce score to be numeric
dta$Score <- as.numeric(dta$Score)
# create a condition x group indicator
dta$Cond_Grp <- with(dta, interaction(Condition, Group),
                     drop = TRUE)
# generate time points
time_pt <- seq(-200, 1000, 2)
# indicate channels monitored in the study
erpR_coord <- rbind(c(NA, NA, "FP1", NA, "FP2", NA, NA),
                    c(NA, "F7", "F3", "FZ", "F4", "F8", NA),
                    c(NA, "FT7", "FC3", "FCZ", "FC4", "FT8", NA),
                    c("T5", "T3", "C3", "CZ", "C4", "T4", "T6"),
                    c(NA, "TP7", "CP3", "CPZ", "CP4", "TP8", NA),
                    c(NA, NA, "P3", "PZ", "P4", NA, NA),
                    c(NA, NA, "O1", "OZ", "O2", NA, NA))