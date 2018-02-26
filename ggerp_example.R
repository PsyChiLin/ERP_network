## load (and install) packages and functions
library(pacman)
pacman::p_load(ERP, mnormt, fdrtool, tidyverse, gridExtra, crayon,
               boot, reshape2, ggthemes, devtools, ggerp, qgraph)
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
Fig01 <- plot_tete(data = dta,
                   channel = 1,
                   subject = 2,
                   uV = 6:606,
                   frames = time_pt,
                   test = 607,
                   mode = "mean",
                   scalp = TRUE,
                   curve.col = c("chartreuse4",
                                 "firebrick", "gold", "blue"),
                   coord.mat = erpR_coord,
                   ylim = c(-5, 15))
## Figure 2
# subset data
dta_c <- filter(dta, Group == "High",
                Channel %in% c("FZ", "FCZ", "CZ", "PZ")) %>%
  droplevels()
Fig02 <- plot_tete(data = dta_c,
                   channel = 1,
                   subject = 2,
                   uV = 6:606,
                   frames = time_pt,
                   test = 4,
                   mode = "raw",
                   ylim = c(-19, 19))
png(file = "Fig02.png", width = 8, height = 5, unit = 'in', res = 300)
print(Fig02.png <- Fig02 +
        theme(legend.position = c(.91, .51)))
dev.off()
## Figure 3
# subset data
dta_c <- filter(dta, Condition == "Failure", Channel == "CZ" ) %>%
  droplevels()
# seed random number generator for replication
set.seed(123)
#
Fig03 <- plot_tete(data = dta_c,
                   channel = 1,
                   subject = 2,
                   uV = 6:606,
                   frames = time_pt,
                   test = 3,
                   mode = "bootci",
                   ylim = c(-14, 16))
#
png(file = "Fig03.png", width = 8, height = 8, unit = 'in', res = 300)
print(Fig03 <- Fig03 +
        theme(legend.position = c(.9, .9)))
dev.off()
## Figure 4
# subset data
dta_c <- dta %>%
  filter(Group == "High", Channel %in% c("FZ","FCZ","CZ")) %>%
  droplevels()
#
Fig04a <- plot_fa(data = dta_c,
                  channel = 1,
                  subject = 2,
                  uV = 6:606,
                  frames = time_pt,
                  test = 4,
                  mode = "test_signal",
                  design = (~Subject + Condition),
                  design0 = (~Subject),
                  nbf = 5,
                  ylim = c(-6, 9))
Fig04a$Plot <- Fig04a$Plot +
  theme(legend.position = c(.91,.82))
#
dta_c <- dta %>%
  filter(Condition=="Failure", Channel %in% c("FZ","FCZ","CZ")) %>%
  droplevels()
#
Fig04b <- plot_fa(data = dta_c,
                  channel = 1,
                  subject = 2,
                  uV = 6:606,
                  frames = time_pt,
                  test = 3,
                  mode = "test_signal",
                  design = (~Group),
                  design0 = (~1),
                  nbf = 5,
                  ylim = c(-7, 13))
Fig04b$Plot <- Fig04b$Plot +
  theme(legend.position = c(.91, .82))
png(file = "Fig04.png", width = 8, height = 6, res = 300, unit ='in')
grid.arrange(Fig04a$Plot +
               ggtitle("A") +
               theme(plot.title = element_text(hjust = 0)),
             Fig04b$Plot +
               ggtitle("B") +
               theme(plot.title = element_text(hjust = 0)))
dev.off()
## Figure 5
# subset data
dta_c <- dta %>%
  filter(Group == "High") %>%
  droplevels()
# save test results
test_res <- plot_fa(data = dta_c,
                    channel = 1,
                    subject = 2,
                    uV = 6:606,
                    frames = time_pt,
                    test = 4,
                    mode = "test_signal",
                    design = (~Subject + Condition),
                    design0 = (~Subject),
                    nbf = 5,
                    ylim = c(-6, 13))
# GIF file for animation
Fig05 <- plot_coord(tests_rst = test_res$Test_Rst,
                    frames = time_pt,show = seq(250, 350, by = 2),
                    loop = 1,
                    interval = 0.5,
                    filename = "Fig05.gif")
## Figure 6
# subset data
dta_c <- dta %>%
  filter(Condition=="Failure", Channel %in% c("FZ","FCZ","CZ")) %>%
  droplevels()
#
Fig06a <- plot_tete(data = dta_c,
                    channel = 1,
                    subject = 2,
                    uV = 6:606,
                    frames = time_pt,
                    test = 5,
                    mode = "raw",
                    ylim = c(-9, 28))
Fig06a <- Fig06a +
  theme(legend.position = "top", legend.box = "horizontal")
Fig06b <- plot_tete(data = dta_c,
                    channel = 1,
                    subject = 2,
                    uV = 6:606,
                    frames = time_pt,
                    test = 5,
                    mode = "bootci",
                    ylim = c(-1, 1),
                    labs = list(y = "Correlation", x = "Time (ms)"))
#
Fig06c <- plot_fa(data = dta_c,
                  frames = time_pt,
                  channel = 1,
                  subject = 2,
                  uV = 6:606,
                  test = 5,
                  mode = "test",
                  design = (~Score),
                  nbf = 5,
                  ylim=c(-1, 1),
                  labs = list(y = "Correlation", x = "Time (ms)"))
#
png(file = "Fig06.png", width = 8, height = 7, unit = 'in', res = 300)
grid.arrange(Fig06a +
               ggtitle("A") +
               theme(plot.title = element_text(hjust = 0)),
             Fig06b +
               ggtitle("B") +
               theme(plot.title = element_text(hjust = 0)),
             Fig06c$Plot +
               ggtitle("C") +
               theme(plot.title = element_text(hjust = 0)))
dev.off()