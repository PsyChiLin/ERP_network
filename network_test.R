library(pacman)
pacman::p_load(ERP, mnormt, fdrtool, tidyverse, gridExtra, crayon,
               boot, reshape2, ggthemes, devtools, ggerp, qgraph,animation)
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

dta_HF <- filter(dta, Group == "High",Condition == "Failure") %>% droplevels()
dta_HS <- filter(dta, Group == "High",Condition == "Success") %>% droplevels()
dta_LF <- filter(dta, Group == "Low",Condition == "Failure") %>% droplevels()
dta_LS <- filter(dta, Group == "Low",Condition == "Success") %>% droplevels()

dta_HF_wide <- list()
dta_HS_wide <- list()
dta_LF_wide <- list()
dta_LS_wide <- list()

for (i in 1:601){
  # HS
  dta_HS_wide[[i]] <- dcast(dta_HS[,c(1,2,i+5)],Subject~Channel, value.var = colnames(dta_HS)[i+5])
  names(dta_HS_wide)[[i]] <- colnames(dta_HS)[i+5]
  # HF
  dta_HF_wide[[i]] <- dcast(dta_HF[,c(1,2,i+5)],Subject~Channel, value.var = colnames(dta_HF)[i+5])
  names(dta_HF_wide)[[i]] <- colnames(dta_HF)[i+5]
  # LS
  dta_LS_wide[[i]] <- dcast(dta_LS[,c(1,2,i+5)],Subject~Channel, value.var = colnames(dta_LS)[i+5])
  names(dta_LS_wide)[[i]] <- colnames(dta_LS)[i+5]
  # LF
  dta_LF_wide[[i]] <- dcast(dta_LF[,c(1,2,i+5)],Subject~Channel, value.var = colnames(dta_LF)[i+5])
  names(dta_LF_wide)[[i]] <- colnames(dta_LF)[i+5]
}

pos = "firebrick"
neg = "chartreuse4"

saveGIF({
  for(i in seq(250, 350, by = 2)){
    par(mfrow=c(2,2))
    # HS
    qgraph(cor(dta_HS_wide[[i]][,-1]),layout = as.matrix(erpR_coord),posCol = pos, negCol = neg)
    title(paste0("Success.High: ",names(dta_HS_wide)[[i]]), line = 1.5, adj = 0.5, cex.main = 3)
    # HF
    qgraph(cor(dta_HF_wide[[i]][,-1]),layout = as.matrix(erpR_coord),posCol = pos, negCol = neg)
    title(paste0("Failure.High: ",names(dta_HF_wide)[[i]]), line = 1.5, adj = 0.5, cex.main = 3)
    # LS
    qgraph(cor(dta_LS_wide[[i]][,-1]),layout = as.matrix(erpR_coord),posCol = pos, negCol = neg)
    title(paste0("Success.Low: ",names(dta_LS_wide)[[i]]), line = 1.5, adj = 0.5, cex.main = 3)
    # LF
    qgraph(cor(dta_LF_wide[[i]][,-1]),layout = as.matrix(erpR_coord),posCol = pos, negCol = neg)
    title(paste0("Failure.Low: ",names(dta_LF_wide)[[i]]), line = 1.5, adj = 0.5, cex.main = 3)
    print(i)
  }
},interval = 0.25,ani.width = 1600, ani.height= 1400,movie.name = "test.gif")


############################




saveHTML({
  for(i in 1:601){
    par(mfrow=c(2,2))
    # HS
    qgraph(cor(dta_HS_wide[[i]][,-1]),layout = as.matrix(erpR_coord))
    title(names(dta_HS_wide)[[i]], line = 2.5)
    # HF
    qgraph(cor(dta_HF_wide[[i]][,-1]),layout = as.matrix(erpR_coord))
    title(names(dta_HF_wide)[[i]], line = 2.5)
    # LS
    qgraph(cor(dta_LS_wide[[i]][,-1]),layout = as.matrix(erpR_coord))
    title(names(dta_LS_wide)[[i]], line = 2.5)
    # LF
    qgraph(cor(dta_LF_wide[[i]][,-1]),layout = as.matrix(erpR_coord))
    title(names(dta_LF_wide)[[i]], line = 2.5)
  }
},ani.width = 700, ani.height=600)




par(mfrow=c(1,2))
F1 <- qgraph(cor(dta_HF_wide[[1]][,-1]),layout = as.matrix(erpR_coord))
title(names(dta_HF_wide)[[i]], line = 2.5)
F2 <- qgraph(cor(dta_HF_wide[[2]][,-1]),layout = as.matrix(erpR_coord))
title(names(dta_HF_wide)[[2]], line = 2.5)

grid.arrange(F1,F2)




# EFA
library("psych")

pca1 <- principal(cor(dta_HF_wide[[1]][,-1]), 3,n.obs = 12, rotate = "promax")
F1 <- qgraph(pca1,layout = "circle")

pca2 <- principal(cor(dta_HF_wide[[2]][,-1]), 3,n.obs = 12, rotate = "promax")
F2 <- qgraph(pca2,layout = "circle")


qgraph(cor(dta_HF_T1_wide[,-1]),layout = as.matrix(erpR_coord) ,overlay = TRUE)

dta_HF_T2 <- dta_HF[,c(1,2,7)]
dta_HF_T2_wide <- dcast(dta_HF_T2,Subject~Channel, value.var = "T_2")

T2 <- qgraph(cor(dta_HF_T2_wide[,-1]),layout = "groups" ,overlay = TRUE)


