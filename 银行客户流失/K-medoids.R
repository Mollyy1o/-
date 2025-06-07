library(dplyr)
library(ISLR)
library(cluster)
library(Rtsne)
library(reshape2)
library(gridExtra)


setwd("D:/")
data_yes <- read.table("cluster_data_yes.csv",sep=",",header = T)
data_no <- read.table("cluster_data_no.csv",sep=",",header = T)
rownames(data_yes)=data_yes[,1]
rownames(data_no)=data_no[,1]

data_yes[,7] <- as.factor(data_yes[,7])
data_no[,7] <- as.factor(data_no[,7])
type <- c("SLIVER","GOLD","PLATINUM","DIAMOND")
data_yes[,9] <- ordered(data_yes[,9], levels=type, labels=c("S","G","P","D"))
data_no[,9] <- ordered(data_no[,9], levels=type, labels=c("S","G","P","D"))


VariationWeight <- function(Data){
  X_dist_var <- c()
  for(i in 1:length(Data)){
    X_dist_var[i] <- cluster::daisy(Data[i],
                                    metric = "gower",
                                    stand = "TRUE") %>%
      as.vector()%>%
      var()
  }
  VAR_weight <- 1/X_dist_var
  names(VAR_weight) <- colnames(Data)
  return(VAR_weight)
}

data_yes_1 <- data_yes[,-c(1,3,5)]
var_weight_yes <- VariationWeight(data_yes_1)

data_no_1 <- data_no[,-c(1,3,5)]
var_weight_no <- VariationWeight(data_no_1)

 gower_dist_yes <- daisy(data_yes[,-c(1,3,5)],
                        metric = "gower",  
                        weights = var_weight_yes)

pam_fit_yes <- pam(gower_dist_yes, diss = TRUE, k = 2)
center_yes <- pam_fit_yes$medoids
data_yes[center_yes,-c(1,3,5)]

gower_dist_no <- daisy(data_no[,-c(1,3,5)],
                       metric = "gower",
                       weights = var_weight_no)

pam_fit_no <- pam(gower_dist_no, diss = TRUE, k = 2)
center_no <- pam_fit_no$medoids
data_no[center_no,-c(1,3,5)]

th_yes <- c(NA)
dis_yes <- c(NA)
for(i in 2:8){
  pam_fit_yes <- pam(gower_dist_yes,
                 diss = TRUE,
                 k = i)
  sil_width_yes[i-1] <- pam_fit_yes$silinfo$avg.widt
  dis_yes[i-1] <- sum(pam_fit_yes$clusinfo[,1] * pam_fit_yes$clusinfo[,3])
}

sil_width_no <- c(NA)
dis_no <- c(NA)
for(i in 2:8){
  pam_fit_no <- pam(gower_dist_no,
                     diss = TRUE,
                     k = i)
  sil_width_no[i-1] <- pam_fit_no$silinfo$avg.widt
  dis_no[i-1] <- sum(pam_fit_no$clusinfo[,1] * pam_fit_no$clusinfo[,3])
}
## ????