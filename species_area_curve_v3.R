#extrapolate from 1m2 to 1km2 (1'000'000m2)

library(tidyverse)
library(data.table)
library(reshape)
library(iNEXT)

#--- 1) re-structure data into required format
data_cereal <- read.table("S:/Datenlieferungen/OUT/EPFL/Crosnier_Agathe/Wheat_Cereal_ALLEMA_v3.csv", sep=";", header=T)
dim(data_cereal)
length(unique(data_cereal$pointID)) #149 Plots
# data_cereal <- data_cereal[which(data_cereal$Plot_with_Triticum_sp == 1),]
# length(unique(data_cereal$pointID)) #99 Plots
# data_cereal <- data_cereal[which(data_cereal$Plot_with_Triticum_aestivum == 1),]
# length(unique(data_cereal$pointID)) #63 Plots

#remove "unbestimmte Arten"
data_cereal2 <- data_cereal[!is.na(data_cereal$species),]

#make incidence data from abundance data
data_cereal_inc <- data_cereal2 %>%
  mutate(incidence = ifelse(dominance > 0, 1, dominance))

data_cereal_inc$pointID <- as.factor(data_cereal_inc$pointID)
levels(as.factor(data_cereal_inc$species))

#make 3 datasets
Tri_aes	<- data_cereal_inc[which(data_cereal_inc$Plot_with_Triticum_aestivum==1),]
Tri_aes_pl	<- data.frame(cast(Tri_aes, species ~ pointID, fun.aggregate = "sum", value = "incidence"))
Tri_aes_Vec	<- as.incfreq(Tri_aes_pl[,-1])

Tri_sp	<- data_cereal_inc[which(data_cereal_inc$Plot_with_Triticum_sp==1),]
Tri_sp_pl	<- data.frame(cast(Tri_sp, species ~ pointID, fun.aggregate = "sum", value = "incidence"))
Tri_sp_Vec	<- as.incfreq(Tri_sp_pl[,-1])

Cer		<- data_cereal_inc[which(data_cereal_inc$Plot_with_Cereal==1),]
Cer_pl	<- data.frame(cast(Cer, species ~ pointID, fun.aggregate = "sum", value = "incidence"))
Cer_Vec	<- as.incfreq(Cer_pl[,-1])

df3_list	<- list(Tri_aes_Vec, Tri_sp_Vec, Cer_Vec)
names(df3_list)	<- c("Tri_aes_Vec", "Tri_sp_Vec", "Cer_Vec")


#--- 2) calculate an iNEXT-object
#up to douple sample size
nex_3groups <- iNEXT(df3_list, datatype = "incidence_freq") 
#up to certain number of samples
size <- round(seq(10, 1000, length.out=20))
nex_3groups_1000 <- iNEXT(df3_list, datatype = "incidence_freq",size=size) 

#--- 3) plot the rarefaction curves
# sample size-based rarefaction/extrapolation curve
ggiNEXT(nex_3groups, type = 1)
ggiNEXT(nex_3groups_1000, type = 1)

# # sample completeness curve
# ggiNEXT(nex_3groups, type = 2)
# 
# # coverage-based rarefaction/extrapolation curve
# ggiNEXT(nex_3groups, type = 3)
# 


