#------------------------------------------------------#
#####       projet EPHE - UE terrain       #####
      # perception des paysages acoustiques
              # 12 septembre 2024
# Gwenn Essirard - Pierre Lespagnol - Charlotte Lerisson
      # charlotte.lerisson@etu.ephe.psl.eu
#------------------------------------------------------#

## librairies ------------------------------------------------------------------

library(soundecology)
library(tuneR)
library(seewave)
library(ggplot2)
library(viridis)
library(stringr)
library(readxl)
library(PerformanceAnalytics)
library(tidyr)
library(lme4)
library(ggeffects)
library(patchwork)

source("scripts/fonctions/Soundscape-analysis-with-R-master/AcouIndexAlpha.R") # https://github.com/agasc/Soundscape-analysis-with-R

## data ------------------------------------------------------------------------

cartnat1 <- read.csv("data/ephe-vercors-2024-cartnat1-zonstat.csv")
cn1 <- cartnat1[,c("Zone.ID","Mean","Standard.deviation")]
colnames(cn1) <- c("site_sig","mean.cartnat1","sd.cartnat1")

cartnat4 <- read.csv("data/ephe-vercors-2024-cartnat4-zonstat.csv")
cn4 <- cartnat4[,c("Zone.ID","Mean","Standard.deviation")]
colnames(cn4) <- c("site_sig","mean.cartnat4","sd.cartnat4")

cn <- merge(cn1,cn4,by = "site_sig")

sites <-
  as.data.frame(read_excel("data/ephe-vercors-2024-sites.xlsx", sheet = "Feuil1"))

cn.sites <- merge(sites,cn,by = "site_sig")

enquete <-
  as.data.frame(read_excel("data/ephe-vercors-2024-enquete.xlsx", sheet = "villageois"))

## calcul des indices écoacoustiques -------------------------------------------

# fichiers sons utilisés pour l'enquête

sound.select <- list.files("data/sounds/export")
df.ecoac <- data.frame()

for(i in 1:length(sound.select)){

path <- paste("data/sounds/export/",sound.select[i],sep="")
sound.file <- paste(path,"/",list.files(path)[1],sep="")

filetype <- str_sub(sound.file, start= -3)

if(filetype == "wav"){
soundfile <- readWave(sound.file) # si l'extrait est en .wav
} else if(filetype == "mp3"){
soundfile <- readMP3(sound.file)   # si l'extrait est en mp3
}

# calcul d'indices écoacoustiques
acou.ind <-
  AcouIndexAlpha(
    soundfile,
    stereo = FALSE,
    min_freq = 500,
    max_freq = 12000,
    anthro_min = 500,
    anthro_max = 1500,
    bio_min = 1500,
    bio_max = 12000,
    wl = 512,
    j = 5,
    AcouOccupancy = F,
    Bioac = T,
    Hf = F,
    Ht = F,
    H = F,
    ACI = T,
    AEI_villa = F,
    M = F,
    NDSI = T,
    ADI = F,
    NP = T
  )$Mono_left 

df.ecoac <- rbind(df.ecoac,acou.ind)
}

df.ecoac$recorder <- sound.select

df.cn.ecoac <- merge(cn.sites,df.ecoac,by = "recorder")

## explorer les données liées aux sites ----------------------------------------

# relation cartnat - score d'anthropisation du projet

p.cartnat4 <- ggplot(df.cn.ecoac)+
  aes(x = anthro_benchmark, y = mean.cartnat4)+
  geom_point()

p.cartnat1 <- ggplot(df.cn.ecoac)+
  aes(x = factor(anthro_benchmark), y = mean.cartnat1)+
  geom_boxplot()

# relation écoacoustique - score d'anthropisation du projet

p.ndsi <- ggplot(df.cn.ecoac)+
  aes(x = anthro_benchmark, y = NDSI_left)+
  geom_point()

## enquete ---------------------------------------------------------------------

### sensibilité environnementale

sens.box <- ggplot(enquete)+
  aes(x = factor(id), y = sensibilite_env)+
  geom_boxplot()

### check data -----------------------------------------------------------------

c1.lans <- ggplot(enquete)+
  aes(x = factor(image_lans), y = son_lans)+
  geom_boxplot()
c1.lans

enq.red <- enquete[, c(1, 8:ncol(enquete))]
enquete.long <- as.data.frame(pivot_longer(enq.red, !id,
                                           names_to = "modalite",
                                           values_to = "score"))

enquete.long2 <-
  data.frame(enquete.long, matrix(unlist(
    strsplit(enquete.long$modalite, split = "_")
  ), ncol = 2, byrow = T))

colnames(enquete.long2) <- c("id","modalite","score","type","lieu")

son_img <- ggplot(enquete.long2)+
  aes(x = type, y = score)+
  geom_boxplot()
son_img

tapply(enquete.long2$score, INDEX = enquete.long2$type, FUN = "median")
tapply(enquete.long2$score, INDEX = enquete.long2$type, FUN = "sd")

### corrélation image - son ----------------------------------------------------

# on reformate les données par facilité d'usage

enquete.long3 <- enquete.long2[, -2]
enquete.wide2 <-
  as.data.frame(pivot_wider(enquete.long3,
                            names_from = type, values_from = score))
enquete.wide2$lieu <-
  factor(
    enquete.wide2$lieu,
    levels = c("chateaujulien", "peuil", "villard", "lans", "cote2000")
  )

# corrélation des données brutes

cor.type <- ggplot(enquete.wide2)+
  aes(x = image, y = son,col = lieu, size = 2)+
  geom_point(alpha = 0.3)+
  scale_colour_viridis_d()+
  theme_classic()

# on quantifie la relation avec un modèle mixte 

m1 <- lmer(son ~ image + (image  |id), data = enquete.wide2)

# check des conditions d'application (pas terrible mais ça suffira ici)

plot(m1)
hist(residuals(m1))

# paramètres du modèle : la relation image - son est d'à peu près 0.8
#  = quand l'artificialisation perçue sur l'image augmente d'une unité de score, 
# l'artificialisation perçue sur le son augmente de 0.8

summary(m1)

# représentation graphique du modèle + données avec un IC à 95%

plot.lmer <- ggpredict(m1)
plot.lmer3 <- plot(plot.lmer,show_data = T,
                    dot_size = 2,alpha = 0.3,show_title = F)+
  labs(x = "artificialisation - image",y = "artificialisation - son",main = "")+
  theme_classic()
plot.lmer3
ggsave("outputs/plot_model.png",width = 5, height = 5)

### delta image - son ----------------------------------------------------------

enquete.wide2$delta <- apply(enquete.wide2[,c("son","image")],1,"diff")
hist(enquete.wide2$delta)

pdelta <- ggplot(enquete.wide2)+
  aes(x = delta)+
  geom_histogram(bins=6,fill="gray90",col="gray30")+
  theme_classic()+
  labs(x = "delta (son - image)",y = "nombre de personnes interrogées")
pdelta
ggsave("outputs/delta_histogram.png",width = 5, height = 5)

psites <- ggplot(enquete.wide2)+
  aes(x = lieu, y = delta)+
  geom_boxplot()+
  labs(x = "site (du moins au plus artificialisé)", y = "delta (son - image")+
  theme_classic()
psites
ggsave("outputs/delta_sites.png", width = 5, height = 5)

pindiv <- ggplot(enquete.wide2)+
  aes(x = id, y = delta)+
  geom_boxplot()

### profil des personnes -------------------------------------------------------

people <- unique(enquete[,c("id","age","genre","residence","sensibilite_env")])
people.enqw2 <- merge(people,enquete.wide2,by = "id")

page <- ggplot(people.enqw2)+
  aes(x = age, y = delta)+
  geom_boxplot()+
  labs(x="âge",y = "delta (son - image)")+
  theme_classic()

psex <- ggplot(people.enqw2)+
  aes(x = genre, y = delta)+
  geom_boxplot()+
  labs(x="genre",y = "delta (son - image)")+
  theme_classic()

penv <- ggplot(people.enqw2)+
  aes(x = sensibilite_env, y = delta)+
  geom_point()+
  labs(x="sensibilité environnementale",y = "delta (son - image)")+
  theme_classic()

pres <- ggplot(people.enqw2)+
  aes(x = residence, y = delta)+
  geom_boxplot()+
  labs(x="résidence",y = "delta (son - image)")+
  theme_classic()

(page+ psex) / (penv + pres)
ggsave("outputs/delta_individus.png",width = 5,height = 5)

### corrélations avec les indices quantitatifs ---------------------------------

enq.sit <-
  merge(enquete.wide2, df.cn.ecoac, by.x = "lieu", by.y = "site2")

