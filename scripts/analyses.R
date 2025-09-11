#-----------------------------------------------------------#
#####       projet EPHE - UE terrain       #####
# variations temporelles des paysages acoustiques du Vercors
# analyses statistiques
# septembre 2025
#-----------------------------------------------------------#

## librairies ------------------------------------------------------------------

# généralistes
library(readxl)
library(ggplot2)
library(mgcv)
library(patchwork)

# modèles
library(lme4)
library(lmerTest)
library(ggeffects)
library(mgcv)

# dates et heures
library(suncalc)
library(lubridate)

# spatial
library(sf)
library(mapview)
library(terra)

## données ---------------------------------------------------------------------

# métadonnées des enregistreurs

meta0 <- as.data.frame(read_excel("data/vercors-25-metadata.xlsx", sheet = 1))
meta0$'date-debut' <- as.Date(meta0$'date-debut', format = "%Y-%m-%d",tz = "Europe/Paris")

# localisation des enregistreurs

loc.sm <- st_read(dsn = "data/vercors-25-locs.kml")
xy <- st_coordinates(loc.sm)
rownames(xy) <- loc.sm$Name

meta <- merge(meta0,xy,by.x = "code-point",by.y = 0)

# ouvrir tous les fichiers d'indices écoacoustiques disponibles

df.acou <- data.frame()

for (i in 1:nrow(meta)) {
  name.tp <- paste("outputs/", meta[i, "SM-nom"], "-", meta[i, "saison"], ".csv", sep = "")
  
  file.tp <- read.csv2(name.tp, header = T)
  file.tp$SM.nom <- meta[i, "SM-nom"]
  file.tp$saison <- meta[i, "saison"]
  file.tp$index.table <- name.tp
  df.acou <- rbind(df.acou, file.tp)
}

df.all1 <- merge(df.acou, meta, by.x = c("SM.nom","saison"), by.y = c("SM-nom","saison"))

## recalculer dates - heures (colonne START) -
# résout une erreur silencieuse dans le script "calcul-indices-acoustiques",
# les heures en 00:00 sont NA

dt.start <- nchar(df.all1$FILE) - 18
dt.file <-
  substring(df.all1$FILE, first = dt.start, last = nchar(df.all1$FILE))
yr <- substring(dt.file, first = 1, last = 4)
mth <- substring(dt.file, first = 5, last = 6)
dy <- substring(dt.file, first = 7, last = 8)

hr <- substring(dt.file, first = 10, last = 11)
mn <- substring(dt.file, first = 12, last = 13)
sc <- substring(dt.file, first = 14, last = 15)
start <- ymd_hms(paste(yr, mth, dy, hr, mn, sc, sep = "-"))

df.all1$START <- start
df.all1$date.record <- start <- ymd(paste(yr, mth, dy, sep = "-"))

## catégorisation jour - nuit --------------------------------------------------

# lever - coucher du soleil

dat.suncalc <- unique(df.all1[,c("date.record","Y","X")])
colnames(dat.suncalc) <- c("date","lat","lon")
dat.suncalc$date <- as.Date(dat.suncalc$date)
suncalc.out <- getSunlightTimes(data = dat.suncalc, keep = c("sunrise","sunset"),tz = "Europe/Paris")
df.all <- merge(df.all1,suncalc.out,by.x = c("date.record","X","Y"),by.y = c("date","lon","lat"))

df.all$is.day <- "no"

for (i in 1:nrow(df.all)) {
  day <- interval(df.all[i, "sunrise"], df.all[i, "sunset"])
  if (!is.na(df.all[i, "START"]) & df.all[i,"START"] %within% day) {
    df.all[i, "is.day"] = "yes"
  } else if (is.na(df.all[i,"START"])){
    df.all[i,"is.day"] = "no"
  }
  
}

## différences entre les enregistreurs -----------------------------------------

# sélectionner une saison (ou df.k= df.all pour toutes saisons ensemble)

k = "automne"
df.k <- subset(df.all, saison == k)

# histogrammes sur quelques indices (sans dimension temporelle)

h1 <- ggplot(df.all)+
  aes(x  = ACI)+
  geom_histogram()+
  facet_wrap(~lieu)

h1

ggsave(
  filename = paste("outputs/histo-aci-", k, ".png", sep = ""),
  width = 10,
  height = 5,
  bg = "white"
)

h2 <- ggplot(df.k)+
  aes(x  = NDSI)+
  geom_histogram()+
  facet_wrap(~lieu)

h2


ggsave(
  filename = paste("outputs/histo-ndsi-", k, ".png", sep = ""),
  width = 10,
  height = 5,
  bg = "white"
)

h3 <- ggplot(df.k)+
  aes(x  = BIOAC)+
  geom_histogram()+
  facet_wrap(~lieu)

# boxplots

b1 <- ggplot(df.k)+
  aes(x = lieu, y = ACI)+
  geom_boxplot()

b2 <- ggplot(df.k)+
  aes(x = lieu, y = NDSI)+
  geom_boxplot()

b3 <- ggplot(df.k)+
  aes(x = lieu, y = BIOAC)+
  geom_boxplot()

(b1+b2)/(b3+plot_spacer())

ggsave(
  filename = paste("outputs/boxplots-all-", k, ".png", sep = ""),
  width = 10,
  height = 5,
  bg = "white"
)

# les mêmes, seulement jour

df.day <- subset(df.k, is.day == "yes")

bd1 <- ggplot(df.day)+
  aes(x = lieu, y = ACI)+
  geom_boxplot()

bd2 <- ggplot(df.day)+
  aes(x = lieu, y = NDSI)+
  geom_boxplot()

bd3 <- ggplot(df.day)+
  aes(x = lieu, y = BIOAC)+
  geom_boxplot()

(bd1+bd2)/(bd3+plot_spacer())

ggsave(
  filename = paste("outputs/boxplots-day-", k, ".png", sep = ""),
  width = 10,
  height = 5,
  bg = "white"
)

df.noc <- subset(df.k, is.day == "no")

bn1 <- ggplot(df.noc)+
  aes(x = lieu, y = ACI)+
  geom_boxplot()

bn2 <- ggplot(df.noc)+
  aes(x = lieu, y = NDSI)+
  geom_boxplot()

bn3 <- ggplot(df.noc)+
  aes(x = lieu, y = BIOAC)+
  geom_boxplot()

(bn1+bd2)/(bn3+plot_spacer())

ggsave(
  filename = paste("outputs/boxplots-night-", k, ".png", sep = ""),
  width = 10,
  height = 5,
  bg = "white"
)

## tendances temporelles lissées par saisons------------------------------------

# NDSI

df.k <- subset(df.all, saison == k)
ts1 <- ggplot(df.k)+
  aes(x = START, y = NDSI)+
  geom_point()+
  geom_smooth(se = T,method = "gam")+
  geom_segment(aes(x = sunrise,xend = sunset, y = -0.75, yend = -0.75))+
  facet_wrap(~lieu)+
  theme_minimal()
ts1

ggsave(
  filename = paste("outputs/ndsi-trend-", k, ".png", sep = ""),
  width = 10,
  height = 5,
  bg = "white"
)

# ACI 

ts2 <- ggplot(df.k)+
  aes(x = START, y = ACI)+
  geom_point()+
  geom_smooth(se = T,method = "gam")+
  geom_segment(aes(x = sunrise,xend = sunset, y = 4200, yend = 4200))+
  facet_wrap(~lieu)+
  theme_minimal()
ts2

ggsave(
  filename = paste("outputs/aci-trend-", k, ".png", sep = ""),
  width = 10,
  height = 5,
  bg = "white"
)

# BIOAC

ts3 <- ggplot(df.k)+
  aes(x = START, y = BIOAC)+
  geom_point()+
  geom_smooth(se = T,method = "gam")+
  geom_segment(aes(x = sunrise,xend = sunset, y = 5, yend = 5))+
  facet_wrap(~lieu)+
  theme_minimal()
ts3

## analyses finalisées oral ----------------------------------------------------

# cartes

avg.site <- aggregate(df.all[,c("ACI","NDSI")], by = list(df.all$saison, df.all$lieu), FUN = "mean")
sd.site <- aggregate(df.all[,c("ACI","NDSI")], by = list(df.all$saison, df.all$lieu), FUN = "sd")

stats.site <- cbind(avg.site,sd.site[,-c(1,2)])
colnames(stats.site) <- c("saison","lieu","ACI.av","NDSI.av","ACI.sd","NDSI.sd")

meta.xy <- unique(df.all[,c("saison","lieu","X","Y")])

stats.xy <- merge(stats.site,meta.xy,by = c("lieu","saison"))

m1 <- subset(stats.xy, saison == "printemps")
m2 <- subset(stats.xy, saison == "ete")
m3 <- subset(stats.xy, saison == "automne")

m.aci.pri <- mapview(xcol = "X", ycol = "Y", zcol = "ACI.av", x = m1, crs = crs(loc.sm))
m.aci.et <- mapview(xcol = "X", ycol = "Y", zcol = "ACI.av", x = m2, crs = crs(loc.sm))
m.aci.au <- mapview(xcol = "X", ycol = "Y", zcol = "ACI.av", x = m3, crs = crs(loc.sm))

m.ndsi.pri <- mapview(xcol = "X", ycol = "Y", zcol = "NDSI.av", x = m1, crs = crs(loc.sm))
m.ndsi.et <- mapview(xcol = "X", ycol = "Y", zcol = "NDSI.av", x = m2, crs = crs(loc.sm))
m.ndsi.au <- mapview(xcol = "X", ycol = "Y", zcol = "NDSI.av", x = m3, crs = crs(loc.sm))

# séries temporelles (modèles additifs généralisés)
df.all$lieu <- factor(df.all$lieu)
df.all$saison <- factor(df.all$saison)
ts.ndsi <- gamm(NDSI ~ s(as.numeric(START), by = saison), random = list(lieu = ~ 1) , data = df.all)
plot(ts.ndsi$gam)

# ACI ~ NDSI

rl1 <- ggplot(df.all)+
  aes(x = NDSI, y = ACI)+
  geom_point()+
  facet_wrap(~lieu*saison)
rl1

rl2 <- ggplot(df.all)+
  aes(x = NDSI, y = ACI, col = lieu)+
  geom_point()+
  facet_wrap(~saison)
rl2

# modèle

df.all$saison <- factor(df.all$saison)
df.all$lieu <- factor(df.all$lieu)

df.all$saison <- factor(df.all$saison)

mod1 <- lm(ACI ~ NDSI + lieu + saison 
           + (NDSI : saison) + (NDSI : lieu), data = df.all)

par(mfrow = c(2,2))
plot(mod1)

plot(fitted(mod1),residuals(mod1))

summary(mod1)

pred1 <- ggpredict(mod1, terms = c("NDSI","saison"))
pred1.1 <- plot(pred1, show_residuals = T)

pred2 <- ggpredict(mod1, terms = c("NDSI","lieu"))
pred1.2 <- plot(pred2, show_residuals = T)

pred1.1 + pred1.2
ggsave(filename = "outputs/modele.png", width = 15, height = 7.5)

# même modèle, jour

df.day <- subset(df.all, is.day == "yes" )
mod1.day <- lm(ACI ~ NDSI + lieu + saison 
           + (NDSI : saison) + (NDSI : lieu), data = df.day)

pred1.day <- ggpredict(mod1.day, terms = c("NDSI","saison"))
pred1.day.1 <- plot(pred1.day, show_residuals = T)

pred2.day <- ggpredict(mod1.day, terms = c("NDSI","lieu"))
pred2.day.2 <- plot(pred2.day, show_residuals = T)

pred1.day.1 + pred2.day.2
ggsave(filename = "outputs/modele-jour.png", width = 15, height = 7.5)

# même modèle, nuit

df.night <- subset(df.all, is.day == "no" )
mod1.night <- lm(ACI ~ NDSI + lieu + saison 
               + (NDSI : saison) + (NDSI : lieu), data = df.night)

pred1.night <- ggpredict(mod1.night, terms = c("NDSI","saison"))
pred1.night.1 <- plot(pred1.night, show_residuals = T)

pred2.night <- ggpredict(mod1.night, terms = c("NDSI","lieu"))
pred2.night.2 <- plot(pred2.night, show_residuals = T)

pred1.night.1 + pred2.night.2
ggsave(filename = "outputs/modele-nuit.png", width = 15, height = 7.5)

## ACP -------------------------------------------------------------------------

library(ade4)

df.pca <- df.all[, c("BIOAC","HT","HF","H","ACI","NDSI","ADI","NP")]

pca <- dudi.pca(df.pca)

s.corcircle(pca$co)
