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

# dates et heures
library(suncalc)
library(lubridate)

# spatial
library(sf)

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
df.all1$START <- as.POSIXct(df.all1$START, format = "%Y-%m-%d %H:%M:%S")
df.all1$date.record <- as.Date(df.all1$START)

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

# histogrammes sur quelques indices (sans dimension temporelle)

h1 <- ggplot(df.all)+
  aes(x  = ACI)+
  geom_histogram()+
  facet_wrap(~lieu)

h2 <- ggplot(df.all)+
  aes(x  = NDSI)+
  geom_histogram()+
  facet_wrap(~lieu)

h3 <- ggplot(df.all)+
  aes(x  = BIOAC)+
  geom_histogram()+
  facet_wrap(~lieu)

# boxplots

b1 <- ggplot(df.all)+
  aes(x = lieu, y = ACI)+
  geom_boxplot()

b2 <- ggplot(df.all)+
  aes(x = lieu, y = NDSI)+
  geom_boxplot()

b3 <- ggplot(df.all)+
  aes(x = lieu, y = BIOAC)+
  geom_boxplot()

(b1+b2)/(b3+plot_spacer())

# les mêmes, seulement jour

df.day <- subset(df.all, is.day == "yes")

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


df.noc <- subset(df.all, is.day == "no")

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

## tendances temporelles lissées -----------------------------------------------

# NDSI

ts1 <- ggplot(df.all)+
  aes(x = START, y = NDSI)+
  geom_point()+
  geom_smooth(se = T,method = "gam")+
  geom_segment(aes(x = sunrise,xend = sunset, y = -0.75, yend = -0.75))+
  facet_wrap(~lieu)+
  theme_minimal()
ts1

# ACI 

ts2 <- ggplot(df.all)+
  aes(x = START, y = ACI)+
  geom_point()+
  geom_smooth(se = T,method = "gam")+
  geom_segment(aes(x = sunrise,xend = sunset, y = 4200, yend = 4200))+
  facet_wrap(~lieu)+
  theme_minimal()
ts2

# BIOAC

ts3 <- ggplot(df.all)+
  aes(x = START, y = BIOAC)+
  geom_point()+
  geom_smooth(se = T,method = "gam")+
  geom_segment(aes(x = sunrise,xend = sunset, y = 5, yend = 5))+
  facet_wrap(~lieu)+
  theme_minimal()
ts3
