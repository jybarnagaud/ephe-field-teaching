#-----------------------------------------------------------#
#####       projet EPHE - UE terrain       #####
# variations temporelles des paysages acoustiques du Vercors
# calcul des indices acoustiques
# septembre 2025
#-----------------------------------------------------------#

setwd("D:/EPHE_enseignement/module terrain/module terrain 2025/EPHE-UE-terrain-2025")

## librairies ------------------------------------------------------------------

# packages essentiels pour les calculs
library(soundecology)
library(tuneR)
library(seewave)

library(lubridate) # formater les dates
library(beepr) # émettre un son à la fin d'une opération

source("scripts/fonctions/Soundscape-analysis-with-R-master/AcouIndexAlpha.R") # https://github.com/agasc/Soundscape-analysis-with-R

#-----------------------------------------#
##### chemin d'accès aux fichiers son #####
#-----------------------------------------#

season <- "ete"
path.folder <- paste("data/saison",season,sep="-")
content.folder <- dir(path = path.folder)
path.acou <- content.folder[grep("SMA", content.folder)]

#---------------------------------------#
###### ouvrir un fichier acoustique #####
#---------------------------------------#

# loop sur les sites
t1 <- Sys.time()

#for(k in 1:length(path.acou)){

# alternative : run un seul site

k <-3

# liste des fichiers acoustiques d'un site 

m <- path.acou[k]
path <- paste(path.folder, m, "Data", sep = "/")
audio.wav <- dir(path, pattern = ".wav")

# loop sur les j fichiers audio

ind.acou <- data.frame()

for (j in 1:length(audio.wav)){
  
  # ouvrir un fichier son
  
  wave.name <- audio.wav[j]
  wave.path <- paste(path, "/", wave.name, sep = "")
  wave0 <- try(readWave(wave.path))
  
  # si le fichier est corrompu --> listé dans un .txt
  
  if(class(wave0)=="try-error"){
    write(wave.path,paste("outputs/failed_wave_Vercors-25","-",season,".txt",sep=""),append=T)
  } else {
    
    # date du début enregistrement
    dt.start <- nchar(wave.name) - 18
    dt.file <-
      substring(wave.name, first = dt.start, last = nchar(wave.name))
    yr <- substring(dt.file, first = 1, last = 4)
    mth <- substring(dt.file, first = 5, last = 6)
    dy <- substring(dt.file, first = 7, last = 8)
    
    hr <- substring(dt.file, first = 10, last = 11)
    mn <- substring(dt.file, first = 12, last = 13)
    sc <- substring(dt.file, first = 14, last = 15)
    start <- ymd_hms(paste(yr, mth, dy, hr, mn, sc, sep = "-"), tz = "CET")
    
    # on ne fait le calcul que si le fichier fait bien 5 minutes
    if(seewave::duration(wave0) == 300) { 
      
      Result <-
        AcouIndexAlpha(
          wave0,
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
          Hf = T,
          Ht = T,
          H = T,
          ACI = T,
          AEI_villa = F,
          M = F,
          NDSI = T,
          ADI = T,
          NP = T
        )
      
      cat(paste(wave.name,as.character(Sys.time()),"////", sep= " "),file=paste("outputs/outfile",k,".txt",sep=""),append=TRUE)
      
      tp <- Result$Mono_left
      tp$FILE <- wave.name
      tp$START <- start
      tp$SM <-
        substring(wave.name, first = 1, last = nchar(wave.name) - 20)
      
      ind.acou <- rbind(ind.acou, tp)
      
      
    } # if 5mn  
    rm("wave0")
  } # else
} #j

# noms de lignes / colonnes

colnames(ind.acou) <- c("BIOAC",
                        "HT",
                        "HF",
                        "H",
                        "ACI",
                        "NDSI",
                        "ADI",
                        "NP",
                        "FILE",
                        "START",
                        "SM"
                        )

# write
nfichier <- paste("outputs/",path.acou[k],"-",season,".csv",sep="")
write.csv2(ind.acou,file=nfichier,row.names=F)

# } # k sites
t2 <- Sys.time()
beep()
