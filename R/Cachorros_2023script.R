rm(list=ls())
library(MuMIn)
library(ggplot2)
library (seewave)
library (tuneR)
library(signal)
library(soundecology)
 



#Abrimos el coroD:

setwd("D:/Doctorado 2022/AI_pups/AI_pups_2024/coroscachorros_otros")
sound <- readWave("5D8013F0_FERRRS1AU3V16.wav")
sound

#Realizamos espectrograma

spectro(sound, flim=c(0,3.9), ovlp=87.5,wl=1024,
        collevels=seq(-60,0,1),fastdisp = TRUE,
        osc=TRUE)
#An?lisis para todo el coro

#Filtro de amplitud del 5%

filamp<- afilter(sound, threshold=5,
                 output="Wave", plot=FALSE)

spectro(filamp, flim=c(0,3.9), ovlp=87.5,wl=1024,
        collevels=seq(-60,0,1),fastdisp = TRUE,
        osc=TRUE)
#Aplicamos filtro pasa banda y nos quedamos con las frecuencias entre 300 y 800 Hz

confiltro <- fir(filamp, from=300, to=800, output="Wave")

spectro(confiltro, flim=c(0,3.9), ovlp=87.5,wl=1024,
        collevels=seq(-60,0,1),fastdisp = TRUE,
        osc=TRUE)

#Tomamos las variables para todo el coro

cat("ACI",ACI(confiltro))

acomp <-acoustic_complexity(confiltro)

acdiv<- acoustic_diversity(confiltro)

acev<- acoustic_evenness(confiltro)

bioac<- bioacoustic_index(confiltro,  min_freq = 300, max_freq = 800, fft_w = 1024)

nd<-ndsi(confiltro, bio_max = 4000)  #spectral entropy

acentin<-H(confiltro)


res <- meanspec(confiltro, wl=1024, ovlp=87.5, plot=FALSE)
selo<-specprop(res, plot=FALSE)


#Valores de las variables

variables_cachorros <- c(cat("Cent", selo$cent,"\n"),
                         cat("Q25", selo$Q25, "\n"),
                         cat("Q75", selo$Q75, "\n"),
                         cat("IQR", selo$IQR, "\n"),
                         cat("Skewness", selo$skewness, "\n"),
                         cat("kurtosis", selo$kurtosis, "\n"),
                         cat("sh", selo$sh, "\n"),
                         cat("ACI",ACI(confiltro),"\n"),
                         cat("ACompInd", acomp$AciTotAll_left, "\n"),
                         cat("ADI", acdiv$adi_left, "\n"),
                         cat("acev", acev$aei_left, "\n"),
                         cat("Bioac", bioac$left_area, "\n"),
                         cat("nd", nd$ndsi_left, "\n"),
                         cat("Acentin", acentin, "\n"))

#An?lisis dividiendo por segmentos

# time positions of the spectra
pos <- seq(from=0, to=duration(sound)-5, by=5)
pos

# number of segments
l <- length(pos)
l


# Loop para analizar cada segmento  
for(i in 1:l) 
{
  segm1 <- cutw(sound, from=i, to=i+5, output="Wave")
  segm1
  
  filamp<- afilter(segm1, threshold=5,
                   output="Wave", plot=FALSE)#Filtro amplitud del segmento
  
  segmfiltro <- fir(filamp, from=300, to=800, output="Wave")#Pasabanda del segmento
  
  
  
  cat("ACI",ACI(segmfiltro))
  
  acomp <-acoustic_complexity(segmfiltro)
  
  acdiv<- acoustic_diversity(segmfiltro)
  
  acev<- acoustic_evenness(segmfiltro)
  
  bioac<- bioacoustic_index(segmfiltro, min_freq = 300, max_freq = 800, fft_w = 1024)
  
  nd<-ndsi(segmfiltro, bio_max = 4000)  #spectral entropy
  
  acentin<-H(segmfiltro)
  
  
  res <- meanspec(segmfiltro, wl=1024, ovlp=87.5, plot=FALSE)
  selo<-specprop(res, plot=FALSE)
  
  cat("Segmento",i," ", "\n") 
  cat("Cent", selo$cent,"\n")
  cat("Q25", selo$Q25, "\n")
  cat("Q75", selo$Q75, "\n")
  cat("IQR", selo$IQR, "\n")
  cat("Skewness", selo$skewness, "\n")
  cat("kurtosis", selo$kurtosis, "\n")
  cat("sh", selo$sh, "\n")
  cat("ACI",ACI(segmfiltro),"\n")
  cat("ACompInd", acomp$AciTotAll_left, "\n")
  cat("ADI", acdiv$adi_left, "\n")
  cat("acev", acev$aei_left, "\n")
  cat("Bioac", bioac$left_area, "\n")
  cat("nd", nd$ndsi_left, "\n")
  cat("Acentin", acentin, "\n")
  
  cat ("Press [enter] to continue")
  line <- readline()
  
  
}




setwd("D:/Doctorado 2022/AI_pups/AI_pups_2024/coroscachorros_otros")
sound <- readWave("5D8013F0_FERRRS1AU3V16.wav")
sound

#Realizamos espectrograma

spectro(sound, flim=c(0,3.9), ovlp=87.5,wl=1024,
        collevels=seq(-60,0,1),fastdisp = TRUE,
        osc=TRUE)
#An?lisis para todo el coro

#Filtro de amplitud del 5%

filamp<- afilter(sound, threshold=5,
                 output="Wave", plot=FALSE)

spectro(filamp, flim=c(0,3.9), ovlp=87.5,wl=1024,
        collevels=seq(-60,0,1),fastdisp = TRUE,
        osc=TRUE)
  
  

#Aplicamos filtro pasa banda y nos quedamos con las frecuencias entre 800 y 1300 Hz
  
  confiltro <- fir(filamp, from=800, to=1300, output="Wave")
  
  spectro(confiltro, flim=c(0,3.9), ovlp=87.5,wl=1024,
          collevels=seq(-60,0,1),fastdisp = TRUE,
          osc=TRUE)
  
  
  #Tomamos las variables para todo el coro
  
  cat("ACI",ACI(confiltro))
  
  acomp <-acoustic_complexity(confiltro)
  
  acdiv<- acoustic_diversity(confiltro)
  
  acev<- acoustic_evenness(confiltro)
  
  bioac<- bioacoustic_index(confiltro,min_freq = 800, max_freq = 1300, fft_w = 1024 )
  
  nd<-ndsi(confiltro, bio_max = 4000)  #spectral entropy
  
  acentin<-H(confiltro)
  
  
  res <- meanspec(confiltro, wl=1024, ovlp=87.5, plot=FALSE)
  selo<-specprop(res, plot=FALSE)
  
  #Valores de las variables
  
  variables_cachorros <- c(cat("Cent", selo$cent,"\n"),
                           cat("Q25", selo$Q25, "\n"),
                           cat("Q75", selo$Q75, "\n"),
                           cat("IQR", selo$IQR, "\n"),
                           cat("Skewness", selo$skewness, "\n"),
                           cat("kurtosis", selo$kurtosis, "\n"),
                           cat("sh", selo$sh, "\n"),
                           cat("ACI",ACI(confiltro),"\n"),
                           cat("ACompInd", acomp$AciTotAll_left, "\n"),
                           cat("ADI", acdiv$adi_left, "\n"),
                           cat("acev", acev$aei_left, "\n"),
                           cat("Bioac", bioac$left_area, "\n"),
                           cat("nd", nd$ndsi_left, "\n"),
                           cat("Acentin", acentin, "\n"))
  
  
  
  
  
  #An?lisis dividiendo por segmentos
  
  # time positions of the spectra
  pos <- seq(from=0, to=duration(sound)-5, by=5)
  pos
  
  # number of segments
  l <- length(pos)
  l
  
  # Loop para analizar cada segmento  
  for(i in 1:l) 
  {
    segm1 <- cutw(sound, from=i, to=i+5, output="Wave")
    segm1
    
    filamp<- afilter(segm1, threshold=5,
                     output="Wave", plot=FALSE)#Filtro amplitud del segmento
    
    segmfiltro <- fir(filamp, from=800, to=1300, output="Wave")#Pasabanda del segmento
    
    
    
    cat("ACI",ACI(segmfiltro))
    
    acomp <-acoustic_complexity(segmfiltro)
    
    acdiv<- acoustic_diversity(segmfiltro)
    
    acev<- acoustic_evenness(segmfiltro)
    
    bioac<- bioacoustic_index(segmfiltro, min_freq = 800, max_freq = 1300, fft_w = 1024)
    
    nd<-ndsi(segmfiltro, bio_max = 4000)  #spectral entropy
    
    acentin<-H(segmfiltro)
    
    
    res <- meanspec(segmfiltro, wl=1024, ovlp=87.5, plot=FALSE)
    selo<-specprop(res, plot=FALSE)
    
    cat("Segmento",i," ", "\n") 
    cat("Cent", selo$cent,"\n")
    cat("Q25", selo$Q25, "\n")
    cat("Q75", selo$Q75, "\n")
    cat("IQR", selo$IQR, "\n")
    cat("Skewness", selo$skewness, "\n")
    cat("kurtosis", selo$kurtosis, "\n")
    cat("sh", selo$sh, "\n")
    cat("ACI",ACI(segmfiltro),"\n")
    cat("ACompInd", acomp$AciTotAll_left, "\n")
    cat("ADI", acdiv$adi_left, "\n")
    cat("acev", acev$aei_left, "\n")
    cat("Bioac", bioac$left_area, "\n")
    cat("nd", nd$ndsi_left, "\n")
    cat("Acentin", acentin, "\n")
    
    cat ("Press [enter] to continue")
    line <- readline()
    
    
  }

 
  setwd("D:/Doctorado 2022/AI_pups/AI_pups_2024/coroscachorros_otros")
  sound <- readWave("5D8013F0_FERRRS1AU3V16.wav")
  
  
#Realizamos espectrograma
  
  spectro(sound, flim=c(0,3.9), ovlp=87.5,wl=1024,
          collevels=seq(-60,0,1),fastdisp = TRUE,
          osc=TRUE)
  #An?lisis para todo el coro
  
  #Filtro de amplitud del 5%
  
  filamp<- afilter(sound, threshold=5,
                   output="Wave", plot=FALSE)
  
  spectro(filamp, flim=c(0,3.9), ovlp=87.5,wl=1024,
          collevels=seq(-60,0,1),fastdisp = TRUE,
          osc=TRUE)
  
  #Aplicamos filtro pasa banda y nos quedamos con las frecuencias entre 1300 y 1800 Hz
  
  confiltro <- fir(filamp, from=1300, to=1800, output="Wave")
  
  spectro(confiltro, flim=c(0,3.9), ovlp=87.5,wl=1024,
          collevels=seq(-60,0,1),fastdisp = TRUE,
          osc=TRUE)
  
  #Tomamos las variables para todo el coro
  
  cat("ACI",ACI(confiltro))
  
  acomp <-acoustic_complexity(confiltro)
  
  acdiv<- acoustic_diversity(confiltro)
  
  acev<- acoustic_evenness(confiltro)
  
  bioac<- bioacoustic_index(confiltro, min_freq = 1300, max_freq = 1800, fft_w = 1024)
  
  nd<-ndsi(confiltro, bio_max = 4000)  #spectral entropy
  
  acentin<-H(confiltro)
  
  
  res <- meanspec(confiltro, wl=1024, ovlp=87.5, plot=FALSE)
  selo<-specprop(res, plot=FALSE)
  
  #Valores de las variables
  
  variables_cachorros <- c(cat("Cent", selo$cent,"\n"),
                           cat("Q25", selo$Q25, "\n"),
                           cat("Q75", selo$Q75, "\n"),
                           cat("IQR", selo$IQR, "\n"),
                           cat("Skewness", selo$skewness, "\n"),
                           cat("kurtosis", selo$kurtosis, "\n"),
                           cat("sh", selo$sh, "\n"),
                           cat("ACI",ACI(confiltro),"\n"),
                           cat("ACompInd", acomp$AciTotAll_left, "\n"),
                           cat("ADI", acdiv$adi_left, "\n"),
                           cat("acev", acev$aei_left, "\n"),
                           cat("Bioac", bioac$left_area, "\n"),
                           cat("nd", nd$ndsi_left, "\n"),
                           cat("Acentin", acentin, "\n"))

  
  
  
  #An?lisis dividiendo por segmentos
  
  # time positions of the spectra
  pos <- seq(from=0, to=duration(sound)-5, by=5)
  pos
  
  # number of segments
  l <- length(pos)
  l
  
  # Loop para analizar cada segmento  
  for(i in 1:l) 
  {
    segm1 <- cutw(sound, from=i, to=i+5, output="Wave")
    segm1
    
    filamp<- afilter(segm1, threshold=5,
                     output="Wave", plot=FALSE)#Filtro amplitud del segmento
    
    segmfiltro <- fir(filamp, from=1300, to=1800, output="Wave")#Pasabanda del segmento
    
    
    
    cat("ACI",ACI(segmfiltro))
    
    acomp <-acoustic_complexity(segmfiltro)
    
    acdiv<- acoustic_diversity(segmfiltro)
    
    acev<- acoustic_evenness(segmfiltro)
    
    bioac<- bioacoustic_index(segmfiltro,  min_freq = 1300, max_freq = 1800, fft_w = 1024)
    
    nd<-ndsi(segmfiltro, bio_max = 4000)  #spectral entropy
    
    acentin<-H(segmfiltro)
    
    
    res <- meanspec(segmfiltro, wl=1024, ovlp=87.5, plot=FALSE)
    selo<-specprop(res, plot=FALSE)
    
    cat("Segmento",i," ", "\n") 
    cat("Cent", selo$cent,"\n")
    cat("Q25", selo$Q25, "\n")
    cat("Q75", selo$Q75, "\n")
    cat("IQR", selo$IQR, "\n")
    cat("Skewness", selo$skewness, "\n")
    cat("kurtosis", selo$kurtosis, "\n")
    cat("sh", selo$sh, "\n")
    cat("ACI",ACI(segmfiltro),"\n")
    cat("ACompInd", acomp$AciTotAll_left, "\n")
    cat("ADI", acdiv$adi_left, "\n")
    cat("acev", acev$aei_left, "\n")
    cat("Bioac", bioac$left_area, "\n")
    cat("nd", nd$ndsi_left, "\n")
    cat("Acentin", acentin, "\n")
    
    cat ("Press [enter] to continue")
    line <- readline()
    
    
  }

  
  
  
  
 