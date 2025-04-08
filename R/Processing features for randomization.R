library(gibbonR)
library(caret)
library(soundecology)
library(dplyr)
library(stringr)
library(ggplot2)
library (seewave)
library (tuneR)
library(signal)
library(soundecology)
# V1 changes wav2vec2 so that we take mean/sd for each 1-s

# Set input directory
#input.dir.wavs <- "C:/Users/PC RACING/Desktop/Denacolab_2025/MFCC/clips_5s/wolfclips"
input.dir.wavs <- "data/wolfclips"

# MFCCs -------------------------------------------------------------------

chorus.list <-
  list.files(input.dir.wavs, full.names = TRUE, pattern = '.wav', recursive = TRUE)

chorus.list.short <-
  list.files(input.dir.wavs, full.names = FALSE, pattern = '.wav', recursive = TRUE)
Short <-basename(chorus.list.short)
packid <- str_split_fixed(chorus.list.short,'_',n=2)[,1]



RecordingID <- Short

# Split the string at all underscores in the current file name (ensure the correct list is used)
split_all <- str_split(Short, "_")[[1]]

# Get the first part (PackID)
PackID <- split_all[1]
print(paste("PackID:", PackID))  # Debugging

# Get the last part (segment number)
segmentnumber <- split_all[length(split_all)]
print(paste("Segment Number:", segmentnumber))  # Debugging

# Get the middle parts (datetime, everything between the first and last)
datetime <- paste(split_all[2:(length(split_all) - 1)], collapse = "_")  # Combine middle parts into a single string
print(paste("Datetime:", datetime))  # Debugging
# Initialize an empty data frame to store the results

mfcc.vector.df <- data.frame()

# Loop through all files and calculate MFCCs
for (a in 1:length(chorus.list)) {
  print(a)

  # Read in the audio file
  short.wav <- readWave(chorus.list[a])
  dur <- duration(short.wav)

  # Calculate MFCCs
  melfcc.output <- tuneR::melfcc(
    short.wav,
    minfreq = 300,
    maxfreq = 1800
  )

  # Calculate the mean and standard deviation of MFCCs
  MFCCMean <- t(apply(melfcc.output, 2, mean))
  MFCCSD <- t(apply(melfcc.output, 2, sd))

  # Calculate delta cepstral coefficients
  deltas.output <- tuneR::deltas(melfcc.output)
  DeltaMean <- t(apply(deltas.output, 2, mean))
  DeltaSD <- t(apply(deltas.output, 2, sd))

  # Extract information from the file name (RecordingID, PackID, segmentnumber, datetime)
  short_file_name <- Short[a]

  # Split the file name at underscores to extract the different parts
  split_all <- str_split(short_file_name, "_")[[1]]

  # Get PackID and segmentnumber from the file name
  PackID <- split_all[1]
  segmentnumber <- split_all[length(split_all)]
  datetime <- paste(split_all[2:(length(split_all) - 1)], collapse = "_")

  # Extract the folder name (parent directory)
  folder_name <- basename(dirname(chorus.list[a]))  # Get the immediate folder name (parent directory)

  # Ensure only the same number of time windows are used for MFCC and delta coefficients
  TempMFCCRow <- cbind.data.frame(PackID, datetime, segmentnumber, MFCCMean, MFCCSD, dur, Recording = short_file_name, Folder = folder_name)

  # Append the result to the data frame
  mfcc.vector.df <- rbind.data.frame(mfcc.vector.df, TempMFCCRow)
}

print(mfcc.vector.df)
write.csv(mfcc.vector.df,'data/MFCCpups2.csv',row.names = F)
#write.csv(mfcc.vector.df,'C:/Users/PC RACING/Desktop/Denacolab_2025/MFCC/clips_5s/results_data/MFCCpups2.csv',row.names = F)

# Acoustic Indices --------------------------------------------------------
input.dir.wavs <- "C:/Users/PC RACING/Desktop/Denacolab_2025/MFCC/clips_5s/wolfclips/Pups"

chorus.list <-
  list.files(input.dir.wavs, full.names = T, pattern = '.wav')

chorus.list.short <-
  list.files(input.dir.wavs, full.names = F, pattern = '.wav')

AcousticIndicesDF <- data.frame()

for (a in 1:length(chorus.list)) {
  print(a)
  short.wav <- readWave(chorus.list[a])




  acentin<-H(short.wav, min_freq = 300, max_freq = 1800)

  res <- meanspec(short.wav, min_freq = 300, max_freq = 1800, wl=1024, ovlp=87.5, plot=FALSE)

  selo<-specprop(res, plot=FALSE)

  ACI <- acoustic_complexity(short.wav, min_freq=500, , max_freq = 1800)$AciTotAll_left

  ADI <- acoustic_diversity(short.wav, , max_freq = 1800)$adi_left

  AEI <- acoustic_evenness(short.wav, , max_freq = 1800)$aei_left

  bioindex <- bioacoustic_index(short.wav, min_freq=300, , max_freq = 1800)$left_area

  duration <- seewave::duration(short.wav)




  Individual <- paste(str_split_fixed(chorus.list.short[a],
                 pattern = '_',n=6)[,4],
  str_split_fixed(chorus.list.short[a],
                 pattern = '_',n=6)[,5],sep='_')
 RecorderID <- paste(str_split_fixed(chorus.list.short[a],
                   pattern = '_',n=6)[,1],
   str_split_fixed(chorus.list.short[a],
                                 pattern = '_',n=6)[,2],
                   str_split_fixed(chorus.list.short[a],
             pattern = '_',n=6)[,3],sep='_')



  TempAcoDiv <- cbind.data.frame(ndsi,acentin,res, selo, ACI,ADI,AEI,bioindex,duration)
  AcousticIndicesDF <- rbind.data.frame(AcousticIndicesDF,TempAcoDiv)
  write.csv(AcousticIndicesDF,'C:/Users/PC RACING/Desktop/Denacolab_2025/MFCC/clips_5s/AcousticIndicespups.csv',row.names = F)
}

AcousticIndicesDF$Individual <- as.factor(AcousticIndicesDF$Individual)

# BirdNET features --------------------------------------------------------

BirdNetFiles <-
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/PlaybackBirdNetEmbeddingsDownsample",
             full.names = T)

BirdNetFilesShort <-
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/PlaybackBirdNetEmbeddingsDownsample",
             full.names = F)

RecorderID <- str_split_fixed(BirdNetFilesShort,pattern = '_',n=2)[,1]

packid <- str_split_fixed(BirdNetFilesShort,'_',n=4)[,4]
packid <- str_split_fixed(packid,'_snr',n=2)[,1]

IndividualID <- paste(str_split_fixed(packid,'_',n=3)[,1],
                      str_split_fixed(packid,'_',n=3)[,2],sep='_')

RecordingID <- paste(str_split_fixed(BirdNetFilesShort,'_',n=4)[,1],
                     str_split_fixed(BirdNetFilesShort,'_',n=4)[,2],
                     str_split_fixed(BirdNetFilesShort,'_',n=4)[,3],
                     sep='_')

Recorder <- str_split_fixed(BirdNetFilesShort,'_',n=4)[,1]

BirdNetFeatures <- data.frame()

for(a in 1:length(BirdNetFiles)){ #
  print(paste(a, ' out of this many files', length(BirdNetFiles)))
  Individual <- IndividualID[a]
  print(Individual)
  TempDF <-read.table(BirdNetFiles[a])
  CommaSplit <- strsplit(TempDF$V3,split = ',')

  CombinedValsDF <- data.frame()
  for(b in 1:length(CommaSplit)){
    TempVec <- CommaSplit[[b]]
    CombinedVals <- data.frame()
    CombinedVals <- rbind.data.frame(CombinedVals,TempVec)
    colnames(CombinedVals) <- paste('var_', seq(1,length(TempVec),1),sep='' )
    CombinedValsDF <- rbind.data.frame(CombinedValsDF,CombinedVals)
  }

  CombinedValsDF <-
    CombinedValsDF %>% mutate_if(is.character,as.numeric)

  CombinedValsDFMean <- t(apply(CombinedValsDF,2,mean))
  CombinedValsSD <- t(apply(CombinedValsDF,2,sd))
  CombinedVals <-  cbind.data.frame(CombinedValsDFMean,CombinedValsSD)
  NewDataFrame <- data.frame()

  NewDataFrame <- rbind.data.frame(NewDataFrame,CombinedVals)
  colnames(NewDataFrame) <-  paste('var_', seq(1,ncol(NewDataFrame),1),sep='' )
  NewDataFrame$packid <- packid[a]
  NewDataFrame$Individual <- Individual
  NewDataFrame$Recorder  <- Recorder[a]
  NewDataFrame$Duration <- max(TempDF$V2)
  NewDataFrame$RecordingID <-RecordingID[a]
  BirdNetFeatures <- rbind.data.frame(BirdNetFeatures,NewDataFrame)
  write.csv(BirdNetFeatures,'/Users/denaclink/Desktop/RStudio Projects/Gibbon-feature-comparison/data/BirdNetFeaturesplayback.csv',row.names = F)
  rm(TempDF)
  rm(CommaSplit)
}



BirdNetFeatures$Individual <- as.factor(BirdNetFeatures$Individual)


# VGGish features ---------------------------------------------------------

TempfilesVGGish<-
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/VGGish",full.names = T,
             recursive = T)

VGGishDF <- data.frame()
for(i in 1:length(TempfilesVGGish)){
  print(i)
  Tempcsv <- read.table(TempfilesVGGish[i], sep = ',')
  Duration <- nrow(Tempcsv)
  n.slash  <- str_count(TempfilesVGGish[i], pattern = "/")[1] + 1

  Temp.name <- str_split_fixed(TempfilesVGGish[i],pattern = "/",n=n.slash)[,n.slash]
  Class <- str_split_fixed(Temp.name,pattern = '.snr',n=2)[,1]

  Individual <- paste(str_split_fixed(Class,
                                      pattern = '_',n=6)[,4],
                      str_split_fixed(Class,
                                      pattern = '_',n=6)[,5],sep='_')

  RecorderID <- paste(str_split_fixed(Class,
                                      pattern = '_',n=6)[,1],
                      str_split_fixed(Class,
                                      pattern = '_',n=6)[,2],
                      str_split_fixed(Class,
                                      pattern = '_',n=6)[,3],sep='_')

  TempcsvMean <- t(colMeans(Tempcsv))
  TempcsvSD <- t(apply(Tempcsv,2,sd))

  TempRow <- cbind.data.frame(TempcsvMean,TempcsvSD)


  colnames(TempRow) <- paste('V',seq(1,ncol(TempRow),1),sep='')

  TempRow$Individual <- Individual
  TempRow$Duration <- Duration
  TempRow$RecorderID <- RecorderID
  VGGishDF <- rbind.data.frame(VGGishDF,TempRow )
  write.csv(VGGishDF,'/Users/denaclink/Desktop/RStudio Projects/Gibbon-feature-comparison/data/VGGishDFPlayback.csv',row.names = F)
  rm(Tempcsv)
}

VGGishDF$Individual <- as.factor( VGGishDF$Individual)

# wav2vec2 features -------------------------------------------------------

TempfilesWav2Vec <-
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/wav2vec2featuresmeansd",full.names = T,
             recursive = T)

wav2vecDF <- data.frame()

for(i in 1:length(TempfilesWav2Vec)){
  print(i)
  TempWav2Vec <- read.csv(TempfilesWav2Vec[i])
  Temp.name <- TempWav2Vec$file[1]
  Class <- str_split_fixed(Temp.name,pattern = '.snr',n=2)[,1]

  TempWav2VecComb <-  cbind.data.frame(t(TempWav2Vec$mean),t(TempWav2Vec$std))
  colnames(TempWav2VecComb) <- paste0("y", 1:(ncol(TempWav2VecComb)))
  TempWav2VecComb$Individual <- paste(str_split_fixed(Class,
                                                  pattern = '_',n=6)[,4],
                                  str_split_fixed(Class,
                                                  pattern = '_',n=6)[,5],sep='_')

  TempWav2VecComb$RecorderID <- paste(str_split_fixed(Class,
                                                  pattern = '_',n=6)[,1],
                                  str_split_fixed(Class,
                                                  pattern = '_',n=6)[,2],
                                  str_split_fixed(Class,
                                                  pattern = '_',n=6)[,3],sep='_')

  TempWav2VecComb$duration <-  TempWav2Vec$duration[1]

  wav2vecDF <- rbind.data.frame(wav2vecDF,TempWav2VecComb)

  write.csv(wav2vecDF,'data/wav2vecmeansdDFPlayback.csv',row.names = F)
}



wav2vecDF$Individual <- as.factor( wav2vecDF$Individual)


# Running the long ones on their own --------------------------------------

library(gibbonR)
library(caret)
library(soundecology)
library(dplyr)
library(stringr)
library(ggplot2)


# Set input directory
input.dir.wavs <- "/Volumes/DJC 1TB/VocalIndividualityClips/SoundFilesDownsample"

# BirdNET features --------------------------------------------------------

BirdNetFilesAll <-
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/PlaybackBirdNetEmbeddingsDownsample",
             full.names = T)

BirdNetFilesShort <-
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/PlaybackBirdNetEmbeddingsDownsample",
             full.names = F)

RecorderID <- str_split_fixed(BirdNetFilesShort,pattern = '_',n=2)[,1]



Recorder <- str_split_fixed(BirdNetFilesShort,'_',n=4)[,1]


UniqueRecorder <- unique(Recorder)

for(j in 1:length(UniqueRecorder)){

  RecorderIndex <-   which(Recorder ==UniqueRecorder[j])

  TempfilesBirdNet <- BirdNetFilesAll[RecorderIndex]
  TempfilesBirdNetShort <- BirdNetFilesShort[RecorderIndex]

  packid <- str_split_fixed(TempfilesBirdNetShort,'_',n=4)[,4]
  packid <- str_split_fixed(packid,'_snr',n=2)[,1]

  IndividualID <- paste(str_split_fixed(packid,'_',n=3)[,1],
                        str_split_fixed(packid,'_',n=3)[,2],sep='_')

  RecordingID <- paste(str_split_fixed(BirdNetFilesShort,'_',n=4)[,1],
                       str_split_fixed(BirdNetFilesShort,'_',n=4)[,2],
                       str_split_fixed(BirdNetFilesShort,'_',n=4)[,3],
                       sep='_')

BirdNetFeatures <- data.frame()

for(a in 1:length(TempfilesBirdNet)){ #
  print(paste(a, ' out of this many files', length(TempfilesBirdNet)))
  TempDF <-read.table(TempfilesBirdNet[a])
  CommaSplit <- strsplit(TempDF$V3,split = ',')

  CombinedValsDF <- do.call(rbind.data.frame, lapply(CommaSplit, function(TempVec) {
    TempVec <- as.numeric(TempVec)
    data.frame(t(TempVec), stringsAsFactors = FALSE)
  }))


  CombinedValsDF <-
    CombinedValsDF %>% mutate_if(is.character,as.numeric)

  CombinedValsDFMean <- t(apply(CombinedValsDF,2,mean))
  CombinedValsSD <- t(apply(CombinedValsDF,2,sd))
  CombinedVals <-  cbind.data.frame(CombinedValsDFMean,CombinedValsSD)

  NewDataFrame <- data.frame((CombinedVals), Individual = IndividualID[a], Recorder = UniqueRecorder[j], Duration = max(TempDF$V2), RecordingID = RecordingID[a], stringsAsFactors = FALSE)

  BirdNetFeatures <- rbind.data.frame(BirdNetFeatures,NewDataFrame)
  FileName <- paste('/Users/denaclink/Desktop/RStudio Projects/Gibbon-feature-comparison/data/',
                    j,'_','BirdNETDFPlayback.csv')
  write.csv(BirdNetFeatures,FileName,row.names = F)
  rm(TempDF)
  rm(CommaSplit)
}

}

BirdNetFeatures$Individual <- as.factor(BirdNetFeatures$Individual)


# VGGish features ---------------------------------------------------------

TempfilesVGGishAll<-
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/VGGish",full.names = T,
             recursive = T)

TempfilesVGGishShort<-
  list.files("/Volumes/DJC 1TB/VocalIndividualityClips/VGGish",full.names = F,
             recursive = T)

Recorder <- str_split_fixed(TempfilesVGGishShort,pattern = '_', n=2)[,1]
UniqueRecorder <- unique(Recorder)

for(j in 1:length(UniqueRecorder)){

RecorderIndex <-   which(Recorder ==UniqueRecorder[j])

TempfilesVGGish <- TempfilesVGGishAll[RecorderIndex]

VGGishDF <- data.frame()
for(i in 1:length(TempfilesVGGish)){
  print(i)
  Tempcsv <- read.table(TempfilesVGGish[i], sep = ',')
  Duration <- nrow(Tempcsv)
  n.slash  <- str_count(TempfilesVGGish[i], pattern = "/")[1] + 1

  Temp.name <- str_split_fixed(TempfilesVGGish[i],pattern = "/",n=n.slash)[,n.slash]
  Class <- str_split_fixed(Temp.name,pattern = '.snr',n=2)[,1]

  Individual <- paste(str_split_fixed(Class,
                                      pattern = '_',n=6)[,4],
                      str_split_fixed(Class,
                                      pattern = '_',n=6)[,5],sep='_')

  RecorderID <- paste(str_split_fixed(Class,
                                      pattern = '_',n=6)[,1],
                      str_split_fixed(Class,
                                      pattern = '_',n=6)[,2],
                      str_split_fixed(Class,
                                      pattern = '_',n=6)[,3],sep='_')

  TempcsvMean <- t(colMeans(Tempcsv))
  TempcsvSD <- t(apply(Tempcsv,2,sd))

  TempRow <- cbind.data.frame(TempcsvMean,TempcsvSD)


  colnames(TempRow) <- paste('V',seq(1,ncol(TempRow),1),sep='')

  TempRow$Individual <- Individual
  TempRow$Duration <- Duration
  TempRow$RecorderID <- RecorderID
  VGGishDF <- rbind.data.frame(VGGishDF,TempRow )
  FileName <- paste('/Users/denaclink/Desktop/RStudio Projects/Gibbon-feature-comparison/data/',
  j,'_','VGGishDFPlayback.csv')
  write.csv(VGGishDF,FileName,row.names = F)
  rm(Tempcsv)
}
}


VGGishDF$Individual <- as.factor( VGGishDF$Individual)

