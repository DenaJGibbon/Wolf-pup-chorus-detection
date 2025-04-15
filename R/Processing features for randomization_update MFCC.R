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
n.windows <- 9
num.cep <- 12

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
  wav.dur <- duration(short.wav)
  win.time <- wav.dur / n.windows

  # Calculate MFCCs
  melfcc.output <-
    tuneR::melfcc(
      short.wav,
      minfreq = 400,
      hoptime = win.time,
      maxfreq = 1300,
      numcep = num.cep,
      wintime = win.time
    )

  # Calculate delta cepstral coefficients
  deltas.output <- tuneR::deltas(melfcc.output)

  # Ensure only same number of time windows are used for MFCC and delta coefficients Also append .wav duration
  mfcc.vector <-
    c(as.vector(t(melfcc.output[1:(n.windows - 1), 2:num.cep])), as.vector(t(deltas.output[1:(n.windows - 1), 2:num.cep])), wav.dur)

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
  TempMFCCRow <- cbind.data.frame(PackID, datetime, segmentnumber, t(mfcc.vector), Recording = short_file_name, Folder = folder_name)

  # Append the result to the data frame
  mfcc.vector.df <- rbind.data.frame(mfcc.vector.df, TempMFCCRow)
}

print(mfcc.vector.df)
write.csv(mfcc.vector.df,'data/MFCCpups_9windows.csv',row.names = F)
#write.csv(mfcc.vector.df,'C:/Users/PC RACING/Desktop/Denacolab_2025/MFCC/clips_5s/results_data/MFCCpups2.csv',row.names = F)

# Acoustic Indices --------------------------------------------------------
# input.dir.wavs <- "C:/Users/PC RACING/Desktop/Denacolab_2025/MFCC/clips_5s/wolfclips/Pups"
#
# chorus.list <-
#   list.files(input.dir.wavs, full.names = T, pattern = '.wav')
#
# chorus.list.short <-
#   list.files(input.dir.wavs, full.names = F, pattern = '.wav')
#
# AcousticIndicesDF <- data.frame()
#
# for (a in 1:length(chorus.list)) {
#   print(a)
#   short.wav <- readWave(chorus.list[a])
#
#
#
#
#   acentin<-H(short.wav, min_freq = 300, max_freq = 1800)
#
#   res <- meanspec(short.wav, min_freq = 300, max_freq = 1800, wl=1024, ovlp=87.5, plot=FALSE)
#
#   selo<-specprop(res, plot=FALSE)
#
#   ACI <- acoustic_complexity(short.wav, min_freq=500, , max_freq = 1800)$AciTotAll_left
#
#   ADI <- acoustic_diversity(short.wav, , max_freq = 1800)$adi_left
#
#   AEI <- acoustic_evenness(short.wav, , max_freq = 1800)$aei_left
#
#   bioindex <- bioacoustic_index(short.wav, min_freq=300, , max_freq = 1800)$left_area
#
#   duration <- seewave::duration(short.wav)
#
#
#
#
#   Individual <- paste(str_split_fixed(chorus.list.short[a],
#                  pattern = '_',n=6)[,4],
#   str_split_fixed(chorus.list.short[a],
#                  pattern = '_',n=6)[,5],sep='_')
#  RecorderID <- paste(str_split_fixed(chorus.list.short[a],
#                    pattern = '_',n=6)[,1],
#    str_split_fixed(chorus.list.short[a],
#                                  pattern = '_',n=6)[,2],
#                    str_split_fixed(chorus.list.short[a],
#              pattern = '_',n=6)[,3],sep='_')
#
#
#
#   TempAcoDiv <- cbind.data.frame(ndsi,acentin,res, selo, ACI,ADI,AEI,bioindex,duration)
#   AcousticIndicesDF <- rbind.data.frame(AcousticIndicesDF,TempAcoDiv)
#   write.csv(AcousticIndicesDF,'C:/Users/PC RACING/Desktop/Denacolab_2025/MFCC/clips_5s/AcousticIndicespups.csv',row.names = F)
# }
#
# AcousticIndicesDF$Individual <- as.factor(AcousticIndicesDF$Individual)

