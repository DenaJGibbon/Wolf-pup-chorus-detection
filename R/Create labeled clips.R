# Load required libraries
library(tidyverse)  # For data manipulation
library(stringr)    # For string operations
library(tuneR)      # For reading and processing WAV files
library(seewave)    # For working with acoustic signals
library(dplyr)      # For data wrangling

# Read in observer data from an Excel file
pups_segments_MFCC_DF <- as.data.frame(readxl::read_xlsx('data/pups_segments_MFCC.xlsx'))

# Display the first few rows of the dataset to check structure
head(pups_segments_MFCC_DF)

# Specify the output directory where audio clips will be saved
OutputDir <- 'data/wolfclips/'

# Calculate the sum of observer agreement (columns 8 to 10)
pups_segments_MFCC_DF$SumObservation <- rowSums(pups_segments_MFCC_DF[,c(8:10)])

# Assign categories based on observer agreement:
# - 0 = "NoPups" (no pup sounds detected)
# - 3 = "Pups" (all observers agree there are pup sounds)
# - Other values = "Unsure" (disagreement among observers)
pups_segments_MFCC_DF$Class  <- case_when(
  pups_segments_MFCC_DF$SumObservation  == 0 ~ "NoPups",
  pups_segments_MFCC_DF$SumObservation  == 3 ~ "Pups",
  TRUE ~ "Unsure"  # Default case for uncertainty
)

# Display the first few rows again to check the new classification
head(pups_segments_MFCC_DF)

# List all .WAV files in the 'segmentswolves' directory
FullWavNames <- list.files('/Users/denaclink/Downloads/wolfchorus/',
                           pattern = '.WAV', full.names = TRUE)

# Extract short names from full file paths (remove the .WAV extension)
ShortWavNames <- basename(FullWavNames)
ShortWavNames <- str_split_fixed(ShortWavNames, pattern = '.WAV', n=2)[,1]

# Find unique recording IDs in the dataset
UniqueRecording <- unique(pups_segments_MFCC_DF$`Record ID`)

# Loop through each unique recording ID to process the corresponding WAV file
for(i in 1:length(UniqueRecording)){
  # Subset the data for the current recording ID
  SingleRecordingSubset <- subset(pups_segments_MFCC_DF, `Record ID` == UniqueRecording[i])

  # Find the matching full WAV file path
  SingleFullWavPath <- FullWavNames[which(ShortWavNames %in% UniqueRecording[i])]

  # Process the WAV file if a match is found
  if(length(SingleFullWavPath) > 0){
    TempWav <- readWave(SingleFullWavPath)  # Read the WAV file

    # Define segment start times (5-second intervals)
    SegmentSequence <- seq(0, (nrow(SingleRecordingSubset) * 5 + 5), 5)

    duration( TempWav)
    # Loop through each segment and extract the corresponding audio clip
    for(j in 1:nrow(SingleRecordingSubset)){
      # Cut the segment from the full WAV file
      TempWavShort <- cutw(TempWav,
                           from = SegmentSequence[j],
                           to = SegmentSequence[j + 1],
                           output = 'Wave')

      # Create a directory for storing clips based on the classification (Pups, NoPups, Unsure)
      ClipDirectory <- paste(OutputDir, SingleRecordingSubset[j,]$Class, sep = '')
      dir.create(ClipDirectory, recursive = TRUE, showWarnings = FALSE)  # Avoid errors if the folder exists

      # Define the output filename based on Record ID and Segment
      WavName <- paste(SingleRecordingSubset[j,]$`Record ID`, '_',
                       SingleRecordingSubset[j,]$Segm, '.wav', sep = '')

      # Save the segmented WAV file to the appropriate directory
      writeWave(TempWavShort, paste(ClipDirectory, WavName, sep = '/'))
    }
  }
}
