library(stringr)
library(dplyr)
# BirdNET features --------------------------------------------------------

BirdNetFiles <-
  list.files("/Volumes/DJC Files/Totalclips5sec_pups_nopups_JV_BM/output/",
             full.names = T,recursive = T)

BirdNetFilesShort <-
  list.files("/Volumes/DJC Files/Totalclips5sec_pups_nopups_JV_BM/output/",
             full.names = F,recursive = T)

CallTypeAll <- str_split_fixed(BirdNetFilesShort,pattern = '/',n=2)[,1]


BirdNetFeatures <- data.frame()

for(a in 1:length(BirdNetFiles)){ #
  print(paste(a, ' out of this many files', length(BirdNetFiles)))
  CallType <- CallTypeAll[a]
  print(CallType)
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
  NewDataFrame$CallType <- CallTypeAll[a]

  BirdNetFeatures <- rbind.data.frame(BirdNetFeatures,NewDataFrame)
  write.csv(BirdNetFeatures,'data/BirdNetFeatures.csv',row.names = F)
  rm(TempDF)
  rm(CommaSplit)
}




