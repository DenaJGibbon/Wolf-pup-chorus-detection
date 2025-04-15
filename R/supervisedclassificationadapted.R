# Load necessary libraries
library(randomForest)
library(caret)
library(stringr)
library(umap)
library(apcluster)

# Read MFCCpups.csv (update file path if needed)
MFCCpups <- read.csv('data/MFCCpups_9windows.csv')

MFCCpups$type <- MFCCpups$Folder

# Extract the recorder information from the 'Recording' column
MFCCpups$Recording <- str_split_fixed(MFCCpups$Recording, pattern = '_', n = 1)[, 1]

# Filter dataset to include only 'Pups' and 'NoPups' in the 'type' column
MFCCpups_filtered <- subset(MFCCpups, type %in% c("Pups", "NoPups"))

# Convert 'type' column to a factor (classification labels)
MFCCpups_filtered$type <- as.factor(MFCCpups_filtered$type)

# Convert the feature columns (MFCC values) to numeric, excluding 'Recording' and 'type' columns
# Ensure we only convert the correct columns by selecting numeric columns starting from the 3rd column onward
#MFCCpups_filtered_numeric <- MFCCpups_filtered[, -c(1, 2)]  # Remove 'Recording' and 'type' columns

# # Convert all feature columns to numeric with the comma as the decimal separator
# MFCCpups_filtered[, -c(1, 2)] <- lapply(MFCCpups_filtered[, -c(1, 2)], function(x) {
#   # Replace comma with period and then convert to numeric
#   as.numeric(gsub(",", ".", as.character(x)))
# })

# Check the structure of the numeric data to ensure it's correct
#str(MFCCpups_filtered)

# HaMFCCpups_filtered# Handle any potential NAs that may have been introduced during the conversion process (if any)
# You can choose to remove rows with NAs or impute them based on your analysis needs.
# if(any(is.na(MFCCpups_filtered))) {
#   MFCCpups_filtered <- na.omit(MFCCpups_filtered)  # Remove rows with NAs
# }

# Set a seed for reproducibility
set.seed(0)

# Split the data into training and test sets (80% train, 20% test)
trainLength <- 0.8 * nrow(MFCCpups_filtered)
trainIndex <- sample(1:nrow(MFCCpups_filtered),  trainLength, replace = FALSE)
TrainingData <- MFCCpups_filtered[trainIndex, ]
TestData <- MFCCpups_filtered[-trainIndex, ]

colnames(TrainingData)

# Build a random forest model using the training data
ml.model.rf.mfcc.original <- randomForest::randomForest(
  x = TrainingData[, -c(1:3,181:183)],
  y = TrainingData$type,         # Target variable is 'type' (Pups or NoPups)
  ntree = 1000,                   # Number of trees in the random forest
  random_state = 0,
  keep.forest = TRUE
)

# Print the Random Forest model summary
print(ml.model.rf.mfcc.original)

# Make predictions on the test data
TestPredictions <- predict(ml.model.rf.mfcc.original, newdata = TestData[, -c(1, 2)])

# Compute accuracy of the predictions
ConfMatrix <- confusionMatrix(TestData$type, TestPredictions, mode = 'everything')
ConfMatrix
AccuracyVal <- ConfMatrix$overall[1]

# Print accuracy results
print(paste("Accuracy: ", AccuracyVal))


# Plot results ------------------------------------------------------------

AcousticSignals.umap <-
  umap::umap(
    TrainingData[, -c(1:3,181:183)],
    n_neighbors = 12,
    controlscale = TRUE,
    scale = 3
  )

plot.for.AcousticSignals <-
  cbind.data.frame(AcousticSignals.umap$layout[, 1:2],
                   TrainingData$type)

colnames(plot.for.AcousticSignals) <-
  c("Dim.1", "Dim.2", "class")

plot.for.AcousticSignals$class <-
  as.factor(plot.for.AcousticSignals$class)

ggpubr::ggscatter(
    data = plot.for.AcousticSignals,
    x = "Dim.1",
    y = "Dim.2",
    color  = "class"
)


