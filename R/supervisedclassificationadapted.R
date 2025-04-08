# Load necessary libraries
library(randomForest)
library(caret)
library(stringr)
library(umap)
library(apcluster)

# Read MFCCpups.csv (update file path if needed)
MFCCpups <- read.csv('data/MFCCpups2.csv')

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
if(any(is.na(MFCCpups_filtered))) {
  MFCCpups_filtered <- na.omit(MFCCpups_filtered)  # Remove rows with NAs
}

# Set a seed for reproducibility
set.seed(0)

# Split the data into training and test sets (80% train, 20% test)
trainLength <- 0.8 * nrow(MFCCpups_filtered)
trainIndex <- sample(1:nrow(MFCCpups_filtered),  trainLength, replace = FALSE)
TrainingData <- MFCCpups_filtered[trainIndex, ]
TestData <- MFCCpups_filtered[-trainIndex, ]

# Build a random forest model using the training data
ml.model.rf.mfcc.original <- randomForest::randomForest(
  x = TrainingData[, -c(1, 2)],
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






# Load necessary libraries
library(caret)
library(randomForest)

# Define the cross-validation setup
train_control <- trainControl(method = "cv", number = 5)  # 5-fold cross-validation

# Define a grid for tuning 'mtry'
tune_grid <- expand.grid(mtry = c(2, 4, 6, 8, 10))

# Train the Random Forest model with tuning
rf_tuned <- train(
  type ~ .,
  data = TrainingData,
  method = "rf",
  trControl = train_control,
  tuneGrid = tune_grid
)

# View the best model and results
print(rf_tuned)




# Train the Random Forest model with additional parameters like 'ntree' and 'nodesize'
rf_tuned <- train(
  type ~ .,
  data = TrainingData,
  method = "rf",
  trControl = train_control,
  tuneGrid = tune_grid,
  ntree = 1000,  # You can specify the number of trees directly
  nodesize = 5   # You can specify the minimum size of nodes directly
)

# View the results
print(rf_tuned)














# Store accuracy and recorder information (for your analysis or logging)
Recorder <- "Overall"  # You can use this for the overall model or loop per recorder
Features <- 'MFCC'

# Ensure that the numeric data is clean and ready for UMAP
AcousticSignals.umap <- umap::umap(MFCCpups_filtered, controlscale = TRUE, scale = 3, n_neighbors = 10)

# Now apply apcluster to the layout (2D representation from UMAP)
TempCluster <- apcluster::apcluster(
  apcluster::negDistMat(r = 2),  # Set the distance function
  AcousticSignals.umap$layout[, 1:2],  # Use the first two dimensions from UMAP layout
  maxits = 5000,
  convits = 500,
  nonoise = TRUE
)

# Check the result of apcluster
print(TempCluster)


# Get the cluster assignments from apcluster
cluster_assignments <- TempCluster@idx

# Check if the lengths match
if (length(cluster_assignments) != length(TrainingData$type)) {
  stop("The lengths of the clustering results and the true labels do not match.")
}
print(cluster_assignments[1:10])  # Print first 10 cluster assignments
print(TrainingData$type[1:10])   # Print first 10 true labels

# Compute the Normalized Mutual Information (NMI) using the cluster assignments and true labels
NMI.val <- NMI(cluster_assignments, TrainingData$type)  # Compute Normalized Mutual Information

# Print NMI result
print(paste("NMI: ", NMI.val))

# Get the number of clusters and other statistics
N.cluster <- length(unique(TempCluster@idx))
N.Individual <- length(unique(TrainingData$type))  # 'type' represents the individual categories
NMI.val <- NMI(TempCluster@idx, TrainingData$type)  # Compute Normalized Mutual Information

# Create a row of results for logging or further analysis
TempMFCCRow <- data.frame(AccuracyVal, Recorder, Features, N.cluster, N.Individual, NMI.val)

# Output results to CSV (optional)
write.csv(TempMFCCRow, 'C:/Users/PC RACING/Desktop/Denacolab_2025/MFCC/clips_5s/results_data/results_mfcc/RandomizationAccuracyMFCC.csv', row.names = FALSE)

# Print the result row
print(TempMFCCRow)













  # Clear all objects from the environment
  rm(list = ls())










# Load the data from the specified file path, using read.csv2() for comma as decimal separator
file_path <- "C:/Users/PC RACING/Desktop/Denacolab_2025/MFCC/clips_5s/results_data/results_mfcc/MFCCpups.csv"
MFCCpups <- read.csv2(file_path)  # This will correctly read the comma as decimal separator

# View the first few rows of the dataset to ensure it's loaded correctly
head(MFCCpups)

# Convert all the MFCC columns (from 3 to the last column) to numeric
MFCCpups[, 3:ncol(MFCCpups)] <- lapply(MFCCpups[, 3:ncol(MFCCpups)], function(x) as.numeric(gsub(",", ".", x)))

# View the first few rows to check that conversion is successful
head(MFCCpups)

# Filter dataset to include only 'Pups' and 'NoPups' in the 'type' column
MFCCpups_filtered <- subset(MFCCpups, type %in% c("NoPups","Pups"))

# Convert 'type' column to a factor (classification labels)
MFCCpups_filtered$type <- as.factor(MFCCpups_filtered$type)

# Split the data into training (80%) and test (20%) sets
set.seed(123)
TrainingN <- 0.8 * nrow(MFCCpups_filtered)  # 80% for training
TrainingSamples <- sample(seq(1, nrow(MFCCpups_filtered)), TrainingN, replace = FALSE)
TrainingData <- droplevels(MFCCpups_filtered[TrainingSamples, ])
TestData <- droplevels(MFCCpups_filtered[-TrainingSamples, ])

# Build a Random Forest model using the training data
pup_model <- randomForest::randomForest(
  x = TrainingData[, 3:ncol(MFCCpups_filtered)],  # Features (MFCC columns)
  y = TrainingData$type,  # Target variable (Type) - use the 'type' column
  ntree = 1000,  # Number of trees in the forest
  random_state = 0, #better result without
  keep.forest = TRUE , # Keeping the forest for later use
  classwt = c("NoPups" = 2, "Pups" = 1)
)

# Make predictions on the test data
TestPredictions <- predict(pup_model, newdata = TestData[, 3:ncol(MFCCpups_filtered)])

# Print the first few predictions
head(TestPredictions)

# You can also compute the confusion matrix to assess the performance

ConfMatrix <- confusionMatrix(TestPredictions, TestData$type, positive = "Pups", mode = 'everything')
print(ConfMatrix)

# Compute accuracy of the predictions
library(caret)
ConfMatrix <- confusionMatrix(TestPredictions, TestData$Type,  mode = 'everything')
AccuracyVal <- ConfMatrix$overall[1]  # Accuracy is in the first index
print(AccuracyVal)
# Store accuracy and other details
Recorder <- unique(TrainingData$Recording)  # The recording names (you can adjust this as needed)
Features <- 'MFCC'  # Feature type, since you're using MFCC
N.cluster <- length(unique(TrainingData$Type))  # Number of unique clusters (types)
N.Individual <- length(unique(TrainingData$Recording))  # Number of unique recordings
NMI.val <- NMI(TestPredictions, TestData$Type)  # Normalized Mutual Information, if you want to measure clustering quality

# Combine results into a data frame for easy tracking
TempMFCCRow <- cbind.data.frame(AccuracyVal, Recorder, Features, N.cluster, N.Individual, NMI.val)

# Store results into a global dataframe (if you're looping through multiple scenarios)
RandomizationAccuracyMFCC <- rbind.data.frame(RandomizationAccuracyMFCC, TempMFCCRow)

# Print and save the results to a CSV file
print(TempMFCCRow)
write.csv(RandomizationAccuracyMFCC, 'data/randomization_affinity/RandomizationAccuracyMFCC.csv')
















