#DATA PREPROCESSING
#Exploratoty Data Analysis(EDA)

#Loading Necessary Libraries
library(tidyverse)
library(caret)
library(ggplot2)
library(ggcorrplot)
library(gridExtra)
library(randomForest)
library(reshape2)
library(GGally)
library(survival)
library(randomForestSRC)
library(gbm)

#Importing Raw Dataset
Breast_Cancer_Data <- read_csv("MSC PROJECT/Breast_Cancer.csv")

# Select only the numerical columns
numerical_summary <- summary(Breast_Cancer_Data[, sapply(Breast_Cancer_Data, is.numeric)])

# Display the summary
print(numerical_summary)

#View the 1st five rows of our dataset
head(Breast_Cancer_Data)

View(Breast_Cancer_Data)

#Checking for the shape 
dim(Breast_Cancer_Data)

#Checking the structure of my dataset
str(Breast_Cancer_Data)

#Clean Column Names
colnames(Breast_Cancer_Data) <- make.names(colnames(Breast_Cancer_Data), unique = TRUE)
print("Updated Column Names:")
print(colnames(Breast_Cancer_Data))

#Check for missing values
sum(is.na(Breast_Cancer_Data))

#CHECK FOR UNIQUE VALUES
# Check for categorical features (character or factor columns)
categorical_columns <- names(Breast_Cancer_Data)[sapply(Breast_Cancer_Data, function(x) is.character(x) || is.factor(x))]
# Loop through each categorical feature and print its unique value counts
for (col in categorical_columns) {
  cat(paste(col, "unique values:\n"))
  print(table(Breast_Cancer_Data[[col]]))  # Count occurrences of each unique value
  cat(strrep("_", 30), "\n")  # Add a separator line
}


#ANALYSIS VISUALIZATION
#PLOTTING FOR VISUALIZATION
#Having TWO OR MORE PLOTS IN SAME WINDOW
par(mfrow=c(2,2))

hist(Breast_Cancer_Data$Age,col="purple")
hist(Breast_Cancer_Data$Tumor.Size,col="blue")
hist(Breast_Cancer_Data$Regional.Node.Examined,col="green")
hist(Breast_Cancer_Data$Reginol.Node.Positive,col="red")
hist(Breast_Cancer_Data$Survival.Months,col="orange")

#Checking For Outliers Using Boxplot
# List of continuous columns
continuous_columns <- c("Age", "Tumor.Size", "Regional.Node.Examined", "Reginol.Node.Positive", "Survival.Months")
# Create a list of ggplot boxplots for each continuous variable
boxplots <- lapply(continuous_columns, function(col) {
  ggplot(Breast_Cancer_Data, aes_string(y = col)) +
    geom_boxplot() +
    ggtitle(paste("Boxplot of", col)) +
    theme_minimal() +
    theme(axis.title.x = element_blank(), axis.text.x = element_blank())
})
# Arrange all the boxplots in a grid
grid.arrange(grobs = boxplots, ncol = 2)


#HANDLING OUTLIERS
#By capping extreme values
cap_outliers <- function(x) {
  qnt <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  caps <- c(qnt[1] - 1.5 * IQR(x), qnt[2] + 1.5 * IQR(x))
  x[x < caps[1]] <- caps[1]
  x[x > caps[2]] <- caps[2]
  return(x)
}

Breast_Cancer_Data[continuous_columns] <- lapply(Breast_Cancer_Data[continuous_columns], cap_outliers)

#view for correlation
#select numeric columns
numeric_data <- Breast_Cancer_Data[sapply(Breast_Cancer_Data, is.numeric)]
# Calculate the correlation matrix
cor_matrix <- cor(numeric_data, use = "complete.obs")
# Reshape the correlation matrix into a format suitable for ggplot
melted_cor_matrix <- melt(cor_matrix)
# Create the heatmap using geom_tile
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(color = "white") +                # Add white borders around boxes
  scale_fill_gradient2(
    low = "blue", mid = "white", high = "red", midpoint = 0, 
    limits = c(-1, 1),                        # Force scale from -1 to 1
    name = "Correlation"
  ) +
  theme_minimal() +                           # Clean theme
  labs(
    title = "Correlation Heatmap of Numerical Features",
    x = "Features",
    y = "Features"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),  # Rotate x-axis labels
    axis.text.y = element_text(size = 10),              # Adjust y-axis font size
    panel.grid = element_blank()                       # Remove grid lines
  )

# Create the count plot
ggplot(Breast_Cancer_Data, aes(x = Status, fill = Status)) +
  geom_bar() +                              # Create a bar plot
  scale_fill_manual(values = c("Alive" = "green", "Dead" = "red")) +  # Set colors
  labs(
    title = "Countplot of Survival Status",
    x = "Status",
    y = "Count"
  ) +
  theme_minimal() +                         # Apply a minimal theme
  theme(
    plot.title = element_text(hjust = 0.5), # Center the title
    axis.text = element_text(size = 12),    # Adjust axis text size
    axis.title = element_text(size = 14)    # Adjust axis title size
  )


#BREAKDWON OF SURVIVAL FEATURES BY CATEGORICAL VARIBALES
# Ensure column names are valid
colnames(Breast_Cancer_Data) <- make.names(colnames(Breast_Cancer_Data))
# Identify categorical columns
categorical_columns <- names(Breast_Cancer_Data)[sapply(Breast_Cancer_Data, is.character)]
# Set up the plotting area for multiple plots (2 rows, 2 columns)
par(mfrow = c(2, 2), mar = c(4, 4, 2, 1))  # Adjust margins for better spacing
# Loop through the first 4 categorical features and plot
for (i in 1:min(4, length(categorical_columns))) {
  feature <- categorical_columns[i]
  
  # Get unique categories for the feature
  categories <- unique(Breast_Cancer_Data[[feature]])
  colors <- rainbow(length(categories))  # Dynamically assign distinct colors
  
  # Generate the bar plot for each categorical feature
  barplot(
    table(Breast_Cancer_Data[[feature]], Breast_Cancer_Data$Status), 
    beside = TRUE,                      # Side-by-side bars
    col = colors,                       # Assign dynamic colors
    legend = categories,                # Use unique categories as legend
    main = paste("Survival Status by", feature),
    xlab = feature,
    ylab = "Count"
  )
}
# Reset to default layout
par(mfrow = c(1, 1))

#CREATING PAIRPLOT FOR MY VARIABLES 
# Select the numerical columns and include the hue (Status) column
numerical_columns <- c("Age", "Tumor.Size", "Regional.Node.Examined", "Reginol.Node.Positive", "Survival.Months","Status")
selected_data <- Breast_Cancer_Data[, numerical_columns]
# Create the pairplot
ggpairs(
  data = selected_data,
  columns = 1:5,              # Numerical columns to plot
  aes(color = Status),        # Map Status to color
  title = "Pairplot of Numerical Features by Survival Status"
)


