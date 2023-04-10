#
# Selection of Premium Wines
# David List, 2023
#
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(Rborist)) install.packages("Rborist", repos = "http://cran.us.r-project.org")
if(!require(ggrepel)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(gridExtra)) install.packages("gridExtra", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(caret)
library(randomForest)
library(ggrepel)
library(grid)
library(gridExtra)

###
### 2.1 Data cleaning
###

# Load the red wines.
wines_full <- read_delim('winequality-red.csv',
                    delim = ';',
                    col_types = cols(
                      `fixed acidity` = col_double(),
                      `volatile acidity` = col_double(),
                      `citric acid` = col_double(),
                      `residual sugar` = col_double(),
                      chlorides = col_double(),
                      `free sulfur dioxide` = col_number(),
                      `total sulfur dioxide` = col_number(),
                      density = col_double(),
                      pH = col_double(),
                      sulphates = col_double(),
                      alcohol = col_double(),
                      quality = col_number()))

# Get rid of the spaces from the column names.
colnames(wines_full) <- c('fixed_acidity', 'volatile_acidity', 'citric_acid',
                          'residual_sugar', 'chlorides', 'free_sulfur_dioxide',
                          'total_sulfur_dioxide', 'density', 'pH', 'sulphates',
                          'alcohol', 'quality')

# Verify the data has no NA values, and calculate dataset statistics.
dc_df <- data.frame(Description = "Fixed Acidity (g/L)", NAs = sum(is.na(wines_full$fixed_acidity)),
                    Min = min(wines_full$fixed_acidity), Max = max(wines_full$fixed_acidity),
                    Mean = mean(wines_full$fixed_acidity), SD = sd(wines_full$fixed_acidity))
dc_df[nrow(dc_df) + 1,] <- list("Volatile Acidity (g/L)", sum(is.na(wines_full$volatile_acidity)),
                                min(wines_full$volatile_acidity), max(wines_full$volatile_acidity),
                                mean(wines_full$volatile_acidity), sd(wines_full$volatile_acidity))
dc_df[nrow(dc_df) + 1,] <- list("Citric Acid (g/L)", sum(is.na(wines_full$citric_acid)),
                                min(wines_full$citric_acid), max(wines_full$citric_acid),
                                mean(wines_full$citric_acid), sd(wines_full$citric_acid))
dc_df[nrow(dc_df) + 1,] <- list("Residual Sugar (g/L)", sum(is.na(wines_full$residual_sugar)),
                                min(wines_full$residual_sugar), max(wines_full$residual_sugar),
                                mean(wines_full$residual_sugar), sd(wines_full$residual_sugar))
dc_df[nrow(dc_df) + 1,] <- list("Chlorides (g/L)", sum(is.na(wines_full$chlorides)),
                                min(wines_full$chlorides), max(wines_full$chlorides),
                                mean(wines_full$chlorides), sd(wines_full$chlorides))
dc_df[nrow(dc_df) + 1,] <- list("Free Sulfur Dioxide (mg/L)", sum(is.na(wines_full$free_sulfur_dioxide)),
                                min(wines_full$free_sulfur_dioxide), max(wines_full$free_sulfur_dioxide),
                                mean(wines_full$free_sulfur_dioxide), sd(wines_full$free_sulfur_dioxide))
dc_df[nrow(dc_df) + 1,] <- list("Total Sulfur Dioxide (mg/L)", sum(is.na(wines_full$total_sulfur_dioxide)),
                                min(wines_full$total_sulfur_dioxide), max(wines_full$total_sulfur_dioxide),
                                mean(wines_full$total_sulfur_dioxide), sd(wines_full$total_sulfur_dioxide))
dc_df[nrow(dc_df) + 1,] <- list("Density (g/mL)", sum(is.na(wines_full$density)),
                                min(wines_full$density), max(wines_full$density),
                                mean(wines_full$density), sd(wines_full$density))
dc_df[nrow(dc_df) + 1,] <- list("pH", sum(is.na(wines_full$pH)),
                                min(wines_full$pH), max(wines_full$pH),
                                mean(wines_full$pH), sd(wines_full$pH))
dc_df[nrow(dc_df) + 1,] <- list("Sulphates (g/L)", sum(is.na(wines_full$sulphates)),
                                min(wines_full$sulphates), max(wines_full$sulphates),
                                mean(wines_full$sulphates), sd(wines_full$sulphates))
dc_df[nrow(dc_df) + 1,] <- list("Alcohol (% volume)", sum(is.na(wines_full$alcohol)),
                                min(wines_full$alcohol), max(wines_full$alcohol),
                                mean(wines_full$alcohol), sd(wines_full$alcohol))
dc_df[nrow(dc_df) + 1,] <- list("Quality", sum(is.na(wines_full$quality)),
                                min(wines_full$quality), max(wines_full$quality),
                                mean(wines_full$quality), sd(wines_full$quality))
dc_table <- dc_df %>% knitr::kable(col.names = c('Description', 'NAs', 'Min', 'Max', 'Mean',
                                                 'Standard Deviation'),
                                   digits = 5)
dc_table

###
### 2.1.1 Duplicate Values
###

# The data has 240 duplicate rows.  The question is whether these duplicates
# are legitimate data points or actual duplicates.  To help answer, count the
# number of unique rows including the quality (wine rating) column and not.

initial_count <- nrow(wines_full)
initial_count
unique_count <- nrow(unique(wines_full))
unique_count
unique_count_minus_quality <- nrow(unique((wines_full %>%
                                             select('fixed_acidity', 'volatile_acidity',
                                                    'citric_acid', 'residual_sugar', 'chlorides',
                                                    'free_sulfur_dioxide', 'total_sulfur_dioxide',
                                                    'density', 'pH', 'sulphates', 'alcohol'))))
unique_count_minus_quality

# The count of unique rows is the same whether quality is included or not
# suggesting the 240 duplicates are actually duplicates and not different
# ratings with coincidentally identical attributes so remove the duplicates.

# Remove duplicate rows.
wines_full <- unique(wines_full)

###
### 2.2 Qf and Grade
###

# Add qf as a factor version of quality
wines_full <- wines_full %>% mutate(qf = factor(quality))

# Add grade as a factor with quality < 7 Regular and >= 7 Premium
wines_full <- wines_full %>%
  mutate(grade = factor(ifelse(quality < 7, 'Regular', 'Premium')))

###
### 2.3 Cross Validation
###

# Create the final training and test sets.
# Variables:
#     wines_full -  The full set of loaded wines w/duplicates removed.  Not modified
#                   below.
#     final_test -  The final test set used only for calculating the final results.
#     wines -       The final training set.  This is split again into wines_train and
#                   wines_test to select the model and model parameters.
#     wines_train - Cross validation training set.
#     wines_test -  Cross validation test set.
#
set.seed(1, sample.kind = 'Rounding')
test_index <- createDataPartition(wines_full$quality, times = 1, p = 0.2, list = FALSE)
final_test <- wines_full[test_index,]
wines <- wines_full[-test_index,]

set.seed(1, sample.kind = 'Rounding')
test_index <- createDataPartition(wines$quality, times = 1, p = 0.2, list = FALSE)
wines_test <- wines[test_index,]
wines_train <- wines[-test_index,]

###
### 2.4 Data Exploration
###
### Done on the full training set (wines) to get the most data w/out using the final
### test set.
###

###
### 2.4.1 Feature Histograms
###

fixed_acidity_hist <- wines %>%
  ggplot(aes(fixed_acidity)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 1: Fixed Acidity', x = 'fixed acidity (g/L)', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

volatile_acidity_hist <- wines %>%
  ggplot(aes(volatile_acidity)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 2: Volatile Acidity', x = 'volatile acidity (g/L)', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

citric_acid_hist <- wines %>%
  ggplot(aes(citric_acid)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 3: Citric Acid', x = 'citric acid (g/L)', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

residual_sugar_hist <- wines %>%
  ggplot(aes(residual_sugar)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 4: Residual Sugar', x = 'residual sugar (g/L)', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

chlorides_hist <- wines %>%
  ggplot(aes(chlorides)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 5: Chlorides', x = 'cholorides (g/L)', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

free_sulfur_dioxide_hist <- wines %>%
  ggplot(aes(free_sulfur_dioxide)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 6: Free Sulfur Dioxide', x = 'free sulfur dioxide (mg/L)', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

total_sulfur_dioxide_hist <- wines %>%
  ggplot(aes(total_sulfur_dioxide)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 7: Total Sulfur Dioxide', x = 'total sulfur dioxide (mg/L)', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

density_hist <- wines %>%
  ggplot(aes(density)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 8: Density', x = 'density (g/mL)', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

pH_hist <- wines %>%
  ggplot(aes(pH)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 9: pH', x = 'pH', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

sulphates_hist <- wines %>%
  ggplot(aes(sulphates)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 10: Sulphates', x = 'sulphates (g/L)', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

alcohol_hist <- wines %>%
  ggplot(aes(alcohol)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 11: Alcohol', x = 'alcohol (% volume)', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

quality_hist <- wines %>%
  ggplot(aes(quality)) + geom_histogram(bins = 25, color = 'black', fill = 'white') +
  labs(title = 'Figure 12: Quality', x = 'quality', y = '# of wines') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

# Combined plots for Section 2.4.1.  Note that unlike ggplot, grid.arrange doesn't
# allow the plot to be saved in a variable, so this code is repeated in the Rmd
# file.
grid.arrange(fixed_acidity_hist, volatile_acidity_hist, ncol = 2)
grid.arrange(citric_acid_hist, residual_sugar_hist, ncol = 2)
grid.arrange(chlorides_hist, free_sulfur_dioxide_hist, ncol = 2)
grid.arrange(total_sulfur_dioxide_hist, density_hist, ncol = 2)
grid.arrange(pH_hist, sulphates_hist, ncol = 2)
grid.arrange(alcohol_hist, quality_hist, ncol = 2)

###
### 2.4.2 Feature vs. Quality Box Plots
###

fixed_acidity_box <- wines %>%
  ggplot(aes(qf, fixed_acidity)) + geom_boxplot() +
  labs(title = 'Figure 13: Fixed Acidity vs. Quality', x = 'quality', y = 'fixed acidity (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

volatile_acidity_box <- wines %>%
  ggplot(aes(qf, volatile_acidity)) + geom_boxplot() +
  labs(title = 'Figure 14: Volatile Acidity vs. Quality', x = 'quality', y = 'volatile acidity (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

citric_acid_box <- wines %>%
  ggplot(aes(qf, citric_acid)) + geom_boxplot() +
  labs(title = 'Figure 15: Citric Acid vs. Quality', x = 'quality', y = 'citric acid (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

residual_sugar_box <- wines %>%
  ggplot(aes(qf, residual_sugar)) + geom_boxplot() +
  labs(title = 'Figure 16: Residual Sugar vs. Quality', x = 'quality', y = 'residual sugar (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

chlorides_box <- wines %>%
  ggplot(aes(qf, chlorides)) + geom_boxplot() +
  labs(title = 'Figure 17: Chlorides vs. Quality', x = 'quality', y = 'chlorides (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

free_sulfur_dioxide_box <- wines %>%
  ggplot(aes(qf, free_sulfur_dioxide)) + geom_boxplot() +
  labs(title = 'Figure 18: Free Sulfur Dioxide vs. Quality', x = 'quality', y = 'free sulfur dioxide (mg/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

total_sulfur_dioxide_box <- wines %>%
  ggplot(aes(qf, total_sulfur_dioxide)) + geom_boxplot() +
  labs(title = 'Figure 19: Total Sulfur Dioxide vs. Quality', x = 'quality', y = 'total sulfur dioxide (mg/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

density_box <- wines %>%
  ggplot(aes(qf, density)) + geom_boxplot() +
  labs(title = 'Figure 20: Density vs. Quality', x = 'quality', y = 'density (g/mL)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

pH_box <- wines %>%
  ggplot(aes(qf, pH)) + geom_boxplot() +
  labs(title = 'Figure 21: pH vs. Quality', x = 'quality', y = 'pH') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

sulphates_box <- wines %>%
  ggplot(aes(qf, sulphates)) + geom_boxplot() +
  labs(title = 'Figure 22: Sulphates vs. Quality', x = 'quality', y = 'sulphates (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

alcohol_box <- wines %>%
  ggplot(aes(qf, alcohol)) + geom_boxplot() +
  labs(title = 'Figure 23: Alcohol vs. Quality', x = 'quality', y = 'alcohol (% volume)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

# Combined plots for Section 2.4.2.  Note that unlike ggplot, grid.arrange doesn't
# allow the plot to be saved in a variable, so this code is repeated in the Rmd
# file.
grid.arrange(fixed_acidity_box, volatile_acidity_box, ncol = 2)
grid.arrange(citric_acid_box, residual_sugar_box, ncol = 2)
grid.arrange(chlorides_box, free_sulfur_dioxide_box, ncol = 2)
grid.arrange(total_sulfur_dioxide_box, density_box, ncol = 2)
grid.arrange(pH_box, sulphates_box, ncol = 2)
alcohol_box

###
### 2.4.3 Feature vs. Quality Scatter Plots
###

fixed_acidity_scatter <- wines %>%
  ggplot(aes(quality, fixed_acidity)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 24: Fixed Acidity vs. Quality', x = 'quality', y = 'fixed acidity (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

volatile_acidity_scatter <- wines %>%
  ggplot(aes(quality, volatile_acidity)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 25: Volatile Acidity vs. Quality', x = 'quality', y = 'volatile acidity (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

citric_acid_scatter <- wines %>%
  ggplot(aes(quality, citric_acid)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 26: Citric Acid vs. Quality', x = 'quality', y = 'citric acid (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

residual_sugar_scatter <- wines %>%
  ggplot(aes(quality, residual_sugar)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 27: Residual Sugar vs. Quality', x = 'quality', y = 'residual sugar (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

chlorides_scatter <- wines %>%
  ggplot(aes(quality, chlorides)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 28: Chlorides vs. Quality', x = 'quality', y = 'chlorides (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

free_sulfur_dioxide_scatter <- wines %>%
  ggplot(aes(quality, free_sulfur_dioxide)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 29: Free Sulfur Dioxide vs. Quality', x = 'quality', y = 'free sulfur dioxide (mg/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

total_sulfur_dioxide_scatter <- wines %>%
  ggplot(aes(quality, total_sulfur_dioxide)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 30: Total Sulfur Dioxide vs. Quality', x = 'quality', y = 'total sulfur dioxide (mg/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

density_scatter <- wines %>%
  ggplot(aes(quality, density)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 31: Density vs. Quality', x = 'quality', y = 'density (g/mL)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

pH_scatter <- wines %>%
  ggplot(aes(quality, pH)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 32: pH vs. Quality', x = 'quality', y = 'pH') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

sulphates_scatter <- wines %>%
  ggplot(aes(quality, sulphates)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 33: Sulphates vs. Quality', x = 'quality', y = 'sulphates (g/L)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

alcohol_scatter <- wines %>%
  ggplot(aes(quality, alcohol)) + geom_point(alpha = 0.25) +
  geom_smooth(method = 'lm', formula = 'y ~ x') +
  labs(title = 'Figure 34: Alcohol vs. Quality', x = 'quality', y = 'alcohol (% volume)') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

# Combined plots for Section 2.4.3.  Note that unlike ggplot, grid.arrange doesn't
# allow the plot to be saved in a variable, so this code is repeated in the Rmd
# file.
grid.arrange(fixed_acidity_scatter, volatile_acidity_scatter, ncol = 2)
grid.arrange(citric_acid_scatter, residual_sugar_scatter, ncol = 2)
grid.arrange(chlorides_scatter, free_sulfur_dioxide_scatter, ncol = 2)
grid.arrange(total_sulfur_dioxide_scatter, density_scatter, ncol = 2)
grid.arrange(pH_scatter, sulphates_scatter, ncol = 2)
alcohol_scatter

###
### 2.4.4 Feature/Quality Correlation
###

# Calculate the correlation coefficient with each variable against quality.
cor_df <- data.frame(description = "Fixed Acidity", cor = cor(wines$quality, wines$fixed_acidity))
cor_df[nrow(cor_df) + 1,] <- list("Volatile Acidity", cor(wines$quality, wines$volatile_acidity))
cor_df[nrow(cor_df) + 1,] <- list("Citric Acid", cor(wines$quality, wines$citric_acid))
cor_df[nrow(cor_df) + 1,] <- list("Residual Sugar", cor(wines$quality, wines$residual_sugar))
cor_df[nrow(cor_df) + 1,] <- list("Chlorides", cor(wines$quality, wines$chlorides))
cor_df[nrow(cor_df) + 1,] <- list("Free Sulfur Dioxide", cor(wines$quality, wines$free_sulfur_dioxide))
cor_df[nrow(cor_df) + 1,] <- list("Total Sulfur Dioxide", cor(wines$quality, wines$total_sulfur_dioxide))
cor_df[nrow(cor_df) + 1,] <- list("Density", cor(wines$quality, wines$density))
cor_df[nrow(cor_df) + 1,] <- list("pH", cor(wines$quality, wines$pH))
cor_df[nrow(cor_df) + 1,] <- list("Sulphates", cor(wines$quality, wines$sulphates))
cor_df[nrow(cor_df) + 1,] <- list("Alcohol", cor(wines$quality, wines$alcohol))
# Sort by absolute value of correlation coefficient.
cor_table <- cor_df %>% arrange(desc(abs(cor))) %>%
  knitr::kable(col.names = c('Description', 'Correlation Coefficient'), digits = 5)
cor_table

# Plot the top 3 against each other.
alcohol_volatile_acidity_plot <- wines %>%
  ggplot(aes(alcohol, volatile_acidity, fill = qf)) + geom_point(cex=3, pch=21) +
  labs(title = 'Figure 35: Volatile Acidity vs. Alcohol', x = 'alcohol (% volume)', y = 'volatile acidity (g/L)', fill = 'quality') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))
alcohol_sulphates_plot <- wines %>%
  ggplot(aes(alcohol, sulphates, fill = qf)) + geom_point(cex=3, pch=21) +
  labs(title = 'Figure 36: Sulphates vs. Alcohol', x = 'alcohol (% volume)', y = 'sulphates (g/L)', fill = 'quality') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))
volatile_acidity_sulphates_plot <- wines %>%
  ggplot(aes(volatile_acidity, sulphates, fill = qf)) + geom_point(cex=3, pch=21) +
  labs(title = 'Figure 37: Sulphates vs. Volatile Acidity', x = 'volatile acidity (g/L)', y = 'sulphates (g/L)', fill = 'quality') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

###
### 2.5 Linear Regression
###

# Define a function for calculating rmse.
rmse <- function(y_hat, y) {
  sqrt(mean((y_hat - y)^2))
}

# Average value of quality from the wines_train.
mu <- mean(wines_train$quality)

# RMSE, accuracy, and accuracy +/-1 for always guessing mu.
y_hat <- mu
rmse_mu <- rmse(y_hat, wines_test$quality)
acc_mu <- mean(round(y_hat) == wines_test$quality)
accp1_mu <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df <- data.frame(description = "Guess Average", rmse = rmse_mu, accuracy = acc_mu,
                     accuracy_p1 = accp1_mu)

# Perform linear regression starting with the factor with the highest correlation
# coefficient.  At each round add the factor with the next highest correlation
# coefficient.  For each round record RMSE, accuracy, and accuracy +/-1.

fit_lm1 <- lm(quality ~ alcohol, data = wines_train)
y_hat <- predict(fit_lm1, wines_test)
rmse_lm1 <- rmse(y_hat, wines_test$quality)
acc_lm1 <- mean(round(y_hat) == wines_test$quality)
accp1_lm1 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("Top Factor", rmse_lm1, acc_lm1, accp1_lm1)

fit_lm2 <- lm(quality ~ alcohol + volatile_acidity, data = wines_train)
y_hat <- predict(fit_lm2, wines_test)
rmse_lm2 <- rmse(y_hat, wines_test$quality)
acc_lm2 <- mean(round(y_hat) == wines_test$quality)
accp1_lm2 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("Top Two", rmse_lm2, acc_lm2, accp1_lm2)

fit_lm3 <- lm(quality ~ alcohol + volatile_acidity + sulphates, data = wines_train)
y_hat <- predict(fit_lm3, wines_test)
rmse_lm3 <- rmse(y_hat, wines_test$quality)
acc_lm3 <- mean(round(y_hat) == wines_test$quality)
accp1_lm3 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("Top Three", rmse_lm3, acc_lm3, accp1_lm3)

fit_lm4 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid,
          data = wines_train)
y_hat <- predict(fit_lm4, wines_test)
rmse_lm4 <- rmse(y_hat, wines_test$quality)
acc_lm4 <- mean(round(y_hat) == wines_test$quality)
accp1_lm4 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("Top Four", rmse_lm4, acc_lm4, accp1_lm4)

fit_lm5 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
          + density, data = wines_train)
y_hat <- predict(fit_lm5, wines_test)
rmse_lm5 <- rmse(y_hat, wines_test$quality)
acc_lm5 <- mean(round(y_hat) == wines_test$quality)
accp1_lm5 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("Top Five", rmse_lm5, acc_lm5, accp1_lm5)

fit_lm6 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
          + density + total_sulfur_dioxide, data = wines_train)
y_hat <- predict(fit_lm6, wines_test)
rmse_lm6 <- rmse(y_hat, wines_test$quality)
acc_lm6 <- mean(round(y_hat) == wines_test$quality)
accp1_lm6 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("Top Six", rmse_lm6, acc_lm6, accp1_lm6)

fit_lm7 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
          + density + total_sulfur_dioxide + chlorides, data = wines_train)
y_hat <- predict(fit_lm7, wines_test)
rmse_lm7 <- rmse(y_hat, wines_test$quality)
acc_lm7 <- mean(round(y_hat) == wines_test$quality)
accp1_lm7 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("Top Seven", rmse_lm7, acc_lm7, accp1_lm7)

fit_lm8 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
          + density + total_sulfur_dioxide + chlorides + fixed_acidity,
          data = wines_train)
y_hat <- predict(fit_lm8, wines_test)
rmse_lm8 <- rmse(y_hat, wines_test$quality)
acc_lm8 <- mean(round(y_hat) == wines_test$quality)
accp1_lm8 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("Top Eight", rmse_lm8, acc_lm8, accp1_lm8)

fit_lm9 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
          + density + total_sulfur_dioxide + chlorides + fixed_acidity + pH,
          data = wines_train)
y_hat <- predict(fit_lm9, wines_test)
rmse_lm9 <- rmse(y_hat, wines_test$quality)
acc_lm9 <- mean(round(y_hat) == wines_test$quality)
accp1_lm9 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("Top Nine", rmse_lm9, acc_lm9, accp1_lm9)

fit_lm10 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
          + density + total_sulfur_dioxide + chlorides + fixed_acidity + pH
          + free_sulfur_dioxide, data = wines_train)
y_hat <- predict(fit_lm10, wines_test)
rmse_lm10 <- rmse(y_hat, wines_test$quality)
acc_lm10 <- mean(round(y_hat) == wines_test$quality)
accp1_lm10 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("Top Ten", rmse_lm10, acc_lm10, accp1_lm10)

fit_lm11 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
          + density + total_sulfur_dioxide + chlorides + fixed_acidity + pH
          + free_sulfur_dioxide + residual_sugar, data = wines_train)
y_hat <- predict(fit_lm11, wines_test)
rmse_lm11 <- rmse(y_hat, wines_test$quality)
acc_lm11 <- mean(round(y_hat) == wines_test$quality)
accp1_lm11 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)

lm_df[nrow(lm_df) + 1,] <- list("All Eleven", rmse_lm11, acc_lm11, accp1_lm11)
lm_table <- lm_df %>%
  knitr::kable(col.names = c('Description', 'RMSE', 'Accuracy', 'Accuracy +/-1'), digits = 5)
lm_table

###
### 2.6 Classification with k-Nearest Neighbors
###

knn_class_df <- data.frame(description = "Guess Average", accuracy = acc_mu,
                           accuracy_p1 = accp1_mu, k = NA)

# Perform knn classification starting with the factor with the highest correlation
# coefficient.  At each round add the factor with the next highest correlation
# coefficient.  For each round record accuracy, accuracy +/-1, and k.

fit <- train(qf ~ alcohol, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc1 <- mean(y_hat == wines_test$qf)
accp1_knnc1 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc1 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("Top Factor", acc_knnc1, accp1_knnc1, k_knnc1)

fit <- train(qf ~ alcohol + volatile_acidity, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc2 <- mean(y_hat == wines_test$qf)
accp1_knnc2 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc2 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("Top Two", acc_knnc2, accp1_knnc2, k_knnc2)

fit <- train(qf ~ alcohol + volatile_acidity + sulphates, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc3 <- mean(y_hat == wines_test$qf)
accp1_knnc3 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc3 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("Top Three", acc_knnc3, accp1_knnc3, k_knnc3)

fit <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid,
             method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc4 <- mean(y_hat == wines_test$qf)
accp1_knnc4 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc4 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("Top Four", acc_knnc4, accp1_knnc4, k_knnc4)

fit <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density,
             method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc5 <- mean(y_hat == wines_test$qf)
accp1_knnc5 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc5 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("Top Five", acc_knnc5, accp1_knnc5, k_knnc5)

fit <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc6 <- mean(y_hat == wines_test$qf)
accp1_knnc6 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc6 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("Top Six", acc_knnc6, accp1_knnc6, k_knnc6)

fit <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc7 <- mean(y_hat == wines_test$qf)
accp1_knnc7 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc7 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("Top Seven", acc_knnc7, accp1_knnc7, k_knnc7)

fit <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc8 <- mean(y_hat == wines_test$qf)
accp1_knnc8 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc8 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("Top Eight", acc_knnc8, accp1_knnc8, k_knnc8)

fit <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc9 <- mean(y_hat == wines_test$qf)
accp1_knnc9 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc9 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("Top Nine", acc_knnc9, accp1_knnc9, k_knnc9)

fit <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc10 <- mean(y_hat == wines_test$qf)
accp1_knnc10 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc10 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("Top Ten", acc_knnc10, accp1_knnc10, k_knnc10)

fit <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit, wines_test, type = 'raw')
acc_knnc11 <- mean(y_hat == wines_test$qf)
accp1_knnc11 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knnc11 <- fit$bestTune$k

knn_class_df[nrow(knn_class_df) + 1,] <- list("All Eleven", acc_knnc11, accp1_knnc11, k_knnc11)
knn_class_table <- knn_class_df %>%
  knitr::kable(col.names = c('Description', 'Accuracy', 'Accuracy +/-1', 'k'), digits = 5)
knn_class_table

###
### 2.7 Classification with k-Nearest Neighbors and Scaled Factors
###

# Calculate correlation coefficients against just wines_train.
fixed_acidity_cor <- cor(wines_train$quality, wines_train$fixed_acidity)
fixed_acidity_cor
volatile_acidity_cor <- cor(wines_train$quality, wines_train$volatile_acidity)
volatile_acidity_cor
citric_acid_cor <- cor(wines_train$quality, wines_train$citric_acid)
citric_acid_cor
residual_sugar_cor <- cor(wines_train$quality, wines_train$residual_sugar)
residual_sugar_cor
chlorides_cor <- cor(wines_train$quality, wines_train$chlorides)
chlorides_cor
free_sulfur_dioxide_cor <- cor(wines_train$quality, wines_train$free_sulfur_dioxide)
free_sulfur_dioxide_cor
total_sulfur_dioxide_cor <- cor(wines_train$quality, wines_train$total_sulfur_dioxide)
total_sulfur_dioxide_cor
density_cor <- cor(wines_train$quality, wines_train$density)
density_cor
pH_cor <- cor(wines_train$quality, wines_train$pH)
pH_cor
sulphates_cor <- cor(wines_train$quality, wines_train$sulphates)
sulphates_cor
alcohol_cor <- cor(wines_train$quality, wines_train$alcohol)
alcohol_cor

# Calculate mean.
fixed_acidity_mu <- mean(wines_train$fixed_acidity)
fixed_acidity_mu
volatile_acidity_mu <- mean(wines_train$volatile_acidity)
volatile_acidity_cor
citric_acid_mu <- mean(wines_train$citric_acid)
citric_acid_mu
residual_sugar_mu <- mean(wines_train$residual_sugar)
residual_sugar_mu
chlorides_mu <- mean(wines_train$chlorides)
chlorides_mu
free_sulfur_dioxide_mu <- mean(wines_train$free_sulfur_dioxide)
free_sulfur_dioxide_mu
total_sulfur_dioxide_mu <- mean(wines_train$total_sulfur_dioxide)
total_sulfur_dioxide_mu
density_mu <- mean(wines_train$density)
density_mu
pH_mu <- mean(wines_train$pH)
pH_mu
sulphates_mu <- mean(wines_train$sulphates)
sulphates_mu
alcohol_mu <- mean(wines_train$alcohol)
alcohol_mu

# Calculate standard deviation.
fixed_acidity_sd <- sd(wines_train$fixed_acidity)
fixed_acidity_sd
volatile_acidity_sd <- sd(wines_train$volatile_acidity)
volatile_acidity_cor
citric_acid_sd <- sd(wines_train$citric_acid)
citric_acid_sd
residual_sugar_sd <- sd(wines_train$residual_sugar)
residual_sugar_sd
chlorides_sd <- sd(wines_train$chlorides)
chlorides_sd
free_sulfur_dioxide_sd <- sd(wines_train$free_sulfur_dioxide)
free_sulfur_dioxide_sd
total_sulfur_dioxide_sd <- sd(wines_train$total_sulfur_dioxide)
total_sulfur_dioxide_sd
density_sd <- sd(wines_train$density)
density_sd
pH_sd <- sd(wines_train$pH)
pH_sd
sulphates_sd <- sd(wines_train$sulphates)
sulphates_sd
alcohol_sd <- sd(wines_train$alcohol)
alcohol_sd

# Record for display.
std_df <- data.frame(description = "Fixed Acidity", cor = fixed_acidity_cor,
                     mu = fixed_acidity_mu, sd = fixed_acidity_sd)
std_df[nrow(std_df) + 1,] <- list("Volatile Acidity", volatile_acidity_cor,
                                  volatile_acidity_mu, volatile_acidity_sd)
std_df[nrow(std_df) + 1,] <- list("Citric Acid", citric_acid_cor,
                                  citric_acid_mu, citric_acid_sd)
std_df[nrow(std_df) + 1,] <- list("Residual Sugar", residual_sugar_cor,
                                  residual_sugar_mu, residual_sugar_sd)
std_df[nrow(std_df) + 1,] <- list("Chlorides", chlorides_cor,
                                  chlorides_mu, chlorides_sd)
std_df[nrow(std_df) + 1,] <- list("Free Sulfur Dioxide", free_sulfur_dioxide_cor,
                                  free_sulfur_dioxide_mu, free_sulfur_dioxide_sd)
std_df[nrow(std_df) + 1,] <- list("Total Sulfur Dioxide", total_sulfur_dioxide_cor,
                                  total_sulfur_dioxide_mu, total_sulfur_dioxide_sd)
std_df[nrow(std_df) + 1,] <- list("Density", density_cor,
                                  density_mu, density_sd)
std_df[nrow(std_df) + 1,] <- list("pH", pH_cor,
                                  pH_mu, pH_sd)
std_df[nrow(std_df) + 1,] <- list("Sulphates", sulphates_cor,
                                  sulphates_mu, sulphates_sd)
std_df[nrow(std_df) + 1,] <- list("Alcohol", alcohol_cor,
                                  alcohol_mu, alcohol_sd)
std_table <- std_df %>% arrange(desc(abs(cor))) %>%
  knitr::kable(col.names = c('Description', 'Correlation Coefficient', 'Mean', 'Standard Deviation'),
               digits = 5)
std_table

# Add scaled column to wines_train.
wines_train <- wines_train %>% mutate(
  fixed_acidity_std = (fixed_acidity - fixed_acidity_mu)/(fixed_acidity_cor*fixed_acidity_sd),
  volatile_acidity_std = (volatile_acidity - volatile_acidity_mu)/(volatile_acidity_cor*volatile_acidity_sd),
  citric_acid_std = (citric_acid - citric_acid_mu)/(citric_acid_cor*citric_acid_sd),
  residual_sugar_std = (residual_sugar - residual_sugar_mu)/(residual_sugar_cor*residual_sugar_sd),
  chlorides_std = (chlorides - chlorides_mu)/(chlorides_cor*chlorides_sd),
  free_sulfur_dioxide_std = (free_sulfur_dioxide - free_sulfur_dioxide_mu)/(free_sulfur_dioxide_cor*free_sulfur_dioxide_sd),
  total_sulfur_dioxide_std = (total_sulfur_dioxide - total_sulfur_dioxide_mu)/(total_sulfur_dioxide_cor*total_sulfur_dioxide_sd),
  density_std = (density - density_mu)/(density_cor*density_sd),
  pH_std = (pH - pH_mu)/(pH_cor*pH_sd),
  sulphates_std = (sulphates - sulphates_mu)/(sulphates_cor*sulphates_sd),
  alcohol_std = (alcohol - alcohol_mu)/(alcohol_cor*alcohol_sd)
)

# Add scaled column to wines_test.  Note the values are scaled with values calculated
# solely from wines_train and not at all from wines_test.
wines_test <- wines_test %>% mutate(
  fixed_acidity_std = (fixed_acidity - fixed_acidity_mu)/(fixed_acidity_cor*fixed_acidity_sd),
  volatile_acidity_std = (volatile_acidity - volatile_acidity_mu)/(volatile_acidity_cor*volatile_acidity_sd),
  citric_acid_std = (citric_acid - citric_acid_mu)/(citric_acid_cor*citric_acid_sd),
  residual_sugar_std = (residual_sugar - residual_sugar_mu)/(residual_sugar_cor*residual_sugar_sd),
  chlorides_std = (chlorides - chlorides_mu)/(chlorides_cor*chlorides_sd),
  free_sulfur_dioxide_std = (free_sulfur_dioxide - free_sulfur_dioxide_mu)/(free_sulfur_dioxide_cor*free_sulfur_dioxide_sd),
  total_sulfur_dioxide_std = (total_sulfur_dioxide - total_sulfur_dioxide_mu)/(total_sulfur_dioxide_cor*total_sulfur_dioxide_sd),
  density_std = (density - density_mu)/(density_cor*density_sd),
  pH_std = (pH - pH_mu)/(pH_cor*pH_sd),
  sulphates_std = (sulphates - sulphates_mu)/(sulphates_cor*sulphates_sd),
  alcohol_std = (alcohol - alcohol_mu)/(alcohol_cor*alcohol_sd)
)

knn_class2_df <- data.frame(description = "Guess Average", accuracy = acc_mu,
                            accuracy_p1 = accp1_mu, k = NA)

# Perform knn classification starting with the factor with the highest correlation
# coefficient.  At each round add the factor with the next highest correlation
# coefficient.  For each round record accuracy, accuracy +/-1, and k.

fit_knncs1 <- train(qf ~ alcohol_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncs1, wines_test, type = 'raw')
acc_knncs1 <- mean(y_hat == wines_test$qf)
accp1_knncs1 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs1 <- fit_knncs1$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("Top Factor", acc_knncs1, accp1_knncs1, k_knncs1)

fit_knncs2 <- train(qf ~ alcohol_std + volatile_acidity_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 75, 1)))
y_hat <- predict(fit_knncs2, wines_test, type = 'raw')
acc_knncs2 <- mean(y_hat == wines_test$qf)
accp1_knncs2 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs2 <- fit_knncs2$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("Top Two", acc_knncs2, accp1_knncs2, k_knncs2)

fit_knncs3 <- train(qf ~ alcohol_std + volatile_acidity_std + sulphates_std, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncs3, wines_test, type = 'raw')
acc_knncs3 <- mean(y_hat == wines_test$qf)
accp1_knncs3 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs3 <- fit_knncs3$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("Top Three", acc_knncs3, accp1_knncs3, k_knncs3)

fit_knncs4 <- train(qf ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std,
             method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncs4, wines_test, type = 'raw')
acc_knncs4 <- mean(y_hat == wines_test$qf)
accp1_knncs4 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs4 <- fit_knncs4$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("Top Four", acc_knncs4, accp1_knncs4, k_knncs4)

fit_knncs5 <- train(qf ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std,
             method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncs5, wines_test, type = 'raw')
acc_knncs5 <- mean(y_hat == wines_test$qf)
accp1_knncs5 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs5 <- fit_knncs5$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("Top Five", acc_knncs5, accp1_knncs5, k_knncs5)

fit_knncs6 <- train(qf ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncs6, wines_test, type = 'raw')
acc_knncs6 <- mean(y_hat == wines_test$qf)
accp1_knncs6 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs6 <- fit_knncs6$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("Top Six", acc_knncs6, accp1_knncs6, k_knncs6)

fit_knncs7 <- train(qf ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std + chlorides_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncs7, wines_test, type = 'raw')
acc_knncs7 <- mean(y_hat == wines_test$qf)
accp1_knncs7 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs7 <- fit_knncs7$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("Top Seven", acc_knncs7, accp1_knncs7, k_knncs7)

fit_knncs8 <- train(qf ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std + chlorides_std + fixed_acidity_std, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncs8, wines_test, type = 'raw')
acc_knncs8 <- mean(y_hat == wines_test$qf)
accp1_knncs8 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs8 <- fit_knncs8$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("Top Eight", acc_knncs8, accp1_knncs8, k_knncs8)

fit_knncs9 <- train(qf ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std + chlorides_std + fixed_acidity_std + pH_std, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncs9, wines_test, type = 'raw')
acc_knncs9 <- mean(y_hat == wines_test$qf)
accp1_knncs9 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs9 <- fit_knncs9$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("Top Nine", acc_knncs9, accp1_knncs9, k_knncs9)

fit_knncs10 <- train(qf ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std + chlorides_std + fixed_acidity_std + pH_std
             + free_sulfur_dioxide_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncs10, wines_test, type = 'raw')
acc_knncs10 <- mean(y_hat == wines_test$qf)
accp1_knncs10 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs10 <- fit_knncs10$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("Top Ten", acc_knncs10, accp1_knncs10, k_knncs10)

fit_knncs11 <- train(qf ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std + chlorides_std + fixed_acidity_std + pH_std
             + free_sulfur_dioxide_std + residual_sugar_std, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncs11, wines_test, type = 'raw')
acc_knncs11 <- mean(y_hat == wines_test$qf)
accp1_knncs11 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
k_knncs11 <- fit_knncs11$bestTune$k

knn_class2_df[nrow(knn_class2_df) + 1,] <- list("All Eleven", acc_knncs11, accp1_knncs11, k_knncs11)
knn_class2_table <- knn_class2_df %>%
  knitr::kable(col.names = c('Description', 'Accuracy', 'Accuracy +/-1', 'k'), digits = 5)
knn_class2_table

# Plot Accuracy vs. k for the fit with the best accuracy.
knn_class_plot <- ggplot(fit_knncs6, highlight = TRUE) +
  labs(title = 'Figure 38: Optimization of k for kNN Classification') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

###
### 2.8 Regression with k-Nearest Neighbors and Scaled Factors
###

knn_reg_df <- data.frame(description = "Guess Average", rmse = rmse_mu, accuracy = acc_mu,
                         accuracy_p1 = accp1_mu, k = NA)

# Perform knn regression starting with the factor with the highest correlation
# coefficient.  At each round add the factor with the next highest correlation
# coefficient.  For each round record RMSE, accuracy, accuracy +/-1, and k.

fit_knnr1 <- train(quality ~ alcohol_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr1, wines_test, type = 'raw')
rmse_knnr1 <- rmse(y_hat, wines_test$quality)
acc_knnr1 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr1 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr1 <- fit_knnr1$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("Top Factor", rmse_knnr1, acc_knnr1, accp1_knnr1, k_knnr1)

fit_knnr2 <- train(quality ~ alcohol_std + volatile_acidity_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr2, wines_test, type = 'raw')
rmse_knnr2 <- rmse(y_hat, wines_test$quality)
acc_knnr2 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr2 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr2 <- fit_knnr2$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("Top Two", rmse_knnr2, acc_knnr2, accp1_knnr2, k_knnr2)

fit_knnr3 <- train(quality ~ alcohol_std + volatile_acidity_std + sulphates_std, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr3, wines_test, type = 'raw')
rmse_knnr3 <- rmse(y_hat, wines_test$quality)
acc_knnr3 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr3 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr3 <- fit_knnr3$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("Top Three", rmse_knnr3, acc_knnr3, accp1_knnr3, k_knnr3)

fit_knnr4 <- train(quality ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std,
             method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr4, wines_test, type = 'raw')
rmse_knnr4 <- rmse(y_hat, wines_test$quality)
acc_knnr4 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr4 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr4 <- fit_knnr4$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("Top Four", rmse_knnr4, acc_knnr4, accp1_knnr4, k_knnr4)

fit_knnr5 <- train(quality ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std,
             method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr5, wines_test, type = 'raw')
rmse_knnr5 <- rmse(y_hat, wines_test$quality)
acc_knnr5 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr5 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr5 <- fit_knnr5$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("Top Five", rmse_knnr5, acc_knnr5, accp1_knnr5, k_knnr5)

fit_knnr6 <- train(quality ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr6, wines_test, type = 'raw')
rmse_knnr6 <- rmse(y_hat, wines_test$quality)
acc_knnr6 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr6 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr6 <- fit_knnr6$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("Top Six", rmse_knnr6, acc_knnr6, accp1_knnr6, k_knnr6)

fit_knnr7 <- train(quality ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std + chlorides_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr7, wines_test, type = 'raw')
rmse_knnr7 <- rmse(y_hat, wines_test$quality)
acc_knnr7 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr7 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr7 <- fit_knnr7$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("Top Seven", rmse_knnr7, acc_knnr7, accp1_knnr7, k_knnr7)

fit_knnr8 <- train(quality ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std + chlorides_std + fixed_acidity_std, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr8, wines_test, type = 'raw')
rmse_knnr8 <- rmse(y_hat, wines_test$quality)
acc_knnr8 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr8 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr8 <- fit_knnr8$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("Top Eight", rmse_knnr8, acc_knnr8, accp1_knnr8, k_knnr8)

fit_knnr9 <- train(quality ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std + chlorides_std + fixed_acidity_std + pH_std, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr9, wines_test, type = 'raw')
rmse_knnr9 <- rmse(y_hat, wines_test$quality)
acc_knnr9 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr9 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr9 <- fit_knnr9$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("Top Nine", rmse_knnr9, acc_knnr9, accp1_knnr9, k_knnr9)

fit_knnr10 <- train(quality ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std + chlorides_std + fixed_acidity_std + pH_std
             + free_sulfur_dioxide_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr10, wines_test, type = 'raw')
rmse_knnr10 <- rmse(y_hat, wines_test$quality)
acc_knnr10 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr10 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr10 <- fit_knnr10$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("Top Ten", rmse_knnr10, acc_knnr10, accp1_knnr10, k_knnr10)

fit_knnr11 <- train(quality ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std + chlorides_std + fixed_acidity_std + pH_std
             + free_sulfur_dioxide_std + residual_sugar_std, method = 'knn',
             data = wines_train, tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knnr11, wines_test, type = 'raw')
rmse_knnr11 <- rmse(y_hat, wines_test$quality)
acc_knnr11 <- mean(round(y_hat) == wines_test$quality)
accp1_knnr11 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
k_knnr11 <- fit_knnr11$bestTune$k

knn_reg_df[nrow(knn_reg_df) + 1,] <- list("All Eleven", rmse_knnr11, acc_knnr11, accp1_knnr11, k_knnr11)
knn_reg_table <- knn_reg_df %>%
  knitr::kable(col.names = c('Description', 'RMSE', 'Accuracy', 'Accuracy +/-1', 'k'), digits = 5)
knn_reg_table

# Plot RMSE vs. k for the fit with the best accuracy.
knn_reg_plot <- ggplot(fit_knnr4, highlight = TRUE) +
  labs(title = 'Figure 39: Optimization of k for kNN Regression') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

###
### 2.9 Classification with Random Forests (Rborist)
###

rf_class_df <- data.frame(description = "Guess Average", accuracy = acc_mu,
                          accuracy_p1 = accp1_mu, minNode = NA)

# Perform random forest classification starting with the factor with the highest
# correlation coefficient.  At each round add the factor with the next highest correlation
# coefficient.  For each round record accuracy, accuracy +/-1, and minNode.

fit_rfc1 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 1, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc1, wines_test, type = 'raw')
acc_rfc1 <- mean(y_hat == wines_test$qf)
accp1_rfc1 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc1 <- fit_rfc1$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 1", acc_rfc1, accp1_rfc1, mn_rfc1)

fit_rfc2 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 2, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc2, wines_test, type = 'raw')
acc_rfc2 <- mean(y_hat == wines_test$qf)
accp1_rfc2 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc2 <- fit_rfc2$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 2", acc_rfc2, accp1_rfc2, mn_rfc2)

fit_rfc3 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 3, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc3, wines_test, type = 'raw')
acc_rfc3 <- mean(y_hat == wines_test$qf)
accp1_rfc3 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc3 <- fit_rfc3$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 3", acc_rfc3, accp1_rfc3, mn_rfc3)

fit_rfc4 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 4, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc4, wines_test, type = 'raw')
acc_rfc4 <- mean(y_hat == wines_test$qf)
accp1_rfc4 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc4 <- fit_rfc4$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 4", acc_rfc4, accp1_rfc4, mn_rfc4)

fit_rfc5 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 5, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc5, wines_test, type = 'raw')
acc_rfc5 <- mean(y_hat == wines_test$qf)
accp1_rfc5 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc5 <- fit_rfc5$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 5", acc_rfc5, accp1_rfc5, mn_rfc5)

fit_rfc6 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 6, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc6, wines_test, type = 'raw')
acc_rfc6 <- mean(y_hat == wines_test$qf)
accp1_rfc6 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc6 <- fit_rfc6$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 6", acc_rfc6, accp1_rfc6, mn_rfc6)

fit_rfc7 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 7, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc7, wines_test, type = 'raw')
acc_rfc7 <- mean(y_hat == wines_test$qf)
accp1_rfc7 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc7 <- fit_rfc7$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 7", acc_rfc7, accp1_rfc7, mn_rfc7)

fit_rfc8 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 8, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc8, wines_test, type = 'raw')
acc_rfc8 <- mean(y_hat == wines_test$qf)
accp1_rfc8 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc8 <- fit_rfc8$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 8", acc_rfc8, accp1_rfc8, mn_rfc8)

fit_rfc9 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 9, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc9, wines_test, type = 'raw')
acc_rfc9 <- mean(y_hat == wines_test$qf)
accp1_rfc9 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc9 <- fit_rfc9$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 9", acc_rfc9, accp1_rfc9, mn_rfc9)

fit_rfc10 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 10, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc10, wines_test, type = 'raw')
acc_rfc10 <- mean(y_hat == wines_test$qf)
accp1_rfc10 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc10 <- fit_rfc10$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 10", acc_rfc10, accp1_rfc10, mn_rfc10)

fit_rfc11 <- train(qf ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 11, minNode = seq(1, 20)))
y_hat <- predict(fit_rfc11, wines_test, type = 'raw')
acc_rfc11 <- mean(y_hat == wines_test$qf)
accp1_rfc11 <- mean(abs(as.numeric(as.character(y_hat)) - wines_test$quality) <= 1)
mn_rfc11 <- fit_rfc11$bestTune$minNode

rf_class_df[nrow(rf_class_df) + 1,] <- list("PredFixed = 11", acc_rfc11, accp1_rfc11, mn_rfc11)
rf_class_table <- rf_class_df %>%
  knitr::kable(col.names = c('Description', 'Accuracy', 'Accuracy +/-1', 'minNode'), digits = 5)
rf_class_table

# Plot Accuracy vs. minNode for the fit with the best accuracy.
rf_class_plot <- ggplot(fit_rfc4, highlight = TRUE) +
  labs(title = 'Figure 40: Optimization of minNode for Random Forest Classification') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

###
### 2.10 Regression with Random Forests (Rborist)
###

rf_reg_df <- data.frame(description = "Guess Average", rmse = rmse_mu, accuracy = acc_mu,
                         accuracy_p1 = accp1_mu, minNode = NA)

# Perform random forest regression starting with the factor with the highest
# correlation coefficient.  At each round add the factor with the next highest correlation
# coefficient.  For each round record RMSE, accuracy, accuracy +/-1, and minNode.

fit_rfr1 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 1, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr1, wines_test, type = 'raw')
rmse_rfr1 <- rmse(y_hat, wines_test$quality)
acc_rfr1 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr1 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr1 <- fit_rfr1$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 1", rmse_rfr1, acc_rfr1, accp1_rfr1, mn_rfr1)

fit_rfr2 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 2, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr2, wines_test, type = 'raw')
rmse_rfr2 <- rmse(y_hat, wines_test$quality)
acc_rfr2 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr2 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr2 <- fit_rfr2$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 2", rmse_rfr2, acc_rfr2, accp1_rfr2, mn_rfr2)

fit_rfr3 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 3, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr3, wines_test, type = 'raw')
rmse_rfr3 <- rmse(y_hat, wines_test$quality)
acc_rfr3 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr3 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr3 <- fit_rfr3$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 3", rmse_rfr3, acc_rfr3, accp1_rfr3, mn_rfr3)

fit_rfr4 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 4, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr4, wines_test, type = 'raw')
rmse_rfr4 <- rmse(y_hat, wines_test$quality)
acc_rfr4 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr4 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr4 <- fit_rfr4$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 4", rmse_rfr4, acc_rfr4, accp1_rfr4, mn_rfr4)

fit_rfr5 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 5, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr5, wines_test, type = 'raw')
rmse_rfr5 <- rmse(y_hat, wines_test$quality)
acc_rfr5 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr5 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr5 <- fit_rfr5$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 5", rmse_rfr5, acc_rfr5, accp1_rfr5, mn_rfr5)

fit_rfr6 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 6, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr6, wines_test, type = 'raw')
rmse_rfr6 <- rmse(y_hat, wines_test$quality)
acc_rfr6 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr6 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr6 <- fit_rfr6$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 6", rmse_rfr6, acc_rfr6, accp1_rfr6, mn_rfr6)

fit_rfr7 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 7, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr7, wines_test, type = 'raw')
rmse_rfr7 <- rmse(y_hat, wines_test$quality)
acc_rfr7 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr7 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr7 <- fit_rfr7$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 7", rmse_rfr7, acc_rfr7, accp1_rfr7, mn_rfr7)

fit_rfr8 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 8, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr8, wines_test, type = 'raw')
rmse_rfr8 <- rmse(y_hat, wines_test$quality)
acc_rfr8 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr8 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr8 <- fit_rfr8$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 8", rmse_rfr8, acc_rfr8, accp1_rfr8, mn_rfr8)

fit_rfr9 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 9, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr9, wines_test, type = 'raw')
rmse_rfr9 <- rmse(y_hat, wines_test$quality)
acc_rfr9 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr9 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr9 <- fit_rfr9$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 9", rmse_rfr1, acc_rfr9, accp1_rfr9, mn_rfr9)

fit_rfr10 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 10, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr10, wines_test, type = 'raw')
rmse_rfr10 <- rmse(y_hat, wines_test$quality)
acc_rfr10 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr10 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr10 <- fit_rfr10$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 10", rmse_rfr10, acc_rfr10, accp1_rfr10, mn_rfr10)

fit_rfr11 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
             + total_sulfur_dioxide + chlorides + fixed_acidity + pH
             + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
             data = wines_train, tuneGrid = data.frame(predFixed = 11, minNode = seq(1, 20)))
y_hat <- predict(fit_rfr11, wines_test, type = 'raw')
rmse_rfr11 <- rmse(y_hat, wines_test$quality)
acc_rfr11 <- mean(abs(round(y_hat) == wines_test$quality))
accp1_rfr11 <- mean(abs(round(y_hat) - wines_test$quality) <= 1)
mn_rfr11 <- fit_rfr11$bestTune$minNode

rf_reg_df[nrow(rf_reg_df) + 1,] <- list("PredFixed = 11", rmse_rfr11, acc_rfr11, accp1_rfr11, mn_rfr11)
rf_reg_table <- rf_reg_df %>%
  knitr::kable(col.names = c('Description', 'RMSE', 'Accuracy', 'Accuracy +/-1', 'minNode'), digits = 5)
rf_reg_table

# Plot RMSE vs. minNode for the fit with the best accuracy.
rf_reg_plot <- ggplot(fit_rfr6, highlight = TRUE) +
  labs(title = 'Figure 41: Optimization of minNode for Random Forest Regression') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

###
### 2.11 Variable Importance
###

# Use randomForest (not RBorist) to calculate variable importance using parameters
# similar to the best fit from above.
fit_randomf <- randomForest(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
                            + total_sulfur_dioxide + chlorides + fixed_acidity + pH
                            + free_sulfur_dioxide + residual_sugar,
                            data = wines_train, mtry = 6, minsize = 5)

rf_imp_df <- data.frame(description = "Fixed Acidity",
                        importance = fit_randomf$importance['fixed_acidity',])
rf_imp_df[nrow(rf_imp_df) + 1,] <- list("Volatile Acidity",
                                        fit_randomf$importance['volatile_acidity',])
rf_imp_df[nrow(rf_imp_df) + 1,] <- list("Citric Acid",
                                        fit_randomf$importance['citric_acid',])
rf_imp_df[nrow(rf_imp_df) + 1,] <- list("Residual Sugar",
                                        fit_randomf$importance['residual_sugar',])
rf_imp_df[nrow(rf_imp_df) + 1,] <- list("Chlorides",
                                        fit_randomf$importance['chlorides',])
rf_imp_df[nrow(rf_imp_df) + 1,] <- list("Free Sulfur Dioxide",
                                        fit_randomf$importance['free_sulfur_dioxide',])
rf_imp_df[nrow(rf_imp_df) + 1,] <- list("Total Sulfur Dioxide",
                                        fit_randomf$importance['total_sulfur_dioxide',])
rf_imp_df[nrow(rf_imp_df) + 1,] <- list("Density",
                                        fit_randomf$importance['density',])
rf_imp_df[nrow(rf_imp_df) + 1,] <- list("pH",
                                        fit_randomf$importance['pH',])
rf_imp_df[nrow(rf_imp_df) + 1,] <- list("Sulphates",
                                        fit_randomf$importance['sulphates',])
rf_imp_df[nrow(rf_imp_df) + 1,] <- list("Alcohol",
                                        fit_randomf$importance['alcohol',])

rf_imp_table <- rf_imp_df %>% arrange(desc(importance)) %>%
  knitr::kable(col.names = c('Description', 'Importance'), digits = 5)
rf_imp_table

# Re-scale the coefficients for linear regression so they should be similar to the
# corresponding correlation coefficients.
lm_imp_df <- data.frame(description = "Fixed Acidity", importance =
                          fit_lm11$coefficients[['fixed_acidity']]*sd(wines_train$fixed_acidity)/
                          sd(wines_train$quality))
lm_imp_df[nrow(lm_imp_df) + 1,] <- list("Volatile Acidity", fit_lm11$coefficients[['volatile_acidity']]*
                                          sd(wines_train$volatile_acidity)/sd(wines_train$quality))
lm_imp_df[nrow(lm_imp_df) + 1,] <- list("Citric Acid", fit_lm11$coefficients[['citric_acid']]*
                                          sd(wines_train$citric_acid)/sd(wines_train$quality))
lm_imp_df[nrow(lm_imp_df) + 1,] <- list("Residual Sugar", fit_lm11$coefficients[['residual_sugar']]*
                                          sd(wines_train$residual_sugar)/sd(wines_train$quality))
lm_imp_df[nrow(lm_imp_df) + 1,] <- list("Chlorides", fit_lm11$coefficients[['residual_sugar']]*
                                          sd(wines_train$chlorides)/sd(wines_train$quality))
lm_imp_df[nrow(lm_imp_df) + 1,] <- list("Free Sulfur Dioxide", fit_lm11$coefficients[['free_sulfur_dioxide']]*
                                          sd(wines_train$free_sulfur_dioxide)/sd(wines_train$quality))
lm_imp_df[nrow(lm_imp_df) + 1,] <- list("Total Sulfur Dioxide", fit_lm11$coefficients[['total_sulfur_dioxide']]*
                                          sd(wines_train$total_sulfur_dioxide)/sd(wines_train$quality))
lm_imp_df[nrow(lm_imp_df) + 1,] <- list("Density", fit_lm11$coefficients[['density']]*
                                          sd(wines_train$density)/sd(wines_train$quality))
lm_imp_df[nrow(lm_imp_df) + 1,] <- list("pH", fit_lm11$coefficients[['pH']]*
                                          sd(wines_train$pH)/sd(wines_train$quality))
lm_imp_df[nrow(lm_imp_df) + 1,] <- list("Sulphates", fit_lm11$coefficients[['sulphates']]*
                                          sd(wines_train$sulphates)/sd(wines_train$quality))
lm_imp_df[nrow(lm_imp_df) + 1,] <- list("Alcohol", fit_lm11$coefficients[['alcohol']]*
                                          sd(wines_train$alcohol)/sd(wines_train$quality))

lm_imp_table <- lm_imp_df %>% arrange(desc(abs(importance))) %>%
  knitr::kable(col.names = c('Description', 'Importance'), digits = 5)
lm_imp_table

###
### 2.12 Premium vs. Regular Wines
###

# Plot the top 3 against each other, but this time with colors corresponding to
# grade instead of quality.
alcohol_volatile_acidity_grade_plot <- wines %>%
  ggplot(aes(alcohol, volatile_acidity, fill = grade)) + geom_point(cex=3, pch=21) +
  labs(title = 'Figure 42: Volatile Acidity vs. Alcohol by Grade', x = 'alcohol (% volume)', y = 'volatile acidity (g/L)', fill = 'grade') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))
alcohol_sulphates_grade_plot <- wines %>%
  ggplot(aes(alcohol, sulphates, fill = grade)) + geom_point(cex=3, pch=21) +
  labs(title = 'Figure 43: Sulphates vs. Alcohol by Grade', x = 'alcohol (% volume)', y = 'sulphates (g/L)', fill = 'grade') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))
volatile_acidity_sulphates_grade_plot <- wines %>%
  ggplot(aes(volatile_acidity, sulphates, fill = grade)) + geom_point(cex=3, pch=21) +
  labs(title = 'Figure 44: Sulphates vs. Volatile Acidity by Grade', x = 'volatile acidity (g/L)', y = 'sulphates (g/L)', fill = 'grade') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

###
### 2.12.1 Classification Models
###

# Use the best knn model from above for classification of premium wines.
fit_knncg <- train(grade ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std + density_std
             + total_sulfur_dioxide_std, method = 'knn', data = wines_train,
             tuneGrid = data.frame(k = seq(1, 100, 1)))
y_hat <- predict(fit_knncg, wines_test, type = 'raw')
acc_knncg <- mean(y_hat == wines_test$grade)
k_knncg <- fit_knncg$bestTune$k
cm_knncg <- confusionMatrix(factor(y_hat), wines_test$grade)

cm_knncg_table <- cm_knncg$table %>% knitr::kable()

# Do the same for random forests.
fit_rfg <- train(grade ~ alcohol + volatile_acidity + sulphates + citric_acid + density
                  + total_sulfur_dioxide + chlorides + fixed_acidity + pH
                  + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
                  data = wines_train, tuneGrid = data.frame(predFixed = 4, minNode = seq(1, 20)))
y_hat <- predict(fit_rfg, wines_test, type = 'raw')
acc_rfg <- mean(y_hat == wines_test$grade)
mn_rfg <- fit_rfg$bestTune$minNode
cm_rfg <- confusionMatrix(factor(y_hat), wines_test$grade)

cm_rfg_table <- cm_rfg$table %>% knitr::kable()

# Put everything in a table.
grade_class_df <- data.frame(Description = "k-Nearest Neighbor",
                             Accuracy = cm_knncg$overall[['Accuracy']],
                             Sensitivity = cm_knncg$byClass[['Sensitivity']],
                             Specificity = cm_knncg$byClass[['Specificity']],
                             Precision = cm_knncg$byClass[['Precision']],
                             P_Value = cm_knncg$overall[['AccuracyPValue']])
grade_class_df[nrow(grade_class_df) + 1,] <- list("Random Forest",
                                                  cm_rfg$overall[['Accuracy']],
                                                  cm_rfg$byClass[['Sensitivity']],
                                                  cm_rfg$byClass[['Specificity']],
                                                  cm_rfg$byClass[['Precision']],
                                                  cm_rfg$overall[['AccuracyPValue']])
grade_class_table <- grade_class_df %>%
  knitr::kable(col.names = c('Description', 'Accuracy', 'Sensitivity', 'Specificity', 'Precision',
                             'P-Value'),
               digits = 5)
grade_class_table

# Monte carlo simulation to show precision is statistically significant.
mc_knncg <- mean(replicate(10000, {
  sum(sample(wines_test$grade, 14) == 'Premium')
}) >= 10)

mc_rfg <- mean(replicate(10000, {
  sum(sample(wines_test$grade, 18) == 'Premium')
}) >= 13)

###
### 2.12.2 Regression Model Selection
###

# Linear Regression ROC Plot.

y_hat <- predict(fit_lm6, wines_test)
thresholds <- seq(4, 7, 0.1)
lm6_roc <- map_df(thresholds, function(t) {
  list(
    model = 'Top 6',
    threshold = t,
    TPR = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    FPR = 1 - sum(y_hat < t & wines_test$quality < 7)/sum(wines_test$quality < 7)
  )
})

y_hat <- predict(fit_lm10, wines_test)
thresholds <- seq(4, 7, 0.1)
lm10_roc <- map_df(thresholds, function(t) {
  list(
    model = 'Top 10',
    threshold = t,
    TPR = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    FPR = 1 - sum(y_hat < t & wines_test$quality < 7)/sum(wines_test$quality < 7)
  )
})

lm_roc <- bind_rows(lm6_roc, lm10_roc)
lm_roc_plot <- lm_roc %>% ggplot(aes(FPR, TPR, color = model, label = threshold)) +
  geom_line() + geom_point() +
  labs(title = 'Figure 45: Linear Regression ROC Plots', x = 'False Positive Rate (FPR)',
       y = 'True Positive Rate (TPR)') +
  theme(legend.position = 'bottom', plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

# k-Nearest Neighbors ROC Plot

y_hat <- predict(fit_knnr4, wines_test)
thresholds <- seq(4, 7, 0.1)
knn4_roc <- map_df(thresholds, function(t) {
  list(
    model = 'Top 4',
    threshold = t,
    TPR = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    FPR = 1 - sum(y_hat < t & wines_test$quality < 7)/sum(wines_test$quality < 7)
  )
})

y_hat <- predict(fit_knnr6, wines_test)
thresholds <- seq(4, 7, 0.1)
knn6_roc <- map_df(thresholds, function(t) {
  list(
    model = 'Top 6',
    threshold = t,
    TPR = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    FPR = 1 - sum(y_hat < t & wines_test$quality < 7)/sum(wines_test$quality < 7)
  )
})

knn_roc <- bind_rows(knn4_roc, knn6_roc)
knn_roc_plot <- knn_roc %>% ggplot(aes(FPR, TPR, color = model, label = threshold)) +
  geom_line() + geom_point() +
  labs(title = 'Figure 46: k-Nearest Neighbors ROC Plots', x = 'False Positive Rate (FPR)',
       y = 'True Positive Rate (TPR)') +
  theme(legend.position = 'bottom', plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

# Random Forests ROC Plot

y_hat <- predict(fit_rfr6, wines_test, type = 'raw')
thresholds <- seq(4, 7, 0.1)
rf6_roc <- map_df(thresholds, function(t) {
  list(
    model = 'Top 6',
    threshold = t,
    TPR = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    FPR = 1 - sum(y_hat < t & wines_test$quality < 7)/sum(wines_test$quality < 7)
  )
})

rf_roc <- rf6_roc
rf_roc_plot <- rf_roc %>% ggplot(aes(FPR, TPR, color = model, label = threshold)) +
  geom_line() + geom_point() +
  labs(title = 'Figure 47: Random Forest ROC Plots', x = 'False Positive Rate (FPR)',
       y = 'True Positive Rate (TPR)') +
  theme(legend.position = 'bottom', plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

###
### 2.12.3 Regression Model Ranges
###

reg_ranges_df <- data.frame(description = "Linear Regression", min = min(predict(fit_lm10, wines_test)),
                            max = max(predict(fit_lm10, wines_test)))
reg_ranges_df[nrow(reg_ranges_df) + 1,] <- list("k-Nearest Neighbor", min(predict(fit_knnr4, wines_test)),
                                                max(predict(fit_knnr4, wines_test)))
reg_ranges_df[nrow(reg_ranges_df) + 1,] <- list("Random Forest", min(predict(fit_rfr6, wines_test)),
                                                max(predict(fit_rfr6, wines_test)))
reg_ranges_table <- reg_ranges_df %>%
  knitr::kable(col.names = c('Description', 'Min', 'Max'), digits = 5)
reg_ranges_table

###
### 2.12.4 Regression Accuracy
###

# Linear Regression
y_hat <- predict(fit_lm10, wines_test)
thresholds <- seq(4, 7, 0.1)
lm10_acc <- map_df(thresholds, function(t) {
  list(
    model = 'Linear Regression',
    threshold = t,
    accuracy = mean((y_hat >= t & wines_test$quality >= 7) | (y_hat < t & wines_test$quality < 7))
  )
})

lm10_acc_threshold <- lm10_acc$threshold[which.max(lm10_acc$accuracy)]
cm_lm10_acc <- confusionMatrix(factor(ifelse(predict(fit_lm10, wines_test) >= lm10_acc_threshold,
                                             'Premium', 'Regular'),
                                      levels = c('Premium', 'Regular')), wines_test$grade)

# k-Nearest Neighbors
y_hat <- predict(fit_knnr4, wines_test)
thresholds <- seq(4, 7, 0.1)
knn4_acc <- map_df(thresholds, function(t) {
  list(
    model = 'k-Nearest Neighbor',
    threshold = t,
    accuracy = mean((y_hat >= t & wines_test$quality >= 7) | (y_hat < t & wines_test$quality < 7))
  )
})

knn4_acc_threshold <- knn4_acc$threshold[which.max(knn4_acc$accuracy)]
cm_knn4_acc <- confusionMatrix(factor(ifelse(predict(fit_knnr4, wines_test) >= knn4_acc_threshold,
                                             'Premium', 'Regular'),
                                      levels = c('Premium', 'Regular')), wines_test$grade)

# Random Forests
y_hat <- predict(fit_rfr6, wines_test, type = 'raw')
thresholds <- seq(4, 7, 0.1)
rf6_acc <- map_df(thresholds, function(t) {
  list(
    model = 'Random Forest',
    threshold = t,
    accuracy = mean((y_hat >= t & wines_test$quality >= 7) | (y_hat < t & wines_test$quality < 7))
  )
})

rf6_acc_threshold <- rf6_acc$threshold[which.max(rf6_acc$accuracy)]
cm_rf6_acc <- confusionMatrix(factor(ifelse(predict(fit_rfr6, wines_test) >= rf6_acc_threshold,
                                             'Premium', 'Regular'),
                                      levels = c('Premium', 'Regular')), wines_test$grade)

# Plot them all together.
multi_acc <- bind_rows(lm10_acc, knn4_acc, rf6_acc)
regression_accuracy <- multi_acc %>% ggplot(aes(threshold, accuracy, color = model)) +
  geom_line() + geom_point() +
  labs(title = 'Figure 48: Regression Model Accuracy vs Threshold', x = 'threshold', y = 'accuracy') +
  theme(legend.position = 'bottom', plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

# Put all the statistics in a table.
accuracy_df <- data.frame(Description = "Linear Regression",
                          Threshold = lm10_acc$threshold[which.max(lm10_acc$accuracy)],
                          Accuracy = cm_lm10_acc$overall[['Accuracy']],
                          Sensitivity = cm_lm10_acc$byClass[['Sensitivity']],
                          Specificity = cm_lm10_acc$byClass[['Specificity']],
                          Precision = cm_lm10_acc$byClass[['Precision']],
                          P_Value = cm_lm10_acc$overall[['AccuracyPValue']])
accuracy_df[nrow(accuracy_df) + 1,] <- list("k-Nearest Neighbor",
                                            knn4_acc$threshold[which.max(knn4_acc$accuracy)],
                                            cm_knn4_acc$overall[['Accuracy']],
                                            cm_knn4_acc$byClass[['Sensitivity']],
                                            cm_knn4_acc$byClass[['Specificity']],
                                            cm_knn4_acc$byClass[['Precision']],
                                            cm_knn4_acc$overall[['AccuracyPValue']])
accuracy_df[nrow(accuracy_df) + 1,] <- list("Random Forest",
                                            rf6_acc$threshold[which.max(rf6_acc$accuracy)],
                                            cm_rf6_acc$overall[['Accuracy']],
                                            cm_rf6_acc$byClass[['Sensitivity']],
                                            cm_rf6_acc$byClass[['Specificity']],
                                            cm_rf6_acc$byClass[['Precision']],
                                            cm_rf6_acc$overall[['AccuracyPValue']])
accuracy_table <- accuracy_df %>%
  knitr::kable(col.names = c('Description', 'Threshold', 'Accuracy', 'Sensitivity', 'Specificity',
                             'Precision', 'P-Value'),
               digits = 5)
accuracy_table

###
### 2.12.5 F1-Score
###

# Linear Regression
y_hat <- predict(fit_lm10, wines_test)
thresholds <- seq(4, 7, 0.1)
lm10_f1 <- map_df(thresholds, function(t) {
  list(
    model = 'Linear Regression',
    threshold = t,
    recall = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    precision = sum(y_hat >= t & wines_test$quality >= 7)/(sum(y_hat >= t & wines_test$quality >= 7) + sum(y_hat >= t & wines_test$quality < 7))
  )
}) %>% mutate(f1 = 2/(1/recall + 1/precision))

lm10_f1_threshold <- lm10_f1$threshold[which.max(lm10_f1$f1)]
cm_lm10_f1 <- confusionMatrix(factor(ifelse(predict(fit_lm10, wines_test) >= lm10_f1_threshold,
                                            'Premium', 'Regular'),
                                     levels = c('Premium', 'Regular')), wines_test$grade)

# k-Nearest Neighbor
y_hat <- predict(fit_knnr4, wines_test)
thresholds <- seq(4, 7, 0.1)
knn4_f1 <- map_df(thresholds, function(t) {
  list(
    model = 'k-Nearest Neighbor',
    threshold = t,
    recall = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    precision = sum(y_hat >= t & wines_test$quality >= 7)/(sum(y_hat >= t & wines_test$quality >= 7) + sum(y_hat >= t & wines_test$quality < 7))
  )
}) %>% mutate(f1 = 2/(1/recall + 1/precision))

knn4_f1_threshold <- knn4_f1$threshold[which.max(knn4_f1$f1)]
cm_knn4_f1 <- confusionMatrix(factor(ifelse(predict(fit_knnr4, wines_test) >= knn4_f1_threshold,
                                            'Premium', 'Regular'),
                                     levels = c('Premium', 'Regular')), wines_test$grade)

# Random Forest
y_hat <- predict(fit_rfr6, wines_test, type = 'raw')
thresholds <- seq(4, 7, 0.1)
rf6_f1 <- map_df(thresholds, function(t) {
  list(
    model = 'Random Forest',
    threshold = t,
    recall = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    precision = sum(y_hat >= t & wines_test$quality >= 7)/(sum(y_hat >= t & wines_test$quality >= 7) + sum(y_hat >= t & wines_test$quality < 7))
  )
}) %>% mutate(f1 = 2/(1/recall + 1/precision))

rf6_f1_threshold <- rf6_f1$threshold[which.max(rf6_f1$f1)]
cm_rf6_f1 <- confusionMatrix(factor(ifelse(predict(fit_rfr6, wines_test) >= rf6_f1_threshold,
                                           'Premium', 'Regular'),
                                    levels = c('Premium', 'Regular')), wines_test$grade)

# Plot them all together.
multi_f1 <- bind_rows(lm10_f1, knn4_f1, rf6_f1)
regression_f1 <- multi_f1 %>% ggplot(aes(threshold, f1, color = model, label = threshold)) +
  geom_line() + geom_point() +
  labs(title = 'Figure 49: F1-Score vs. Threshold', x = 'threshold', y = 'f1-score') +
  theme(legend.position = 'bottom', plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

# Put all the statistics in a table.
f1_df <- data.frame(Description = "Linear Regression",
                    Threshold = lm10_f1$threshold[which.max(lm10_f1$f1)],
                    F1 = lm10_f1$f1[which.max(lm10_f1$f1)],
                    Accuracy = cm_lm10_f1$overall[['Accuracy']],
                    Sensitivity = cm_lm10_f1$byClass[['Sensitivity']],
                    Specificity = cm_lm10_f1$byClass[['Specificity']],
                    Precision = cm_lm10_f1$byClass[['Precision']],
                    P_Value = cm_lm10_f1$overall[['AccuracyPValue']])
f1_df[nrow(f1_df) + 1,] <- list("k-Nearest Neighbor",
                                knn4_f1$threshold[which.max(knn4_f1$f1)],
                                knn4_f1$f1[which.max(knn4_f1$f1)],
                                cm_knn4_f1$overall[['Accuracy']],
                                cm_knn4_f1$byClass[['Sensitivity']],
                                cm_knn4_f1$byClass[['Specificity']],
                                cm_knn4_f1$byClass[['Precision']],
                                cm_knn4_f1$overall[['AccuracyPValue']])
f1_df[nrow(f1_df) + 1,] <- list("Random Forest",
                                rf6_f1$threshold[which.max(rf6_f1$f1)],
                                rf6_f1$f1[which.max(rf6_f1$f1)],
                                cm_rf6_f1$overall[['Accuracy']],
                                cm_rf6_f1$byClass[['Sensitivity']],
                                cm_rf6_f1$byClass[['Specificity']],
                                cm_rf6_f1$byClass[['Precision']],
                                cm_rf6_f1$overall[['AccuracyPValue']])
f1_table <- f1_df %>%
  knitr::kable(col.names = c('Description', 'Threshold', 'F1', 'Accuracy', 'Sensitivity', 'Specificity',
                             'Precision', 'P-Value'),
               digits = 5)
f1_table

###
### 2.12.6 ROC Plots
###

# Linear Regression
y_hat <- predict(fit_lm10, wines_test)
thresholds <- seq(4, 7, 0.1)
lm10m_roc <- map_df(thresholds, function(t) {
  list(
    model = 'Linear Regression',
    threshold = t,
    TPR = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    FPR = 1 - sum(y_hat < t & wines_test$quality < 7)/sum(wines_test$quality < 7)
  )
})

# k-Nearest Neighbor
y_hat <- predict(fit_knnr4, wines_test)
thresholds <- seq(4, 7, 0.1)
knn4m_roc <- map_df(thresholds, function(t) {
  list(
    model = 'k-Nearest Neighbor',
    threshold = t,
    TPR = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    FPR = 1 - sum(y_hat < t & wines_test$quality < 7)/sum(wines_test$quality < 7)
  )
})

# Random Forest
y_hat <- predict(fit_rfr6, wines_test, type = 'raw')
thresholds <- seq(4, 7, 0.1)
rf6m_roc <- map_df(thresholds, function(t) {
  list(
    model = 'Random Forest',
    threshold = t,
    TPR = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    FPR = 1 - sum(y_hat < t & wines_test$quality < 7)/sum(wines_test$quality < 7)
  )
})

# Plot them all together.
multi_roc <- bind_rows(lm10m_roc, knn4m_roc, rf6m_roc)
regression_roc <- multi_roc %>% ggplot(aes(FPR, TPR, color = model, label = threshold)) +
  geom_line() + geom_point() +
  labs(title = 'Figure 50: Regression ROC Plots', x = 'False Positive Rate (FPR)',
       y = 'True Positive Rate (TPR)') +
  theme(legend.position = 'bottom', plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

# Plot just Random Forests including thresholds.
rf6_roc_plot <- rf6m_roc %>% ggplot(aes(FPR, TPR, fill = model, label = threshold)) +
  geom_line() + geom_point(cex=2, pch=21) + geom_text_repel(nudge_x = 0.01, nudge_y = -0.01) +
  labs(title = 'Figure 51: Random Forest ROC Plots', x = 'False Positive Rate (FPR)',
       y = 'True Positive Rate (TPR)') +
  theme(legend.position = 'none', plot.title = element_text(face = 'bold', size = 10, hjust = 0.5))

###
### 2.12.7 Precision-Recall Plots
###

# Linear Regression
y_hat <- predict(fit_lm10, wines_test)
thresholds <- seq(4, 7, 0.1)
lm10_pr <- map_df(thresholds, function(t) {
  list(
    model = 'Linear Regression',
    threshold = t,
    recall = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    precision = sum(y_hat >= t & wines_test$quality >= 7)/(sum(y_hat >= t & wines_test$quality >= 7) + sum(y_hat >= t & wines_test$quality < 7))
  )
})

# k-Nearest Neighbors
y_hat <- predict(fit_knnr4, wines_test)
thresholds <- seq(4, 7, 0.1)
knn4_pr <- map_df(thresholds, function(t) {
  list(
    model = 'k-Nearest Neighbor',
    threshold = t,
    recall = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    precision = sum(y_hat >= t & wines_test$quality >= 7)/(sum(y_hat >= t & wines_test$quality >= 7) + sum(y_hat >= t & wines_test$quality < 7))
  )
})

# Random Forest
y_hat <- predict(fit_rfr6, wines_test, type = 'raw')
thresholds <- seq(4, 7, 0.1)
rf6_pr <- map_df(thresholds, function(t) {
  list(
    model = 'Random Forest',
    threshold = t,
    recall = sum(y_hat >= t & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    precision = sum(y_hat >= t & wines_test$quality >= 7)/(sum(y_hat >= t & wines_test$quality >= 7) + sum(y_hat >= t & wines_test$quality < 7))
  )
})

# Guessing
thresholds <- seq(4, 7, 0.1)
guessing <- map_df(thresholds, function(t) {
  p = (t - 4)/3
  y_hat <- sample(c('Regular', 'Premium'), length(wines_test$grade), replace = TRUE, prob = c(p, 1-p))
  list(
    model = 'Guessing',
    threshold = t,
    recall = sum(y_hat == 'Premium' & wines_test$quality >= 7)/sum(wines_test$quality >= 7),
    precision = sum(y_hat == 'Premium' & wines_test$quality >= 7)/(sum(y_hat == 'Premium' & wines_test$quality >= 7) + sum(y_hat == 'Premium' & wines_test$quality < 7))
  )
})

# Plot them all together.
multi_pr <- bind_rows(lm10_pr, knn4_pr, rf6_pr, guessing)
regression_pr <- multi_pr %>% ggplot(aes(recall, precision, color = model, label = threshold)) +
  geom_line() + geom_point() +
  labs(title = 'Figure 52: Regression Precision-Recall Plots', x = 'recall', y = 'precision') +
  theme(legend.position = 'bottom', plot.title = element_text(face = 'bold', size = 10, hjust = 0.5)) +
  ylim(0, 1) + scale_color_manual(values = c('#c77cff', '#f8766d', '#00ba38', '#619cff'))
lm10_pr_plot <- lm10_pr %>% ggplot(aes(recall, precision, fill = model, label = threshold)) +
  geom_line() + geom_point(cex=2, pch=21) + geom_text_repel(nudge_x = 0.01, nudge_y = -0.01) +
  labs(title = 'Figure 53: Random Forest ROC Plots', x = 'False Positive Rate (FPR)',
       y = 'True Positive Rate (TPR)') +
  theme(legend.position = 'none', plot.title = element_text(face = 'bold', size = 10, hjust = 0.5)) +
  ylim(0, 1)

###
### 2.12.8 Selected Model Training Test Set Results
###

# Confusion Matrix
cm_test <- confusionMatrix(factor(ifelse(predict(fit_lm10, wines_test) >= 6.5, 'Premium', 'Regular'),
                                  levels = c('Regular', 'Premium')), wines_test$grade)
cm_test_table <- cm_test$table %>% knitr::kable()

# Statistics table
grade_reg_df <- data.frame(Description = "Linear Regression",
                             Accuracy = cm_test$overall[['Accuracy']],
                             Sensitivity = cm_test$byClass[['Sensitivity']],
                             Specificity = cm_test$byClass[['Specificity']],
                             Precision = cm_test$byClass[['Precision']],
                             P_Value = cm_test$overall[['AccuracyPValue']])
grade_reg_table <- grade_reg_df %>%
  knitr::kable(col.names = c('Description', 'Accuracy', 'Sensitivity', 'Specificity', 'Precision',
                             'P-Value'),
               digits = 5)
grade_reg_table

# Selected wines table
selected <- wines_test %>%
  mutate(prediction = factor(ifelse(predict(fit_lm10, .) >= 6.5, 'Premium', 'Regular'),
                             levels = c('Premium', 'Regular'))) %>%
  filter(prediction == 'Premium')

test_results_table <- selected %>%
  select(alcohol, volatile_acidity, sulphates, quality) %>%
  arrange(desc(quality)) %>% knitr::kable(row.names = TRUE)

# Selected wines plot
selected_plot <- selected %>%
  ggplot(aes(volatile_acidity, sulphates, fill = qf)) + geom_point(cex=3, pch=21) +
  labs(title = 'Figure 54: Sulphates vs. Volatile Acidity', x = 'volatile acidity (g/L)', y = 'sulphates (g/L)', fill = 'quality') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5)) +
  ylim(0.37, 2) + xlim(0.12, 1.58) +
  scale_fill_manual(values = c('#619cff', '#f564e3'))

###
### 3 Results
###

# Train final model.
fit_final <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
                + density + total_sulfur_dioxide + chlorides + fixed_acidity + pH
                + free_sulfur_dioxide, data = wines)
# Final confusion matrix
cm_final <- confusionMatrix(factor(ifelse(predict(fit_final, final_test) >= 6.5, 'Premium', 'Regular'),
                                   levels = c('Premium', 'Regular')), final_test$grade)

cm_final_table <- cm_final$table %>% knitr::kable()

# Statistics table
grade_reg_final_df <- data.frame(Description = "Linear Regression",
                                 Accuracy = cm_final$overall[['Accuracy']],
                                 Sensitivity = cm_final$byClass[['Sensitivity']],
                                 Specificity = cm_final$byClass[['Specificity']],
                                 Precision = cm_final$byClass[['Precision']],
                                 P_Value = cm_final$overall[['AccuracyPValue']])
grade_reg_final_table <- grade_reg_final_df %>%
  knitr::kable(col.names = c('Description', 'Accuracy', 'Sensitivity', 'Specificity', 'Precision',
                             'P-Value'),
               digits = 5)
grade_reg_final_table

# Selected wines
final_selected <- final_test %>%
  mutate(prediction = factor(ifelse(predict(fit_final, .) >= 6.5, 'Premium', 'Regular'),
                             levels = c('Regular', 'Premium'))) %>%
  filter(prediction == 'Premium')

final_results_table <- final_selected %>%
  select(alcohol, volatile_acidity, sulphates, quality) %>%
  arrange(desc(quality)) %>% knitr::kable(row.names = TRUE)

# Selected wines plot
final_selected_plot <- final_selected %>%
  ggplot(aes(volatile_acidity, sulphates, fill = qf)) + geom_point(cex=3, pch=21) +
  labs(title = 'Figure 55: Sulphates vs. Volatile Acidity', x = 'volatile acidity (g/L)', y = 'sulphates (g/L)', fill = 'quality') +
  theme(plot.title = element_text(face = 'bold', size = 10, hjust = 0.5)) +
  ylim(0.37, 2) + xlim(0.12, 1.58) +
  scale_fill_manual(values = c('#00ba38', '#00bfc4', '#619cff', '#f564e3'))

###
### 3.1 Results Discussion
###

# Reset the seed.
set.seed(2, sample.kind = 'Rounding')
test_index2 <- createDataPartition(wines$quality, times = 1, p = 0.2, list = FALSE)
wines_test2 <- wines[test_index2,]
wines_train2 <- wines[-test_index2,]

# Recalculate scaling parameters for k-Nearest Neighbors.
fixed_acidity_cor2 <- cor(wines_train2$quality, wines_train2$fixed_acidity)
volatile_acidity_cor2 <- cor(wines_train2$quality, wines_train2$volatile_acidity)
citric_acid_cor2 <- cor(wines_train2$quality, wines_train2$citric_acid)
residual_sugar_cor2 <- cor(wines_train2$quality, wines_train2$residual_sugar)
chlorides_cor2 <- cor(wines_train2$quality, wines_train2$chlorides)
free_sulfur_dioxide_cor2 <- cor(wines_train2$quality, wines_train2$free_sulfur_dioxide)
total_sulfur_dioxide_cor2 <- cor(wines_train2$quality, wines_train2$total_sulfur_dioxide)
density_cor2 <- cor(wines_train2$quality, wines_train2$density)
pH_cor2 <- cor(wines_train2$quality, wines_train2$pH)
sulphates_cor2 <- cor(wines_train2$quality, wines_train2$sulphates)
alcohol_cor2 <- cor(wines_train2$quality, wines_train2$alcohol)

fixed_acidity_mu2 <- mean(wines_train2$fixed_acidity)
volatile_acidity_mu2 <- mean(wines_train2$volatile_acidity)
citric_acid_mu2 <- mean(wines_train2$citric_acid)
residual_sugar_mu2 <- mean(wines_train2$residual_sugar)
chlorides_mu2 <- mean(wines_train2$chlorides)
free_sulfur_dioxide_mu2 <- mean(wines_train2$free_sulfur_dioxide)
total_sulfur_dioxide_mu2 <- mean(wines_train2$total_sulfur_dioxide)
density_mu2 <- mean(wines_train2$density)
pH_mu2 <- mean(wines_train2$pH)
sulphates_mu2 <- mean(wines_train2$sulphates)
alcohol_mu2 <- mean(wines_train2$alcohol)

fixed_acidity_sd2 <- sd(wines_train2$fixed_acidity)
volatile_acidity_sd2 <- sd(wines_train2$volatile_acidity)
citric_acid_sd2 <- sd(wines_train2$citric_acid)
residual_sugar_sd2 <- sd(wines_train2$residual_sugar)
chlorides_sd2 <- sd(wines_train2$chlorides)
free_sulfur_dioxide_sd2 <- sd(wines_train2$free_sulfur_dioxide)
total_sulfur_dioxide_sd2 <- sd(wines_train2$total_sulfur_dioxide)
density_sd2 <- sd(wines_train2$density)
pH_sd2 <- sd(wines_train2$pH)
sulphates_sd2 <- sd(wines_train2$sulphates)
alcohol_sd2 <- sd(wines_train2$alcohol)

wines_train2 <- wines_train2 %>% mutate(
  fixed_acidity_std = (fixed_acidity - fixed_acidity_mu2)/(fixed_acidity_cor2*fixed_acidity_sd2),
  volatile_acidity_std = (volatile_acidity - volatile_acidity_mu2)/(volatile_acidity_cor2*volatile_acidity_sd2),
  citric_acid_std = (citric_acid - citric_acid_mu2)/(citric_acid_cor2*citric_acid_sd2),
  residual_sugar_std = (residual_sugar - residual_sugar_mu2)/(residual_sugar_cor2*residual_sugar_sd2),
  chlorides_std = (chlorides - chlorides_mu2)/(chlorides_cor2*chlorides_sd2),
  free_sulfur_dioxide_std = (free_sulfur_dioxide - free_sulfur_dioxide_mu2)/(free_sulfur_dioxide_cor2*free_sulfur_dioxide_sd2),
  total_sulfur_dioxide_std = (total_sulfur_dioxide - total_sulfur_dioxide_mu2)/(total_sulfur_dioxide_cor2*total_sulfur_dioxide_sd2),
  density_std = (density - density_mu2)/(density_cor2*density_sd2),
  pH_std = (pH - pH_mu2)/(pH_cor2*pH_sd2),
  sulphates_std = (sulphates - sulphates_mu2)/(sulphates_cor2*sulphates_sd2),
  alcohol_std = (alcohol - alcohol_mu2)/(alcohol_cor2*alcohol_sd2)
)

wines_test2 <- wines_test2 %>% mutate(
  fixed_acidity_std = (fixed_acidity - fixed_acidity_mu2)/(fixed_acidity_cor2*fixed_acidity_sd2),
  volatile_acidity_std = (volatile_acidity - volatile_acidity_mu2)/(volatile_acidity_cor2*volatile_acidity_sd2),
  citric_acid_std = (citric_acid - citric_acid_mu2)/(citric_acid_cor2*citric_acid_sd2),
  residual_sugar_std = (residual_sugar - residual_sugar_mu2)/(residual_sugar_cor2*residual_sugar_sd2),
  chlorides_std = (chlorides - chlorides_mu2)/(chlorides_cor2*chlorides_sd2),
  free_sulfur_dioxide_std = (free_sulfur_dioxide - free_sulfur_dioxide_mu2)/(free_sulfur_dioxide_cor2*free_sulfur_dioxide_sd2),
  total_sulfur_dioxide_std = (total_sulfur_dioxide - total_sulfur_dioxide_mu2)/(total_sulfur_dioxide_cor2*total_sulfur_dioxide_sd2),
  density_std = (density - density_mu2)/(density_cor2*density_sd2),
  pH_std = (pH - pH_mu2)/(pH_cor2*pH_sd2),
  sulphates_std = (sulphates - sulphates_mu2)/(sulphates_cor2*sulphates_sd2),
  alcohol_std = (alcohol - alcohol_mu2)/(alcohol_cor2*alcohol_sd2)
)

# Redo models.
fit_lm10_2 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
                 + density + total_sulfur_dioxide + chlorides + fixed_acidity + pH
                 + free_sulfur_dioxide, data = wines_train2)
fit_knnr4_2 <- train(quality ~ alcohol_std + volatile_acidity_std + sulphates_std + citric_acid_std,
                   method = 'knn', data = wines_train2,
                   tuneGrid = data.frame(k = 34))
fit_rfr6_2 <- train(quality ~ alcohol + volatile_acidity + sulphates + citric_acid + density
                  + total_sulfur_dioxide + chlorides + fixed_acidity + pH
                  + free_sulfur_dioxide + residual_sugar, method = 'Rborist',
                  data = wines_train2, tuneGrid = data.frame(predFixed = 6, minNode = 2))

# New Precision-Recall Plot.
y_hat <- predict(fit_lm10_2, wines_test2)
thresholds <- seq(4, 7, 0.1)
lm10_pr2 <- map_df(thresholds, function(t) {
  list(
    model = 'Linear Regression',
    threshold = t,
    recall = sum(y_hat >= t & wines_test2$quality >= 7)/sum(wines_test2$quality >= 7),
    precision = sum(y_hat >= t & wines_test2$quality >= 7)/(sum(y_hat >= t & wines_test2$quality >= 7) + sum(y_hat >= t & wines_test2$quality < 7))
  )
})
y_hat <- predict(fit_knnr4_2, wines_test2)
thresholds <- seq(4, 7, 0.1)
knn4_pr2 <- map_df(thresholds, function(t) {
  list(
    model = 'k-Nearest Neighbor',
    threshold = t,
    recall = sum(y_hat >= t & wines_test2$quality >= 7)/sum(wines_test2$quality >= 7),
    precision = sum(y_hat >= t & wines_test2$quality >= 7)/(sum(y_hat >= t & wines_test2$quality >= 7) + sum(y_hat >= t & wines_test2$quality < 7))
  )
})
y_hat <- predict(fit_rfr6_2, wines_test2, type = 'raw')
thresholds <- seq(4, 7, 0.1)
rf6_pr2 <- map_df(thresholds, function(t) {
  list(
    model = 'Random Forest',
    threshold = t,
    recall = sum(y_hat >= t & wines_test2$quality >= 7)/sum(wines_test2$quality >= 7),
    precision = sum(y_hat >= t & wines_test2$quality >= 7)/(sum(y_hat >= t & wines_test2$quality >= 7) + sum(y_hat >= t & wines_test2$quality < 7))
  )
})
thresholds <- seq(4, 7, 0.1)
guessing2 <- map_df(thresholds, function(t) {
  p = (t - 4)/3
  y_hat <- sample(c('Regular', 'Premium'), length(wines_test2$grade), replace = TRUE, prob = c(p, 1-p))
  list(
    model = 'Guessing',
    threshold = t,
    recall = sum(y_hat == 'Premium' & wines_test2$quality >= 7)/sum(wines_test2$quality >= 7),
    precision = sum(y_hat == 'Premium' & wines_test2$quality >= 7)/(sum(y_hat == 'Premium' & wines_test2$quality >= 7) + sum(y_hat == 'Premium' & wines_test2$quality < 7))
  )
})

multi_pr2 <- bind_rows(lm10_pr2, knn4_pr2, rf6_pr2, guessing2)
regression_pr2 <- multi_pr2 %>% ggplot(aes(recall, precision, color = model, label = threshold)) +
  geom_line() + geom_point() +
  labs(title = 'Figure 56: Regression Precision-Recall Plots', x = 'recall', y = 'precision') +
  theme(legend.position = 'bottom', plot.title = element_text(face = 'bold', size = 10, hjust = 0.5)) +
  ylim(0, 1) + scale_color_manual(values = c('#c77cff', '#f8766d', '#00ba38', '#619cff'))

# Redo Linear Regression models.
lm_df_2 <- data.frame(description = "Guess Average", rmse = rmse_mu, accuracy = acc_mu,
                    accuracy_p1 = accp1_mu)

fit_lm1_2 <- lm(quality ~ alcohol, data = wines_train2)
y_hat <- predict(fit_lm1_2, wines_test2)
rmse_lm1_2 <- rmse(y_hat, wines_test2$quality)
acc_lm1_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm1_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("Top Factor", rmse_lm1_2, acc_lm1_2, accp1_lm1_2)

fit_lm2_2 <- lm(quality ~ alcohol + volatile_acidity, data = wines_train2)
y_hat <- predict(fit_lm2_2, wines_test2)
rmse_lm2_2 <- rmse(y_hat, wines_test2$quality)
acc_lm2_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm2_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("Top Two", rmse_lm2_2, acc_lm2_2, accp1_lm2_2)

fit_lm3_2 <- lm(quality ~ alcohol + volatile_acidity + sulphates, data = wines_train2)
y_hat <- predict(fit_lm3_2, wines_test2)
rmse_lm3_2 <- rmse(y_hat, wines_test2$quality)
acc_lm3_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm3_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("Top Three", rmse_lm3_2, acc_lm3_2, accp1_lm3_2)

fit_lm4_2 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid,
              data = wines_train2)
y_hat <- predict(fit_lm4_2, wines_test2)
rmse_lm4_2 <- rmse(y_hat, wines_test2$quality)
acc_lm4_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm4_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("Top Four", rmse_lm4_2, acc_lm4_2, accp1_lm4_2)

fit_lm5_2 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
              + density, data = wines_train2)
y_hat <- predict(fit_lm5_2, wines_test2)
rmse_lm5_2 <- rmse(y_hat, wines_test2$quality)
acc_lm5_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm5_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("Top Five", rmse_lm5_2, acc_lm5_2, accp1_lm5_2)

fit_lm6_2 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
              + density + total_sulfur_dioxide, data = wines_train2)
y_hat <- predict(fit_lm6_2, wines_test2)
rmse_lm6_2 <- rmse(y_hat, wines_test2$quality)
acc_lm6_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm6_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("Top Six", rmse_lm6_2, acc_lm6_2, accp1_lm6_2)

fit_lm7_2 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
              + density + total_sulfur_dioxide + chlorides, data = wines_train2)
y_hat <- predict(fit_lm7_2, wines_test2)
rmse_lm7_2 <- rmse(y_hat, wines_test2$quality)
acc_lm7_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm7_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("Top Seven", rmse_lm7_2, acc_lm7_2, accp1_lm7_2)

fit_lm8_2 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
              + density + total_sulfur_dioxide + chlorides + fixed_acidity,
              data = wines_train2)
y_hat <- predict(fit_lm8_2, wines_test2)
rmse_lm8_2 <- rmse(y_hat, wines_test2$quality)
acc_lm8_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm8_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("Top Eight", rmse_lm8_2, acc_lm8_2, accp1_lm8_2)

fit_lm9_2 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
              + density + total_sulfur_dioxide + chlorides + fixed_acidity + pH,
              data = wines_train2)
y_hat <- predict(fit_lm9_2, wines_test2)
rmse_lm9_2 <- rmse(y_hat, wines_test2$quality)
acc_lm9_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm9_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("Top Nine", rmse_lm9_2, acc_lm9_2, accp1_lm9_2)

fit_lm10_2 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
               + density + total_sulfur_dioxide + chlorides + fixed_acidity + pH
               + free_sulfur_dioxide, data = wines_train2)
y_hat <- predict(fit_lm10_2, wines_test2)
rmse_lm10_2 <- rmse(y_hat, wines_test2$quality)
acc_lm10_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm10_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("Top Ten", rmse_lm10_2, acc_lm10_2, accp1_lm10_2)

fit_lm11_2 <- lm(quality ~ alcohol + volatile_acidity + sulphates + citric_acid
               + density + total_sulfur_dioxide + chlorides + fixed_acidity + pH
               + free_sulfur_dioxide + residual_sugar, data = wines_train2)
y_hat <- predict(fit_lm11_2, wines_test2)
rmse_lm11_2 <- rmse(y_hat, wines_test2$quality)
acc_lm11_2 <- mean(round(y_hat) == wines_test2$quality)
accp1_lm11_2 <- mean(abs(round(y_hat) - wines_test2$quality) <= 1)

lm_df_2[nrow(lm_df_2) + 1,] <- list("All Eleven", rmse_lm11_2, acc_lm11_2, accp1_lm11_2)
lm_table_2 <- lm_df_2 %>%
  knitr::kable(col.names = c('Description', 'RMSE', 'Accuracy', 'Accuracy +/-1'), digits = 5)
lm_table_2
