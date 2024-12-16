# Load necessary libraries
library(readr)
library(car)

# Load the data
file_path <- "C:/Users/Cipher/Desktop/AMDA/rehab_data.csv"
df <- read_csv(file_path)
head(df)
colnames(df)

#1

library(MVN)
library(dplyr)

# Check assumptions

# a. Adequate sample size
cat("Sample size:", nrow(df), "observations.\n")
cat("Groups:", table(df$program), "\n")

# b. Independence of observations
# Assumes study design ensures independence; this is verified through experimental setup.

# c. Absence of univariate outliers
boxplot(df$physical_health ~ df$program, main = "Physical Health by Program")
boxplot(df$psychological_wellbeing ~ df$program, main = "Psychological Well-being by Program")
# Check for univariate outliers - Physical Health and Psychological Well-being

# Summary statistics for physical_health by program
summary_physical_health <- df %>%
  group_by(program) %>%
  summarise(
    Min = min(physical_health, na.rm = TRUE),
    Q1 = quantile(physical_health, 0.25, na.rm = TRUE),
    Median = median(physical_health, na.rm = TRUE),
    Mean = mean(physical_health, na.rm = TRUE),
    Q3 = quantile(physical_health, 0.75, na.rm = TRUE),
    Max = max(physical_health, na.rm = TRUE),
    SD = sd(physical_health, na.rm = TRUE)
  )
print(summary_physical_health)

# Summary statistics for psychological_wellbeing by program
summary_psychological_wellbeing <- df %>%
  group_by(program) %>%
  summarise(
    Min = min(psychological_wellbeing, na.rm = TRUE),
    Q1 = quantile(psychological_wellbeing, 0.25, na.rm = TRUE),
    Median = median(psychological_wellbeing, na.rm = TRUE),
    Mean = mean(psychological_wellbeing, na.rm = TRUE),
    Q3 = quantile(psychological_wellbeing, 0.75, na.rm = TRUE),
    Max = max(psychological_wellbeing, na.rm = TRUE),
    SD = sd(psychological_wellbeing, na.rm = TRUE)
  )
print(summary_psychological_wellbeing)

# Boxplots to visualize univariate outliers
boxplot(df$physical_health ~ df$program, main = "Physical Health by Program")
boxplot(df$psychological_wellbeing ~ df$program, main = "Psychological Well-being by Program")


# d. Absence of multivariate outliers
mahal <- mahalanobis(df[, c("physical_health", "psychological_wellbeing")], 
                     colMeans(df[, c("physical_health", "psychological_wellbeing")]), 
                     cov(df[, c("physical_health", "psychological_wellbeing")]))
cutoff <- qchisq(0.99, df = 2) # Degrees of freedom = number of dependent variables
outliers <- which(mahal > cutoff)
cat("Multivariate outliers (Mahalanobis):", outliers, "\n")

# e. Multivariate normality
mvn_test <- mvn(data = df[, c("physical_health", "psychological_wellbeing")], mvnTest = "mardia")
print(mvn_test)

# f. Linearity

# 1. Pearson Correlation Coefficient
correlation <- cor(df$physical_health, df$psychological_wellbeing)
cat("Pearson Correlation Coefficient: ", correlation, "\n")

# 2. Linear Regression Model
lm_model <- lm(psychological_wellbeing ~ physical_health, data = df)
summary(lm_model)  # This will give you the coefficients, R-squared, and p-value

# 3. Plotting the data and the linear regression line
plot(df$physical_health, df$psychological_wellbeing, 
     main = "Physical Health vs. Psychological Well-being", 
     xlab = "Physical Health", 
     ylab = "Psychological Well-being")
abline(lm_model, col = "red")  # Adds a red line representing the linear model

# 4. Residual Plot
plot(lm_model$residuals, 
     main = "Residuals Plot", 
     xlab = "Fitted Values", 
     ylab = "Residuals")
abline(h = 0, col = "red")



# g. Homogeneity of variances (Levene's Test)
leveneTest(df$physical_health ~ df$program)
leveneTest(df$psychological_wellbeing ~ df$program)

# h. Homogeneity of variance-covariance matrices (Box's M test)

library(biotools)
boxM <- boxM(df[, c("physical_health", "psychological_wellbeing")], df$program)
print(boxM)

#2

# Fit MANOVA model using Pillai's test
manova_model <- manova(cbind(physical_health, psychological_wellbeing) ~ program, data = df)

# Summary of the MANOVA model with Pillai's test
summary(manova_model, test = "Pillai")

#3
# a. ANOVA for physical_health
anova_physical_health <- aov(physical_health ~ program, data = df)
summary(anova_physical_health)

# b. ANOVA for psychological_wellbeing
anova_psychological_wellbeing <- aov(psychological_wellbeing ~ program, data = df)
summary(anova_psychological_wellbeing)

# c. Tukey's HSD for pairwise comparisons for physical_health if ANOVA is significant
if (summary(anova_physical_health)[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_physical_health <- TukeyHSD(anova_physical_health)
  print(tukey_physical_health)
}

# d. Tukey's HSD for pairwise comparisons for psychological_wellbeing if ANOVA is significant
if (summary(anova_psychological_wellbeing)[[1]]$`Pr(>F)`[1] < 0.05) {
  tukey_psychological_wellbeing <- TukeyHSD(anova_psychological_wellbeing)
  print(tukey_psychological_wellbeing)
}

#4.
# Load required libraries
library(ggplot2)
library(dplyr)

# a. Boxplot for physical_health by program
ggplot(df, aes(x = program, y = physical_health, fill = program)) +
  geom_boxplot() +
  labs(title = "Boxplot of Physical Health Scores by Program",
       x = "Program", y = "Physical Health") +
  theme_minimal()

# b. Boxplot for psychological_wellbeing by program
ggplot(df, aes(x = program, y = psychological_wellbeing, fill = program)) +
  geom_boxplot() +
  labs(title = "Boxplot of Psychological Wellbeing Scores by Program",
       x = "Program", y = "Psychological Wellbeing") +
  theme_minimal()

# c. Scatter plot of physical_health vs. psychological_wellbeing by program
ggplot(df, aes(x = physical_health, y = psychological_wellbeing, color = program)) +
  geom_point() +
  labs(title = "Scatter Plot of Physical vs. Psychological Scores by Program",
       x = "Physical Health", y = "Psychological Wellbeing") +
  theme_minimal()

# Statistics summary for physical_health by program
summary_physical_health <- df %>%
  group_by(program) %>%
  summarise(
    Mean = mean(physical_health, na.rm = TRUE),
    Median = median(physical_health, na.rm = TRUE),
    SD = sd(physical_health, na.rm = TRUE),
    Min = min(physical_health, na.rm = TRUE),
    Max = max(physical_health, na.rm = TRUE)
  )
print(summary_physical_health)

# Statistics summary for psychological_wellbeing by program
summary_psychological_wellbeing <- df %>%
  group_by(program) %>%
  summarise(
    Mean = mean(psychological_wellbeing, na.rm = TRUE),
    Median = median(psychological_wellbeing, na.rm = TRUE),
    SD = sd(psychological_wellbeing, na.rm = TRUE),
    Min = min(psychological_wellbeing, na.rm = TRUE),
    Max = max(psychological_wellbeing, na.rm = TRUE)
  )
print(summary_psychological_wellbeing)
