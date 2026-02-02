# load the dataset
load(mysample.RData)

# check what breeds are in the data
table(mysample$Breed)

# create dataframe and assign mysample file to it
df <- mysample

# data cleaning
# calculate statistics for the missing data
# count rows where rehoming time is 99999
n_missing_rehomed <- sum(df$Rehomed == 99999, na.rm = TRUE)
pct_missing_rehomed <- (n_missing_rehomed / nrow(df)) * 100

# count rows where breed is NA
n_missing_breed <- sum(is.na(df$Breed))
pct_missing_breed <- (n_missing_breed / nrow(df)) * 100

# remove rows with missing values
df_clean <- df[df$`Rehomed` != 99999 & !is.na(df$Breed), ]

# calculate total removed
total_removed <- nrow(df) - nrow(df_clean)
pct_total_removed <- (total_removed /nrow(df)) * 100

# summary of the dropped rows
cat("Number of rows in original dataset:",nrow(df))
cat("Number of missing values in Rehomed:", n_missing_rehomed)
cat("Percentage of Missing Values in Rehomed:", pct_missing_rehomed)
cat("Number of missing values in Breed:", n_missing_breed)
cat("Percentage of missing values in Breed:", pct_missing_breed)
cat("Total observations removed:", total_removed)
cat("Percentage of total observations removed:", pct_total_removed)
cat("Final cleaned dataset size of n:", nrow(df_clean))

# data exploration
# load necessary packages
library(dplyr)
library(ggplot2)
library(broom)

# numerical summary of rehoming time
summary_table_rehomed <- df_clean %>%
  group_by(Breed) %>%
  summarise(
    Count = n(),
    mean_rehomed = mean(Rehomed, na.rm = TRUE),
    median_rehomed = median(Rehomed, na.rm = TRUE),
    sd_rehomed = sd(Rehomed, na.rm = TRUE), 
    iqr_rehomed = IQR(Rehomed, na.rm = TRUE),
  )
View(summary_table_rehomed)

# histogram
plot1 <- ggplot(df_clean, aes(x = Rehomed, fill = Breed)) +
  geom_histogram(bins = 20, color = "black", alpha = 0.7) +
  facet_wrap(~Breed, scales = "free_y") +
  labs(title = "Distribution of Rehoming Time per Breed",
       x = "Days to Rehome", y = "Count") +
  theme_minimal()
print(plot1)

# density
plot2 <- ggplot(df_clean, aes(x = Rehomed, fill = Breed)) +
  geom_histogram(aes(y = ..density..), bins = 20, color = "black", alpha = 0.5) +
  geom_density(alpha = 0.2, fill = NA, size = 1) + 
  facet_wrap(~Breed, scales = "free_y") +
  labs(title = "Distribution of Rehoming Time per Breed",
       x = "Weeks to Rehome", y = "Density") +
  theme_minimal()
print(plot2)

# create scatter plot of Rehomed vs Health
colors <- c("Rottweiler" = "red", "Stafford" = "darkgreen", "Westie" = "blue")

plot(
  x = df_clean$Health,
  y = df_clean$Rehomed,
  main = "Rehoming Time vs. Dog Health",
  xlab = "Health Score (0 = Worst, 100 = Perfect)",
  ylab = "Rehoming Time (Weeks)",
  pch = 16,
  col = colors[df_clean$Breed],
  cex = 1.2,
  ylim = c(0, max(df_clean$Rehomed, na.rm = TRUE) + 5)
)

legend(
  x = "topright",
  legend = c("Rottweiler", "Stafford", "Westie"),
  pch = 16,
  col = colors[df_clean$Breed],
  title = "Breed",
  cex = 0.8
)

abline(lm(Rehomed ~ Health, data = df_clean),
       col = "black",
       lty = 2,
       lwd = 3)

# Pearson correlation
correlation_results <- df_clean %>%
  group_by(Breed) %>%
  summarise(
    Pearson_R_Coefficient = cor(Rehomed, Health,
                                method = "pearson")
  )
print(correlation_results)

#modelling and estimation
#setup subsets for each breed
rottweiler_data <- df_clean$Rehomed[df_clean$Breed == "Rottweiler"]
stafford_data <- df_clean$Rehomed[df_clean$Breed == "Staffordshire Bull Terrier"]
westie_data <- df_clean$Rehomed[df_clean$Breed == "West Highland White Terrier"]

#shapiro-wilk test for normality
# h0: data is normally distributed. p < 0.05 implies rejection
sw_test_rottweiler <- shapiro.test(rottweiler_data)
sw_test_stafford <- shapiro.test(stafford_data)
sw_test_westie <- shapiro.test(westie_data)
print(sw_test_rottweiler)
print(sw_test_stafford)
print(sw_test_westie)

# t-test
# Rottweilers
rottweiler_ttest_data <- subset(df_clean, Breed == "Rottweiler")
t.test(rottweiler_ttest_data$Rehomed, mu = 27)

# Staffordshire Bull Terriers
stafford_ttest_data <- subset(df_clean, Breed == "Staffordshire Bull Terrier")
t.test(stafford_ttest_data$Rehomed, mu = 27)

# West Highland White Terriers
westie_ttest_data <- subset(df_clean, Breed == "West Highland White Terrier")
t.test(westie_ttest_data$Rehomed, mu = 27)

# prepare the data
plot_data <- df_clean %>%
  group_by(Breed) %>%
  do(tidy(t.test(.$Rehomed, mu = 27))) %>%
  mutate(
    label_text = sprintf("%.1f (%.1f, %.1f), p %s",
                         estimate, conf.low, conf.high,
                         ifelse(p.value < 0.001, "<0.001", sprintf("= %.3f", p.value)))
  )

# construct caterpillar CI plot
ggplot(plot_data, aes(x = estimate, y = Breed)) +
  geom_vline(xintercept = 27, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_point(size = 3) +
  geom_text(aes(label = label_text, x = 35), hjust = 0) +
  labs(
    title = "Mean Rehoming Time by Breed",
    subtitle = "95% Confidence Intervals",
    x = "Weeks",
    y = ""
  ) +
  theme_bw() +
  scale_x_continuous(limits = c(0, 50))

#comparison of variance between Stafford and Westie 
#check assumption of equal variances
var_stafford <- var(stafford_data)   #approx 111.594
var_westie <- var(westie_data)   #approx 102.279
var_ratio <- var_westie / var_stafford

# check against rule of thumb: assume equal variance if ratio is between 1/3 and 3
cat("Variance for Stafford:", var_stafford)
cat("Variance for Westie:", var_westie)
cat("Variance Ratio:", var_ratio) #ratio is 0.917, hence approximately equal

#perform two-sample t-test
#since ratio (~0.917) is within the range [1/3, 3], we set var.equal = TRUE
# we use paired = FALSE because these are independent samples
test_result <- t.test(x = westie_data,
                      y = stafford_data,
                      mu = 0,
                      paired = FALSE,
                      var.equal = TRUE,
                      conf.level = 0.95)
test_result

