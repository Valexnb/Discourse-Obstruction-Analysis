####Intercoder reliability

library(irr)
library(dplyr)

data <- IRR_data2

str(data)

coder_data <- data[, c("ASReview_lab_1", "ASReview_lab_2", "asreview_lab_3", "asreview_label")]

head(coder_data)

x <- as.matrix(coder_data)

print(kappam.fleiss(x))


####Checking Assumptions 

library(ggplot2)
library(gridExtra)


model <- aov(Amount ~ Region * COT, data = data_long)


residuals_model <- residuals(model)

# 1. Q-Q Plot
p1 <- ggplot(data.frame(residuals = residuals_model), aes(sample = residuals)) +
  stat_qq(color = "steelblue", size = 2) +
  stat_qq_line(color = "red", size = 1) +
  labs(title = "Q-Q Plot of Residuals",
       x = "Theoretical Quantiles",
       y = "Sample Quantiles") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p1)

# 2. Boxplot
p2 <- ggplot(data_long, aes(x = interaction(Region, COT), y = Amount, fill = Region)) +
  geom_boxplot() +
  labs(title = "Boxplot of Amount by Region and COT",
       x = "Region × COT",
       y = "Amount") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_fill_manual(values = c("QR" = "steelblue", "OX" = "coral"))

print(p2)

# 3. Histogram of Residuals
p3 <- ggplot(data.frame(residuals = residuals_model), aes(x = residuals)) +
  geom_histogram(aes(y = ..density..), bins = 20, fill = "steelblue", alpha = 0.7) +
  geom_density(color = "red", size = 1) +
  labs(title = "Histogram of Residuals",
       x = "Residuals",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p3)

# 4. Residuals vs Fitted
p4 <- ggplot(data.frame(fitted = fitted(model), residuals = residuals_model), 
             aes(x = fitted, y = residuals)) +
  geom_point(color = "steelblue", size = 2) +
  geom_hline(yintercept = 0, color = "red", linetype = "dashed", size = 1) +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))

print(p4)

# Arrange all plots together
grid.arrange(p1, p2, p3, p4, ncol = 2, nrow = 2)


#####Fisher's exact test for binary data 




for (cot_category in unique(data_for_anova$COT)) {
  cat("\n=== COT Category:", cot_category, "===\n")
  
  subset_data <- data_for_anova %>% filter(COT == cot_category)
  
  # Create contingency table
  contingency_table <- table(subset_data$Region, subset_data$Amount)
  print(contingency_table)
  
  # Fisher's Exact Test
  fisher_result <- fisher.test(contingency_table)
  cat("\nFisher's Exact Test p-value:", fisher_result$p.value, "\n")
  cat("Significant at α = 0.05:", ifelse(fisher_result$p.value < 0.05, "YES ✓", "NO ✗"), "\n")
}
