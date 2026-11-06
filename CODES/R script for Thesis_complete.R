#Data for labeling 
#After loading SAAMPLE_VALEX from supervisor

SAMPLE_VALEX$abstract <- as.character(SAMPLE_VALEX$ad_creative_bodies)
class(SAMPLE_VALEX$abstract)
SAMPLE_VALEX_text <- SAMPLE_VALEX[,-3]
sample_valex$title <- as.character(SAMPLE_VALEX$bylines)

SAMPLE_VALEX_text <- SAMPLE_VALEX

write_csv(SAMPLE_VALEX_text, "test_LO.csv")


#Data for intercoder reliability
ICR <- SAMPLE_VALEX_text[sample(5000, 100), ]

#Data for the labeling session 
data_Sofia <- SAMPLE_VALEX_text[1:1000, ]
data_Ellen <- SAMPLE_VALEX_text[1001:2000, ]
data_Sara <- SAMPLE_VALEX_text[2001:3000, ]
data_valex <- SAMPLE_VALEX_text[3001:5000, ]


#Packages  for Analysis 

library(irr)      
library(dplyr)      
library(tidyr)      
library(ggplot2)    
library(gridExtra)  
library(car)        

# Preparation of the data for intercoder reliability
# Read IRR data
IRR_data2 <- read.csv("IRR_data2.csv")  

# Read QR and OX data
QR <- read.csv("QR_data.csv")           
OX <- read.csv("OX_data.csv")           

# Preparation data for Two way anova
# Clean data (keep rows with at least one 1)
QR_clean <- QR[rowSums(QR[, 2:4] == 1) > 0, ] %>% mutate(Region = "QR")
OX_clean <- OX[rowSums(OX[, 2:4] == 1) > 0, ] %>% mutate(Region = "OX")

# Combine datasets
combined_data <- bind_rows(QR_clean, OX_clean)

# Rename columns for easier handling
colnames(combined_data) <- c("ID", "Phenomenon", "Action", "Source_Credibility", "Region")

# Reshape to long format for ANOVA
data_long <- combined_data %>%
  pivot_longer(
    cols = c(Phenomenon, Action, Source_Credibility),
    names_to = "COT",
    values_to = "Amount"
  )

# INTERCODER RELIABILITY

str(data)

# Select coder columns
coder_data <- data[, c("ASReview_lab_1", "ASReview_lab_2", "asreview_lab_3", "asreview_label")]

# View first few rows
head(coder_data)

# Convert to matrix
x <- as.matrix(coder_data)

# Calculate Fleiss' Kappa
fleiss_kappa <- kappam.fleiss(x)
print(fleiss_kappa)

# ANOVA ASSUMPTION CHECKS

#two-way ANOVA model
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

# FISHER'S EXACT TEST

data_for_anova <- data_long

fisher_results <- data.frame()

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
  
  # Store results
  fisher_results <- rbind(fisher_results, data.frame(
    COT = cot_category,
    p_value = fisher_result$p.value,
    Significant = ifelse(fisher_result$p.value < 0.05, "YES", "NO")
  ))
}


