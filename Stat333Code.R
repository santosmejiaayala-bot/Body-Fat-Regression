# =========================
# BODY FAT ANALYSIS PROJECT
# =========================

# Load libraries
library(ggplot2)
library(dplyr)
library(GGally)

# -------------------------
# 1. LOAD DATA
# -------------------------
data <- read.csv("BodyFat.csv")

# Inspect structure
str(data)
summary(data)

# -------------------------
# 2. DATA CLEANING
# -------------------------

# Remove ID column (not useful for modeling) and Density column (mathematically linked to body fat)
data <- data %>% select(-IDNO)
data <- data %>% select(-DENSITY)

# Check for missing values
colSums(is.na(data))

# Remove unrealistic body fat values (if any)
data <- data %>% filter(BODYFAT > 0 & BODYFAT < 60)

# -------------------------
# 3. EXPLORATORY ANALYSIS
# -------------------------

# Histogram of body fat
hist_plot <- ggplot(data, aes(x = BODYFAT)) +
  geom_histogram(bins = 20) +
  labs(title = "Distribution of Body Fat", x = "Body Fat (%)") +
  theme_minimal()

ggsave("hist_bodyfat.png", hist_plot, width = 6, height = 4)

# Scatterplot: BODYFAT vs ABDOMEN
scatter_plot <- ggplot(data, aes(x = ABDOMEN, y = BODYFAT)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Body Fat vs Abdomen Circumference") +
  theme_minimal()

ggsave("scatter_abdomen.png", scatter_plot, width = 6, height = 4)

# Correlation matrix
cor_matrix <- cor(data)
print(cor_matrix)

# -------------------------
# 4. MODEL BUILDING
# -------------------------

full_model <- lm(BODYFAT ~ ., data = data)
summary(full_model)

model1 <- lm(BODYFAT ~ ABDOMEN, data = data)
summary(model1)

model2 <- lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST, data = data)
summary(model2)
# -------------------------
# 5. MODEL COMPARISON
# -------------------------

anova(model1, full_model)
anova(model2, full_model)
anova(model1, model2)

# -------------------------
# 6. FINAL MODEL
# -------------------------

final_model <- model2
summary(final_model)

# -------------------------
# 7. MODEL DIAGNOSTICS
# -------------------------

# Residual plot
residual_plot <- ggplot(data, aes(x = ABDOMEN, y = residuals(final_model))) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title = "Residual Plot") +
  theme_minimal()

ggsave("residual_plot.png", residual_plot, width = 6, height = 4)

# Diagnostic plots (base R)
png("diagnostic_plots.png", width = 800, height = 800)
par(mfrow = c(2,2))
plot(final_model)
dev.off()

# -------------------------
# 8. OUTLIER CHECK
# -------------------------

# Cook's distance
cooks <- cooks.distance(final_model)

png("cooks_distance.png", width = 600, height = 400)
plot(cooks, type = "h", main = "Cook's Distance")
dev.off()

i <- which.max(cooks.distance(final_model))
dataOutlierless <- data[-i, ]
modelOutlierless <- lm(BODYFAT ~ ABDOMEN + WEIGHT + WRIST, data = dataOutlierless)
summary(modelOutlierless)
# -------------------------
# 9. PREDICTION EXAMPLE
# -------------------------

new_person <- data.frame(ABDOMEN = 90)
predict(final_model, newdata = new_person, interval = "confidence")

# -------------------------
# 10. FINAL INTERPRETATION
# -------------------------

coef(final_model)