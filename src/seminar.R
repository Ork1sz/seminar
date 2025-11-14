#########################################################################

# This file is used to run several models in many different specifications and can widely be disregarded as the most important findings can be obtained through the python notebook

##########################################################################

#Load libraries
library(logistf)
library(dplyr)
library(MLmetrics)
library(caret)
library(sandwich)
library(lmtest)
library(car)
library(corrplot)
library(tidyverse)
library(nlme)
library(polyreg)
library(nnet)
library(fixest)

# Set direction
setwd("C://Users//orkan//OneDrive//Pulpit//Seminar//Team 2//Team 2")
df_full <- read.csv('bay_map_f.csv', header = TRUE)

#The selection of variables is done ad-hoc and does not correspond with the final selection
X_logit <- df_full %>%
  select(Industry.and.Commerce_std, Nr_schools_cont_std, Apartments_cont_std, Residential_Square_meters_cont_std, financial_aid_std) %>%
  replace(is.na(.), 0)

# Define y
y_logit <- df_full$Ref_cent
data_logit <- data.frame(y = y_logit, X_logit)



# Logit Model -------------------------------------------------------------

logisticBin_simple <- glm(y ~ .-Nr_schools_cont_std-Industry.and.Commerce_std,
                   data = data_logit,
                   family = binomial)
summary(logisticBin_simple)

nonlinear_logit <- glm(formula = y ~ (Industry.and.Commerce_std + financial_aid_std + 
                     Apartments_cont_std + Residential_Square_meters_cont_std)^2 + 
                    I(Industry.and.Commerce_std^2) + I(Nr_schools_cont_std^2) + 
                    I(Apartments_cont_std^2) + I(Residential_Square_meters_cont_std^2),
                    family = binomial, data = data_logit)

summary(nonlinear_logit)

nonlinear_logit_signf <-  glm(formula = y ~ (Industry.and.Commerce_std + Nr_schools_cont_std + 
                                               Apartments_cont_std + Residential_Square_meters_cont_std)^2 + 
                                I(Industry.and.Commerce_std^2) + I(Nr_schools_cont_std^2) + 
                                I(Apartments_cont_std^2) + I(Residential_Square_meters_cont_std^2) - 
                                Nr_schools_cont_std:Residential_Square_meters_cont_std - 
                                Nr_schools_cont_std:Apartments_cont_std - Industry.and.Commerce_std:Nr_schools_cont_std - 
                                Industry.and.Commerce_std:Residential_Square_meters_cont_std, 
                              family = binomial, data = data_logit)
summary(nonlinear_logit_signf)

logisticBin <- glm(y ~ Nr_schools_cont_std +
                      Apartments_cont_std + Residential_Square_meters_cont_std +
                     I(Nr_schools_cont_std^2) +
                     I(Apartments_cont_std^2),
                   data = data_logit,
                   family = binomial)
summary(logisticBin)

robust_e <- sqrt(diag(vcovHC(logisticBin, type="HC0")))
summary(robust_e)
probs_1 <- predict(logisticBin_simple, type = "response")

brier_glm <- mean((probs_1 - data_logit$y)^2)
cat("Brier Score (GLM):", round(brier_glm, 4), "\n")

# Try a grid of thresholds from 0.01 to 0.99
thresholds_1 <- seq(0.01, 0.99, by = 0.01)
f1_scores_1 <- sapply(thresholds_1, function(t) {
  preds_1 <- ifelse(probs_1 >= t, 1, 0)
  F1_Score(y_pred = preds_1, y_true = data_logit$y, positive = "1")
})

# Find the best threshold
best_idx_1 <- which.max(f1_scores_1)
best_threshold_1 <- thresholds_1[best_idx_1]
best_f1_1 <- f1_scores_1[best_idx_1]

cat("🔍 Best threshold:", round(best_threshold_1, 2), "\n")
cat("📈 Best F1 score:", round(best_f1_1, 3), "\n")


pred_class_1 <- ifelse(probs_1 >= best_threshold_1, 1, 0)

confusionMatrix(factor(pred_class_1), factor(data_logit$y), positive = "1")



# Fixed Effect Model (second stage) --------------------------------------

EM <- exp(predict(logisticBin, type = "link"))

# Stack the two AFD share columns
AFD_df <- data.frame(
  AFD_share = c(df_full$AFD_share_13, df_full$AFD_share_17),
  group = c(rep(0, nrow(df_full)), rep(1, nrow(df_full))),
  EM_value = c(rep(0, nrow(df_full)), EM),
  ZIP = c(df_full$ZIP, df_full$ZIP),
  Election = c(df_full$Election_13, df_full$Election_17)
)
AFD_df$ZIP_first4 <- substr(as.character(AFD_df$ZIP), 1, 4)

# Model: Y ~ EM * Post2015 + time and district fixed effects
final_model <- feols(AFD_share ~ I(EM_value * group) | ZIP+Election, data = AFD_df)

# Summary output
summary(final_model)

resid_fm <- resid(final_model)
plot(AFD_df$AFD_share, resid_fm)

# Firth Model -------------------------------------------------------------

firth_model <- logistf(y ~ ., data = data_logit)
summary(firth_model)
probs_2 <- predict(firth_model, type = "response")

brier_firth <- mean((probs_2 - data_logit$y)^2)
cat("Brier Score (Firth):", round(brier_firth, 4), "\n")

# Try a grid of thresholds from 0.01 to 0.99
thresholds_2 <- seq(0.01, 0.99, by = 0.01)
f1_scores_2 <- sapply(thresholds_2, function(t) {
  preds_2 <- ifelse(probs_2 >= t, 1, 0)
  F1_Score(y_pred = preds_2, y_true = data_logit$y, positive = "1")
})

# Find the best threshold
best_idx_2 <- which.max(f1_scores_2)
best_threshold_2 <- thresholds_1[best_idx_2]
best_f1_2 <- f1_scores_2[best_idx_2]

cat("🔍 Best threshold:", round(best_threshold_2, 2), "\n")
cat("📈 Best F1 score:", round(best_f1_2, 3), "\n")


pred_class_2 <- ifelse(probs_2 >= best_threshold_2, 1, 0)

confusionMatrix(factor(pred_class_2), factor(data_logit$y), positive = "1")

# Testing -----------------------------------------------------------------
# LM TEST for Heterogeniety (Marketing Models)
data_logit$phat <- predict(logisticBin, type = "response")
data_logit$std_resid <- (data_logit$y - data_logit$phat) / sqrt(data_logit$phat * (1 - data_logit$phat))
data_logit$f_score <- data_logit$phat * (1 - data_logit$phat)

X_mat <- model.matrix(logisticBin)
Z <- data_logit[, c("Industry.and.Commerce_std", "Nr_schools_cont_std", "Apartments_cont_std", "Residential_Square_meters_cont_std")]  

# Calculate -Xβ
xbeta <- predict(logisticBin, type = "link")  # linear predictor

# Create interaction terms
data_logit$X_weighted <- data_logit$f_score / sqrt(data_logit$phat * (1 - data_logit$phat))
data_logit$XZ_weighted <- data_logit$X_weighted * (-xbeta)

# Build regression design matrix
aux_data <- cbind(data_logit$X_weighted * X_mat[, -1],
                  data_logit$XZ_weighted * as.matrix(Z))

# Final auxiliary regression
aux_model <- lm(data_logit$std_resid ~ aux_data)
summary(aux_model)


plot(data_logit$phat, (data_logit$std_resid), main = "Residuals vs Index", xlab = "phat", ylab = "Pearson Residuals")
abline(h = 0, col = "red")

#RESET Test for Misspecification --------------------------------------
data_logit$log_odds <- predict(logisticBin, type = "link")
data_logit$log_odds_sq <- data_logit$log_odds^2
data_logit$log_odds_cu <- data_logit$log_odds^3
data_logit$log_odds_qua <- data_logit$log_odds^4

logit_reset <- glm(y ~ (Industry.and.Commerce_std + Nr_schools_cont_std +
                          Apartments_cont_std + Residential_Square_meters_cont_std)^2 +
                     I(Industry.and.Commerce_std^2) +
                     I(Nr_schools_cont_std^2) +
                     I(Apartments_cont_std^2) +
                     I(Residential_Square_meters_cont_std^2)-
                     Nr_schools_cont_std:Residential_Square_meters_cont_std-
                     Nr_schools_cont_std:Apartments_cont_std-
                     Industry.and.Commerce_std:Nr_schools_cont_std-
                     Industry.and.Commerce_std:Residential_Square_meters_cont_std+
                     log_odds_cu + log_odds_qua, family = binomial, data = data_logit)
summary(logit_reset)

robust_e_reset <- sqrt(diag(vcovHC(logit_reset, type="HC0")))
robust_vcov <- vcovHC(logit_reset, type = "HC3")


linearHypothesis(
  model = logit_reset,
  hypothesis.matrix = c("log_odds_cu = 0", "log_odds_qua = 0"),
  vcov = robust_vcov,
  test = "Chisq"  # Wald test is chi-squared for GLMs
)

BIC(logit_reset)
BIC(logisticBin)

AIC(logit_reset)
AIC(logisticBin)

# Multicollinearity Test --------------------------------------------------
X <- model.matrix(
  ~ (Industry.and.Commerce_std + Nr_schools_cont_std +
       Apartments_cont_std + Residential_Square_meters_cont_std)^2 +
    I(Industry.and.Commerce_std^2) +
    I(Nr_schools_cont_std^2) +
    I(Apartments_cont_std^2) +
    I(Residential_Square_meters_cont_std^2) -
    Nr_schools_cont_std:Residential_Square_meters_cont_std -
    Nr_schools_cont_std:Apartments_cont_std -
    Industry.and.Commerce_std:Nr_schools_cont_std -
    Industry.and.Commerce_std:Residential_Square_meters_cont_std,
  data = data_logit
)
X_no_intercept <- X[, -1]
cor_matrix <- cor(X_no_intercept)

eig <- eigen(cor_matrix)
eig$values

round(cor_matrix, 3) 

corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8)


# Filtered Analysis -------------------------------------------------------

df_filtered <- df_full[!is.na(df_full$Total_Capacity) & df_full$Total_Capacity != 0, ]

X_filt <- df_filtered %>%
  select(Nr_schools_cont_std, Leisure.Facilities_std, Residential.Area_std, Revenue_per_capita, ) %>%
  replace(is.na(.), 0)

# Define y
y_filt <- df_filtered$Ref_cent_cont
data_filtered <- data.frame(y = y_filt, X_filt)
data_filtered$log_y <- log(y_filt)




# Multinomial regression approach -----------------------------------------
num_bins = 2

breaks <- unique(quantile(data_filtered$y, probs = seq(0, 1, length.out = num_bins + 1), na.rm = TRUE))

data_filtered$y_group <- cut(data_filtered$y,
                             breaks = breaks,
                             include.lowest = TRUE,
                             labels = FALSE)

print(sum(data_filtered$y_group == 1, na.rm = TRUE))
print(sum(data_filtered$y_group == 2, na.rm = TRUE))


data_filtered$y_group <- as.factor(data_filtered$y_group)

model_multinom <- multinom(y_group ~ ., data = data_filtered[, !colnames(data_filtered) %in% c("y", "log_y", "Leisure.Facilities_std", "milt_n_std", "Residential.Area_std")])
summary(model_multinom)

coefs <- summary(model_multinom)$coefficients
ses <- summary(model_multinom)$standard.errors

t_stats <- coefs / ses
p_values <- 2 * (1 - pnorm(abs(t_stats)))
print(p_values)



for (varname in names(data_filtered)) {
  if (is.numeric(data_filtered[[varname]])) {
    plot(data_filtered[[varname]],data_filtered$y,
         type = "p",
         main = paste(varname, "vs. y"),
         col = "steelblue", pch = 16)
  }
}





# Linear Model OLS/GLS models ---------------------------------------------
linModel <- glm(I(log(y))~., data = data_filtered)
summary(linModel)

# Residuals Analysis
whitetest <- bptest(linModel, ~ .^2, data = FGLSdata)
print(whitetest)

coeftest(linModel, vcov = vcovHC(linModel, type = "HC0"))

trend <- seq(1,length(resid(linModel))) 
residuals_linM <- resid(linModel)


sorted_order <- order(data_filtered$log_y)
sorted_log_y <- data_filtered$log_y[sorted_order]

data_sorted <- data_filtered[sorted_order, ]

plot(seq_along(sorted_log_y), sorted_log_y,
     main = "log(y) (sorted)",
     xlab = "Index (sorted by log(y))", ylab = "log(y)",
     pch = 16, col = "darkred")

plot(data_sorted$Residential_Square_meters_cont_std, sorted_log_y,
     main = "log(y) (sorted)",
     xlab = "NR Schools", ylab = "log(y)",
     pch = 16, col = "darkred")
sorted_index <- 1:nrow(data_sorted)

# Loop through each variable
for (varname in names(data_sorted)) {
  if (is.numeric(data_sorted[[varname]])) {
    plot(sorted_index, data_sorted[[varname]],
         type = "p",
         main = paste(varname, "vs. sorted index"),
         xlab = "Sorted Index",
         ylab = varname,
         col = "steelblue", pch = 16)
  }
}

abline(h = 0, col = "red")

hist(data_filtered$log_y)
vif(linModel)

# First stage continous case reg ------------------------------------------
max_degree <- 10
aic_scores <- numeric(max_degree)
bic_scores <- numeric(max_degree)
for (d in 1:max_degree) {
  formula_poly <- as.formula(
    paste0("log_y ~ ", 
           paste(paste0("poly(", names(X_filt), ",", d, ", raw = TRUE)"), collapse = " + "))
  )
  
  model <- lm(formula_poly, data = data_filtered)
  aic_scores[d] <- AIC(model)
  bic_scores[d] <- BIC(model)
}

# Plot AIC for each degree
plot(1:max_degree, aic_scores, type = "b", pch = 19,
     xlab = "Polynomial Degree", ylab = "AIC",
     main = "Polynomial Degree Optimization (AIC)")
# Plot BIC for each degree
plot(1:max_degree, bic_scores, type = "b", pch = 19,
     xlab = "Polynomial Degree", ylab = "BIC",
     main = "Polynomial Degree Optimization (BIC)")

# Assuming X_filt is already created as:
X_filt <- df_filtered %>%
  select(Nr_schools_cont_std, Leisure.Facilities_std, Residential.Area_std, milt_n_std, Industry.and.Commerce_std) %>%
  replace(is.na(.), 0)

poly_matrix <- model.matrix(~ 
                                    poly(Nr_schools_cont_std, 3, raw = TRUE) +
                                    poly(Leisure.Facilities_std, 3, raw = TRUE) +
                                    poly(Residential.Area_std, 3, raw = TRUE) +
                                    poly(milt_n_std, 3, raw = TRUE) +
                                    poly(Industry.and.Commerce_std, 3, raw = TRUE) +
                                    
                                    # 2-way interactions
                                    I(Nr_schools_cont_std * Leisure.Facilities_std) +
                                    I(Nr_schools_cont_std * Residential.Area_std) +
                                    I(Nr_schools_cont_std * milt_n_std) +
                                    I(Nr_schools_cont_std * Industry.and.Commerce_std) +
                                    I(Leisure.Facilities_std * Residential.Area_std) +
                                    I(Leisure.Facilities_std * milt_n_std) +
                                    I(Leisure.Facilities_std * Industry.and.Commerce_std) +
                                    I(Residential.Area_std * milt_n_std) +
                                    I(Residential.Area_std * Industry.and.Commerce_std) +
                                    I(milt_n_std * Industry.and.Commerce_std) +
                                    
                                    # 2-way squared interactions
                                    I(Nr_schools_cont_std^2 * Leisure.Facilities_std) +
                                    I(Nr_schools_cont_std * Leisure.Facilities_std^2) +
                                    I(Nr_schools_cont_std^2 * Residential.Area_std) +
                                    I(Nr_schools_cont_std * Residential.Area_std^2) +
                                    I(Nr_schools_cont_std^2 * milt_n_std) +
                                    I(Nr_schools_cont_std * milt_n_std^2) +
                                    I(Nr_schools_cont_std^2 * Industry.and.Commerce_std) +
                                    I(Nr_schools_cont_std * Industry.and.Commerce_std^2) +
                                    
                                    I(Leisure.Facilities_std^2 * Residential.Area_std) +
                                    I(Leisure.Facilities_std * Residential.Area_std^2) +
                                    I(Leisure.Facilities_std^2 * milt_n_std) +
                                    I(Leisure.Facilities_std * milt_n_std^2) +
                                    I(Leisure.Facilities_std^2 * Industry.and.Commerce_std) +
                                    I(Leisure.Facilities_std * Industry.and.Commerce_std^2) +
                                    
                                    I(Residential.Area_std^2 * milt_n_std) +
                                    I(Residential.Area_std * milt_n_std^2) +
                                    I(Residential.Area_std^2 * Industry.and.Commerce_std) +
                                    I(Residential.Area_std * Industry.and.Commerce_std^2) +
                                    
                                    I(milt_n_std^2 * Industry.and.Commerce_std) +
                                    I(milt_n_std * Industry.and.Commerce_std^2) +
                                    
                                    # 3-way interactions
                                    I(Nr_schools_cont_std * Leisure.Facilities_std * Residential.Area_std) +
                                    I(Nr_schools_cont_std * Leisure.Facilities_std * milt_n_std) +
                                    I(Nr_schools_cont_std * Leisure.Facilities_std * Industry.and.Commerce_std) +
                                    I(Nr_schools_cont_std * Residential.Area_std * milt_n_std) +
                                    I(Nr_schools_cont_std * Residential.Area_std * Industry.and.Commerce_std) +
                                    I(Nr_schools_cont_std * milt_n_std * Industry.and.Commerce_std) +
                                    I(Leisure.Facilities_std * Residential.Area_std * milt_n_std) +
                                    I(Leisure.Facilities_std * Residential.Area_std * Industry.and.Commerce_std) +
                                    I(Leisure.Facilities_std * milt_n_std * Industry.and.Commerce_std) +
                                    I(Residential.Area_std * milt_n_std * Industry.and.Commerce_std)
                                  
                                  , data = X_filt)

FGLSdata <- data.frame(y = data_filtered$log_y, poly_matrix)

OLSreg <- glm(y ~., data = FGLSdata)
summary(OLSreg)

resids_OLS <- resid(OLSreg)

plot(FGLSdata$y, resids_OLS)
hist(resids_OLS)

whitetest <- bptest(OLSreg, ~ .^2, data = FGLSdata)
print(whitetest)

Omega_hat <- resids_OLS %*% t(resids_OLS)
P_hat <- chol(Omega_hat)

eigenvals_Omega_hat <- eigen(Omega_hat)
print(eigenvals_Omega_hat)

# The full manual FGLS model was abandoned after it became clear that it would not enhance efficiency.
