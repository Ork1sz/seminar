########################################################################

# Turnout Analysis (In this file several regressions on the effect on turnout are run)

########################################################################

# Load required libraries
library(sf)
library(dplyr)
library(ggplot2)
library(broom)
library(writexl)
library(fixest)
library(modelsummary)
library(mediation)
library(sensitivity)
#---Data Setting ----
# Set the path to your shapefile (without the .shp extension)
setwd('/set/your/directory')

# Read shapefile
shape_data <- st_read("bay_map_full.shp")




# Drop geometry to keep only data
df <- read.csv("bay_map_f.csv")

df <- df %>%
  mutate(
    Turnout_13       = as.numeric(gsub(",", ".", Turnout_13)),
    Turnout_17       = as.numeric(gsub(",", ".", Turnout_17))
  )


df <- df %>%
  mutate(
    delta_turnout       = Turnout_17 - Turnout_13,
    delta_afd_share     = (AFD_share_17     - AFD_share_13)     * 100,
    delta_cdu_csu_share = (CDU_CSU_share_17 - CDU_CSU_share_13) * 100,
    delta_fdp_share     = (FDP_share_17     - FDP_share_13)     * 100,
    delta_spd_share     = (SPD_share_17     - SPD_share_13)     * 100,
    delta_gruene_share  = (GRUENE_share_17  - GRUENE_share_13)  * 100,
    delta_linke_share   = (LINKE_share_17   - LINKE_share_13)   * 100,
    delta_other_share   = (Other_share_17   - Other_share_13)   * 100
  )


# Add EM as a new column to your existing dataframe
df$EM <- EM

# Basic scatter plot
ggplot(df, aes(x = Turnout_13, y = Turnout_17)) +
  geom_point(color = "steelblue", alpha = 0.7) +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed", color = "red") +
  labs(
    title = "Voter Turnout: 2013 vs 2017",
    x = "Turnout 2013 (%)",
    y = "Turnout 2017 (%)"
  ) +
  theme_minimal()

#--- Naive Delta regression ----

# Run regression models
model_afd     <- lm(delta_afd_share     ~ delta_turnout, data = df)
model_cdu     <- lm(delta_cdu_csu_share ~ delta_turnout, data = df)
model_fdp     <- lm(delta_fdp_share     ~ delta_turnout, data = df)
model_spd     <- lm(delta_spd_share     ~ delta_turnout, data = df)
model_gruene  <- lm(delta_gruene_share  ~ delta_turnout, data = df)
model_linke   <- lm(delta_linke_share   ~ delta_turnout, data = df)
model_other   <- lm(delta_other_share   ~ delta_turnout, data = df)

# Show regression summaries
summary(model_afd)
summary(model_cdu)
summary(model_fdp)
summary(model_spd)
summary(model_gruene)
summary(model_linke)
summary(model_other)


# Tidy up all regression models
results <- bind_rows(
  tidy(model_afd)     %>% mutate(party = "AFD"),
  tidy(model_cdu)     %>% mutate(party = "CDU/CSU"),
  tidy(model_fdp)     %>% mutate(party = "FDP"),
  tidy(model_spd)     %>% mutate(party = "SPD"),
  tidy(model_gruene)  %>% mutate(party = "GRUENE"),
  tidy(model_linke)   %>% mutate(party = "LINKE"),
  tidy(model_other)   %>% mutate(party = "Other")
)

# Filter only the delta_turnout coefficient rows (skip intercepts)
results_filtered <- results %>%
  filter(term == "delta_turnout") %>%
  select(party, estimate, std.error, statistic, p.value)

# Write to Excel file
write_xlsx(results_filtered, "regression_results_by_party.xlsx")




#--- Naive Delta share regression ----


# Run regression models
model_afd     <- lm(delta_afd_share     ~ Turnout_17, data = df)
model_cdu     <- lm(delta_cdu_csu_share ~ Turnout_17, data = df)
model_fdp     <- lm(delta_fdp_share     ~ Turnout_17, data = df)
model_spd     <- lm(delta_spd_share     ~ Turnout_17, data = df)
model_gruene  <- lm(delta_gruene_share  ~ Turnout_17, data = df)
model_linke   <- lm(delta_linke_share   ~ Turnout_17, data = df)
model_other   <- lm(delta_other_share   ~ Turnout_17, data = df)

# Show regression summaries
summary(model_afd)
summary(model_cdu)
summary(model_fdp)
summary(model_spd)
summary(model_gruene)
summary(model_linke)
summary(model_other)


# Tidy up all regression models
results <- bind_rows(
  tidy(model_afd)     %>% mutate(party = "AFD"),
  tidy(model_cdu)     %>% mutate(party = "CDU/CSU"),
  tidy(model_fdp)     %>% mutate(party = "FDP"),
  tidy(model_spd)     %>% mutate(party = "SPD"),
  tidy(model_gruene)  %>% mutate(party = "GRUENE"),
  tidy(model_linke)   %>% mutate(party = "LINKE"),
  tidy(model_other)   %>% mutate(party = "Other")
)

# Filter only the delta_turnout coefficient rows (skip intercepts)
results_filtered <- results %>%
  filter(term == "Turnout_17") %>%
  select(party, estimate, std.error, statistic, p.value)

# Write to Excel file
write_xlsx(results_filtered, "regression_results_by_party_real_turnout.xlsx")




#--- Naive real regression ----



# Run regression models using AFD_share_17 and other parties' shares
model_afd     <- lm(AFD_share_17*100     ~ Turnout_17, data = df)
model_cdu     <- lm(CDU_CSU_share_17*100 ~ Turnout_17, data = df)
model_fdp     <- lm(FDP_share_17*100     ~ Turnout_17, data = df)
model_spd     <- lm(SPD_share_17*100     ~ Turnout_17, data = df)
model_gruene  <- lm(GRUENE_share_17*100  ~ Turnout_17, data = df)
model_linke   <- lm(LINKE_share_17*100   ~ Turnout_17, data = df)
model_other   <- lm(Other_share_17*100   ~ Turnout_17, data = df)


# Show regression summaries
summary(model_afd)
summary(model_cdu)
summary(model_fdp)
summary(model_spd)
summary(model_gruene)
summary(model_linke)
summary(model_other)


# Tidy up all regression models
results <- bind_rows(
  tidy(model_afd)     %>% mutate(party = "AFD"),
  tidy(model_cdu)     %>% mutate(party = "CDU/CSU"),
  tidy(model_fdp)     %>% mutate(party = "FDP"),
  tidy(model_spd)     %>% mutate(party = "SPD"),
  tidy(model_gruene)  %>% mutate(party = "GRUENE"),
  tidy(model_linke)   %>% mutate(party = "LINKE"),
  tidy(model_other)   %>% mutate(party = "Other")
)

# Filter only the delta_turnout coefficient rows (skip intercepts)
results_filtered <- results %>%
  filter(term == "Turnout_17") %>%
  select(party, estimate, std.error, statistic, p.value)

# Write to Excel file
write_xlsx(results_filtered, "regression_results_by_party_real_everything.xlsx")





#--- Naive Delta turnout regression ----


# Run regression models using AFD_share_17 and other parties' shares
model_afd     <- lm(AFD_share_17*100     ~ delta_turnout, data = df)
model_cdu     <- lm(CDU_CSU_share_17*100 ~ delta_turnout, data = df)
model_fdp     <- lm(FDP_share_17*100     ~ delta_turnout, data = df)
model_spd     <- lm(SPD_share_17*100     ~ delta_turnout, data = df)
model_gruene  <- lm(GRUENE_share_17*100  ~ delta_turnout, data = df)
model_linke   <- lm(LINKE_share_17*100   ~ delta_turnout, data = df)
model_other   <- lm(Other_share_17*100   ~ delta_turnout, data = df)


# Show regression summaries
summary(model_afd)
summary(model_cdu)
summary(model_fdp)
summary(model_spd)
summary(model_gruene)
summary(model_linke)
summary(model_other)


# Tidy up all regression models
results <- bind_rows(
  tidy(model_afd)     %>% mutate(party = "AFD"),
  tidy(model_cdu)     %>% mutate(party = "CDU/CSU"),
  tidy(model_fdp)     %>% mutate(party = "FDP"),
  tidy(model_spd)     %>% mutate(party = "SPD"),
  tidy(model_gruene)  %>% mutate(party = "GRUENE"),
  tidy(model_linke)   %>% mutate(party = "LINKE"),
  tidy(model_other)   %>% mutate(party = "Other")
)

# Filter only the delta_turnout coefficient rows (skip intercepts)
results_filtered <- results %>%
  filter(term == "delta_turnout") %>%
  select(party, estimate, std.error, statistic, p.value)

# Write to Excel file
write_xlsx(results_filtered, "regression_results_by_party_real_vote.xlsx")


# --- Regress Turnout on EM ----

model_em <- lm(delta_turnout ~ EM, data = df)

summary(model_em)

model_em2 <- lm(Turnout_17/100 ~ EM, data = df)

summary(model_em2)






# --- Final model including turnout as fixed ----

# Stack the two AFD share columns
AFD_df <- data.frame(
  AFD_share = c(df_full$AFD_share_13, df_full$AFD_share_17),
  group = c(rep(0, nrow(df_full)), rep(1, nrow(df_full))),
  EM_value = c(rep(0, nrow(df_full)), EM),
  ZIP = c(df_full$ZIP, df_full$ZIP),
  Election = c(df_full$Election_13, df_full$Election_17),
  Turnout = c (df$Turnout_13, df$Turnout_17)
)
AFD_df$ZIP_first4 <- substr(as.character(AFD_df$ZIP), 1, 4)

# Model: Y ~ EM * Post2015 + time and district fixed effects
model <- feols(AFD_share ~ I(EM_value * group) | ZIP_first4+Election+Turnout, data = AFD_df)

# Summary output
summary(model)



# Stack the two AFD share columns
AFD_df <- data.frame(
  AFD_share = c(df_full$AFD_share_13, df_full$AFD_share_17),
  group = c(rep(0, nrow(df_full)), rep(1, nrow(df_full))),
  EM_value = c(rep(0, nrow(df_full)), EM),
  ZIP = c(df_full$ZIP, df_full$ZIP),
  Election = c(df_full$Election_13, df_full$Election_17),
  Turnout = c (rep(0, nrow(df_full)), df$delta_turnout)
)
AFD_df$ZIP_first4 <- substr(as.character(AFD_df$ZIP), 1, 4)

# Model: Y ~ EM * Post2015 + time and district fixed effects
model2 <- feols(AFD_share ~ I(EM_value * group) | ZIP_first4+Election+Turnout, data = AFD_df)

# Summary output
summary(model2)

# --- final model including turnout as explonetary variable ----




# Stack the two AFD share columns
AFD_df <- data.frame(
  AFD_share = c(df_full$AFD_share_13, df_full$AFD_share_17),
  group = c(rep(0, nrow(df_full)), rep(1, nrow(df_full))),
  EM_value = c(rep(0, nrow(df_full)), EM),
  ZIP = c(df_full$ZIP, df_full$ZIP),
  Election = c(df_full$Election_13, df_full$Election_17),
  Turnout = c (df$Turnout_13, df$Turnout_17)
)
AFD_df$ZIP_first4 <- substr(as.character(AFD_df$ZIP), 1, 4)

# Model: Y ~ EM * Post2015 + time and district fixed effects
model3 <- feols(AFD_share ~ I(EM_value * group) + Turnout | ZIP_first4+Election, data = AFD_df)

# Summary output
summary(model3)

# --- Final model turnout as dependent variable ----


# Stack the two AFD share columns
AFD_df <- data.frame(
  AFD_share = c(df$AFD_share_13*100, df$AFD_share_17*100),
  group = c(rep(0, nrow(df)), rep(1, nrow(df))),
  EM_value = c(rep(0, nrow(df)), df$EM),
  ZIP = c(df$ZIP, df$ZIP),
  Election = c(df$Election_13, df$Election_17),
  Delta_Turnout = c (rep(0, nrow(df)), df$Turnout_17-df$Turnout_13),
  Turnout = c (df$Turnout_13, df$Turnout_17),
  Neighbour = c (rep(0, nrow(df)), df$neighbor_prob_mean)
)
AFD_df$ZIP_first4 <- substr(as.character(AFD_df$ZIP), 1, 4)

# Model: Y ~ EM * Post2015 + time and district fixed effects
model4 <- feols(Turnout ~ I(EM_value * group) + I(Neighbour * group) | ZIP+Election, data = AFD_df)
model_t <-  feols(AFD_share ~   Turnout | ZIP+Election, data = AFD_df)
model_dt <- feols(AFD_share ~  I (Delta_Turnout * group)| ZIP+Election, data = AFD_df)
model4 <- feols(AFD_share ~ I(EM_value * group) + I(Neighbour * group) | ZIP+Election, data = AFD_df)
model4 <- feols(AFd_share ~ I(EM_value * group) + I(Neighbour * group) + I(Delta_Turnout * EM_value * group) + I (Delta_Turnout * Group) | ZIP+Election, data = AFD_df)


# Summary output
summary(model4)
summary(model_t)
summary(model_dt)

model <- list (
  "Turnout" = model4,
  "AFD share" = model_t
)

modelsummary(model, "model_t.xlsx")
# --- Final model delta turnout as an interatction ----

# Stack the two AFD share columns
AFD_df <- data.frame(
  AFD_share = c(df_full$AFD_share_13, df_full$AFD_share_17),
  group = c(rep(0, nrow(df_full)), rep(1, nrow(df_full))),
  EM_value = c(rep(0, nrow(df_full)), EM),
  ZIP = c(df_full$ZIP, df_full$ZIP),
  Election = c(df_full$Election_13, df_full$Election_17),
  Turnout = c (rep(0, nrow(df_full)), df$delta_turnout)
)
AFD_df$ZIP_first4 <- substr(as.character(AFD_df$ZIP), 1, 4)

# Model: Y ~ EM * Post2015 + time and district fixed effects
model6 <- feols(AFD_share ~ I(EM_value * Turnout * group) + I(EM_value * group) + I(Turnout * group) | ZIP + Election, data = AFD_df)

# Summary output
summary(model6)


# --- Mediation analysis ---- 

# Mediator model: Turnout ~ EM
med.fit <- lm(Turnout_17 ~ EM + AFD_share_13 , data = df)

# Outcome model: AFD ~ EM + Turnout
out.fit <- lm(AFD_share_17 ~ EM + Turnout_17 +AFD_share_13, data = df)


med.out <- mediate(med.fit, out.fit, treat = "EM", mediator = "Turnout_17", sims = 1000)
summary(med.out)

plot(med.out)


#---- output ----

models <- list(
  "Turnout ~ EM (Δ)" = model_em,
  "Turnout_17 ~ EM" = model_em2,
  "AFD_share | Turnout FE" = model,
  "AFD_share | ΔTurnout FE" = model2,
  "AFD_share |  Turnout+ em*post15 +fe" = model3,
  "Turnout ~ EM*group" = model4,
  "AFD shara = em*post15 + delta_turnout*post15 + delta_turnout*em*post 15 + fe" = model6
)

modelsummary(models, output = "summary_models.xlsx")



