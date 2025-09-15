source("R/oxygen_preliminary_plots.R")
library(nlme)

summary(data_subset)
data_subset$position <- factor(data_subset$Column_Number, 
                                       levels = c("C1", "C2", "C3"),
                                       labels = c("Column 1", "Column 2", "Column 3"))

data_subset$chainID <- factor(data_subset$replicate)
data_subset$day <-  factor(data_subset$Sample_Name, 
                           levels = c("S08", "S11", "S13", "S13.5", "S14.5", "S15", "S17.5", "S18.5", "S19"),
                           labels = c("0", "1", "3", "6", "8", "9", "13", "15", "17"))


# 4. Power correlation structure (CAR1 for unequally spaced data)
model_power <- lme(Oxygen ~ day * position,
                   random = ~1|chainID,
                   correlation = corCAR1(form = ~ day | chainID/position),
                   data = data_subset)

simple_model <- lme(Oxygen ~ day * position,
    random = ~1|chainID,
    data = data_subset)

simple_model_simpler <- lm(Oxygen ~ day * position,
                           data = data_subset)

# Model comparisons
anova(simple_model, model_power, simple_model_simpler)
AIC(simple_model, model_power, simple_model_simpler)
BIC(simple_model, model_power, simple_model_simpler)


# Use model_power for your post-hoc analyses
library(emmeans)

# Estimated marginal means by time (Sample_Name)
posthoc_spesific_position <- emmeans(model_power, ~ day|position)
pairwise_comparisons <- contrast(posthoc_spesific_position, method = "pairwise", adjust = "tukey")

# Look at the model summary
summary(model_power)

final <- posthoc_spesific_position |> as.data.frame() |> as.data.table()

