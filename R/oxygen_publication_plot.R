library(ggplot2)
library(ggpubr)
library(dplyr)

significant_p_values <- as.data.table(as.data.frame(pairwise_comparisons)) |>
  filter(p.value < 0.05) 

model_effects <- effects::Effect(c("day", "position"), model_power, adjust = "tukey") |> as.data.frame()

#Check for confidence intervals without the tukey correction and compare results just in case
posthoc_spesific_position
intervals(model_power)

# setting the dodge distance
pd <- position_dodge(width=0.4)

# Function to safely determine the maximum y position for a given enzyme and day
safe_max <- function(day_val) {
  max_val <- max(final$upper.CL[final$day == regmatches(day_val, regexpr("[0-9]+\\.?[0-9]*", day_val))], na.rm = TRUE)
  if (is.finite(max_val)) {
    return(max_val)
  } else {
    return(0) # Default position if no valid max value is found
  }
}

# Create a data frame for line segments
line_segments <- significant_p_values |>
  mutate(
    day_start = gsub(" .*", "", contrast),
    day_end = gsub(".* - ", "", contrast)) |>
  mutate(
    x_start = as.factor(gsub("day", "", day_start)),
    x_end = as.factor(gsub("day", "", day_end)),
    max_y = pmax(purrr::map_dbl(day_start, safe_max),purrr::map_dbl(day_end, safe_max))
  )

output_time_transformed <- significant_p_values |>
  inner_join(line_segments, by =  join_by(position == position, contrast == contrast, p.value == p.value, 
                                          estimate == estimate, SE == SE, t.ratio == t.ratio, df ==df)) |>
  tidyr::separate(contrast, into = c("group1", "group2"), sep = " - ") |>
  mutate(
    p_symbol = case_when(
      p.value < 0.001 ~ "***",
      p.value < 0.01 ~ "**",
      p.value < 0.05 ~ "*",
      TRUE ~ ""
    ), 
    max_y_corrected = case_when(
      max_y < 9 ~ max_y*1.3,
      TRUE ~ max_y*0.8
    )
  )

# plot
ggplot(data_subset, aes(day, Oxygen, group = position, color = position, shape = position)) + 
  #facet_grid(~position) +
  geom_point(size = 1.5, alpha = 0.3, position = pd) + 
  geom_point(data = final, aes(x = day, y = emmean), size = 2, position = pd) +
  geom_linerange(data = model_effects, aes(y = fit, ymin = lower, ymax = upper), show.legend = TRUE, position = pd) +
  theme_boxplot() + xlab("Days") + ylab(paste0("Oxygen mg L", '\u02C9', "¹")) +
  color_column() + labs(color  = "Column Position", shape = "Column Position") 
  #ggpubr::stat_pvalue_manual(data = output_time_transformed, label = "p_symbol", y.position = "max_y",
                           #  step.increase = 0.05, step.group.by = "position",  tip.length = 0.01, 
                            # xmin = "x_start", xmax = "x_end", bracket.nudge.y = 0.5,
                            # color = "position", show.legend = FALSE) + theme(legend.position = "right") 

# For the model data use the following table for latex conversion
modelsummary::modelsummary(model_power, stars = TRUE, output = "model_table.csv")
