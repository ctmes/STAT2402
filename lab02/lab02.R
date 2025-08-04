### Keeping workspaces separate
rm(list = ls())

if (!is.null(sessionInfo()$otherPkgs)) {
  invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,
                   character.only = TRUE, unload = TRUE))
}

options(stringsAsFactors = FALSE)

###

install.packages("tidyverse")  # if not already installed
library(tidyverse)

install.packages("ggplot2")
library(ggplot2)



# Exercise 1:
# Examine data and report observations
# It looks like the first material has the most inter- and intra-temperature variance
# It looks like the next best is material 2, which is much better in the lower temperatures (>20), but else poor
# Material 3 seems to last the longest across all temperatures and has little inter-temperature variance


battery_data <- data.frame(
  Material = rep(1:3, each = 12),
  Temperature = rep(c(-10, -10, -10, -10, 20, 20, 20, 20, 55, 55, 55, 55), times = 3),
  Life = c(
    130, 155, 74, 180, 34, 40, 80, 75, 20, 70, 82, 58,
    150, 188, 159, 126, 136, 122, 106, 115, 25, 70, 58, 45,
    138, 110, 168, 160, 174, 120, 150, 139, 96, 104, 82, 60
  )
)

battery_data$Material <- factor(battery_data$Material)
battery_data$Temperature <- factor(battery_data$Temperature)

print(battery_data)

summary(battery_data)

model <- lm(Life ~ Material * Temperature, data = battery_data)

summary(model)

ggplot(battery_data, aes(x = Temperature, y = Life, color = Material, group = Material)) +
  stat_summary(fun = mean, geom = "line") +
  stat_summary(fun = mean, geom = "point") +
  labs(title = "Interaction Plot: Battery Life by Material and Temperature",
       x = "Temperature",
       y = "Battery Life",
       color = "Material") +
  theme_minimal()


# Reduced model
optimized_model <- step(model, direction = "backward")
summary(optimized_model)

