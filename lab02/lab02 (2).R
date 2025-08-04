### Keeping workspaces separate
rm(list = ls())

if (!is.null(sessionInfo()$otherPkgs)) {
  invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,
                   character.only = TRUE, unload = TRUE))
}

options(stringsAsFactors = FALSE)

###

library(ggplot2)
library(patchwork)
library(reshape2)

fish <- read.table("Fish", header = TRUE, sep = "\t")  # Use sep="," for CSV
print(fish)

summary(fish)

# Entry 46 is incorrect- weight of 0.0

which(fish$Weight == 0.0)

fish1 <- fish[-46,]

fish1


# Fit the model
model <- lm(Weight ~ .^2, data = fish1)

# Extract residuals and fitted values
residuals <- resid(model)
fitted <- fitted(model)

plot(model$residuals ~ model$fitted.values)

# the datapoints are heavily concentrated around the lower end, with it opening 
# up much more sparsely towards the greater ends of residuals and fitted.values


fish1$Weight <- log(fish1$Weight)

model_log <- lm(Weight ~ .^2, data = fish1)

plot(fitted(model_log), resid(model_log),
     xlab = "Fitted Values",
     ylab = "Residuals",
     main = "Residuals vs Fitted Values (Log-Transformed Weight)",
     pch = 20, col = "blue")
abline(h = 0, col = "red", lty = 2)

summary(model_log)

model_log1 <- update(model_log, . ~ . - Code2)

summary(model_log1)

model_log1 <- update(model_log, . ~ . - Code4)

model_reduced <- step(model_log, direction = "backward", scope = list(lower = ~0.01))
summary(model_reduced)

