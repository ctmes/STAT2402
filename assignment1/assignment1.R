### Keeping workspaces separate
rm(list = ls())

if (!is.null(sessionInfo()$otherPkgs)) {
  invisible(lapply(paste0("package:", names(sessionInfo()$otherPkgs)), detach,
                   character.only = TRUE, unload = TRUE))
}

options(stringsAsFactors = FALSE)

###

if (!requireNamespace("tidyverse", quietly = TRUE)) {
  install.packages("tidyverse")
}
library(ggplot2)
library(tidyverse)

if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}
library(ggplot2)

library(ggplot2)

library(patchwork)
library(reshape2)

DATA <- read.table("VirtR.txt", header = TRUE, sep = "\t")  # Use sep="," for CSV
print(DATA)

summary(DATA)

# Check for 0 values
which(DATA$Anxiety == 0.0)
which(DATA$VR == 0.0)
which(DATA$Sex == 0.0)
which(DATA$Age == 0.0)
which(DATA$PrevSurg == 0.0)
which(DATA$BlineAnx == 0.0)

# Check for NA Values
colSums(is.na(DATA))

# Convert categorical vars
DATA$Sex <- factor(DATA$Sex, labels = c("Male", "Female"))
DATA$VR <- factor(DATA$VR, labels = c("No VR", "VR"))
DATA$PrevSurg <- factor(DATA$PrevSurg, labels = c("No", "Yes"))

ggplot(DATA, aes(x = Age)) + geom_histogram(binwidth = 1) + theme_minimal()
ggplot(DATA, aes(x = Anxiety)) + geom_density() + theme_minimal()

t.test(Anxiety ~ VR, data = DATA)
cor.test(DATA$BlineAnx, DATA$Anxiety)
model <- lm(Anxiety ~ VR + Age + Sex + PrevSurg + BlineAnx, data = DATA)
summary(model)

# How does Anxiety relate to other vars?
# Anxiety vs Age
ggplot(DATA, aes(x = Age, y = Anxiety)) + geom_point() + geom_smooth(method = "lm") + theme_minimal()

# Anxiety by Sex
ggplot(DATA, aes(x = Sex, y = Anxiety)) + geom_boxplot() + theme_minimal()

# Anxiety by VR
ggplot(DATA, aes(x = VR, y = Anxiety)) + geom_boxplot() + theme_minimal()

# Anxiety vs Baseline Anxiety
ggplot(DATA, aes(x = BlineAnx, y = Anxiety)) + geom_point() + geom_smooth(method = "lm") + theme_minimal()


# Fitting LM
model <- lm(Anxiety ~ Age + Sex + VR + PrevSurg + BlineAnx, data = DATA)
summary(model)

par(mfrow = c(2, 2))
plot(model)

fitted_vals <- fitted(model)
which(fitted_vals > 300)
DATA[which(fitted_vals > 300), ]



# Cook's Distance
plot(model, which = 4)  # Cook's distance plot

# model with interactions
model_interact <- lm(Anxiety ~ Age * BlineAnx + Sex * VR + PrevSurg * VR, data = DATA)
summary(model_interact)

