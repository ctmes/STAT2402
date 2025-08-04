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

df = read.csv("irisdata.csv")

head(df)

df[3,2] # X then Y

df[2:4, 3]

df[3, "Sepal.Width"]


unique(df$"Species") # only 3 species


df[c(2:4, 90:92), c("Sepal.Length", "Species")] # c == that filter

df$Sepal.Length[2:4] #$ means access column sepal length
df[["Sepal.Length"]][2:4] #same as above


# Visualizing
str(df)
summary(df)

# Plotting
plot(df$Sepal.Length, df$Sepal.Width)

plot(df$Sepal.Length, df$Sepal.Width, pch = as.numeric(factor(df$Species)))

plot(df[, 1:4], pch = as.numeric(factor(df$Species)))

hist(df$Petal.Length)

hist(df$Petal.Length, 50) #50 cols now

boxplot(Petal.Length ~ Species, data = df)

MASS::parcoord(df[, 1:4])

MASS::parcoord(df[, 1:4], col = c("green", "blue", "black")[as.numeric(factor(df$Species))])

# dotplot basic
p1 = ggplot(data = df, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point()
print(p1)

# dotplot shade differentiation
p2 = ggplot(data = df, aes(x = Sepal.Length, y = Sepal.Width)) + geom_point(alpha = 0.4, size = 2)
print(p2)

# histo color, spec differentiation
p5 = ggplot(data = df, aes(x = Sepal.Length, fill = Species)) + geom_histogram(binwidth = 0.2)
print(p5)

# overlapping histo
p6 = ggplot(data = df, aes(x = Sepal.Length, fill = Species)) + geom_histogram(binwidth = 0.2, position = "identity", alpha = 0.3)
print(p6)

# boxplots
p7 = ggplot(data = df, aes(x = Species, y = Sepal.Length, colour = Species)) + geom_boxplot()
print(p7)

# combined overlapping histo and horizontal boxplot
print(p6/(p7 + coord_flip()))

# RESHAPE LIBRARY
df$id = seq_len(nrow(df))
df.m = reshape2::melt(df, id.vars = c("id", "Species"))

head(df.m)
str(df.m)
nrow(df.m)

p8 = ggplot(data = df.m, aes(x = value, fill = Species)) + geom_histogram(binwidth = 0.2,
                                                                          position = "identity", alpha = 0.3) + facet_grid(rows = vars(variable))
print(p8)

p9 = ggplot(data = df.m, aes(x = Species, y = value, colour = Species)) + geom_boxplot() +
  geom_jitter(width = 0.2, height = 0.1, alpha = 0.6, size = 1.4)
print(p9 + coord_flip() + facet_grid(rows = vars(variable)))

p10 = ggplot(data = df.m, aes(x = variable, y = value, colour = Species, group = id)) +
  geom_line(alpha = 0.3)
print(p10)

print(p10/(p9 + facet_grid(cols = vars(variable)) + theme(axis.text.x = element_text(angle = 45,
                                                                                     hjust = 1))))