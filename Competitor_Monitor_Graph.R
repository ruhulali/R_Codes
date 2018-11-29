rm(list = ls())
install.packages('ggpubr')
devtools::install_github("wilkelab/cowplot")

library(ggpmisc)
library(cowplot)
library(ggplot2)
library(ggpubr)

theme_set(
  theme_minimal() +
    theme(legend.position = "top")
)

df <- read.csv(file.choose())

b <- ggplot(df, aes(x = x, y = y))
x_max <- max(abs(df$x)) 
y_max <- max(abs(df$y)) 
labs <- rownames(df$label)

#--------------------------

# Draw a rectangle underneath the text, making it easier to read.
library(ggrepel)

# 2. Create the plot
b + geom_point(aes(x,y)) +
  geom_label_repel(aes(label = df$label), size = 3)+
  geom_hline(yintercept=0, size=1, color = "black", alpha = 0.5) + 
  geom_vline(xintercept=0, size=1, color = "black", alpha = 0.5) +
  xlim(-x_max, x_max) + ylim(-y_max, y_max)


# Avoid overlaps by repelling text labels
p <- ggplot(df, aes(x, y, label = rownames(df))) + geom_point()
p + geom_label_repel(aes(label = df$label)) + 
  geom_point(size = 3) + 
  geom_hline(yintercept=0, size=1, color = "black", alpha = 0.5) + 
  geom_vline(xintercept=0, size=1, color = "black", alpha = 0.5) +
  xlim(-x_max, x_max) + ylim(-y_max, y_max)


# 1. Open jpeg file
png(width = 800, height = 700, filename="E:/Drive_Data_Ruhul/VA/Competitor Monitor/10_Oct'18/Rplot33.png")
# 2. Create the plot
# 3. Close the file
dev.off()
