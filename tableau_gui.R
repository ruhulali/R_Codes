# From CRAN
install.packages("esquisse", dependencies = T)

# with remotes
remotes::install_github("dreamRs/esquisse")

# or with install-github.me service (based on remotes)
source("https://install-github.me/dreamRs/esquisse")

# or with devtools:
devtools::install_github("dreamRs/esquisse")



#loading tidyverse to read input
library(tidyverse)

# loading itunesr for retrieving itunes review data that we will use in this analysis
install.packages('itunesr', dependencies = T)
library(itunesr)

#loading the magical esquisse library
library(esquisse)
esquisse::esquisser(data = tips)

########### Plot Two Continuous Variables ################

devtools::install_github("wilkelab/cowplot")
install.packages("ggpmisc")

library(ggplot2)
library(ggpubr)
theme_set(
  theme_minimal() +
    theme(legend.position = "top")
)


# Load data
data("mtcars")
df <- mtcars

str(df)
psych::describe(df)
mlr::summarizeColumns(df)

# Convert cyl as a grouping variable
df$cyl <- as.factor(df$cyl)

# Inspect the data
head(df[, c("wt", "mpg", "cyl", "qsec")], 4)



#------ Basic scatter plots ------#

b <- ggplot(df, aes(x = wt, y = mpg))

# Scatter plot with regression line
b + geom_point()+
  geom_smooth(method = "lm") 

# Add a loess smoothed fit curve
b + geom_point()+
  geom_smooth(method = "loess") 

# Add regression line and confidence interval
# Add correlation coefficient: stat_cor()
ggscatter(df, x = "wt", y = "mpg",
          add = "reg.line", conf.int = TRUE,    
          add.params = list(fill = "lightgray"),
          ggtheme = theme_minimal()
)+
  stat_cor(method = "pearson", 
           label.x = 3, label.y = 30) 


#----- Multiple groups -----#

# Change color and shape by groups (cyl)
b + geom_point(aes(color = cyl, shape = cyl))+
  geom_smooth(aes(color = cyl, fill = cyl), method = "lm") +
  geom_rug(aes(color =cyl)) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

# Remove confidence region (se = FALSE)
# Extend the regression lines: fullrange = TRUE
b + geom_point(aes(color = cyl, shape = cyl)) +
  geom_rug(aes(color =cyl)) +
  geom_smooth(aes(color = cyl), method = lm, 
              se = FALSE, fullrange = TRUE)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  ggpubr::stat_cor(aes(color = cyl), label.x = 3)


b + geom_point(aes(color = cyl, shape = cyl))+
  geom_smooth(aes(color = cyl, fill = cyl), 
              method = "lm", fullrange = TRUE) +
  facet_wrap(~cyl) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))+
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  theme_bw()

b + geom_point(aes(color = cyl, shape = cyl))+
  stat_ellipse(aes(color = cyl), type = "t")+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

# Convex hull of groups
b + geom_point(aes(color = cyl, shape = cyl)) +
  stat_chull(aes(color = cyl, fill = cyl), 
             alpha = 0.1, geom = "polygon") +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) 

# Add mean points and confidence ellipses
b + geom_point(aes(color = cyl, shape = cyl)) +
  stat_conf_ellipse(aes(color = cyl, fill = cyl), 
                    alpha = 0.1, geom = "polygon") +
  stat_mean(aes(color = cyl, shape = cyl), size = 2) + 
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) 



#------ Add point text labels -----#

library(ggrepel)

# Add text to the plot
.labs <- rownames(df)
b + geom_point(aes(color = cyl)) +
  geom_text_repel(aes(label = .labs,  color = cyl), size = 3)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

# Draw a rectangle underneath the text, making it easier to read.
b + geom_point(aes(color = cyl)) +
  geom_label_repel(aes(label = .labs,  color = cyl), size = 3)+
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))


#### Bubble chart ####
b + geom_point(aes(color = cyl, size = qsec), alpha = 0.5) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07")) +
  scale_size(range = c(0.5, 12))  # Adjust the range of points size


#### Color by a continuous variable ####
b + geom_point(aes(color = mpg), size = 3) +
  scale_color_gradientn(colors = c("#00AFBB", "#E7B800", "#FC4E07"))



#----- Add marginal density plots -----#

# Create a scatter plot
p <- ggplot(iris, aes(Sepal.Length, Sepal.Width)) +
  geom_point(aes(color = Species), size = 3, alpha = 0.6) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#FC4E07"))

# Add density distribution as marginal plot
install.packages('ggExtra')
library("ggExtra")
ggMarginal(p, type = "density")

# Change marginal plot type
ggMarginal(p, type = "boxplot")

library(ggpubr)
# Grouped Scatter plot with marginal density plots
ggscatterhist(
  iris, x = "Sepal.Length", y = "Sepal.Width",
  color = "Species", size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.params = list(fill = "Species", color = "black", size = 0.2)
)

# Use box plot as marginal plots
ggscatterhist(
  iris, x = "Sepal.Length", y = "Sepal.Width",
  color = "Species", size = 3, alpha = 0.6,
  palette = c("#00AFBB", "#E7B800", "#FC4E07"),
  margin.plot = "boxplot",
  ggtheme = theme_bw()
)


#------ Scatter Plot Matrices -------#

head(iris)

pairs(iris[,1:4], pch = 19)
pairs(iris[,1:4], pch = 19, lower.panel = NULL)

### Color points by groups (species) ###
my_cols <- c("#00AFBB", "#E7B800", "#FC4E07")  
pairs(iris[,1:4], pch = 19,  cex = 0.5,
      col = my_cols[iris$Species],
      lower.panel=NULL)

# Correlation panel
panel.cor <- function(x, y){
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  cex.cor <- 0.8/strwidth(txt)
  text(0.5, 0.5, txt, cex = cex.cor * r)
}

# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch = 19, col = my_cols[iris$Species])
}

# Create the plots
pairs(iris[,1:4], 
      lower.panel = panel.cor,
      upper.panel = upper.panel)


# Customize upper panel
upper.panel<-function(x, y){
  points(x,y, pch=19, col=c("red", "green3", "blue")[iris$Species])
  r <- round(cor(x, y), digits=2)
  txt <- paste0("R = ", r)
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  text(0.5, 0.9, txt)
}
pairs(iris[,1:4], lower.panel = NULL, 
      upper.panel = upper.panel)


library(psych)
pairs.panels(iris[,-5], 
             method = "pearson", # correlation method
             hist.col = "#00AFBB",
             density = TRUE,  # show density plots
             ellipses = TRUE # show correlation ellipses
)


