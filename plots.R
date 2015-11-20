library(ggplot2, warn.conflicts = F, quietly = T, verbose = F)
library(gridExtra, warn.conflicts = F, quietly = T, verbose = F)
library(cowplot, warn.conflicts = F, quietly = T, verbose = F)
library(psych, warn.conflicts = F, quietly = T, verbose = F)

DATA <- D
#DATA <- mutate(train_data, Stars = training$Stars) # data to plot on

# ---------------- -Correlations -----------------------
pairs.panels(DATA, jiggle = T, scale = T, pch = ".", ellipses = F, gap = 0.1, main = "Pairs correlation plot", cex.main = 0.6)

cor.plot(cor(DATA[, -14]))

# ----------------------------------------------------

# scatter plot of response variable vs each predictor
predictors <- names(DATA)[-14] # predictor variables
#response <- names(DATA)[14]

# # ----------------- scatter plots -------------------------
# scatter_plot <- function(s){
#   # plot a scatter plot of response var vs predictors
#   title <- paste("Average of Stars given vs.", x)
#   ggplot(DATA, aes_string(x, "Stars")) +
#     geom_point() + 
#     ggtitle(title) +
#     stat_smooth(method = "lm") +
#     theme_classic()
# }
# 
# g <- lapply(predictors,scatter_plot)
# grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], g[[6]],
#              g[[7]], g[[8]], g[[9]], g[[10]], g[[11]], g[[12]], g[[13]], ncol = 4, nrow = 4)
# --------------------------------------------------------------

### ------------------- density plots for each predictor --------------

density_plot<- function(s){
  ggplot(data = DATA, aes_string(s)) + geom_density(aes(color = Stars)) + theme_bw()
}

g <- lapply(predictors, density_plot) 
grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], g[[6]], g[[7]], g[[8]], g[[9]], g[[10]], g[[11]], g[[12]], g[[13]],
             ncol = 4, nrow = 4)


##------------------ boxplots --------------------------
box_plot <- function(s){
  title <- paste("Predictor = ", s)
  ggplot(DATA, aes_string(x = 'Stars', y = s)) +
    geom_boxplot(aes(fill = Stars)) +
    ggtitle(title) + 
    theme_bw()
}
g <- lapply(predictors, box_plot)

grid.arrange(g[[1]], g[[2]], g[[3]], g[[4]], g[[5]], g[[6]], g[[7]], g[[8]], g[[9]], g[[10]], g[[11]], g[[12]], g[[13]],
             ncol = 4, nrow = 4)
# ------------------------------------------------------

