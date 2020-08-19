
# Prepare Workspace -------------------------------------------------------

library(ISLR)
library(tidyverse)


# Load Data ---------------------------------------------------------------

data("Wage")


# Look at Age vs Wage (Fig 1.1a) ------------------------------------------

ggplot(Wage, aes(x = age, y = wage)) + 
        geom_jitter(width = 0.5, height = 1, alpha = 0.5) + 
        geom_smooth()

# Look at Calendar Year vs Wage (Fig 1.1b) --------------------------------

ggplot(Wage, aes(x = year, y = wage, color = year)) + 
        scale_color_viridis_c(end = 0.7) + 
        geom_point(aes(group = year)) + 
        geom_smooth(method = "lm")


# Look at Education vs Wage (Fig 1.1c) ------------------------------------

ggplot(Wage, aes(x = education, y = wage, fill = education)) + 
        scale_fill_viridis_d() + 
        geom_boxplot()


# Load SandP --------------------------------------------------------------

data("Smarket")


# Look at Direction vs Lag1 (Fig 1.2a) ------------------------------------

ggplot(Smarket, aes(x = Direction, y = Lag1, fill = Direction)) + 
        geom_boxplot() + 
        scale_fill_viridis_d(begin = 0.2, end = 0.7)

# Look at Direction vs Lag2 (Fig 1.2b) ------------------------------------

ggplot(Smarket, aes(x = Direction, y = Lag2, fill = Direction)) + 
        geom_boxplot() + 
        scale_fill_viridis_d(begin = 0.2, end = 0.7)

# Look at Direction vs Lag3 (Fig 1.2c) ------------------------------------

ggplot(Smarket, aes(x = Direction, y = Lag3, fill = Direction)) + 
        geom_boxplot() + 
        scale_fill_viridis_d(begin = 0.2, end = 0.7)


# Fit Quadratic Discriminant Analysis Model (Fig 1.3) ---------------------

### NOT DONE ###


# Load NCI60 --------------------------------------------------------------

data("NCI60")


# Fig 1.4a ----------------------------------------------------------------

### NOT DONE ###


# Perform PCA on NCI60 (Fig 1.4b) -----------------------------------------

pca <- prcomp(NCI60$data)
pca_tib <- tibble(class = NCI60$labs, pca_1 = pca$x[,1], pca_2 = pca$x[,2])
ggplot(pca_tib, aes(x = pca_1, y = pca_2, color = class)) + geom_point()



