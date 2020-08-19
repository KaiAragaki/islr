
# Description -------------------------------------------------------------

# Replication of Figures for Chapter 2 of Introduction to Statistical Learning


# Prepare Workspace -------------------------------------------------------

library(ISLR)
library(tidyverse)


# Read in Data ------------------------------------------------------------

adv <- read_csv("./small-datasets/Advertising.csv", col_types = "_dddd")


# Pivot Longer ------------------------------------------------------------

adv_long <- pivot_longer(adv, cols = TV:newspaper, names_to = "method", values_to = "budget")


# Fig 2.1 -----------------------------------------------------------------

ggplot(adv_long, aes(x = budget, y = sales, color = method)) + 
        scale_color_viridis_d(end = 0.7) + 
        facet_grid(~method, scales = "free_x") + 
        geom_point() + 
        theme(legend.position = "none") + 
        geom_smooth(method = "lm", se = F)

# Plotted without faceting:
# ggplot(adv_long, aes(x = budget, y = sales, color = method)) + 
#         scale_color_viridis_d(end = 0.7) +
#         geom_point() + 
#         theme(legend.position = "none") + 
#         geom_smooth(method = "lm", se = F)


# Fig 2.2a ----------------------------------------------------------------

inc <- read_csv("./small-datasets/Income1.csv", col_types = "_dd")

ggplot(inc, aes(x = Education, y = Income)) + 
        geom_point()


# Fig 2.2b ----------------------------------------------------------------

# Can't generate - I don't know the function they used to generate the points


