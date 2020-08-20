
# Description -------------------------------------------------------------

# Replication of Figures for Chapter 2 of Introduction to Statistical Learning


# Prepare Workspace -------------------------------------------------------

library(ISLR)
library(tidyverse)
library(fields)


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


# Fig 2.3 -----------------------------------------------------------------

# Can't generate - don't know the function used to generate the plane.


# Fig 2.4 -----------------------------------------------------------------

inc_2 <- read_csv("./small-datasets/Income2.csv", col_types = "_ddd")

fig2_4_fit <- lm(Income ~ Education + Seniority, data = inc_2)


# Fig 2.5 -----------------------------------------------------------------

fig2_5_fit <- Tps(inc_2[,1:2], inc_2[,3], lambda = 0.005)
drape.plot(predictSurface(fig2_5_fit), zlim = c(20, 150))


# Figure 2.6 --------------------------------------------------------------

fig2_6_fit <- Tps(inc_2[,1:2], inc_2[,3], lambda = 0.0001)
drape.plot(predictSurface(fig2_6_fit), zlim = c(20, 150))


# Figure 2.7 --------------------------------------------------------------

# Not done - qualitative


# Figure 2.8a -------------------------------------------------------------

# Generate 3 normally distributed groups

g_1 <- tibble(x = rnorm(50, mean = 3), y = rnorm(50, mean = 9))
g_2 <- tibble(x = rnorm(50, mean = 3), y = rnorm(50, mean = 3))
g_3 <- tibble(x = rnorm(50, mean = 9), y = rnorm(50, mean = 6))

gs <- rbind(g_1, g_2, g_3) %>% 
        mutate(group = as.factor(rep(1:3, each = 50)))

ggplot(gs, aes(x = x, y = y, color = group, shape = group)) + geom_point() + coord_cartesian(xlim = c(0, 12), ylim = c(0, 12))

# Figure 2.8b -------------------------------------------------------------

# Generate 3 normally distributed groups

g_1 <- tibble(x = rnorm(50, mean = 3), y = rnorm(50, mean = 6))
g_2 <- tibble(x = rnorm(50, mean = 3), y = rnorm(50, mean = 3))
g_3 <- tibble(x = rnorm(50, mean = 6), y = rnorm(50, mean = 4))

gs <- rbind(g_1, g_2, g_3) %>% 
        mutate(group = as.factor(rep(1:3, each = 50)))

ggplot(gs, aes(x = x, y = y, color = group, shape = group)) + geom_point() + coord_cartesian(xlim = c(0, 8), ylim = c(0, 8))


# Figure 2.9 --------------------------------------------------------------

# WIP

f_true <- tibble(x = seq(-1, 2.5, length.out = 50), y = sin(x))
f_noise <- tibble(x = f_true$x, y = f_true$y + rnorm(length(f_true$y), sd = 0.15))
ggplot(f_noise, aes(x, y)) + 
        geom_point() + 
        geom_line(aes(x = x, y = f_true$y)) + 
        geom_smooth(method = lm, se = F)


# Figure 2.10 -------------------------------------------------------------

# WIP

f_true <- tibble(x = seq(0, 1.5, length.out = 50), y = sin(x))
f_noise <- tibble(x = f_true$x, y = f_true$y + rnorm(length(f_true$y), sd = 0.075))
ggplot(f_noise, aes(x, y)) + 
        geom_point() + 
        geom_line(aes(x = x, y = f_true$y)) + 
        geom_smooth(method = lm, se = F)


# Figure 2.11 -------------------------------------------------------------

# WIP

f_true <- tibble(x = seq(0, 6, length.out = 50), y = sin(x))
f_noise <- tibble(x = f_true$x, y = f_true$y + rnorm(length(f_true$y), sd = 0.1))
ggplot(f_noise, aes(x, y)) + 
        geom_point() + 
        geom_line(aes(x = x, y = f_true$y)) + 
        geom_smooth(method = lm, se = F)
