library(broom)
library(corrr)
library(ggtext)
library(gt)
library(kableExtra)
library(knitr)
library(lmtest)
library(patchwork)
library(scales)
library(stargazer)
library(texreg)
library(tidyverse)
library(RColorBrewer)

sg <- read_csv("sentencing_grid.csv")

glimpse(sg)

# The relationship between offense severity level and presumptive sentence

f1 <- ggplot(data = sg, aes(x = severity_level, y = presumptive_sentence)) + 
  geom_point(aes(color = factor(criminal_history)), alpha = 1, size = 3) + 
  theme_bw(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  ) +
  geom_smooth(se = FALSE) +
  scale_x_continuous(
    name = "Severity Level",
    limits = c(1, 11),
    breaks = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
  ) +
  scale_y_continuous(
    name = "Presumptive Sentence (mos)",
    limits = c(0, 450),
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450)
    ) +
  scale_color_manual(name = "Criminal History", 
                    values = c("0" = "#f7f7f7", "1" = "#d9d9d9", "2" = "#bdbdbd", 
                               "3" = "#969696", "4" = "#737373", "5" = "#525252",
                               "6" = "#252525"), 
                    breaks = c("0", "1", "2", "3", "4", "5", "6"))

# The relationship between criminal history level and presumptive sentence

f2 <- ggplot(data = sg, aes(x = criminal_history, y = presumptive_sentence)) + 
  geom_point(aes(color = factor(severity_level)), alpha = 1, size = 3) + 
  theme_bw(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.title.x = element_markdown(),
    axis.title.y = element_markdown()
  ) +
  geom_smooth(se = FALSE) +
  scale_x_continuous(
    name = "Criminal History",
    limits = c(0, 6),
    breaks = c(0, 1, 2, 3, 4, 5, 6)
  ) +
  scale_y_continuous(
    name = "Presumptive Sentence (mos)",
    limits = c(0, 450),
    breaks = c(0, 50, 100, 150, 200, 250, 300, 350, 400, 450)
  ) +
  scale_color_manual(name = "Severity Level",
                     values = c("1" = "#fff7ec", "2" = "#fef0d9", "3" = "#fee8c8", 
                                "4" = "#fdd49e", "5" = "#fdbb84", "6" = "#fc8d59", 
                                "7" = "#ef6548", "8" = "#d7301f","9" = "#b30000", 
                                "10" = "#7f0000", "11" = "#630000"), 
                     breaks = c("0", "1", "2", "3", "4", "5", "6", "7", "8", "9",
                                "10", "11")
                     )

f1 | f2

########
# Fit regression model
########

lm.1 <- lm(log(presumptive_sentence) ~ 1 + severity_level + criminal_history, 
           data = sg)

# Model level
glance(lm.1)

# Coefficient level
tidy(lm.1)

########
# Plot fitted curve
########

ggplot(data = sg, aes(x = severity_level, y = presumptive_sentence)) + 
  geom_point(aes(color = factor(criminal_history)),
             alpha = 1, 
             size = 3) + 
  scale_color_manual(name = "Criminal History", 
                      values = c("0" = "#f7f7f7", "1" = "#d9d9d9", "2" = "#bdbdbd", 
                                 "3" = "#969696", "4" = "#737373", "5" = "#525252",
                                 "6" = "#252525"), 
                      breaks = c("0", "1", "2", "3", "4", "5", "6")) +
  geom_function(fun = function(x) {exp(1.58) * exp(0.323 * x) * exp(0.114)}, 
                aes(color = "Low Severity/Limited Prior History")) + #History = 1
  geom_function(fun = function(x) {exp(1.58) * exp(0.323 * x) * exp(0.342)}, 
                aes(color = "Medium Severity/Some Prior History")) + #History = 3
  geom_function(fun = function(x) {exp(1.58) * exp(0.323 * x) * exp(0.570)}, 
                aes(color = "High Severity/Extensive Prior History")) + #History = 5            
  theme_bw(base_size = 18) +
  theme(
    panel.grid = element_blank(),
    axis.title.x =  element_markdown(),
    axis.title.y =  element_markdown(),
    legend.position = c(0.3, 0.7)
  ) +
  xlab("Offense Severity Level") +
  ylab("Presumptive Sentence")


########
# Investigating mixed effects relationships
########

ggplot(data = sg, aes(x = criminal_history, y = presumptive_sentence)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_bw() +
  xlab("Criminal History") +
  ylab("Presumptive Sentence") +
  facet_wrap(~severity_level)









