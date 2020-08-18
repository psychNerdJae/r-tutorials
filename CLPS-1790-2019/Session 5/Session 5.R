##### R workshop via CLPS 1790 #####
# Session 4: Plotting
# Written by Jae-Young Son
# Presented 10-18-19


##### Load libraries #####

# tidyverse contains functions for data wrangling, tidying, and visualization
library(tidyverse)

# lme4 and lmerTest contain functions for performing mixed-effects regression
library(lme4)
library(lmerTest)

# broom contains functions for outputting statistical tests as readable tables
library(broom.mixed)


##### Initialize #####

# Create a "default" ggplot template for consistency
gg <- list(
  theme_bw(),
  theme(plot.title = element_text(hjust = 0.5, size=13),
        panel.spacing = unit(0.75, "lines"),
        legend.box.spacing = unit(0.5, "lines"),
        legend.position = "bottom",
        legend.margin = margin(c(0, 0, 0, 0), unit='lines'))
)


##### An introduction to the dataset #####
# From the paper Affective cognition: Exploring lay theories of emotion
# by Desmond C. Ong, Jamil Zaki, & Noah D. Goodman
# Paper: https://web.stanford.edu/~dco/pubs/2015_Affective_Cognition.pdf
# Data + code: https://github.com/desmond-ong/affCog

# This research uses sophisticated Bayesian inference models to understand how people
# integrate contextual information to make judgments of others' emotions.
# That's a little too advanced for us, but it's great practice for plotting data.


##### Load data from experiment 1 #####
# EV = Expected Value of the gamble
# PE = Prediction Error, aka the difference between reward expectation and actual reward

study1 <- read_csv(
  "https://raw.githubusercontent.com/desmond-ong/affCog/master/data/expt1data.csv"
) %>%
  mutate(ev = (payoff1*prob1) + (payoff2*prob2) + (payoff3*prob3)) %>%
  mutate(pe = win - ev) %>%
  select(sub=workerid, happy:disapp, ev, pe) %>%
  arrange(sub) %>%
  group_by(sub) %>%
  mutate(trial = row_number()) %>%
  ungroup() %>%
  pivot_longer(cols=happy:disapp, names_to="emotion", values_to="rating")


##### Plot data #####

# Plot distribution of PEs
study1 %>%
  ggplot(aes(x=pe)) +
  geom_histogram(binwidth=5, fill="white", color="black") +
  # geom_density(size=1) +
  facet_grid(emotion ~ .) +
  geom_vline(xintercept=0, color="red", linetype="dotted") +
  scale_x_continuous(name="Prediction Error")

# Plot distribution of emotion ratings
study1 %>%
  ggplot(aes(x=rating)) +
  geom_bar(fill="white", color="black") +
  facet_grid(emotion ~ .) +
  geom_vline(xintercept=5, color="red", linetype="dotted") +
  scale_x_continuous(name="Emotion Rating", breaks=seq(1, 9, 1))

# Alternative method of plotting distribution of emotion ratings
study1 %>%
  ggplot(aes(x=rating, fill=emotion)) +
  geom_bar(position="dodge", color="black") +
  scale_x_continuous(name="Emotion Rating", breaks=seq(1, 9, 1))

# Plot relationship between PEs and emotion ratings
study1 %>%
  ggplot(aes(x=pe, y=rating)) +
  geom_point(alpha=0.25) +
  facet_grid(. ~ emotion) +
  geom_smooth(method="lm") +
  scale_x_continuous(name="Prediction Error") +
  scale_y_continuous(name="Emotion Rating", breaks=seq(1, 9, 1))


##### Analyze data #####

# Pivot wider because we have multiple DVs
analyze.study1 <- study1 %>%
  pivot_wider(names_from=emotion, values_from=rating)

# Test happiness
analyze.study1 %>%
  lmer(happy ~ pe + (1|sub), data=.) %>%
  tidy(effects="fixed")

# Test disappointment
analyze.study1 %>%
  lmer(disapp ~ pe + (1|sub), data=.) %>%
  tidy(effects="fixed")


##### Confused? Overwhelmed? #####
# Don't worry, it happens to everyone. As I was writing this code, I had to look up
# a LOT of the functions' arguments. This is a normal part of programming.
# With practice, you will gain more confidence and refine your intutitions.

# For a "cheat sheet" describing the ggplot philosophy, reference this:
# https://rstudio.com/wp-content/uploads/2015/03/ggplot2-cheatsheet.pdf


##### Homework #####
# Find an open dataset (psychology, health, whatever you want).
# Read the accompanying materials (the publication, documentation, etc),
# and make sure you understand what variables were analyzed.
# Using those datasets, we're going to practice wrangling, visualization, and analysis.

# There are LOTS of open datasets available (and easily searchable!) at https://osf.io/
# If there's a paper that you find interesting, you can sometimes find the data for that
# work available online. For example, I found today's dataset on Jamil Zaki's website:
# http://ssnl.stanford.edu/publications

