##### R workshop via CLPS 1790 #####
# Session 1: Conceptual introduction
# Written by Jae-Young Son
# Presented 09-20-19


##### Load libraries #####
# Many of the useful functions in R are contained in "libraries" that you have to load
# If it helps, think of this metaphor: you are a magician who needs to cast a lot of magic spells
# However, it's hard for you to constantly remember all of the thousands of spells you need in order
# to get your work done. To make it easier, you've written down most of your spells in books, and
# you only reference the books you need when you need to remember the spells written in them.

# RStudio now has a feature that detects what libraries are being called by your script, checks
# that against a list of the libraries you have already downloaded, and offers to install libraries
# you do not already have on your computer

# tidyverse contains a massive number of tools for data wrangling, tidying, and visualization
library(tidyverse)

# knitr contains a useful function for outputting dataframes as readable tables
library(knitr)

# broom contains functions for outputting statistical tests as readable tables
library(broom)


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


##### Create dataframe #####
# Contains information on a large number of comic book characters published by Marvel and DC
# It is commonly believed that comic books are largely written by men for male audiences, and
# that there is insufficient female representation in comic books
# Here, we will use data to make statistical inferences about whether this belief has merit

characters_marvel <- 
  "https://github.com/fivethirtyeight/data/raw/master/comic-characters/marvel-wikia-data.csv" %>% 
  read_csv() %>%
  mutate(publisher = "Marvel")

characters_dc <- 
  "https://github.com/fivethirtyeight/data/raw/master/comic-characters/dc-wikia-data.csv" %>% 
  read_csv() %>%
  mutate(publisher = "DC")

comics <- bind_rows(characters_marvel, characters_dc) %>%
  select(publisher, name, gender=SEX, appearances=APPEARANCES, year=Year) %>%
  filter(gender=="Male Characters" | gender=="Female Characters")


##### Analyze appearances by gender #####
# Get a sense for how the data are distributed
# Run statistical models to make sense of the data

### Raw number of appearances
# Plot
comics %>%
  ggplot(aes(appearances)) + gg +
  facet_grid(. ~ gender) +
  geom_histogram(fill="white", color="black") +
  ggtitle("Distribution of character appearances") +
  xlab("Number of appearances from each character") +
  ylab("Count")

# Model
m1 <- aov(appearances ~ gender, comics)
m1 %>% tidy()


### Log-transformed number of appearances
# Plot
comics %>%
  ggplot(aes(log(appearances))) + gg +
  facet_grid(. ~ gender) +
  geom_histogram(fill="white", color="black") +
  ggtitle("Distribution of character appearances") +
  xlab("Number of appearances from each character (log-transformed)") +
  ylab("Count")

# Model (ANOVA)
m2 <- aov(log(appearances) ~ gender, comics)
m2 %>% tidy()

# Posthoc tests
TukeyHSD(m2) %>% tidy()

# Model (regression)
m3 <- lm(log(appearances) ~ gender, comics)
m3 %>% tidy()


### How to make sense of the results?
# Observation: even in the log-transformed data, the distribution looks non-normal
# In other words, there is fairly extreme right-hand skew
# Men are over-represented overall, but *where* in the distribution are they over-represented?
# Hypothesis: there are more "unimportant" male characters who only show up a few times

# Plot
comics %>%
  filter(appearances > 9) %>%
  ggplot(aes(log(appearances))) + gg +
  facet_grid(. ~ gender) +
  geom_histogram(fill="white", color="black") +
  ggtitle("Distribution of character appearances") +
  xlab("Number of appearances from each character (log-transformed)") +
  ylab("Count")

# Model
m4 <- comics %>%
  filter(appearances > 9) %>%
  lm(log(appearances) ~ gender, .)
m4 %>% tidy()

# Using the raw data, take a look at the *mean* number of appearances by gender
comics %>%
  group_by(gender) %>%
  summarise(appearances = sum(appearances, na.rm=T),
            N = n(),
            meanAppearances = appearances/N) %>%
  kable()


##### Further exploration #####
# There are lots of other things we could look at:
# Are there differences in gender representation at DC vs Marvel?
# Characters who have been around longer obviously have had more opportunities to
# appear in the comics - how would we model this?


