##### R workshop via CLPS 1790 #####
# Session 3: Wrangling data
# Written by Jae-Young Son
# Presented 10-04-19


##### Misc protip #####
# You can quickly type the variable assignment symbol <- by pressing alt + dash
# In R, this symbol is preferred over = because it distinguishes variable assignment
# from other uses of =, such as providing an argument to a function, or using a named vector
# More on this later...


##### Load libraries #####

# tidyverse contains functions for data wrangling, tidying, and visualization
library(tidyverse)

# knitr contains functions for outputting dataframes as readable tables
library(knitr)

# broom contains functions for outputting statistical tests as readable tables
library(broom.mixed)

# lme4 and lmerTest contain functions for performing mixed-effects regression
library(lme4)
library(lmerTest)


##### Initialize #####

# Get working directory of this script
filepath <- dirname(rstudioapi::getActiveDocumentContext()$path)

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

# Paper: https://www.nature.com/articles/s41598-019-48050-2
# Experiment 5: conformity effects on people's moral judgments
# Full datasets & code available at https://osf.io/8ka47/

# Data collected using Qualtrics, an online survey platform
# Representative of how many online survey platforms output data


##### Read in data #####

# Behavioral data
spp1 <- read_csv(paste0(filepath, "/JustCon5_SPP_Order1.csv")) %>%
  mutate(study="Victim")

spp2 <- writecodehere %>%
  mutate(study="Victim")

tpp1 <- writecodehere %>%
  mutate(study="Juror")

tpp2 <- writecodehere %>%
  mutate(study="Juror")

# Experimenter inferences about which subjects were actually "bots"
bots <- bind_rows(
  read_csv(paste0(filepath, "/JustCon5_SPP_Order1_FlagBots.csv")),
  read_csv(paste0(filepath, "/JustCon5_SPP_Order2_FlagBots.csv")),
  read_csv(paste0(filepath, "/JustCon5_TPP_Order1_FlagBots.csv")),
  read_csv(paste0(filepath, "/JustCon5_TPP_Order2_FlagBots.csv"))
)


##### Merge datasets #####

# Objectives:
#   1. Combine all behavioral datasets
#   2. Pivot to longform w/ key column "trialType" and value column "rating"
#   3. Convert the code in "trialType" into variable columns

# The "trialType" column currently contains a code signifying the information that was presented
#   Entry 1 is crime type (assault/theft) -- column should be named "crime"
#   Entry 2 is severity (hi/lo) -- "severity"
#   Entry 3 is group size (solo, or a group of 0-4 others) -- "punishers"
#   Entry 4 is the repetition number (2 trials/condition were shown) -- "rep"
#   Entry 5 is a throwaway number

punish <- writecodehere %>%
  # For the time being, we only care about behavior, not demographics
  select(sub=mTurkCode, study, starts_with("assault"), starts_with("theft")) %>%
  writecodehere


##### Further tidying #####

# We want to get rid of data that seems to have come from "bots"
# Using subjects' answers to a free-response question, an experimenter guessed which subjects were bots
# Those guesses are contained in the dataframe "bots"
# We therefore want to join that dataframe to our behavioral dataframe
punish <- left_join(punish, bots %>% select(-catch_2), by=c("sub"="mTurkCode")) %>%
  filter(suspectedBot==0) %>%
  select(-suspectedBot)

# For the purpose of this exercise, we also want to get rid of the solo condition,
# recode the punishers variable as numeric, and make sure our ordinal variables are
# encoded as "factors" in the right order
punish <- punish %>%
  filter(punishers!="solo") %>%
  mutate(punishers = str_sub(punishers, -1, -1),
         punishers = as.numeric(punishers)) %>%
  mutate(crime = factor(crime, levels=c("assault", "theft")),
         severity = factor(severity, levels=c("lo", "hi")),
         study = factor(study, levels=c("Victim", "Juror")))


##### Plot data #####

plot.punish <- punish %>%
  group_by(sub, study, crime, severity, punishers) %>%
  summarise(rating = mean(rating)) %>%
  group_by(study, crime, severity, punishers) %>%
  summarise(Mean=mean(rating), SD=sd(rating), N=n(), SE=SD/sqrt(N))

ggplot(plot.punish, aes(x=punishers, y=Mean, color=crime)) +
  gg +
  facet_grid(. ~ study) +
  geom_line(aes(linetype=severity)) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE),
                width=.2, linetype="solid") +
  geom_point()


##### Regress #####

test.punish <- lmer(rating ~ study + crime + severity + punishers + (1+punishers | sub),
                    data=punish)

test.punish %>% tidy(effects="fixed") %>% kable()


