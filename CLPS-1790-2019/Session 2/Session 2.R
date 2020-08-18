##### R workshop via CLPS 1790 #####
# Session 2: Loading datasets & wrangling data
# Written by Jae-Young Son
# Presented 09-27-19


##### Load libraries #####

# tidyverse contains functions for data wrangling, tidying, and visualization
library(tidyverse)

# knitr contains functions for outputting dataframes as readable tables
library(knitr)

# broom contains functions for outputting statistical tests as readable tables
library(broom.mixed)

# psych contains functions that perform statistical tests commonly used in psychology
library(psych)

# haven allows you to read/write data from SPSS, SAS, and Stata
library(haven)

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


##### Reading plaintext data #####
### An introduction to plaintext
# Data are commonly saved with the extensions csv, txt, and tsv
# Unlike files like xlsx (Excel) and sav (SPSS), no company owns plaintext
# Pro: anyone with a computer can open plaintext data using free software
#      this makes it really nice for sharing scientific data!
# Con: it's PLAINtext... there's absolutely no formatting!

### What's in a name?
# What do these names mean? Try opening up the data using a text editor (NOT Excel)
# It should become immediately clear where these names come from
# CSV = comma separated values
# TXT = text
# TSV = tab separated values

### What's with the formatting?
# Recall that this is plaintext. There has to be a way of separating values, or else the
# data would look like one long string of gibberish. So, we use specific symbols, which tell
# the computer how to separate values in plaintext. This symbol is known as a *delimiter*

### Example: Carnegie classification data
# See http://carnegieclassifications.iu.edu/ for details
# R1 = very high research activity
# R2 = high research activity

# Load in data
carnegie_r1 <- read_csv(paste0(filepath, "/R1.csv")) %>%
  mutate(classification="R1")
carnegie_r2 <- read_csv(paste0(filepath, "/R2.csv")) %>%
  mutate(classification="R2")

# Merge dataframes
carnegie_all <- bind_rows(carnegie_r1, carnegie_r2) %>%
  select(name, state, control, profile=`Enrollment Profile`, classification)


### Your turn: Agriculture census data from the USDA
# The entire dataset is several gb!
# We're only looking at a subset here
# I don't know why you'd want it, but if you want the full dataset:
# ftp://ftp.nass.usda.gov/quickstats/

# Write your own code here!
# What works? What doesn't work?
# Hint for people following along from home: what format is this data?
usda_census <- writeyourcode


##### Reading proprietary formats #####
### What is a proprietary data format?
# If a company makes software you have to pay for, and saves data in a format that
# can ONLY be read by that paid software, that's a proprietary format.
# This is a philosophical belief, so critically evaluate for yourself whether you believe it:
# Proprietary data formats are the enemy of science. Full stop.
# They are a barrier to reproducible analysis and replicability. To the extent that we can
# save our data in formats that nobody owns (e.g. plaintext), we have a MORAL obligation to
# do so as scientists whose work is funded by the public.

### Reading in proprietary data
# Luckily, there are some very smart people who have reverse-engineered certain types of
# proprietary data formats, and who have written R packages allowing you to read in those
# data into R. This would allow you to independently check the work of a colleague who
# uses (for example) SPSS using free software.

### One last rant
# Free is not free. Software must be written by somebody. Many core functions are written
# by small teams... and more frequently than you'd think, by a single person who is NOT
# compensated for their work. These software developers do it because they believe in a
# greater good: the free exchange of ideas, code, and data... which collectively advance
# human knowledge, facilitate new discoveries, and hopefully enable us to make the world
# a little better using scientific thinking. We are all guilty of not doing this, but when
# we write scientific papers, we should cite the software/packages we use, and give
# due credit to their authors. That is intellectually honest, and the least we can do.

# For a human interest story about this, read:
# https://onezero.medium.com/the-internet-relies-on-people-working-for-free-a79104a68bcc

# For an editorial published in Nature Methods, read:
# https://www.nature.com/articles/s41592-019-0350-x


### Load in SPSS data
personality_raw <- read_sav(paste0(filepath, "/Personality inventory-2019-SPSS.sav"))


##### Data wrangling: an introduction ####

### Wrangle into "tidy" longform and recode
personality_long <- personality_raw %>%
  select(participant:s6.10) %>%
  pivot_longer(cols=s1.01:s6.10, names_to="itemID", values_to="rating") %>%
  separate(col=itemID, into=c("scaleID", "itemID")) %>%
  mutate(scaleID = recode(scaleID,
                          "s1"="neuroticism",
                          "s2"="creativity",
                          "s3"="optimism",
                          "s4"="resilience",
                          "s5"="prosociality",
                          "s6"="extraversion"),
         itemID = as.numeric(itemID)) %>%
  arrange(participant, scaleID, itemID)

### Wrangle into wideform
# Write your own code here!
# Hint #1: many tidyverse functions have inverses
#   What's the inverse of pivot_longer? The inverse of separate?
# Hint #2: look at the documentation
#   Let's say I want to look at the help documentation for separate.
#   I would run the following code in my console window: ? separate

personality_wide <- writeyourcode


### Compare your class with past years' classes
personality_long <- personality_long %>%
  mutate(whichClass = if_else(str_sub(participant, 1, 3) == "19-",
                              "this class", "past classes"))


##### Wrangling: What's the practical use? #####

### Why wideform?
# Many functions outside of the tidyverse operate on wideform data,
# especially functions that work with matrices
# psych::alpha expects wideform data
personality_wide %>%
  select(neuroticism_1:neuroticism_10) %>%
  alpha(title = "neuroticism")

### Why longform?
# Simply put, the entire tidyverse operates on tidy longform data
# Modern regression also takes longform data

# Individual subjects' datapoints
plot_personality_dotplot <- personality_long %>%
  group_by(whichClass, participant, scaleID) %>%
  summarise(rating = mean(rating))

# Group-level stats
plot_personality_barplot <- plot_personality_dotplot %>%
  group_by(whichClass, scaleID) %>%
  summarise(Mean = mean(rating), SD=sd(rating), N=n(), SE=SD/sqrt(N))

# Plot dataset
ggplot(plot_personality_barplot, aes(x=scaleID, y=Mean, fill=scaleID)) +
  gg +
  facet_grid(. ~ whichClass) +
  geom_bar(stat="identity", color="black") +
  geom_hline(yintercept=3, linetype="dotted") +
  geom_dotplot(data=plot_personality_dotplot %>% mutate(Mean=rating),
               binaxis="y", stackdir="center", binwidth=0.1,
               fill="white", dotsize=0.30, alpha=1, stroke=0.50) +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2, linetype="solid",
                position=position_dodge(0.9)) +
  scale_y_continuous(name="Mean Ratings") +
  scale_fill_manual(values=c("#e41a1c", "#377eb8", "#4daf4a",
                             "#984ea3", "#ff7f00", "#ffff33")) +
  coord_cartesian(ylim=c(1, 5)) +
  guides(fill=F)

# Test significance using regression
personality_long %>%
  lmer(rating ~ scaleID + (1+scaleID|participant), data=.) %>%
  tidy(effects="fixed") %>%
  kable()



