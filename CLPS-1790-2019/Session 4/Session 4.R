##### R workshop via CLPS 1790 #####
# Session 4: Wrangling data
# Written by Jae-Young Son
# Presented 10-11-19


##### Load libraries #####

# tidyverse contains functions for data wrangling, tidying, and visualization
library(tidyverse)

# janitor helps us tidy up bad column names
library(janitor)


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
# 1000s of survey responses about people's preferences for Halloween candy
# Downloaded from The Science Creative Quarterly
# https://www.scq.ubc.ca/so-much-candy-data-seriously/

# Pro-tip: You can download data straight from the web, as well as from your hard drive!
# The column names contain a nonstandard character encoding, so we need to manually specify it
candy_web <- "https://www.scq.ubc.ca/wp-content/uploads/2017/10/candyhierarchy2017.csv" %>%
  read_csv(local = locale(encoding = "latin1"))


##### Wrangle data #####
# Read in the CSV from your hard drive and wrangle into tidy format
# Eventually, we want to have 6 variables:
#   1. sub (an identifier for unique respondents)
#   2. goingOut (whether the respondent is planning on trick-or-treating)
#   3. gender
#   4. age
#   5. item (describes what item is being rated)
#   6. rating

candy <- writecodehere


##### An intro to vectorized operations #####

candy <- candy %>%
  mutate(item = str_sub(item, start=6, end=-1)) %>%
  mutate(age = as.numeric(age)) %>%
  mutate(age = if_else(age<18 | age>100, as.numeric(NA), age),
         ageGroup = cut_number(age, 5, labels=F)) %>%
  mutate(dropthis = if_else(is.na(rating), 1, 0)) %>%
  filter(dropthis==0) %>%
  select(-dropthis) %>%
  mutate(ratingNumeric = recode(rating, "DESPAIR"=-1, "MEH"=0, "JOY"=1))


##### Plot data #####

plot.candy <- candy %>%
  group_by(item) %>%
  summarise(rating = mean(ratingNumeric))

plot.candy %>%
  arrange(rating) %>%
  mutate(colorkey = if_else(row_number()%%2==0, 1, 0)) %>%
  ggplot(aes(x=reorder(item, rating), y=rating, fill=factor(colorkey))) +
  gg +
  geom_bar(stat="identity", color="black") +
  coord_flip(ylim=c(-1, 1)) +
  scale_y_continuous(name="Mean Rating") +
  scale_x_discrete(name="Item") +
  scale_fill_manual(values=c("#EB6123", "#7B2C13"))


