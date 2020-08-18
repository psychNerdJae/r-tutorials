##### R workshop via CLPS 1790 #####
# Capstone project
# Aims:
#   1. Recreate figures from the paper
#   2. Create alternative visualizations of that data
#   3. Replicate statistical analyses


##### An introduction to the dataset #####
# From the paper Propagation of Economic Inequality Through Reciprocity and Reputation
# by Leor Hackel & Jamil Zaki
# Paper: http://ssnl.stanford.edu/sites/default/files/pdf/Hackel%26Zaki_Reciprocity_2018.pdf
# Data + code: https://osf.io/r7d5w/

# This research examines how people use information about a partner's generosity and wealth
# to decide whether to reciprocate prosocial giving. Here's the basic idea: Player A is
# given some amount of money, and can choose to share X% of that money with Player B.
# Wealth is determined by varying how much money is given to Player A, and generosity is
# determined by measuring what percentage of that money Player A shares with Player B.
# Reciprocity is measured as how much Player B later shares with Player A.

# What's the weight parameter w? From the article:
# Our model then specifies the extent to which each recipient weighs generosity and
# reward value when deciding with whom to interact next, through a weighting parameter (w).
# Thus, a recipient with a high weighting parameter learns more from the generosity of a
# giver, whereas a recipient with a low weighting parameter leans more on givers’ 
# reward value.


##### Load libraries #####

# Write code here
library(tidyverse)
library(lme4)
library(lmerTest)
library(broom.mixed)

##### Initialize #####

# Get working directory of this script
filepath <- dirname(rstudioapi::getActiveDocumentContext()$path)

# Create a "default" ggplot template
gg <- list(
  theme_bw(),
  theme(plot.title = element_text(hjust = 0.5, size=13),
        panel.spacing = unit(0.75, "lines"),
        legend.box.spacing = unit(0.5, "lines"),
        legend.position = "bottom",
        legend.margin = margin(c(0, 0, 0, 0), unit='lines'))
)

# Load in datasets
study1a <- read_csv(paste0(filepath, "/Study1.Wave2.Reciprocity.csv")) %>%
  mutate(study="Study 1a")

study1b <- read_csv(paste0(filepath, "/Study1R&E.Wave2.Reciprocity.csv")) %>%
  mutate(study="Study 1b") %>%
  mutate(Sub = Sub + max(study1a$Sub),
         Targ = Targ + max(study1a$Targ))

study1 <- bind_rows(study1a, study1b) %>%
  rename(sub=Sub, target=Targ, wealth=Wealth, pointPool=PointPool,
         pointsShared=PointsShared, propShared=PropShared, weightParameter=w,
         generosity=G, badSub=BadSub) %>%
  filter(badSub==0) %>%
  mutate(wealth = recode(wealth, `-1`="low", `1`="high"),
         wealth = factor(wealth, levels=c("low", "high"))) %>%
  group_by(sub) %>%
  mutate(generosityMeanCentered = scale(generosity, center=T, scale=F)) %>%
  ungroup() %>%
  mutate(generosityZScored = scale(generosityMeanCentered, center=T, scale=T))


##### Plot data #####

# Original figure restricts comparison to 0% vs 100% generosity
study1 %>%
  filter(generosity==0 | generosity==1) %>%
  group_by(sub, wealth, generosity) %>%
  summarise(propShared = mean(propShared)) %>%
  group_by(wealth, generosity) %>%
  summarise(Mean = mean(propShared), SD=sd(propShared), N=n(), SE=SD/sqrt(N)) %>%
  ggplot(aes(x=factor(generosity), y=Mean, fill=wealth)) +
  gg +
  coord_cartesian(ylim=c(0, 1)) +
  geom_bar(stat="identity", position="dodge", color="black") +
  geom_errorbar(aes(ymin=Mean-SE, ymax=Mean+SE), width=.2, linetype="solid",
                position=position_dodge(0.9)) +
  scale_y_continuous(name="Proportion Player B shared w/ Player A",
                     labels=scales::percent) +
  scale_x_discrete(name="Player A's generosity", labels=c("0%", "100%")) +
  scale_fill_manual(name="Player A's wealth:", values=c("#1f78b4", "#e31a1c"))

ggsave(paste0(filepath, "/study1_replication.pdf"),
       plot=last_plot(), height=5, width=5, units='in', dpi=300, useDingbats=FALSE)

# Modified figure to look at intermediary generosity
study1 %>%
  ggplot(aes(generosity, y=propShared, color=wealth)) +
  gg +
  coord_cartesian(ylim=c(0, 1)) +
  geom_point(alpha=0.15) +
  # Fitting a regression line through this data is not appropriate, as geom_smooth
  # outputs a purely fixed-effects model for what is properly mixed-effects data
  # However, it's convenient and useful as an exploratory visualization
  geom_smooth(method = "lm") +
  scale_color_manual(name="Player A's wealth:", values=c("#1f78b4", "#e31a1c"))

ggsave(paste0(filepath, "/study1_scatter.pdf"),
       plot=last_plot(), height=5, width=5, units='in', dpi=300, useDingbats=FALSE)


##### Analyze data #####

# Additive model
study1.m1 <- lmer(propShared ~ wealth + generosityZScored +
                    (1+wealth+generosityZScored|sub),
                  data = study1,
                  control = lmerControl(optimizer = "bobyqa"))
study1.m1 %>% tidy(effects="fixed")

# Interactive model
study1.m2 <- lmer(propShared ~ wealth * generosityZScored +
                    (1+wealth*generosityZScored|sub),
                  data = study1,
                  control = lmerControl(optimizer = "bobyqa"))
study1.m2 %>% tidy(effects="fixed")

# Model selection
anova(study1.m1, study1.m2)

# Look at the results of the better-fitting model
tidy(study1.m1, effects="fixed")


##### Plot regression results #####

predictThis <- with(study1,
                    expand.grid(generosityZScored=seq(round(min(generosityZScored), 1),
                                                      round(max(generosityZScored), 1),
                                                      by = 0.001),
                                wealth=unique(wealth))
                    ) %>%
  mutate(sub = 999)

plot.regress.study1 <- bind_cols(predictThis,
                                 tibble(predicted = predict(study1.m1,
                                                            newdata=predictThis,
                                                            type="response",
                                                            allow.new.levels=T)))

plot.regress.study1 %>%
  ggplot(aes(x=generosityZScored, y=predicted, color=wealth)) +
  gg +
  coord_cartesian(ylim=c(0, 1)) +
  geom_line(size=1) +
  scale_color_manual(name="Player A's wealth:", values=c("#1f78b4", "#e31a1c"))

ggsave(paste0(filepath, "/study1_regression.pdf"),
       plot=last_plot(), height=5, width=5, units='in', dpi=300, useDingbats=FALSE)
