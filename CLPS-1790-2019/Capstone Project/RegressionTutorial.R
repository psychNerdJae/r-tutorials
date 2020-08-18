##### Load libraries #####

# Libraries
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



##### Load in datasets #####

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
  mutate(generosityZScored = scale(generosityMeanCentered, center=T, scale=T)) %>%
  # THIS IS NEW: for doing logistic regression
  mutate(sharedBack = if_else(propShared > 0.5, 1, 0))



##### Linear regression #####

# Additive model
study1.lin.add <- lmer(propShared ~ wealth + generosityZScored +
                         (1+wealth+generosityZScored|sub),
                       data = study1,
                       control = lmerControl(optimizer = "bobyqa"))

# Interactive model
study1.lin.int <- lmer(propShared ~ wealth * generosityZScored +
                         (1+wealth*generosityZScored|sub),
                       data = study1,
                       control = lmerControl(optimizer = "bobyqa"))

# Model selection
anova(study1.lin.add, study1.lin.int)

# Look at the results of the better-fitting model
tidy(study1.lin.add, effects="fixed")



##### Logistic regression #####

# Additive model
study1.log.add <- glmer(sharedBack ~ wealth + generosityZScored +
                          (1+wealth+generosityZScored|sub),
                        data = study1,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))

# Interactive model
study1.log.int <- glmer(sharedBack ~ wealth * generosityZScored +
                          (1+wealth*generosityZScored|sub),
                        data = study1,
                        family = binomial,
                        control = glmerControl(optimizer = "bobyqa"))

# Model selection
anova(study1.log.add, study1.log.int)

# Look at the results of the more parsimonious model
tidy(study1.log.add, effects="fixed")  # Log odds ratios
tidy(study1.log.add, effects="fixed", exponentiate=T)  # Odds ratios


##### Visualize regression results #####

predictThis <- with(study1,
                    expand.grid(generosityZScored=seq(round(min(generosityZScored), 1),
                                                      round(max(generosityZScored), 1),
                                                      by = 0.01),
                                wealth=unique(wealth)))

plot.lin.add <- bind_cols(predictThis,
                          data.frame(AICcmodavg::predictSE(study1.lin.add,
                                                           newdata=predictThis,
                                                           type="response",
                                                           print.matrix=T)))

plot.log.add <- bind_cols(predictThis,
                          data.frame(AICcmodavg::predictSE(study1.log.add,
                                                           newdata=predictThis,
                                                           type="response",
                                                           print.matrix=T)))

plot.lin.add %>%
  ggplot(aes(x=generosityZScored, y=fit, color=wealth)) +
  gg +
  coord_cartesian(ylim=c(0, 1)) +
  geom_ribbon(aes(ymax=fit+se.fit, ymin=fit-se.fit), alpha=0.1, size=0.25) +
  geom_line(size=1) +
  scale_x_continuous(name="Generosity (z-scored)") +
  scale_y_continuous(name="Proportion shared") +
  scale_color_manual(name="Player A's wealth:", values=c("#1f78b4", "#e31a1c"))

plot.log.add %>%
  ggplot(aes(x=generosityZScored, y=fit, color=wealth)) +
  gg +
  geom_ribbon(aes(ymax=fit+se.fit, ymin=fit-se.fit), alpha=0.1, size=0.25) +
  geom_line(size=1) +
  scale_x_continuous(name="Generosity (z-scored)") +
  scale_y_continuous(name="p(Shared > 50%)") +
  scale_color_manual(name="Player A's wealth:", values=c("#1f78b4", "#e31a1c"))

