spp2 <- read_csv(paste0(filepath, "/JustCon5_SPP_Order2.csv")) %>%
  mutate(study="Victim")

tpp1 <- read_csv(paste0(filepath, "/JustCon5_TPP_Order1.csv")) %>%
  mutate(study="Juror")

tpp2 <- read_csv(paste0(filepath, "/JustCon5_TPP_Order2.csv")) %>%
  mutate(study="Juror")


punish <- bind_rows(spp1, spp2, tpp1, tpp2) %>%
  # For the time being, we only care about behavior, not demographics
  select(sub=mTurkCode, study, starts_with("assault"), starts_with("theft")) %>%
  pivot_longer(cols=-c(sub, study), names_to="trialType", values_to="rating") %>%
  separate(trialType, c("crime", "severity", "punishers", "rep"))
