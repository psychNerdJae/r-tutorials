usda_census <- read_tsv(paste0(filepath, "/usda.sample.txt"))

personality_wide <- personality_long %>%
  unite(key, scaleID, itemID) %>%
  pivot_wider(names_from=key, values_from=rating)
