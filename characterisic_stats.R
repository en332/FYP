# requires functions.R, plotting.R

# SD area graph by class

# stats PER GRAPH SECTION ====

# fit linear model
lm_res <- lm(count_by_class ~ binningInteger * endotype, data = count_by_class_plotting %>%
               filter(endoduration %in% c("No diabetes", "T1DE2 (LD)", "T1DE1 (LD)")) %>%
               filter(InsGluStatus %in% "Ins- Gluc+")) # change as needed

library(car)

# ANOVA
Anova(lm_res, type ="III")

# post hoc

library(emmeans)
em_out_category <- emmeans(lm_res, ~ endotype | binningInteger)
res_df <- em_out_category %>% 
  pairs(adjust = "Tukey") %>% 
  test(joint = F)

res_df # <- this gives significance for each of the points on the graph


# for ins/gluc staining =================

# make df  and bind together
results <- quant %>%
  filter(endotype %in% "T1DE2")

results <- eo_ins_vs_gcg(results)

# type setting
results$ins <- as.numeric(results$ins)
results$gcg <- as.numeric(results$gcg)


T1DE2 <- results %>% select(c("bin", "ins"))

# join together
tmp <- left_join(tmp, T1DE2, by = "bin")

# rename columns
tmp <- find_replace_col_name(tmp, "ins", "T1DE2")


# transform data frame
test <- pivot_longer(tmp, names_to = "endotype", values_to = "value", cols = c(ND, T1DE1, T1DE2))

# make linear object
ins.gcg.lm <- lm(value ~ endotype, data = test)

# type 2 anova - bc unbalanced groups
model <- car::Anova(ins.gcg.lm, type = 3)

# tukey comparisons - keeps family-wise error rate low
em_out_category <- emmeans(ins.gcg.lm, ~ endotype)
res_df <- em_out_category %>% 
  pairs(adjust = "Tukey") %>% 
  test(joint = F)

res_df





