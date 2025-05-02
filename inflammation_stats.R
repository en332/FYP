# shapiro wilk test for normality ====

# need to test for normality within each group (ND, T1DE1, T1DE2)

# ND - not normal

ND <- cd45_endo_all_bins %>%
  filter("No diabetes" %in% endotype) 

shapiro.test(ND$cd45_by_study)

# T1DE1 - not normal

T1DE1 <- cd45_endo_all_bins %>%
  filter("T1DE1" %in% endotype)

shapiro.test(T1DE1$cd45_by_study)


# T1DE2 - not normal

T1DE2 <- cd45_endo_all_bins %>%
  filter("T1DE2" %in% endotype)

shapiro.test(T1DE2$cd45_by_study)


# histograms to test - confirm non normal distribution

hist(ND$cd45_by_study)  
hist(T1DE1$cd45_by_study)
hist(T1DE2$cd45_by_study)

# all eo sizes together ====

## Kruskal-wallis test ====

kruskal.test(cd45_by_study ~ endotype, data = cd45_endo_all_bins)

## Dunn post hoc ====

# FSA library
library(FSA)

# perform test
dunnTest(cd45_by_study ~ endotype, data = cd45_endo_all_bins, method = "bh")

# eo sizes separate ====

# split out data based on eo size
small_cd45 <- cd45_endo_by_size %>%
  filter(binSize == "small")

med_cd45 <- cd45_endo_by_size %>%
  filter(binSize == "medium")

large_cd45 <- cd45_endo_by_size %>%
  filter(binSize == "large")

# Kruskal wallis tests for each group
kruskal.test(cd45_by_study ~ endotype, data = small_cd45) # ns

kruskal.test(cd45_by_study ~ endotype, data = med_cd45) # p < 0.001

kruskal.test(cd45_by_study ~ endotype, data = large_cd45) # p < 0.001

# Dunn post hoc within each significant group 

dunnTest(cd45_by_study ~ endotype, data = medium_cd45, method = "bh")

dunnTest(cd45_by_study ~ endotype, data = large_cd45, method = "bh")

# whole area inflammation ====

# test for normality
# ND - not normal
ND <- acinar_cd45 %>%
  filter(endotype == "No diabetes") 

shapiro.test(ND$average_acinar_inflammation)

# T1DE1 - not normal
T1DE1 <- acinar_cd45 %>%
  filter(endotype == "T1DE1")

shapiro.test(T1DE1$average_acinar_inflammation)


# T1DE2 - not normal

T1DE2 <- acinar_cd45 %>%
  filter(endotype == "T1DE2")

shapiro.test(T1DE2$average_acinar_inflammation)

# Histograms to confirm - not normal - non parametric test
hist(ND$average_acinar_inflammation)  
hist(T1DE1$average_acinar_inflammation)
hist(T1DE2$average_acinar_inflammation)

# kruskal wallis test - significant
kruskal.test(average_acinar_inflammation ~ endotype, data = acinar_cd45)

# dunn post hoc - not much difference between T1DE1 and T1DE2 - is inflammation localised to the islets? they do have higher inflammation than ND tho, which makes sense
dunnTest(average_acinar_inflammation ~ endotype, data = acinar_cd45, method = "bh")

