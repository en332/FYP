# PCA - not vulnerable to variables correlating

# Create new df from results ====
# count by bin and class + area by bin and class + CD45 stained area split by small medium large

## select out clinical variables and the study Ids from main quant df ====
pca_df <- quant %>%
  select(c("study_id", 
           "donor_type", 
           "endotype", 
           "duration_status", 
           "endoduration",
           "diabetes_duration",
           "age_group",
           "age_at_onset",
           "sex")) %>%
  distinct() 

# Calculate
count_by_class_quant <- eo_number_class(quant)

area_by_class_quant <- eo_area_class(quant)

# drop empty columns

area_by_class_quant <- area_by_class_quant %>%
  select(!c("bin8_Insp_Glucn", "bin9_Insp_Glucn"))

count_by_class_quant <- count_by_class_quant %>%
  select(!c("bin8_Insp_Glucn", "bin9_Insp_Glucn"))





# problem =====

# change types to numeric
i <- c(2:29)  
area_by_class_quant[, i] <- apply(area_by_class_quant[, i], 2, function(x) as.numeric(as.character(x)))
count_by_class_quant[, i] <- apply(count_by_class_quant[, i], 2, function(x) as.numeric(as.character(x)))

## append area data ====

# rename columns
colnames(area_by_class_quant) <- gsub("Insp_Glucn", "Ins+Gluc-_area", colnames(area_by_class_quant))
colnames(area_by_class_quant) <- gsub("Insp_Glucp", "Ins+Gluc+_area", colnames(area_by_class_quant))
colnames(area_by_class_quant) <- gsub("Insn_Glucp", "Ins-Gluc+_area", colnames(area_by_class_quant))

## append count data ====

# rename columns
colnames(count_by_class_quant) <- gsub("Insp_Glucn", "Ins+Gluc-_count", colnames(count_by_class_quant))
colnames(count_by_class_quant) <- gsub("Insp_Glucp", "Ins+Gluc+_count", colnames(count_by_class_quant))
colnames(count_by_class_quant) <- gsub("Insn_Glucp", "Ins-Gluc+_count", colnames(count_by_class_quant))



# check the type of each column: sapply(count_by_class_quant, class)

## append cd45 data split by endocrine object size (small, medium, large) ====

cd45_pca <- cd45_endo_by_size %>%
  pivot_wider(names_from = "binSize", values_from = "cd45_by_study") %>%
  distinct() %>%
  select(!c("endotype"))

# rename columns
cd45_pca <- find_replace_col_name(cd45_pca, "small", "perc_cd45_small")
cd45_pca <- find_replace_col_name(cd45_pca, "medium", "perc_cd45_medium")
cd45_pca <- find_replace_col_name(cd45_pca, "large", "perc_cd45_large")

# change NAs to 0 ====
area_by_class_quant[is.na(area_by_class_quant)] <- 0
count_by_class_quant[is.na(count_by_class_quant)] <- 0
cd45_pca[is.na(cd45_pca)] <- 0

# join to clinical variables - only have c45 data from 64, filter the clinical variables ====
# from only studies which have cd45 quant and join on columns
pca_df <- pca_df %>%
  filter(study_id %in% cd45_pca$study_id) %>%
  left_join(cd45_pca, by = "study_id") %>%
  left_join(area_by_class_quant, by = "study_id")


# Investigate correlated variables ====

correlated <- cor(pca_df[10:40])

corrplot(correlated, 
         order = "AOE",
         type = "upper") # size and colour indicates correlated variables

pca_result <- prcomp(pca_df[, 10:40], data = pca_df, scale = T, center = T) # generates principle components from data

summary(pca_result)

pca_result$rotation

autoplot(pca_result, data = pca_df, colour = "endotype",
         label = F, 
         shape = T, 
         loadings = T, # overlays PCA loadings
         loadings.colour = "blue",
         loadings.label = T, # shows loadings labels
         loadings.label.size = 3) +
  theme_bw() 


# group the area df into small medium and large instead of by bin

# small ====
small <- cbind(area_by_class_quant[, grep("bin0", colnames(area_by_class_quant))], 
                        area_by_class_quant[, grep("bin1", colnames(area_by_class_quant))],
                        area_by_class_quant[, grep("bin2", colnames(area_by_class_quant))],
                        area_by_class_quant[, grep("bin3", colnames(area_by_class_quant))])

# filter for specific classes
small_insp_glucn <- colnames(small[, grep("Insp_Glucn", colnames(small))])
small_insp_glucp <- colnames(small[, grep("Insp_Glucp", colnames(small))])
small_insn_glucp <- colnames(small[, grep("Insn_Glucp", colnames(small))])


# average small columns from area together
small_insp_glucn_area <- pca_df %>%
  select(all_of(small_insp_glucn)) %>%
  mutate(small_insp_glucn_area = apply(., 1, mean)) %>%
  select("small_insp_glucn_area")

small_insp_glucp_area <- pca_df %>%
  select(all_of(small_insp_glucp)) %>%
  mutate(small_insp_glucp_area = apply(., 1, mean)) %>%
  select("small_insp_glucp_area")

small_insn_glucp_area <- pca_df %>%
  select(all_of(small_insn_glucp)) %>%
  mutate(small_insn_glucp_area = apply(., 1, mean)) %>%
  select("small_insn_glucp_area")


# medium ====
med <- cbind(area_by_class_quant[, grep("bin4", colnames(area_by_class_quant))], 
               area_by_class_quant[, grep("bin5", colnames(area_by_class_quant))],
               area_by_class_quant[, grep("bin6", colnames(area_by_class_quant))])

# filter for specific classes
med_insp_glucn <- colnames(med[, grep("Insp_Glucn", colnames(med))])
med_insp_glucp <- colnames(med[, grep("Insp_Glucp", colnames(med))])
med_insn_glucp <- colnames(med[, grep("Insn_Glucp", colnames(med))])


# average small columns from area together
med_insp_glucn_area <- pca_df %>%
  select(all_of(med_insp_glucn)) %>%
  mutate(med_insp_glucn_area = apply(., 1, mean)) %>%
  select("med_insp_glucn_area")

med_insp_glucp_area <- pca_df %>%
  select(all_of(med_insp_glucp)) %>%
  mutate(med_insp_glucp_area = apply(., 1, mean)) %>%
  select("med_insp_glucp_area")

med_insn_glucp_area <- pca_df %>%
  select(all_of(med_insn_glucp)) %>%
  mutate(med_insn_glucp_area = apply(., 1, mean)) %>%
  select("med_insn_glucp_area")

# large ====
large <- cbind(area_by_class_quant[, grep("bin7", colnames(area_by_class_quant))], 
               area_by_class_quant[, grep("bin8", colnames(area_by_class_quant))],
               area_by_class_quant[, grep("bin9", colnames(area_by_class_quant))])

# filter for specific classes
large_insp_glucn <- colnames(large[, grep("Insp_Glucn", colnames(large))])
large_insp_glucp <- colnames(large[, grep("Insp_Glucp", colnames(large))])
large_insn_glucp <- colnames(large[, grep("Insn_Glucp", colnames(large))])


# average small columns from area together
large_insp_glucn_area <- pca_df %>%
  select(all_of(large_insp_glucn)) %>%
  mutate(large_insp_glucn_area = apply(., 1, mean)) %>%
  select("large_insp_glucn_area")

large_insp_glucp_area <- pca_df %>%
  select(all_of(large_insp_glucp)) %>%
  mutate(large_insp_glucp_area = apply(., 1, mean)) %>%
  select("large_insp_glucp_area")

large_insn_glucp_area <- pca_df %>%
  select(all_of(large_insn_glucp)) %>%
  mutate(large_insn_glucp_area = apply(., 1, mean)) %>%
  select("large_insn_glucp_area")

# combine all grouped columns together

test <- cbind(pca_df[, 1:12],
              small_insn_glucp_area,
              small_insp_glucn_area,
              small_insp_glucp_area,
              med_insn_glucp_area,
              med_insp_glucn_area,
              med_insp_glucp_area,
              large_insn_glucp_area,
              large_insp_glucn_area,
              large_insp_glucp_area) # do pca with this

# PCA ver 2 (in report) ====

# normalise data
test[c(10:19, 21)] <- scale(test[c(10:19, 21)])

# Investigate correlated variables

correlated <- cor(test[c(10:19, 21)])

corrplot(correlated, 
         order = "AOE",
         type = "upper") # size and colour indicates correlated variables

pca_result <- prcomp(test[, c(10:19, 21)], data = test, scale = T, center = T) # generates principle components from data

summary(pca_result)

# scree plot
screeplot(pca_result, type = 'lines', main = 'Scree Plot')

# pca_result$rotation

autoplot(pca_result, data = pca_df, colour = "endotype",
         label = F, 
         shape = T, 
         loadings = T, # overlays PCA loadings
         loadings.colour = "grey",
         loadings.label = T, # shows loadings labels
         loadings.label.size = 3) +
  scale_colour_discrete(aes(fill = 'endotype')) +
  theme_bw() # behaves like a ggplot object

# kmeans on this ^^ ====
  
library(NbClust)
nb <- NbClust(data = test[, c(10:19, 21)], 
              diss = NULL, 
              distance = "euclidean", 
              min.nc = 2,
              max.nc = 15,
              method = "kmeans"
) # says best number of clusters is 2

# using 3 clusters
k_res <-
  kmeans(x = test[, c(10:19, 21)], 
         centers = 2, 
         iter.max = 10, 
         nstart = 1,
  )


test$k_res <- as.factor(k_res$cluster) # join cluster information to iris dataframe at set as factor

# Remake PCA plot with k clusters.
autoplot(pca_result, data = test, colour = "k_res") +
  scale_colour_discrete() +
  theme_bw()


# UMPA attempt (not included in report) ==== 
# using test data from before (generate fresh)

# drop empty column
umap_data <- test[, c(10:19, 21)]

library(umap)

umap.result <- umap(umap_data)

umap.result

head(umap.result$layout, 3)

# make new df for plotting

test$endotype <- as.factor(test$endotype)

umap_plot <- data.frame(endotype = test$endotype, umap_1 = umap.result$layout[, 1], umap_2 = umap.result$layout[, 2])


ggplot(umap_plot, aes(x = umap_1, y = umap_2, colour = endotype)) +
  geom_point() +
  scale_colour_discrete() +
  theme_bw()



