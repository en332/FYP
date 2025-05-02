# continues on from pre-processing version 2
# Libraries ====

library(ggplot2)
library(patchwork)
library(tidyverse)
library(plotrix)

# read in area, stain quantification, and CD45 quant data frames

area <- read.csv("acinar_data.csv")
area <- area[, 2:5]

quant <- read.csv("endocrine_data.csv")
quant <- quant[, 2:16]

cd45_endo <- read.csv("cd45_endo_df.csv")


## Calculations: ====

## Basic area, count, and density ====

# area
area_basic <- eo_area(quant)



## Area by class: ====

# Calculate the percentage contribution of each endocrine object to the total area by class, binning integer, and studyid
area_by_class_quant <- eo_area_class(quant)

# Reshape data
area_by_class <- pivot_longer(area_by_class_quant, 
                              cols = colnames(area_by_class_quant)[2:31], 
                              names_to = "binningInteger", 
                              values_to = "area_by_class") %>%
  mutate(InsGluStatus = "NA") 

# Populate class column
for(i in 1:nrow(area_by_class)){
  if(grepl("Insn_Glucp", area_by_class$binningInteger[i]) == TRUE){
    area_by_class$InsGluStatus[i] <- "Ins- Gluc+"
  }
  if(grepl("Insp_Glucp", area_by_class$binningInteger[i]) == TRUE){
    area_by_class$InsGluStatus[i] <- "Ins+ Gluc+"
  }
  if(grepl("Insp_Glucn", area_by_class$binningInteger[i]) == TRUE){
    area_by_class$InsGluStatus[i] <- "Ins+ Gluc-"
  }
}


# Rename the bins column
for(row in 1:nrow(area_by_class)){
  for(bin in 0:9){
    if(grepl(bin, area_by_class$binningInteger[row]) == TRUE){
      area_by_class$binningInteger[row] <- bin
    }
  }
}

# Add metadata for plotting
area_by_class_plotting <- left_join(area_by_class, quant %>% select(study_id, endoduration, endotype) %>% distinct(), by = "study_id")

# set factor levels for graph order
area_by_class_plotting$InsGluStatus <- factor(area_by_class_plotting$InsGluStatus, levels = c("Ins+ Gluc-", "Ins+ Gluc+", "Ins- Gluc+"))

## Count by Class ====

# Calculate the percentage contribution of each endocrine object to the total count by class, binning integer, and studyid
count_by_class_quant <- eo_number_class(quant)

# Reshape data
count_by_class <- pivot_longer(count_by_class_quant, 
                              cols = colnames(count_by_class_quant)[2:31], 
                              names_to = "binningInteger", 
                              values_to = "count_by_class") %>%
  mutate(InsGluStatus = "NA")

# Populate class column
for(i in 1:nrow(count_by_class)){
  if(grepl("Insn_Glucp", count_by_class$binningInteger[i]) == TRUE){
    count_by_class$InsGluStatus[i] <- "Ins- Gluc+"
  }
  if(grepl("Insp_Glucp", count_by_class$binningInteger[i]) == TRUE){
    count_by_class$InsGluStatus[i] <- "Ins+ Gluc+"
  }
  if(grepl("Insp_Glucn", count_by_class$binningInteger[i]) == TRUE){
    count_by_class$InsGluStatus[i] <- "Ins+ Gluc-"
  }
}

# Rename the bins column
for(row in 1:nrow(count_by_class)){
  for(bin in 0:9){
    if(grepl(bin, count_by_class$binningInteger[row]) == TRUE){
      count_by_class$binningInteger[row] <- bin
    }
  }
}

# Add metadata for plotting
count_by_class_plotting <- left_join(count_by_class, quant %>% select(study_id, endoduration, endotype) %>% distinct(), by = "study_id")

# set factor levels for graph order
count_by_class_plotting$InsGluStatus <- factor(count_by_class_plotting$InsGluStatus, levels = c("Ins+ Gluc-", "Ins+ Gluc+", "Ins- Gluc+"))

## Density by Class ====

# Calculate the percentage of each endocrine objects size to the total number of endocrine objects
density_by_class <- eo_density_class(quant, area)

# Reshape data
density_by_class <- pivot_longer(density_by_class, 
                               cols = colnames(density_by_class)[2:31], 
                               names_to = "binningInteger", 
                               values_to = "density_by_class") %>%
  mutate(InsGluStatus = "NA")

# Populate class column
for(i in 1:nrow(density_by_class)){
  if(grepl("Insn_Glucp", density_by_class$binningInteger[i]) == TRUE){
    density_by_class$InsGluStatus[i] <- "Ins- Gluc+"
  }
  if(grepl("Insp_Glucp", density_by_class$binningInteger[i]) == TRUE){
    density_by_class$InsGluStatus[i] <- "Ins+ Gluc+"
  }
  if(grepl("Insp_Glucn", density_by_class$binningInteger[i]) == TRUE){
    density_by_class$InsGluStatus[i] <- "Ins+ Gluc-"
  }
}

# Rename the bins column
for(row in 1:nrow(density_by_class)){
  for(bin in 0:9){
    if(grepl(bin, density_by_class$binningInteger[row]) == TRUE){
      density_by_class$binningInteger[row] <- bin
    }
  }
}

# Add metadata for plotting
density_by_class_plotting <- left_join(density_by_class, quant %>% select(study_id, endoduration, endotype) %>% distinct(), by = "study_id")

# set factor levels for graph order
density_by_class_plotting$InsGluStatus <- factor(density_by_class_plotting$InsGluStatus, levels = c("Ins+ Gluc-", "Ins+ Gluc+", "Ins- Gluc+"))

# CD45 Quantification ==== 

# Calculate percentage cd45 positive staining per um of endocrine object
cd45_endo_all_bins <- cd45_endo %>%
  mutate(cd45_per_eo = (cd45_area/region_area) * 100) %>%
  group_by(study_id) %>%
  mutate(cd45_by_study = mean(cd45_per_eo)) %>%
  select(c("study_id", "endotype", "cd45_by_study")) %>%
  distinct()

# Calculate percentage cd45 percentage staining for um of endocrine object, and organise into size classes

# Assign sizes
cd45_endo_by_size <- cd45_endo %>%
  mutate(binSize = "NA")

for(i in 1:nrow(cd45_endo_by_size)){
  
  if(cd45_endo_by_size$binningInteger[i] %in% c("0", "1", "2", "3")){
    
    cd45_endo_by_size$binSize[i] <- "small"
    
  }
  
  if(cd45_endo_by_size$binningInteger[i] %in% c("4", "5", "6")){
    
    cd45_endo_by_size$binSize[i] <- "medium"
    
  }
  
  if(cd45_endo_by_size$binningInteger[i] %in% c("7", "8", "9")){
    
    cd45_endo_by_size$binSize[i] <- "large"
    
  }

}

# Calculate

cd45_endo_by_size <- cd45_endo_by_size %>%
  mutate(cd45_per_eo = (cd45_area/region_area) * 100) %>% # calculate percentage for each endocrine object
  group_by(study_id, binSize) %>%
  mutate(cd45_by_study = mean(cd45_per_eo)) %>% # average per study id and size of eo
  ungroup() %>%
  select(c("study_id", "endotype", "cd45_by_study", "binSize")) %>%
  distinct()
  
# Set levels for graph order

cd45_endo_by_size$binSize <- factor(cd45_endo_by_size$binSize, levels = c("small", "medium", "large"))


# Begin plotting: ====
  

## Area by Class ====

# Set variables to correct type
area_by_class_plotting$area_by_class <- as.numeric(area_by_class_plotting$area_by_class)

### SD ====

# Make plotting data frame with summary statistics for short duration
plot_SD_df <- area_by_class_plotting %>%
  filter(endoduration %in% c("No diabetes", "T1DE2 (SD)", "T1DE1 (SD)")) %>% # no .$ and use %in% 
  group_by(InsGluStatus, endoduration, binningInteger) %>%
  dplyr::summarise(
    n = n(), # n should be equal to number of samples NOT number of endocrine objects
    mean = mean(area_by_class, na.rm = F),
    sd = sd(area_by_class, na.rm = F),
    se = std.error(area_by_class, na.rm = F)
  )

# Plot faceted graph for short duration

# Make labels
y_label <- expression(paste("% Proportion of Total Endocrine Area (", mu, "m"^"2",")"))

areaSD_graph <- ggplot(data = plot_SD_df, aes(x = binningInteger, y = mean, colour = endoduration)) +
  ylab(y_label) +
  xlab("Endocrine Object Size") +
  geom_point() +
  geom_line(aes(group = endoduration)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  theme_bw() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14)) +
  facet_wrap(~InsGluStatus) +
  scale_colour_manual(values = c("#31393C","#DE4D86", "#5BC0EB"))#,
                      #labels = c(paste0("No diabetes (n=", nrow(plot_SD_df %>% filter(endoduration == "No diabetes") %>% distinct()), ")"),
                                 #paste0("T1DE1 (n=", nrow(plot_SD_df %>% filter(endoduration == "T1DE1 (SD)") %>% distinct()), ")") ), 
                                 #paste0("T1DE2 (n=", nrow(plot_SD_df %>% filter(endoduration == "T1DE2 (SD)") %>% distinct()), ")") )

### LD ====

# Make plotting data frame with summary statistics for long duration
plot_LD_df <- area_by_class_plotting %>%
  filter(endoduration %in% c("No diabetes", "T1DE2 (LD)", "T1DE1 (LD)")) %>% 
  group_by(InsGluStatus, endoduration, binningInteger) %>%
  dplyr::summarise(
    n = n(), 
    mean = mean(area_by_class, na.rm = F),
    sd = sd(area_by_class, na.rm = F),
    se = std.error(area_by_class, na.rm = F)
  )


# Plot faceted graph for long duration
areaLD_graph <- ggplot(data = plot_LD_df, aes(x = binningInteger, y = mean, colour = endoduration)) +
  ylab(y_label) +
  xlab("Endocrine Object Size") + 
  geom_point() +
  geom_line(aes(group = endoduration)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  theme_bw() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14)) +
  facet_wrap(~InsGluStatus) +
  scale_colour_manual(values = c("#31393C","#DE4D86", "#5BC0EB"))#,


## Count by Class ====

# Set variables to correct type
count_by_class_plotting$count_by_class <- as.numeric(count_by_class_plotting$count_by_class)

### SD ====

y_label <- "% Proportion of the Total Endocrine Object Count"

# Make plotting data frame with summary statistics for short duration
plot_SD_df <- count_by_class_plotting %>%
  filter(endoduration %in% c("No diabetes", "T1DE2 (SD)", "T1DE1 (SD)")) %>% # no .$ and use %in% 
  group_by(InsGluStatus, endoduration, binningInteger) %>%
  dplyr::summarise(
    n = n(), # n should be equal to number of samples NOT number of endocrine objects
    mean = mean(count_by_class),
    sd = sd(count_by_class),
    se = std.error(count_by_class)
  )


# Plot faceted graph for short duration
countSD_graph <- ggplot(data = plot_SD_df, aes(x = binningInteger, y = mean, colour = endoduration)) +
  ylab(y_label) +
  xlab("Endocrine Object Size") +
  geom_point() +
  geom_line(aes(group = endoduration)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  ylim(0, 45) +
  theme_bw() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14)) +
  facet_wrap(~InsGluStatus) +
  scale_colour_manual(values = c("#31393C","#DE4D86", "#5BC0EB"))

### LD ====

# Make plotting data frame with summary statistics for short duration
plot_LD_df <- count_by_class_plotting %>%
  filter(endoduration %in% c("No diabetes", "T1DE2 (LD)", "T1DE1 (LD)")) %>% # no .$ and use %in% 
  group_by(InsGluStatus, endoduration, binningInteger) %>%
  dplyr::summarise(
    n = n(), # n should be equal to number of samples NOT number of endocrine objects
    mean = mean(count_by_class),
    sd = sd(count_by_class),
    se = std.error(count_by_class)
  )


# Plot faceted graph for short duration
countLD_graph <- ggplot(data = plot_LD_df, aes(x = binningInteger, y = mean, colour = endoduration)) +
  ylab(y_label) +
  xlab("Endocrine Object Size") +
  geom_point() +
  geom_line(aes(group = endoduration)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  ylim(0, 45) +
  theme_bw() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14)) +
  facet_wrap(~InsGluStatus) +
  scale_colour_manual(values = c("#31393C","#DE4D86", "#5BC0EB"))


## Density by Class ====

# Set variables to correct type
density_by_class_plotting$density_by_class <- as.numeric(density_by_class_plotting$density_by_class)

### SD ====

# Make plotting data frame with summary statistics for short duration
plot_SD_df <- density_by_class_plotting %>%
  filter(endoduration %in% c("No diabetes", "T1DE2 (SD)", "T1DE1 (SD)")) %>% # no .$ and use %in% 
  group_by(InsGluStatus, endoduration, binningInteger) %>%
  dplyr::summarise(
    n = n(), # n should be equal to number of samples NOT number of endocrine objects
    mean = mean(density_by_class),
    sd = sd(density_by_class, na.rm = F),
    se = std.error(density_by_class, na.rm = F)
  )

y_label <- expression(paste("Average No. of Endocrine Objects per mm"^"2"," of Acinar Tissue"))

# Plot faceted graph for short duration
densitySD_graph <- ggplot(data = plot_SD_df, aes(x = binningInteger, y = mean, colour = endoduration)) +
  ylab(y_label) +
  xlab("Endocrine Object Size") +
  geom_point() +
  geom_line(aes(group = endoduration)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  theme_bw() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14)) +
  facet_wrap(~InsGluStatus) +
  scale_colour_manual(values = c("#31393C","#DE4D86", "#5BC0EB"))


### LD ====

# Make plotting data frame with summary statistics for short duration
plot_LD_df <- density_by_class_plotting %>%
  filter(endoduration %in% c("No diabetes", "T1DE2 (LD)", "T1DE1 (LD)")) %>% # no .$ and use %in% 
  group_by(InsGluStatus, endoduration, binningInteger) %>%
  dplyr::summarise(
    n = n(), # n should be equal to number of samples NOT number of endocrine objects
    mean = mean(density_by_class, na.rm = F),
    sd = sd(density_by_class, na.rm = F),
    se = std.error(density_by_class, na.rm = F)
  )


# Plot faceted graph for short duration
densityLD_graph <- ggplot(data = plot_LD_df, aes(x = binningInteger, y = mean, colour = endoduration)) +
  ylab(y_label) +
  xlab("Endocrine Object Size") +
  geom_point() +
  geom_line(aes(group = endoduration)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  theme_bw() +
  theme(legend.text = element_text(size = 14),
        legend.title = element_text(size = 14),
        axis.title = element_text(size = 14),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.text = element_text(size = 14),
        strip.text = element_text(size = 14)) +
  facet_wrap(~InsGluStatus) +
  scale_colour_manual(values = c("#31393C","#DE4D86", "#5BC0EB"))


# Combine Graphs ====
(areaSD_graph|areaLD_graph)/(countSD_graph|countLD_graph)/(densitySD_graph|densityLD_graph)


# Bar plot to compare insulin vs glucagon staining across each bin size ====

# re run for ND and for endotypes

results <- quant %>%
  filter(endotype %in% "T1DE2") # change here

results <- eo_ins_vs_gcg(results)

# rename columns
results <- find_replace_col_name(results, "ins", "Insulin")
results <- find_replace_col_name(results, "gcg", "Glucagon")

results$Insulin <- as.numeric(results$Insulin)
results$Glucagon <- as.numeric(results$Glucagon)

plotting <- results %>%
  pivot_longer(cols = c(Insulin, Glucagon), names_to = "stain", values_to = "percentage")

plotting$percentage <- as.numeric(plotting$percentage)

plotting %>% ggplot(aes(fill = stain, x = bin, y = percentage)) +
  geom_bar(position = "stack", stat = "identity") +
  scale_fill_manual(values = c("#2274A5", "#FB3640")) +
  theme_bw()

# CD45 ====

## all bins combined ====

cd45_endo_all_bins_plotting <- cd45_endo_all_bins %>%
  group_by(endotype) %>%
  dplyr::summarise(
    n = n(),
    mean = mean(cd45_by_study),
    sd = sd(cd45_by_study),
    se = std.error(cd45_by_study)
  )

cd45_endo_graph_all_bins <- cd45_endo_all_bins_plotting %>% ggplot(aes(x = endotype, y = mean)) +
  geom_bar(stat = "identity", aes(endotype, fill = endotype)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  ylim(0, 3) +
  scale_fill_manual(values = c("#31393C","#DE4D86", "#5BC0EB")) +
  theme_bw()

## bins separated ====

cd45_endo_by_size_plot <- cd45_endo_by_size %>%
  group_by(endotype, binSize) %>%
  dplyr::summarise(
    mean = mean(cd45_by_study),
    sd = sd(cd45_by_study),
    se = std.error(cd45_by_study)
  )

cd45_endo_by_size_plot %>%
  ggplot(aes(x = endotype, y = mean)) +
  geom_bar(stat = "identity", aes(endotype, fill = endotype)) +
  ylim(0, 6.5) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_fill_manual(values = c("#31393C","#DE4D86", "#5BC0EB")) +
  theme_bw() +
  facet_wrap(~binSize)



# CD45 Whole Tissue ====

acinar_cd45 <- read.csv("whole_tissue_inflammation.csv")
acinar_cd45 <- acinar_cd45[ , 2:7]

# Short and long duration combined
# calculate 
cd45_tissue_plot_df <- acinar_cd45 %>%
  group_by(endotype) %>%
  dplyr::summarise(
    n = n(),
    mean = mean(average_acinar_inflammation),
    se = std.error(average_acinar_inflammation)
  )

# plot
cd45_tissue_plot_df %>%
  ggplot(aes(x = endotype, y = mean)) +
  geom_bar(stat = "identity", aes(endotype, fill = endotype)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  ylim(0, 1) +
  scale_fill_manual(values = c("#31393C","#DE4D86", "#5BC0EB")) +
  theme_bw()

# Short duration only
cd45_tissue_plot_df <- acinar_cd45 %>%
  filter(endoduration %in% c("No diabetes", "T1DE1 (SD)", "T1DE2 (SD)")) %>%
  group_by(endoduration) %>%
  dplyr::summarise(
    n = n(),
    mean = mean(average_acinar_inflammation),
    se = std.error(average_acinar_inflammation)
  )

cd45_tissue_plot_df %>%
  ggplot(aes(x = endoduration, y = mean)) +
  geom_bar(stat = "identity", aes(endoduration, fill = endoduration)) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.2) +
  scale_fill_manual(values = c("#31393C","#DE4D86", "#5BC0EB")) +
  theme_bw()

