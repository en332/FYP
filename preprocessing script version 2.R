# Load in libraries =====

library(dplyr)
library(tidyverse)

# Key

# tei_endo = filtered and cleaned tei data ENDOCRINE ONLY
# ella_endo = filtered an cleaned ella data ENDOCRINE ONLY
# endo_df = filtered and cleaned combined data ENDOCRINE ONLY
# tei_acinar = filtered and cleaned tei data ACINAR ONLY
# ella_acinar = filtered an cleaned ella data ACINAR ONLY
# acinar_df = filtered and cleaned combined data ACINAR ONLY



# Read in Tei data ====

tei <- read.csv("eo_acinar_data_insgluccd45(in).csv")

# rename some columns

tei <- find_replace_col_name(tei, "study_ID", "study_id")
tei <- find_replace_col_name(tei, "Region.Area..μm..", "region_area")
tei <- find_replace_col_name(tei, "Glucagon_pink.Area..um.2.", "glucagon_area")
tei <- find_replace_col_name(tei, "Insulin_brown.Area..um.2.", "insulin_area")
tei <- find_replace_col_name(tei, "Diabetes.Duration..yrs.", "diabetes_duration")
tei <- find_replace_col_name(tei, "Donor_type_aab", "donor_type")
tei <- find_replace_col_name(tei, "subset1", "endoduration")
tei <- find_replace_col_name(tei, "Age.At.Onset", "age_at_onset")
tei <- find_replace_col_name(tei, "Sex", "sex")


# set columns to keep

cols_to_keep <- c("study_id",
                  "region_area", 
                  "glucagon_area", 
                  "insulin_area", 
                  "pancreas_location",
                  "diabetes_duration",
                  "donor_type",
                  "endotype",
                  "duration_status",
                  "endoduration",
                  "age_group",
                  "binningInteger",
                  "InsGluStatus",
                  "age_at_onset",
                  "sex")

# substitute T1D medalist to T1D
#tei$donor_type <- gsub("T1D Medalist", "T1D", tei$donor_type)

# Filter and select columns to keep for the endocrine data only
tei_endo <- tei %>%
  filter(Classifier.Label %in% "Endocrine") %>% # filter for endocrine objects
  select(all_of(cols_to_keep)) %>% # select for relevant columns
  filter(donor_type %in% c("T1D", "No diabetes")) 



# Read in my data ======

# read in the HALO_output
HALO_output <- read.csv("HALO_output_21_02.csv")

# read in reference file
ref <- read.csv("study_ID-image_location-clinical_InsGlucCD45_batch2.csv")

# rename columns to match up 
ref <- find_replace_col_name(ref, "Image_Location", "Image.Location")

# join HALO_output and reference data frames together for my data
ella <- left_join(HALO_output, ref, by = "Image.Location")

# check that each study id is matched to the correct image location - will error if not matched

# change study ids to match image location names
ella$study_ID <- gsub("/", " ", ella$study_ID)

for(i in 1:length(ella)) {
  if(grepl(ella$study_ID[i], ella$Image.Location[i]) == FALSE) {
    return("Doesn't match")
  }
}

## check there is one area quantification result per image - will error if not matched 
for(i in 1:length(ella)) {
  if(isTRUE(is.na(ella$Region_Area[i]) == TRUE)) {
    return("Doesn't match")
  }
}

# Read in the clinical information to join with my data ====

# read in clinical data
clinical <- read.csv("nPOD_EADB_DiViDmerge_TLedit-v3(in).csv")

## assign donor information ====

# make dummy column for donor information
clinical <- clinical %>%
  mutate(donor_type = "Other")

# populate column for donor information
for(i in 1:nrow(clinical)){
  if(clinical$Donor.Type[i] == "Non-Diabetic" | clinical$Donor.Type[i] == "No diabetes" |clinical$Donor.Type[i] == "Other-No diabetes"){
    clinical$donor_type[i] <- "No diabetes"
  }
  if(clinical$Donor.Type[i] == "Autoab Pos" |clinical$Donor.Type[i] == "No diabetes (AAb+)"){
    clinical$donor_type[i] <- "No diabetes (AAb+)"
  }
  if(clinical$Donor.Type[i] == "T1D" | clinical$Donor.Type[i] == "T1D Medalist" | clinical$Donor.Type[i] == "Type I - No ICIs" | clinical$Donor.Type[i] == "Type I - Residual ICIs"){
    clinical$donor_type[i] <- "T1D"
  }
}

## assign age groups ====

# round down ages
clinical$Age..yrs. <- floor(clinical$Age..yrs.)

# make dummy column for age group
clinical <- clinical %>%
  mutate(age_group = "NA")

# populate according to age group
for(i in 1:nrow(clinical)){
  if(isTRUE(clinical$Age..yrs.[i] >= 0) == TRUE & isTRUE(clinical$Age..yrs.[i] <= 1) == TRUE){
    clinical$age_group[i] <- "0-1yrs"
  }
  if(isTRUE(clinical$Age..yrs.[i] >= 2) == TRUE & isTRUE(clinical$Age..yrs.[i] <= 6) == TRUE){
    clinical$age_group[i] <- "2-6yrs"
  }
  if(isTRUE(clinical$Age..yrs.[i] >= 7) == TRUE & isTRUE(clinical$Age..yrs.[i] <= 12) == TRUE){
    clinical$age_group[i] <- "7-12yrs"
  }
  if(isTRUE(clinical$Age..yrs.[i] >= 13) == TRUE & isTRUE(clinical$Age..yrs.[i] <= 17) == TRUE){
    clinical$age_group[i] <- "13-17yrs"
  }
  if(isTRUE(clinical$Age..yrs.[i] >= 18) == TRUE){
    clinical$age_group[i] <- "≥18yrs"
  }
}

## assign duration ==== 

# make dummy column to label either long or short duration
clinical <- clinical %>%
  mutate(duration_status = "NA")

# populate label short duration samples
for(i in 1:nrow(clinical)){
  if(isTRUE(clinical$donor_type[i] == "No diabetes") == TRUE | isTRUE(clinical$donor_type[i] == "No diabetes (AAb+)") == TRUE){
    clinical$duration_status[i] <- "No diabetes"
  }
  if(isTRUE(clinical$Diabetes.Duration..yrs.[i] <= 2) == TRUE & isTRUE(clinical$Diabetes.Duration..yrs.[i] >= 0) == TRUE){
    clinical$duration_status[i] <- "SD"
  }
  if(isTRUE(clinical$Diabetes.Duration..yrs.[i] > 2) == TRUE){
    clinical$duration_status[i] <- "LD"
  }
}

## assign endotypes ====

# make dummy column for endotypes
clinical <- clinical %>%
  mutate(endotype = "Other")

# populate endotype column
for(i in 1:nrow(clinical)){
  if(isTRUE(clinical$donor_type[i] == "No diabetes") == TRUE | isTRUE(clinical$donor_type[i] == "No diabetes (AAb+)") == TRUE){
    clinical$endotype[i] <- "No diabetes"
  }
  if(isTRUE(clinical$donor_type[i] == "T1D") == TRUE && isTRUE(clinical$Age.At.Onset[i] <= 13) == TRUE){
    clinical$endotype[i] <- "T1DE1"
  }
  if(isTRUE(clinical$donor_type[i] == "T1D") == TRUE && isTRUE(clinical$Age.At.Onset[i] > 13) == TRUE){
    clinical$endotype[i] <- "T1DE2"
  }
}


# Join my HALO data to the clinical metadata =====

# change study ids to match file-names - reverse as before
ella$study_ID <- gsub(" ", "/", ella$study_ID)

# join on appropriate clinical data information
ella <- left_join(ella, clinical, by = "study_ID")

# Select out the endocrine objects ====

# algorithms to keep
to_keep <- c("InsGlucCD45_area-quant_Pan8" , 
             "InsGlucCD45_area-quant_18490" ,
             "InsGlucCD45_area-quant_333-66",
             "InsGlucCD45_area-quant_8579",
             "InsGlucCD45_area-quant_39-67",
             "InsGlucCD45_area-quant_14666",
             "InsGlucCD45_area-quant_15088",
             "InsGlucCD45_area-quant_24791",
             "InsGlucCD45_area-quant_8501",
             "InsGlucCD45_area-quant_8651",
             "Indica Labs - Area Quantification v2.4.9_RedYellow",
             "Indica Labs - Area Quantification v2.4.9_RedBlue")

# filter for endocrine objects 
ella_endo <- ella %>%
  filter(Algorithm.Name %in% to_keep)

# Copy over the red/yellow classifier results into the red/blue classifer results column to merge them together - would be more efficient to extract NA entries and copy over rather than going through every one
for(i in 1:nrow(ella_endo)){ # now - there are no NAs in any of these columns
  if(is.na(ella_endo$Glucagon_blue.Area..um.2.[i]) == TRUE){
    ella_endo$Glucagon_blue.Area..um.2.[i] <-  ella_endo$Glucagon.Area..um.2.[i]
  }
  if(is.na(ella_endo$Insulin_red.Area..um.2.[i]) == TRUE){
    ella_endo$Insulin_red.Area..um.2.[i] <-  ella_endo$Insulin.Area..um.2.[i]
  }
  if(is.na(ella_endo$CD45_brown.Area..um.2.[i]) == TRUE){
    ella_endo$CD45_brown.Area..um.2.[i] <-  ella_endo$CD45.Area..um.2.[i]
  }
}

# Assign InsGluStatus ====

# designate endocrine objects to Ins+ or Glu+ etc.
ella_endo <- ella_endo %>%
  mutate(InsGluStatus = "NA")

# populate according to stain 
for(i in 1:nrow(ella_endo)){ # there are no NA values in this column
  if(isTRUE(ella_endo$Insulin_red.Area..um.2.[i] >= 40) == TRUE & isTRUE(ella_endo$Glucagon_blue.Area..um.2.[i] < 40) == TRUE){
    ella_endo$InsGluStatus[i] <- "Ins+ Gluc-"
  }
  if(isTRUE(ella_endo$Insulin_red.Area..um.2.[i] >= 40) == TRUE & isTRUE(ella_endo$Glucagon_blue.Area..um.2.[i] >= 40) == TRUE){
    ella_endo$InsGluStatus[i] <- "Ins+ Gluc+"
  }
  if(isTRUE(ella_endo$Insulin_red.Area..um.2.[i] < 40) == TRUE & isTRUE(ella_endo$Glucagon_blue.Area..um.2.[i] < 40) == TRUE){
    ella_endo$InsGluStatus[i] <- "Ins- Gluc-"
  }
  if(isTRUE(ella_endo$Insulin_red.Area..um.2.[i] < 40) == TRUE & isTRUE(ella_endo$Glucagon_blue.Area..um.2.[i] >= 40) == TRUE){
    ella_endo$InsGluStatus[i] <- "Ins- Gluc+"
  }
}


## Add endoduration ====

for(row in 1:nrow(ella_endo)){
  #combine endotype and duration e.g Tide1 (sd)
  ella_endo[row, "endoduration"] <- paste0(ella_endo[row, "endotype"], " (", ella_endo[row, "duration_status"], ")")
}

# change 'No diabetes (No diabetes)' to just 'No diabetes'
ella_endo$endoduration <- gsub(" (No diabetes)", "", ella_endo$endoduration, fixed = TRUE)


# Filter out -/- entries or those under 170 um and calculate bin_size ====

ella_endo <- ella_endo %>% # this doesn't have anything smaller than 170, doesn't have any -/-, and calculates bin sizes BUT includes 10
  filter(.$Region.Area..μm.. > 170) %>%
  filter(.$InsGluStatus != "Ins- Gluc-") %>%
  mutate(binningInteger = floor(log2(.$Region.Area..μm.. / 170)))

# change bin size 10 to 9
ella_endo$binningInteger <- gsub("10", "9", ella_endo$binningInteger)

## Rename columns ====

ella_endo <- find_replace_col_name(ella_endo, "study_ID", "study_id")
ella_endo <- find_replace_col_name(ella_endo, "Region.Area..μm..", "region_area")
ella_endo <- find_replace_col_name(ella_endo, "Glucagon_blue.Area..um.2.", "glucagon_area")
ella_endo <- find_replace_col_name(ella_endo, "Insulin_red.Area..um.2.", "insulin_area")
ella_endo <- find_replace_col_name(ella_endo, "CD45_brown.Area..um.2.", "cd45_area")
ella_endo <- find_replace_col_name(ella_endo, "Diabetes.Duration..yrs.", "diabetes_duration")
ella_endo <- find_replace_col_name(ella_endo, "Age.At.Onset", "age_at_onset")
ella_endo <- find_replace_col_name(ella_endo, "Sex", "sex")

# Extract CD45 data ====

cd45_endo_df <- ella_endo %>%
  select(c("study_id", "region_area", "binningInteger", "cd45_area", "donor_type", "endotype", "endoduration"))

# Select out the columns to keep ====

ella_endo <- ella_endo %>%
  select(all_of(cols_to_keep)) %>% # select for relevant columns
  filter(donor_type %in% c("T1D", "No diabetes")) 

# Join both endocrine data sets together ====

endo_df <- rbind(ella_endo, tei_endo)

# There are no missing values in the area quantification results, and all the caterorical variables are matched nicely. 

# Extract acinar area data ====

## Tei data ====

# separate acinar data
tei_acinar <- tei %>% 
  filter(Analysis.Region %in% "Acinar") %>%
  filter(donor_type %in% c("T1D", "No diabetes"))

## Calculate acinar and endocrine areas per study id for tei data ====

# make empty data frame
tei_acinar_area_df <- data.frame(study_id = "", acinar_area = "", endo_area = "", total_area = "")

# make row counter
row <- 1

# populate data frame
for(ID in unique(tei_acinar$study_id)){
  
  # add study id to each row
  tei_acinar_area_df[row, "study_id"] <- ID
  
  # separate out entries for each id in acinar df
  id_acinar_df <- tei_acinar %>% filter(study_id == ID)
  
  # sum acinar area of each id and add to results data frame
  tei_acinar_area_df[row, "acinar_area"] <- sum(id_acinar_df$region_area)
  
  # separate out entries for each id in endo df
  id_endo_df <- tei_endo %>% filter(study_id == ID)
  
  # sum endocrine area for each id and add to results df
  tei_acinar_area_df[row, "endo_area"] <- sum(id_endo_df$region_area)
  
  # sun both acinar and endocrine area together for total area and add to results df
  tei_acinar_area_df[row, "total_area"] <- as.numeric(tei_acinar_area_df[row, "acinar_area"]) + as.numeric(tei_acinar_area_df[row, "endo_area"])
  
  row <- row + 1
  
}

## Ella data ====

# separate acinar data

ella_acinar <- ella %>%
  filter(Classifier.Label %in% "Acinar") %>%
  filter(Analysis.Region %in% "GoodScan")

ella_acinar <- ella %>%
  filter(Classifier.Label %in% "GoodScan") %>%
  filter(Analysis.Region %in% "Acinar")

## Calculate acinar and endocrine areas per study id for tei data ====

# make empty data frame
ella_acinar_area_df <- data.frame(study_id = "", acinar_area = "", endo_area = "", total_area = "")

# make row counter
row <- 1

# populate data frame
for(ID in unique(ella$study_ID)){
  
  # add study id to each row
  ella_acinar_area_df[row, "study_id"] <- ID
  
  # separate out entries for each id in acinar df
  id_acinar_df <- ella_acinar %>% filter(study_ID == ID)
  
  # sum acinar area of each id and add to results data frame
  ella_acinar_area_df[row, "acinar_area"] <- sum(id_acinar_df$Region.Area..μm..)
  
  # separate out entries for each id in endo df
  id_endo_df <- ella_endo %>% filter(study_id == ID) 
  
  # sum endocrine area for each id and add to results df
  ella_acinar_area_df[row, "endo_area"] <- sum(id_endo_df$region_area)
  
  # sun both acinar and endocrine area together for total area and add to results df
  ella_acinar_area_df[row, "total_area"] <- as.numeric(ella_acinar_area_df[row, "acinar_area"]) + as.numeric(ella_acinar_area_df[row, "endo_area"])
  
  row <- row + 1
  
}

# Bind ella and tei dfs together ====

acinar_df <- rbind(ella_acinar_area_df, tei_acinar_area_df)

# Remove duplicate studies ====

## Acinar ====
# identify duplicates using the acinar data (39)
duplicates <- acinar_df[duplicated(acinar_df$study_id),]

# remove duplicates from tei acinar data (sorry)
tei_acinar_area_df <- tei_acinar_area_df[!tei_acinar_area_df$study_id %in% duplicates$study_id, ]

# rejoin without duplicate studies - 114 unique studies total
acinar_df <- rbind(ella_acinar_area_df, tei_acinar_area_df)

# Quantifying inflammation across whole tissue sections ====

## Read in data 
whole_acinar <- read.csv("whole_acinar_cd45_ver2.csv")

# Select out area cd45 quant - only selects 61 of total 69 images
acinar_cd45 <- whole_acinar[grep("whole-area", whole_acinar$Algorithm.Name), ]

# select columns to keep
acinar_data_cols <- c("Image.Location",
                      "Region.Area",
                      "CD45_brown.Area..um.2.")

# Remove rows with region area under 10,000 um
acinar_cd45 <- acinar_cd45 %>%
  filter(Region.Area > 10000) %>%
  # select for specific columns
  select(all_of(acinar_data_cols)) %>% 
  # and finally calculate percentage inflammation per um^2
  mutate(acinar_inflammation = (CD45_brown.Area..um.2. / Region.Area) * 100)

# combine with study ids 
acinar_cd45 <- acinar_cd45 %>%
  left_join(ref, acinar_cd45, by = "Image.Location")

# select specific columns and average across each study
acinar_cd45 <- acinar_cd45 %>%
  select(c("study_ID", "acinar_inflammation")) %>%
  group_by(study_ID) %>%
  mutate(average_acinar_inflammation = mean(acinar_inflammation)) %>%
  select(c("study_ID", "average_acinar_inflammation")) %>%
  distinct()

# append clinical information
acinar_cd45 <- left_join(acinar_cd45, clinical, by = "study_ID")

# change column name
acinar_cd45 <- find_replace_col_name(acinar_cd45, "study_ID", "study_id")

# only keep specific columns (and add blank one for endoduration)
acinar_cd45 <- acinar_cd45 %>%
  select(c("study_id",
           "average_acinar_inflammation",
           "donor_type",
           "endotype",
           "duration_status")) %>%
  mutate(endoduration = "NA")

# populate endoduration column
for(row in 1:nrow(acinar_cd45)){
  #combine endotype and duration e.g Tide1 (sd)
  acinar_cd45[row, "endoduration"] <- paste0(acinar_cd45[row, "endotype"], " (", acinar_cd45[row, "duration_status"], ")")
}

# change 'No diabetes (No diabetes)' to just 'No diabetes'
acinar_cd45$endoduration <- gsub(" (No diabetes)", "", acinar_cd45$endoduration, fixed = TRUE)


## Endocrine ====

# combine all unique study ids for both ella and tei data
all_study_ids <- c(unique(ella_endo$study_id), unique(tei_endo$study_id))

# get a list of duplicates
duplicates <- all_study_ids[duplicated(all_study_ids)]

# remove duplicates
tei_endo <- tei_endo[!tei_endo$study_id %in% duplicates, ]

# recombine
endo_df <- rbind(ella_endo, tei_endo)

## Final Adjustments ====

# adjust sex
endo_df$sex <- gsub("F", "f", endo_df$sex)
endo_df$sex <- gsub("M", "m", endo_df$sex)


## Export ====

write.csv(endo_df, "endocrine_data.csv")

write.csv(acinar_df, "acinar_data.csv")

write.csv(cd45_endo_df, "cd45_endo_df.csv")

write.csv(acinar_cd45, "whole_tissue_inflammation.csv")


