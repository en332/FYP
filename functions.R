## eo_number ==========================================================================================
eo_number <- function(data){
  
  # set up an empty data frame 
  results <- data.frame(study_ID = "", bin0 = "", bin1 = "", bin2 = "", bin3 = "", bin4 = "", bin5= "", bin6 = "", bin7= "", bin8 = "", bin9 = "")
  
  # set the row number counter
  row = 1
  
  # iterate through the study ids
  for(ID in unique(data$study_ID)){
    
    # filter the data for the study ID
    ID_df <- data %>% filter(.$study_ID == ID)
    # populate each row of the results table with a study ID
    results[row, "study_ID"] <- ID
    
    # find the number of endocrine objects for each study ID
    num_eo <- nrow(ID_df)
    
    # iterate through each bin
    for(bin in 0:9){
      # filter for each bin
      ID_bin_df <- ID_df %>% filter(bin_size == bin)
      # count number of endocrine objects in each bin
      num_eo_bin <- nrow(ID_bin_df)
      
      # calculate the percentage of each endocrine objects size to the total number of endocrine objects
      # and input into results table
      results[row, bin + 2] <- (num_eo_bin/num_eo) * 100
      
    }
    
    row <- row + 1
  }

  return(results)

}



## eo_number_class =====================================================================
eo_number_class <- function(data){
  
  # set up an empty data frame
  results <- data.frame(study_id = "", 
                        bin0_Insn_Glucp = "", bin0_Insp_Glucn = "", bin0_Insp_Glucp = "", 
                        bin1_Insn_Glucp = "", bin1_Insp_Glucn = "", bin1_Insp_Glucp = "",
                        bin2_Insn_Glucp = "", bin2_Insp_Glucn = "", bin2_Insp_Glucp = "",
                        bin3_Insn_Glucp = "", bin3_Insp_Glucn = "", bin3_Insp_Glucp = "",
                        bin4_Insn_Glucp = "", bin4_Insp_Glucn = "", bin4_Insp_Glucp = "",
                        bin5_Insn_Glucp = "", bin5_Insp_Glucn = "", bin5_Insp_Glucp = "",
                        bin6_Insn_Glucp = "", bin6_Insp_Glucn = "", bin6_Insp_Glucp = "",
                        bin7_Insn_Glucp = "", bin7_Insp_Glucn = "", bin7_Insp_Glucp = "",
                        bin8_Insn_Glucp = "", bin8_Insp_Glucn = "", bin8_Insp_Glucp = "",
                        bin9_Insn_Glucp = "", bin9_Insp_Glucn = "", bin9_Insp_Glucp = "")
  
  # set row counter
  row <- 1
  
  # set column numbers
  col_nums <- c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28)
  
  # iterate through the study ids
  for(ID in unique(data$study_id)){
    
    # filter the data for the study ID
    ID_df <- data %>% filter(.$study_id == ID)
    
    # populate each row of the results table with a study ID
    results[row, "study_id"] <- ID
    
    # find total area for each study ID
    num_eo <- nrow(ID_df)
    
    # iterate through each bin
    for(bin in 0:9){
      
      # filter the data for the bin
      ID_bin_df <- ID_df %>% filter(.$binningInteger == bin)
      
      col <- col_nums[bin + 1] 
      
      # iterate through each class
      for(class in unique(data$InsGluStatus)){
        
        col <- col + 1 
        
        # filter data for each class
        ID_bin_class_df <- ID_bin_df %>% filter(.$InsGluStatus == class)
        
        # find total area of endocrine objects in each class for each bin and study ID
        num_class_eo <- nrow(ID_bin_class_df)
        
        # calculate percentage total number of objects in each cat. out of all objects
        results[row, col] <- (num_class_eo/num_eo) * 100
        
      }
      
      
    }
    
    row <- row + 1
  }
  
  return(results)
}








## eo_area =============================================================================
eo_area <- function(data){
  
  # set up an empty data frame 
  results <- data.frame(study_ID = "", bin0 = "", bin1 = "", bin2 = "", bin3 = "", bin4 = "", bin5= "", bin6 = "", bin7= "", bin8 = "", bin9 = "")
  
  # set the row number counter
  row = 1
  
  # iterate through the study ids
  for(ID in unique(data$study_ID)){
    
    # filter the data for the study ID
    ID_df <- data %>% filter(.$study_ID == ID)
    
    # populate each row of the results table with a study ID
    results[row, "study_ID"] <- ID
    
    # find total area for each study ID
    area_eo <- sum(ID_df$region.area.um)
    
    # iterate through each bin
    for(bin in 0:9){
      
      # filter the data for the bin
      ID_bin_df <- ID_df %>% filter(.$bin_size == bin)
      
      # find total area for each bin within each study ID
      area_eo_bin <- sum(ID_bin_df$region.area.um)
      
      # calculate the percentage contribution of each eo area to overall area for each bin size and study id
      # and input into results table
      results[row, bin + 2] <- (area_eo_bin/area_eo) * 100
    }
    row <- row + 1
  }
  
  return(results)
  
}



## eo_area_class =====================================================================
eo_area_class <- function(data){
  
  # set up an empty data frame
  results <- data.frame(study_id = "", 
                        bin0_Insn_Glucp = "", bin0_Insp_Glucn = "", bin0_Insp_Glucp = "", 
                        bin1_Insn_Glucp = "", bin1_Insp_Glucn = "", bin1_Insp_Glucp = "",
                        bin2_Insn_Glucp = "", bin2_Insp_Glucn = "", bin2_Insp_Glucp = "",
                        bin3_Insn_Glucp = "", bin3_Insp_Glucn = "", bin3_Insp_Glucp = "",
                        bin4_Insn_Glucp = "", bin4_Insp_Glucn = "", bin4_Insp_Glucp = "",
                        bin5_Insn_Glucp = "", bin5_Insp_Glucn = "", bin5_Insp_Glucp = "",
                        bin6_Insn_Glucp = "", bin6_Insp_Glucn = "", bin6_Insp_Glucp = "",
                        bin7_Insn_Glucp = "", bin7_Insp_Glucn = "", bin7_Insp_Glucp = "",
                        bin8_Insn_Glucp = "", bin8_Insp_Glucn = "", bin8_Insp_Glucp = "",
                        bin9_Insn_Glucp = "", bin9_Insp_Glucn = "", bin9_Insp_Glucp = "")
  
  # set row counter
  row <- 1
  
  # set column numbers
  col_nums <- c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28)
  
  # iterate through the study ids
  for(ID in unique(data$study_id)){
    
    # filter the data for the study ID
    ID_df <- data %>% filter(.$study_id == ID)
    
    # populate each row of the results table with a study ID
    results[row, "study_id"] <- ID
    
    # find total area for each study ID
    area_eo <- sum(ID_df$region_area)
    
    # iterate through each bin
    for(bin in 0:9){
      
      # filter the data for the bin
      ID_bin_df <- ID_df %>% filter(.$binningInteger == bin)
      
      col <- col_nums[bin + 1] 
      
      # iterate through each class
      for(class in unique(data$InsGluStatus)){
        
        col <- col + 1 
        
        # filter data for each class
        ID_bin_class_df <- ID_bin_df %>% filter(.$InsGluStatus == class)
        
        # find total area of endocrine objects in each class for each bin and study ID
        area_class_eo <- sum(ID_bin_class_df$region_area)
        
        # calculate percentage total of area for each class and input into results table
        results[row, col] <- (area_class_eo/area_eo) * 100
        
      }
      
      
    }
    
    row <- row + 1
  }

  return(results)
}


## EO density =================================================== 

eo_density <- function(data, acinar){
  
  # set up an empty data frame 
  results <- data.frame(study_id = "", bin0 = "", bin1 = "", bin2 = "", bin3 = "", bin4 = "", bin5= "", bin6 = "", bin7= "", bin8 = "", bin9 = "")
  
  # set the row number counter
  row = 1
  
  # iterate through the study ids
  for(ID in unique(data$study_id)){
    
    # filter the data for the study ID
    ID_df <- data %>% filter(.$study_id == ID)
    
    # populate each row of the results table with a study ID
    results[row, "study_id"] <- ID
    
    # iterate through each bin
    for(bin in 0:9){
      # filter for each bin
      ID_bin_df <- ID_df %>% filter(binningInteger == bin)
      # count number of endocrine objects in each bin
      num_eo_bin <- nrow(ID_bin_df)
      
      # calculate the percentage of each endocrine objects size to the total number of endocrine objects
      # and input into results table
      results[row, bin + 2] <- (num_eo_bin / (area[row, 4] * 1.0e-6))
      
    }
    
    row <- row + 1
  }
  
  return(results)
  
}

## EO density with class ===================================================================

eo_density_class <- function(data, acinar){
  
  # set up an empty data frame
  results <- data.frame(study_id = "", 
                        bin0_Insn_Glucp = "", bin0_Insp_Glucn = "", bin0_Insp_Glucp = "", 
                        bin1_Insn_Glucp = "", bin1_Insp_Glucn = "", bin1_Insp_Glucp = "",
                        bin2_Insn_Glucp = "", bin2_Insp_Glucn = "", bin2_Insp_Glucp = "",
                        bin3_Insn_Glucp = "", bin3_Insp_Glucn = "", bin3_Insp_Glucp = "",
                        bin4_Insn_Glucp = "", bin4_Insp_Glucn = "", bin4_Insp_Glucp = "",
                        bin5_Insn_Glucp = "", bin5_Insp_Glucn = "", bin5_Insp_Glucp = "",
                        bin6_Insn_Glucp = "", bin6_Insp_Glucn = "", bin6_Insp_Glucp = "",
                        bin7_Insn_Glucp = "", bin7_Insp_Glucn = "", bin7_Insp_Glucp = "",
                        bin8_Insn_Glucp = "", bin8_Insp_Glucn = "", bin8_Insp_Glucp = "",
                        bin9_Insn_Glucp = "", bin9_Insp_Glucn = "", bin9_Insp_Glucp = "")
  
  # set row counter
  row <- 1
  
  # set column numbers
  col_nums <- c(1, 4, 7, 10, 13, 16, 19, 22, 25, 28)
  
  # iterate through the study ids
  for(ID in unique(data$study_id)){
    
    # filter the data for the study ID
    ID_df <- data %>% filter(.$study_id == ID)
    
    # populate each row of the results table with a study ID
    results[row, "study_id"] <- ID
    
    # iterate through each bin
    for(bin in 0:9){
      
      # filter the data for the bin
      ID_bin_df <- ID_df %>% filter(.$binningInteger == bin)
      
      col <- col_nums[bin + 1] 
      
      # iterate through each class
      for(class in unique(data$InsGluStatus)){
        
        col <- col + 1 
        
        # filter data for each class
        ID_bin_class_df <- ID_bin_df %>% filter(.$InsGluStatus == class)
        
        # find total area of endocrine objects in each class for each bin and study ID
        num_class_eo <- nrow(ID_bin_class_df)
        
        # calculate the percentage of each endocrine objects size to the total number of endocrine objects
        # and input into results table
        results[row, col] <- (num_class_eo / (area[row, 4] * 1.0e-6)) 
        
      }
      
      
    }
    
    row <- row + 1
  }
  
  return(results)
}

## total % insulin/glucagon area across whole area per study ====================

eo_percentage <- function(df, acinar_area){
  
  results <- data.frame(study_ID = "", perIns = "", perGluc = "")
  row <-  1
  
  for(ID in unique(df$study_ID)){
    
    # filter for each study ID
    endo_df <- df %>% filter(.$study_ID == ID)
    area_df <- acinar_area %>% filter(.$study_ID == ID)
    
    # populate table with study ID
    results[row, 1] <- ID
    
    # sum total insulin
    Ins <- endo_df %>% 
      select("insulin.um.2") %>%
      filter(!is.na(.)) %>%
      sum()
    
    # sum total glucagon
    Gluc <- endo_df %>% 
      select("glucagon.um.2") %>%
      filter(!is.na(.)) %>%
      sum()
    
    # calculate percentages
    # input into df
    results[row, "perIns"] <- Ins / (area_df[3] + area_df[4])
    results[row, "perGluc"] <- Gluc / (area_df[3] + area_df[4])
    
    row <-  row + 1
    
  }
  
  return(results)
  
}


# percentage ins and glucagon staining as a portion of total endocrine staining
# stacked bar plot

eo_ins_vs_gcg <- function(df){
  
  # make empty df
  results <- data.frame(bin = "", ins = "", gcg = "")
  
  # row counter
  row <- 1
  
  # select out each bin size
  for(i in unique(df$binningInteger)){
    
    # input bin size into df
    results[row, "bin"] <- i
    
    # filter for each binning integer
    bin_df <- df %>%
      filter(binningInteger == i)
    
    # find total staining for insulin, glucagon in each bin
    total_ins_per_bin <- sum(bin_df$insulin_area)
    total_gcg_per_bin <- sum(bin_df$glucagon_area)
    
    # find total staining for each bin
    total_endo_staining <- sum(total_ins_per_bin, total_gcg_per_bin)
    
    # calculate percentages
    per_ins <- (total_ins_per_bin / total_endo_staining) * 100
    per_gcg <- (total_gcg_per_bin / total_endo_staining) * 100
    
    # input percentages into df
    results[row, "ins"] <- per_ins
    results[row, "gcg"] <- per_gcg
    
    # increase counter
    row <- row + 1
    
  }
  
  return(results)
}


## rename headings ==============================================================

find_replace_col_name <- function(df, target, new_name){   
  colnames(df)[which(colnames(df) == as.character(target))] <- as.character(new_name)   
  return(df) 
}


