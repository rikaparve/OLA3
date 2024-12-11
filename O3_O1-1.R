#### Hent data om forbrugertillid fra DST
library(dkstat)
ftillid_meta <- dst_meta(table = "FORV1", lang = "da")
str(ftillid_meta)

myquery <- list(
  INDIKATOR = "*",
  Tid = "*"
)

ftillid_meta <- dst_get_data(table = "FORV1", query = myquery, lang = "da")

# Lav dataframen om så alle indikatorer er i forskellige kolonner
library(reshape)
ftillid_wide <- reshape(ftillid_meta, 
                        idvar = "TID", 
                        timevar = "INDIKATOR", 
                        direction = "wide")

colnames(ftillid_wide)= c("TID", "DST_FTI", "Q1_Fam_nu", "Q2_Fam_etår", "Q3_Dan_nu", "Q4_Dan_etår", "Q5_Forbrugsgoder_nu", "Q6_Priser_nu", "Q7_Priser_etår", "Q8_Arbejdsløshed", "Q9_Forbrugsgoder_etår", "Q10_Sparop_nu", "Q11_Sparop_etår", "Q12_Famøk_nu")

# Q6 og Q8 skal ganges med -1 på grund af den møde spørgsmål og svar er bygget på
ftillid_wide$Q6_Priser_nu <- ftillid_wide$Q6_Priser_nu*(-1)
ftillid_wide$Q8_Arbejdsløshed <- ftillid_wide$Q8_Arbejdsløshed*(-1)

## Lav dataen om til kvartaler
ftillid_wide <- ftillid_wide[-c(1:255),]
row.names(ftillid_wide) <- NULL

ftillid_K <- data.frame(TID = numeric(),
                        DST_FTI = numeric(),
                        Q1_Fam_nu = numeric(),
                        Q2_Fam_etår = numeric(),
                        Q3_Dan_nu = numeric(),
                        Q4_Dan_etår = numeric(),
                        Q5_Forbrugsgoder_nu = numeric(),
                        Q6_Priser_nu = numeric(),
                        Q7_Priser_etår = numeric(),
                        Q8_Arbejdsløshed = numeric(),
                        Q9_Forbrugsgoder_etår = numeric(),
                        Q10_Sparop_nu = numeric(),
                        Q11_Sparop_etår = numeric(),
                        Q12_Famøk_nu = numeric())

# Lav indikatorerne om til kvartaler med loop
for (i in seq(3, nrow(ftillid_wide), by = 3)) {
  
  # Tidsperioden
  TID <- ftillid_wide$TID[i]
  
  # Regn kvartal-baseret gennemsniter for indikatorer
  DST_FTI <- mean(ftillid_wide$DST_FTI[(i-2):i], na.rm = TRUE)
  Q1_Fam_nu <- mean(ftillid_wide$Q1_Fam_nu[(i-2):i], na.rm = TRUE)
  Q2_Fam_etår <- mean(ftillid_wide$Q2_Fam_etår[(i-2):i], na.rm = TRUE)
  Q3_Dan_nu <- mean(ftillid_wide$Q3_Dan_nu[(i-2):i], na.rm = TRUE)
  Q4_Dan_etår <- mean(ftillid_wide$Q4_Dan_etår[(i-2):i], na.rm = TRUE)
  Q5_Forbrugsgoder_nu <- mean(ftillid_wide$Q5_Forbrugsgoder_nu[(i-2):i], na.rm = TRUE)
  Q6_Priser_nu <- mean(ftillid_wide$Q6_Priser_nu[(i-2):i], na.rm = TRUE)
  Q7_Priser_etår <- mean(ftillid_wide$Q7_Priser_etår[(i-2):i], na.rm = TRUE)
  Q8_Arbejdsløshed <- mean(ftillid_wide$Q8_Arbejdsløshed[(i-2):i], na.rm = TRUE)
  Q9_Forbrugsgoder_etår <- mean(ftillid_wide$Q9_Forbrugsgoder_etår[(i-2):i], na.rm = TRUE)
  Q10_Sparop_nu <- mean(ftillid_wide$Q10_Sparop_nu[(i-2):i], na.rm = TRUE)
  Q11_Sparop_etår <- mean(ftillid_wide$Q11_Sparop_etår[(i-2):i], na.rm = TRUE)
  Q12_Famøk_nu <- mean(ftillid_wide$Q12_Famøk_nu[(i-2):i], na.rm = TRUE)
  
  # Sample gemmensnitter i en ny dataframe
  ftillid_K <- rbind(ftillid_K, 
                     data.frame(TID = TID,
                                DST_FTI = DST_FTI,
                                Q1_Fam_nu = Q1_Fam_nu,
                                Q2_Fam_etår = Q2_Fam_etår,
                                Q3_Dan_nu = Q3_Dan_nu,
                                Q4_Dan_etår = Q4_Dan_etår,
                                Q5_Forbrugsgoder_nu = Q5_Forbrugsgoder_nu,
                                Q6_Priser_nu = Q6_Priser_nu,
                                Q7_Priser_etår = Q7_Priser_etår,
                                Q8_Arbejdsløshed = Q8_Arbejdsløshed,
                                Q9_Forbrugsgoder_etår = Q9_Forbrugsgoder_etår,
                                Q10_Sparop_nu = Q10_Sparop_nu,
                                Q11_Sparop_etår = Q11_Sparop_etår,
                                Q12_Famøk_nu = Q12_Famøk_nu))
}

# Adjust the two datasets so they have the same length
ftillid_K_period <- ftillid_K[-c(1:16),]
row.names(ftillid_K_period) <- NULL

#### Lav alle kombinationer
# Clean the lists before running the code again
comb_list <- NULL
comb_names <- NULL

#Create the lists for combinations and their names
comb_list <- list()
comb_names <- list()  

# Total number of columns for combinations
n_cols <- ncol(ftillid_K_period) - 2  # Excluding columns 1 and 2

# Loop over all combination sizes (from 1 to n_cols)
for (j in 1:n_cols) {
  combinations <- combn(3:14, j, simplify = FALSE)  # Get all combinations of columns 3 to 14
  
  # Calculate row means for each combination of columns
  for (cols in combinations) {
    avg_value <- rowMeans(ftillid_K_period[, cols, drop = FALSE], na.rm = TRUE)  # Calculate the average for the current combination
    comb_list[[length(comb_list) + 1]] <- avg_value  # Store results
    
    # Store the combination name as a character string
    comb_names[[length(comb_names) + 1]] <- paste(colnames(ftillid_K_period)[cols], collapse = ", ")
  }
}

# Check the number of combinations generated
length(comb_list)  # Should be 4095








