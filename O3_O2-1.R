# PCA analysis

PCR_df <- data.frame(forbrug_period$Vaekst,
                    ftillid_K_period$Q1_Fam_nu,
                    ftillid_K_period$Q2_Fam_etår,
                    ftillid_K_period$Q3_Dan_nu,
                    ftillid_K_period$Q4_Dan_etår,
                    ftillid_K_period$Q5_Forbrugsgoder_nu,
                    ftillid_K_period$Q6_Priser_nu,
                    ftillid_K_period$Q7_Priser_etår,
                    ftillid_K_period$Q8_Arbejdsløshed,
                    ftillid_K_period$Q9_Forbrugsgoder_etår,
                    ftillid_K_period$Q10_Sparop_nu,
                    ftillid_K_period$Q11_Sparop_etår,
                    ftillid_K_period$Q12_Famøk_nu)

colnames(PCR_df) <- c("Vaekst", "Q1_Fam_nu", "Q2_Fam_etår", "Q3_Dan_nu", "Q4_Dan_etår", 
                            "Q5_Forbrugsgoder_nu", "Q6_Priser_nu", "Q7_Priser_etår", 
                            "Q8_Arbejdsløshed", "Q9_Forbrugsgoder_etår", "Q10_Sparop_nu", 
                            "Q11_Sparop_etår", "Q12_Famøk_nu")

### Run the PCA model
library(pls)
DST_PCR <- pcr(Vaekst ~ ., data=PCR_df, scale=TRUE, validation="CV")
summary(DST_PCR)

# Get the loadings
loadings_DTS_PCR <- DST_PCR$loadings
weights <- loadings_DTS_PCR[1:12]^2
sum(weights)

validationplot(DST_PCR, val.type="MSEP")

#### Calculate the new Comp1 weighted indicator
optimal_indicator$weighted_ind <- ftillid_K_period$Q1_Fam_nu*loadings_DTS_PCR[1] + 
  ftillid_K_period$Q2_Fam_etår*loadings_DTS_PCR[2] +
  ftillid_K_period$Q3_Dan_nu*loadings_DTS_PCR[3] + 
  ftillid_K_period$Q4_Dan_etår*loadings_DTS_PCR[4] +
  ftillid_K_period$Q5_Forbrugsgoder_nu*loadings_DTS_PCR[5] + 
  ftillid_K_period$Q6_Priser_nu*loadings_DTS_PCR[6] +
  ftillid_K_period$Q7_Priser_etår*loadings_DTS_PCR[7] + 
  ftillid_K_period$Q8_Arbejdsløshed*loadings_DTS_PCR[8] +
  ftillid_K_period$Q9_Forbrugsgoder_etår*loadings_DTS_PCR[9] + 
  ftillid_K_period$Q10_Sparop_nu*loadings_DTS_PCR[10] +
  ftillid_K_period$Q11_Sparop_etår*loadings_DTS_PCR[11] + 
  ftillid_K_period$Q12_Famøk_nu*loadings_DTS_PCR[12]

# Plot the indicator
scale_factor <- 0.5
optimal_indicator$TID <- ftillid_K_period$TID

library(ggplot2)
ggplot(optimal_indicator, aes(x=TID)) +
  geom_line(aes(y=weighted_ind)) +
  geom_point(aes(y=weighted_ind)) +
  geom_bar(aes(y=vaekst / scale_factor), stat="identity", fill="blue", alpha=0.5) +
  geom_hline(yintercept=mean(optimal_indicator$weighted_ind), color="orange") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", limits = as.Date(c("2000-03-01", "2024-06-01"))) +
  scale_y_continuous(name = "PCA vægtet indikator", sec.axis = sec_axis(~ . * scale_factor, name = "Vækst")
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="", title="Vægtet indikator følger realvæksten")



###### Updated Priciple Component Analysis
model_pca <- lm(optimal_indicator$vaekst ~ optimal_indicator$weighted_ind)
summary(model_pca)

pcr_coef <- c(coefficients(model_pca)[2]*loadings_DTS_PCR[,1])

pre <- as.data.frame(matrix(nrow=1, ncol=12))
pre[1,] <- ftillid_K_period[99,3:14]

sum(data.frame(coefficients(model_pca)[1], pcr_coef*pre))
pred_pcr <- predict(DST_PCR, pre, ncomp=1)
pred_pcr


