# Install packages for Data Wrangling
# Packages installation for Data Wrangling 
install.packages("BH")
install.packages("dplyr")
install.packages("tidyr")
library(dplyr)
library(tidyr)


## Reading Data
### "DB_PMP_FINAL.csv" Production Data survey colected
### "hhsize.csv"  household size data (number of persons by family)
RawData <-read.table(file.choose("DB_PMP_FINAL.csv"), header = TRUE, sep =";")
hhsize <- read.table(file.choose("hhsize.csv"), header = TRUE, sep =";")


# See some Data features
str(RawData)
summary(RawData)
tbl_df(RawData)
glimpse(RawData)


## Adding variables for Cluster Analysis
### Add Tot_Lab "Total Labor (working days per ha)"
RawData <- RawData %>%
        mutate(Tot_Lab = (jh_pt_f + jh_pt_c + jh_s_f + jh_s_c + jh_apf_f + jh_apf_c + jh_ahp_f + jh_ahp_c +
                       jh_r_f + jh_r_c + jh_c_f + jh_c_c)/x0)


### Add Hired_Lab "Hired Labor (working days per ha)"
RawData <- RawData %>%
        mutate(Hrd_Lab = (jh_pt_c +  jh_s_c +  jh_apf_c + jh_ahp_c + jh_r_c + jh_c_c)/x0)


### Add Hrd_ratio "Hired ratio (working days per ha) -> Hired labour/Total Labour"
RawData <- RawData %>%
        mutate(Hrd_ratio = Hrd_Lab / Tot_Lab )


## Extracting Only Necessary Data for Cluster Analysis
ClusD <- RawData %>%
        select(folio, comuna, cultivo, yield, cons, p0, x0, income, ctototal, Tot_Lab, Hrd_ratio)

ClusD[is.na(ClusD)] <- 0

#Consumption as the share of the crop consumed by their prices over the total income
ClusD <- ClusD %>%
        mutate(Cons_shr = (cons*p0) / income)

Cons_analysis <- ClusD %>%
        group_by(folio) %>%
        summarise(cons= sum(cons*p0)/sum(income))


### Adding new variables:
### Household size  and 
### commune code (Pencahue = 1; Cauquenes = 2; San Clemente = 3; Parral = 4)
ClusD2 <- left_join(ClusD, hhsize, by = "folio")

# Consumption data of survey used as regional average of consumption, which was used in GAMS
#ClusD2[is.na(ClusD2)] <- 0

###################################################
Consumption <- ClusD2 %>%
        group_by(cultivo) %>%
        summarise(Avg_cons=mean(cons))

####################################################

## Grouping Data by folio and computing new values
ClusD3 <- ClusD2 %>%
        group_by(folio) %>%
        summarise(Farm_size= sum(x0), commune=mean(commune), wmean_Inc = round(weighted.mean(income, x0),2), wmean_Cst = round(weighted.mean(ctototal, x0),2), wmean_TotLab = round(weighted.mean(Tot_Lab, x0),2), wmean_HrdR = round(weighted.mean(Hrd_ratio, x0),2), hhsize = mean(hhsize))

## Changing NA values by 0
ClusD3[is.na(ClusD3)] <- 0

### Adding consumption variable:
ClusD4 <- left_join(ClusD3, Cons_analysis, by = "folio")

### Crops share Data
Crops <- spread(ClusD2, cultivo, x0)
tbl_df(Crops)
Crops[is.na(Crops)] <- 0
tbl_df(Crops)

Crops2 <- Crops %>%
        select(folio, ARROZ, ARVEJA, AVENA, CEBOLLA, GARBANZO, LENTEJA, MAIZ, MAIZ_SEM, MARAVILLA, MELON, MELON_SEM, PAPA, PEPINO_SEM, POROTO, POROTO_VER, REMOLACHA, REPOLLO, REPOLLO_SEM, SANDIA, SANDIA_SEM, SOYA, TABACO, TOMATE, TRIGO, ZAPALLO)

tbl_df(Crops2)


Crops3 <- Crops2 %>%
        group_by(folio) %>%
        summarise(ARROZ = sum(ARROZ), ARVEJA = sum(ARVEJA), AVENA = sum(AVENA), CEBOLLA = sum(CEBOLLA), GARBANZO = sum(GARBANZO), LENTEJA = sum(LENTEJA), MAIZ = sum(MAIZ), MAIZ_SEM = sum(MAIZ_SEM), MARAVILLA = sum(MARAVILLA), MELON = sum(MELON),MELON_SEM = sum(MELON_SEM), PAPA = sum(PAPA), PEPINO_SEM = sum(PEPINO_SEM), POROTO = sum(POROTO), POROTO_VER = sum(POROTO_VER), REMOLACHA = sum(REMOLACHA), REPOLLO = sum(REPOLLO),REPOLLO_SEM = sum(REPOLLO_SEM), SANDIA = sum(SANDIA), SANDIA_SEM = sum(SANDIA_SEM), SOYA = sum(SOYA), TABACO = sum(TABACO), TOMATE = sum(TOMATE),TRIGO = sum(TRIGO), ZAPALLO = sum(ZAPALLO))

tbl_df(Crops3)


### Join Cluster Data 4 with Crop's surfaces
ClusD5 <- left_join(ClusD4, Crops3, by = "folio")

View(ClusD5)

### Add Crop groups shares and select final variables for Final DB for cluster Analisys
ClusD6 <- ClusD5 %>%
        mutate(Grn_shr = (TRIGO + AVENA + ARROZ)/ Farm_size) %>%
        mutate(SV_shr =  (ARVEJA + CEBOLLA + TOMATE + MELON + SANDIA + ZAPALLO)/ Farm_size) %>%
        mutate(SC_shr= (MAIZ + POROTO + PAPA + POROTO_VER + GARBANZO)/ Farm_size) %>%
        mutate(OC_shr= (MARAVILLA + TABACO + REPOLLO + REMOLACHA + SOYA)/ Farm_size) %>%
        mutate(SD_shr= (MAIZ_SEM + MELON_SEM + PEPINO_SEM + REPOLLO_SEM + SANDIA_SEM)/ Farm_size) %>%
        select(folio, commune, hhsize, Farm_size, wmean_HrdR, wmean_TotLab, wmean_Inc, wmean_Cst, Grn_shr, SV_shr, SC_shr, OC_shr, SD_shr, cons)


ClusD6 <- ClusD6 %>%
        rename(Hrd_ratio = wmean_HrdR) %>%
        rename(Ttl_Lab = wmean_TotLab) %>%
        rename(Ttl_Rev = wmean_Inc) %>%
        rename(Ttl_Cst = wmean_Cst) %>%
        rename(Consumption = cons)

tbl_df(ClusD6)
glimpse(ClusD6)

### Saving Data for Cluster Analysis
write.csv(ClusD6, file="csv_files/DataForCluster.csv") 
