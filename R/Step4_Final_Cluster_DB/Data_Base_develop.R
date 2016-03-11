# Data frame folio and Typo
DF_FT  <- FinalCluster %>%
        select(folio, typo)

### Add Family_Lab "Family Labor (working days per ha)"
RawData <- RawData %>%
        mutate(Fam_Lab = (jh_pt_f +  jh_s_f +  jh_apf_f + jh_ahp_f + jh_r_f + jh_c_f)/x0)

# Data frame Labour
DF_Lab <- RawData %>%
        select(folio, Fam_Lab, Hrd_Lab)

# Add "typo" variable to RawData
DF_FstClus  <- left_join(RawData, DF_FT, by = "folio")

#Select variables for Data Base
DF_FstClus2 <- DF_FstClus %>%
        select(typo, comuna, cultivo, sistema, x0, yield, p0, jh_c, jh, ctomaq, ctoinsumosh, ctototal)

#Add Labour Data to Cluster Data Frame
DF_FstClus3 <- bind_cols(DF_FstClus2, DF_Lab)
DF_FstClus3  <- DF_FstClus3 %>%
        select(typo, comuna, cultivo, sistema, x0, yield, p0, Fam_Lab, Hrd_Lab, jh_c, jh, ctomaq, ctoinsumosh, ctototal)

# Adding Labour cost ($CLP/ha)
DF_FstClus4  <- DF_FstClus3 %>%
        mutate(HrdLabCst = Hrd_Lab*jh_c, FamLabCst = Fam_Lab*jh) %>%
        mutate(TtlCst= HrdLabCst + FamLabCst + ctomaq + ctoinsumosh)

######################################--Household Type 1--#############################

Household1 <- DF_FstClus4 %>%
        filter(typo == 1)

Household1<-na.omit(Household1)

Household1 <- Household1 %>%
        group_by(comuna, cultivo, sistema) %>%
        summarise(area = sum(x0), yield = mean(yield), CropPrice = mean(p0), HiredLabor = mean(Hrd_Lab),
                  FamilyLab = mean(Fam_Lab), PriceHrdLab = mean(jh_c), PriceFamLab = mean(jh),
                  rentedMachCosts = mean(ctomaq), InputCosts = mean(ctoinsumosh))

CrpsHT1 <- Household1 %>%
        group_by(cultivo) %>%
        summarise(m_Area = sum(area)) %>%
        mutate(Area_shr = m_Area/sum(m_Area))

SysHT1  <- Household1 %>%
        group_by(tipo_cultivo) %>%
        summarise(m_Sys = sum(area) %>%
        mutate(Sys_shr = m_Sys/sum(m_Sys))

RevHT1  <- Household1 %>%
        group_by(cultivo) %>%
        summarise(m_Rev = sum(area * yield * CropPrice)) %>%
        mutate(Rev_shr = m_Rev/sum(m_Rev))

Household1$HT  <- "H1"

######################################--Household Type 2--#############################

Household2 <- DF_FstClus4 %>%
        filter(typo == 2)

Household2<-na.omit(Household2)

Household2 <- Household2 %>%
        group_by(comuna, cultivo, sistema) %>%
        summarise(area = sum(x0), yield = mean(yield), CropPrice = mean(p0), HiredLabor = mean(Hrd_Lab),
                  FamilyLab = mean(Fam_Lab), PriceHrdLab = mean(jh_c), PriceFamLab = mean(jh),
                  rentedMachCosts = mean(ctomaq), InputCosts = mean(ctoinsumosh))

CrpsHT2 <- Household2 %>%
        group_by(cultivo) %>%
        summarise(m_Area = sum(area)) %>%
        mutate(Area_shr = m_Area/sum(m_Area))

SysHT2  <- Household2 %>%
        group_by(tipo_cultivo) %>%
        summarise(m_Sys = sum(area) %>%
        mutate(Sys_shr = m_Sys/sum(m_Sys))
                  
RevHT2  <- Household2 %>%
        group_by(cultivo) %>%
        summarise(m_Rev = sum(area * yield * CropPrice)) %>%
        mutate(Rev_shr = m_Rev/sum(m_Rev))

Household2$HT  <- "H2"


######################################--Household Type 3--#############################

Household3 <- DF_FstClus4 %>%
        filter(typo == 3)

Household3<-na.omit(Household3)

Household3 <- Household3 %>%
        group_by(comuna, cultivo, sistema) %>%
        summarise(area = sum(x0), yield = mean(yield), CropPrice = mean(p0), HiredLabor = mean(Hrd_Lab),
                  FamilyLab = mean(Fam_Lab), PriceHrdLab = mean(jh_c), PriceFamLab = mean(jh),
                  rentedMachCosts = mean(ctomaq), InputCosts = mean(ctoinsumosh))

CrpsHT3 <- Household3 %>%
        group_by(cultivo) %>%
        summarise(m_Area = sum(area)) %>%
        mutate(Area_shr = m_Area/sum(m_Area))

SysHT3  <- Household3 %>%
        group_by(tipo_cultivo) %>%
        summarise(m_Sys = sum(area) %>%
        mutate(Sys_shr = m_Sys/sum(m_Sys))
                  
RevHT3  <- Household3 %>%
        group_by(cultivo) %>%
        summarise(m_Rev = sum(area * yield * CropPrice)) %>%
        mutate(Rev_shr = m_Rev/sum(m_Rev))
                  

Household3$HT  <- "H3"

######################################--Household Type 4--#############################

Household4 <- DF_FstClus4 %>%
        filter(typo == 4)

Household4<-na.omit(Household4)

Household4 <- Household4 %>%
        group_by(comuna, cultivo, sistema) %>%
        summarise(area = sum(x0), yield = mean(yield), CropPrice = mean(p0), HiredLabor = mean(Hrd_Lab),
                  FamilyLab = mean(Fam_Lab), PriceHrdLab = mean(jh_c), PriceFamLab = mean(jh),
                  rentedMachCosts = mean(ctomaq), InputCosts = mean(ctoinsumosh))

CrpsHT4 <- Household4 %>%
        group_by(cultivo) %>%
        summarise(m_Area = sum(area)) %>%
        mutate(Area_shr = m_Area/sum(m_Area))

SysHT4  <- Household4 %>%
        group_by(tipo_cultivo) %>%
        summarise(m_Sys = sum(area) %>%
        mutate(Sys_shr = m_Sys/sum(m_Sys))
                  
RevHT4  <- Household4 %>%
        group_by(cultivo) %>%
        summarise(m_Rev = sum(area * yield * CropPrice)) %>%
        mutate(Rev_shr = m_Rev/sum(m_Rev))

Household4$HT  <- "H4"

######################################--Household Type 4--#############################

Household5 <- DF_FstClus4 %>%
        filter(typo == 5)

Household5<-na.omit(Household5)

Household5 <- Household5 %>%
        group_by(comuna, cultivo, sistema) %>%
        summarise(area = sum(x0), yield = mean(yield), CropPrice = mean(p0), HiredLabor = mean(Hrd_Lab),
                  FamilyLab = mean(Fam_Lab), PriceHrdLab = mean(jh_c), PriceFamLab = mean(jh),
                  rentedMachCosts = mean(ctomaq), InputCosts = mean(ctoinsumosh))

CrpsHT5 <- Household5 %>%
        group_by(cultivo) %>%
        summarise(m_Area = sum(area)) %>%
        mutate(Area_shr = m_Area/sum(m_Area))

SysHT5  <- Household5 %>%
        group_by(tipo_cultivo) %>%
        summarise(m_Sys = sum(area) %>%
                          mutate(Sys_shr = m_Sys/sum(m_Sys))
                  
RevHT5  <- Household5 %>%
        group_by(cultivo) %>%
        summarise(m_Rev = sum(area * yield * CropPrice)) %>%
        mutate(Rev_shr = m_Rev/sum(m_Rev))
                  
Household5$HT  <- "H5"
                  





################ Binding Households Data frames --> Creating Final DB for household model

FinalDB_0803 <- bind_rows(Household1, Household2, Household3, Household4, Household5)
FinalDB_0803 <- FinalDB_0803[,c(13,1:12)]
FinalDB_0803 <- FinalDB_0803 %>%
        rename(commune=comuna, crop=cultivo, system=sistema)


### Final Details
## Codes for communes
FinalDB_0803$commune <- as.character(FinalDB_0803$commune)
FinalDB_0803$commune[FinalDB_0803$commune == "Pencahue"] <- "PEN"
FinalDB_0803$commune[FinalDB_0803$commune == "Cauquenes"] <- "CAU"
FinalDB_0803$commune[FinalDB_0803$commune == "San Clemente"] <- "SC"
FinalDB_0803$commune[FinalDB_0803$commune == "Parral"] <- "PAR"

## Codes for systems
FinalDB_0803$system <- as.character(FinalDB_0803$system)
FinalDB_0803$system[FinalDB_0803$system == "riego"] <- "irr"
FinalDB_0803$system[FinalDB_0803$system == "secano"] <- "dry"


## Codes for Crops
FinalDB_0803$crop <- as.character(FinalDB_0803$crop)
FinalDB_0803$crop[FinalDB_0803$crop == "ARROZ"] <- "ric"
FinalDB_0803$crop[FinalDB_0803$crop == "ARVEJA"] <- "pea"
FinalDB_0803$crop[FinalDB_0803$crop == "AVENA"] <- "oat"
FinalDB_0803$crop[FinalDB_0803$crop == "CEBOLLA"] <- "oni"
FinalDB_0803$crop[FinalDB_0803$crop == "GARBANZO"] <- "chk"
FinalDB_0803$crop[FinalDB_0803$crop == "LENTEJA"] <- "len"
FinalDB_0803$crop[FinalDB_0803$crop == "MAIZ"] <- "mze"
FinalDB_0803$crop[FinalDB_0803$crop == "MAIZ_SEM"] <- "smze"
FinalDB_0803$crop[FinalDB_0803$crop == "MARAVILLA"] <- "snf"
FinalDB_0803$crop[FinalDB_0803$crop == "MELON"] <- "mel"
FinalDB_0803$crop[FinalDB_0803$crop == "MELON_SEM"] <- "smel"
FinalDB_0803$crop[FinalDB_0803$crop == "PAPA"] <- "pot"
FinalDB_0803$crop[FinalDB_0803$crop == "PEPINO_SEM"] <- "scuc"
FinalDB_0803$crop[FinalDB_0803$crop == "POROTO"] <- "cmb"
FinalDB_0803$crop[FinalDB_0803$crop == "POROTO_VER"] <- "gbn"
FinalDB_0803$crop[FinalDB_0803$crop == "REMOLACHA"] <- "sgb"
FinalDB_0803$crop[FinalDB_0803$crop == "REPOLLO"] <- "cbg"
FinalDB_0803$crop[FinalDB_0803$crop == "REPOLLO_SEM"] <- "scbg"
FinalDB_0803$crop[FinalDB_0803$crop == "SANDIA"] <- "wtm"
FinalDB_0803$crop[FinalDB_0803$crop == "SANDIA_SEM"] <- "swtm"
FinalDB_0803$crop[FinalDB_0803$crop == "SOYA"] <- "soy"
FinalDB_0803$crop[FinalDB_0803$crop == "TABACO"] <- "tob"
FinalDB_0803$crop[FinalDB_0803$crop == "TOMATE"] <- "tom"
FinalDB_0803$crop[FinalDB_0803$crop == "TRIGO"] <- "wht"
FinalDB_0803$crop[FinalDB_0803$crop == "ZAPALLO"] <- "sqh"

## Rounding values
FinalDB_0803$yield <-round(FinalDB_0803$yield,2) 
FinalDB_0803$CropPrice <-round(FinalDB_0803$CropPrice)
FinalDB_0803$HiredLabor <- round(FinalDB_0803$HiredLabor,2)
FinalDB_0803$FamilyLab <- round(FinalDB_0803$FamilyLab,2)
FinalDB_0803$PriceHrdLab <- round(FinalDB_0803$PriceHrdLab)
FinalDB_0803$PriceFamLab <- round(FinalDB_0803$PriceFamLab)
FinalDB_0803$rentedMachCosts <- round(FinalDB_0803$rentedMachCosts)
FinalDB_0803$InputCosts <- round(FinalDB_0803$InputCosts)



#Saving FinalDB as csv
write.csv(FinalDB_0803, file="csv_files/FinalDB_0803.csv") 
