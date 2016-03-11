#Cluster Features

### Read Data
FinalCluster<-read.table(file.choose("Final_cluster.csv"), header = TRUE, sep =",")

Clusters <- FinalCluster %>%
        group_by(typo) %>%
        summarise(com_pres = n(),mhh_size = mean(hhsize), sdhh_size = sd(hhsize), min_hhsize = min(hhsize),max_hhsize = max(hhsize),
                  mFS = mean(Farm_size), sdFS = sd(Farm_size), min_FS = min(Farm_size),max_FS = max(Farm_size),
                  mHR = mean(Hrd_ratio), sdHR = sd(Hrd_ratio), min_HR = min(Hrd_ratio),max_HR = max(Hrd_ratio),
                  mTR = mean(Ttl_Rev), sdTR = sd(Ttl_Rev), min_TR = min(Ttl_Rev),max_TR = max(Ttl_Rev),
                  mGrnSh = mean(Grn_shr), sdGrnSh = sd(Grn_shr), min_GrnSh = min(Grn_shr),max_GrnSh = max(Grn_shr),
                  mSpVSh = mean(SV_shr), sdSpVSh = sd(SV_shr), min_SpVSh = min(SV_shr),max_SpVSh = max(SV_shr),
                  mSpCSh = mean(SC_shr), sdSpCSh = sd(SC_shr), min_SpCSh = min(SC_shr),max_SpCSh = max(SC_shr),
                  mOCSh = mean(OC_shr), sdOCSh = sd(OC_shr), min_OCSh = min(OC_shr),max_OCSh = max(OC_shr),
                  mSDSh = mean(SD_shr), sdSDSh = sd(SD_shr), min_SDSh = min(SD_shr),max_SDSh = max(SD_shr))
        

Cluster 

Cluster1 <- FinalCluster %>%
        filter(typo == 1)

summary(Cluster1)
str(Cluster1)
