### Read Data
All.Data<-read.table(file.choose("DataForCluster.csv"), header = TRUE, sep =",")

#Checking Data
AllData<-na.omit(All.Data)

## Matrix of x-y plots for the variable
pairs(AllData[,-c(1,2)], panel=panel.smooth)

##Distribution of the variables
par(mfrow=c(3,4))
h1<-hist(AllData$Farm_size, main="Farm size")
h2<-hist(AllData$hhsize, main="Household size")
h3<-hist(AllData$Hrd_ratio, main="Hired ratio")
h4<-hist(AllData$Ttl_Lab, main="Total Labour")
h5<-hist(AllData$Ttl_Rev, main="Total Revenue")
h6<-hist(AllData$Ttl_Cst, main="Total Cost")
h7<-hist(AllData$Grn_shr, main="Share of grain crops")
h8<-hist(AllData$SV_shr, main="Share of spring vegetables")
h9<-hist(AllData$SC_shr, main="Share of spring crops")
h10<-hist(AllData$OC_shr, main="Share of other crops")
h11<-hist(AllData$SD_shr, main="Share of seed crops")
h12<-hist(AllData$Consumption, main="Consumption")


## Study of outliers with more details
par(mfrow=c(3,4))
boxplot(AllData$Farm_size, main="Farm size")
boxplot(AllData$Ttl_Lab, main="Tot_Lab")
boxplot(AllData$Ttl_Rev, main="Tot_Rev")
boxplot(AllData$Ttl_Cst, main="Tot_Cst")
boxplot(AllData$Grn_shr, main="Shr_grn")
boxplot(AllData$SV_shr, main="Shr_sprVeg")
boxplot(AllData$SC_shr, main="Shr_sprCrs")
boxplot(AllData$OC_shr, main="Shr_other")
boxplot(AllData$SD_shr, main="Shr_seeds")
boxplot(AllData$Consumption, main="Consum")

#Removing outliers
AllData2<- AllData[AllData$Farm_size < 550,]
AllData2<- AllData2[AllData2$Ttl_Lab < 300,]
AllData2<- AllData2[AllData2$Consumption <= 1,]

par(mfrow=c(3,4))
boxplot(AllData2$Farm_size, main="Farm size")
boxplot(AllData2$Ttl_Lab, main="Tot_Lab")
boxplot(AllData2$Ttl_Rev, main="Tot_Rev")
boxplot(AllData2$Ttl_Cst, main="Tot_Cst")
boxplot(AllData2$Grn_shr, main="Shr_grn")
boxplot(AllData2$SV_shr, main="Shr_sprVeg")
boxplot(AllData2$SC_shr, main="Shr_sprCrs")
boxplot(AllData2$OC_shr, main="Shr_other")
boxplot(AllData2$SD_shr, main="Shr_seeds")
boxplot(AllData2$Consumption, main="Consum")

# Scale
AllData3 <- data.frame(scale(AllData2))


##Distribution of the variables
par(mfrow=c(3,4))
nh1<-hist(AllData3$Farm_size, main="Farm size")
nh2<-hist(AllData3$hhsize, main="Household size")
nh3<-hist(AllData3$Hrd_ratio, main="Hired ratio")
nh4<-hist(AllData3$Ttl_Lab, main="Total Labour")
nh5<-hist(AllData3$Ttl_Rev, main="Total Revenue")
nh6<-hist(AllData3$Ttl_Cst, main="Total Cost")
nh7<-hist(AllData3$Grn_shr, main="Share of grain crops")
nh8<-hist(AllData3$SV_shr, main="Share of spring vegetables")
nh9<-hist(AllData3$SC_shr, main="Share of spring crops")
nh10<-hist(AllData3$OC_shr, main="Share of other crops")
nh11<-hist(AllData3$SD_shr, main="Share of seed crops")
nh12<-hist(AllData3$Consumption, main="Consumption")

 

#PCA on the trasformed data
install.packages("ade4")
library(ade4)

par(mfrow=c(1,1))
DataT <- AllData3[,match(c("Farm_size", "commune", "hhsize", "Hrd_ratio", "Ttl_Lab", "Ttl_Rev", "Ttl_Cst",
                             "Grn_shr", "SV_shr" ,"SC_shr", "OC_shr", "Consumption"), dimnames(AllData3)[[2]])]
Data.pca <- dudi.pca(DataT, center=T, scale=T, scannf=T, nf=5)

Data.pca$eig

cumsum(Data.pca$eig) / sum(Data.pca$eig)

par(mfrow=c(2,2))
s.corcircle(Data.pca$co, xax=1, yax=2, clabel = 1 )
s.corcircle(Data.pca$co, xax=1, yax=3 )
s.corcircle(Data.pca$co, xax=1, yax=4 )
s.corcircle(Data.pca$co, xax=1, yax=5 )

Data.pca$co

#Correlatons
cor.test(AllData3$Farm_size, AllData2$Ttl_Lab, method=c("pearson"))
cor.test(AllData3$Ttl_Rev, AllData2$Ttl_Cst, method=c("pearson"))
cor.test(AllData3$Ttl_Rev, AllData2$Ttl_Lab, method=c("pearson"))

par(mfrow=c(2,2))
s.label(Data.pca$li, xax=1, yax=2)
s.label(Data.pca$li, xax=1, yax=3)
s.label(Data.pca$li, xax=1, yax=4)
s.label(Data.pca$li, xax=1, yax=5)

#To delete the farms from  dataset and selecting variables:

AllData4 <- AllData2 %>%
        filter(Farm_size < 500) 
AllData4<- AllData2[AllData2$Consumption <= 1,]


#Checking Data
AllData4<-na.omit(AllData4)

AllData4 <- select(AllData4, X, folio, commune, hhsize, Farm_size, Hrd_ratio, Ttl_Lab, Ttl_Rev, Grn_shr, SV_shr, SC_shr, Consumption)

# Scale
AllData5 <- data.frame(scale(AllData4))

#To create a new DataT without the variable  and run again the PCA:
par(mfrow=c(1,1))
DataT <- AllData5[,match(c("Farm_size","commune", "hhsize", "Hrd_ratio", "Ttl_Lab," "Ttl_Rev",
                           "Grn_shr", "SV_shr" ,"SC_shr", "Consumption"), dimnames(AllData5)[[2]])]
Data.pca <- dudi.pca(DataT, center=T, scale=T, scannf=T, nf=5)

dev.off()
Data.pca$eig

cumsum(Data.pca$eig) / sum(Data.pca$eig)

par(mfrow=c(2,2))
s.corcircle(Data.pca$co, xax=1, yax=2 )
s.corcircle(Data.pca$co, xax=1, yax=3 )
s.corcircle(Data.pca$co, xax=1, yax=4 )
s.corcircle(Data.pca$co, xax=1, yax=5 )


Data.pca$co


s.label(Data.pca$li, xax=1, yax=2)
s.label(Data.pca$li, xax=1, yax=3)
s.label(Data.pca$li, xax=1, yax=4)
s.label(Data.pca$li, xax=1, yax=5)

AllData6 <- AllData5 %>%
        filter(Farm_size < 5.5) 


################ Final PCA run


#To create a new DataT without the variable  and run again the PCA:

DataT <- DataT %>%
        filter(Farm_size < 5.5) 

par(mfrow=c(1,1))
DataT <- AllData6[,match(c("Farm_size","commune", "hhsize", "Hrd_ratio", "Ttl_Lab," "Ttl_Rev",
                           "Grn_shr", "SV_shr" ,"SC_shr", "Consumption"), dimnames(AllData6)[[2]])]
Data.pca <- dudi.pca(DataT, center=T, scale=T, scannf=T, nf=5)



dev.off()
Data.pca$eig

cumsum(Data.pca$eig) / sum(Data.pca$eig)

par(mfrow=c(2,2))
s.corcircle(Data.pca$co, xax=1, yax=2 )
s.corcircle(Data.pca$co, xax=1, yax=3 )
s.corcircle(Data.pca$co, xax=1, yax=4 )
s.corcircle(Data.pca$co, xax=1, yax=5 )


Data.pca$co


s.label(Data.pca$li, xax=1, yax=2)
s.label(Data.pca$li, xax=1, yax=3)
s.label(Data.pca$li, xax=1, yax=4)
s.label(Data.pca$li, xax=1, yax=5)






scatter(Data.pca)

#3rd and final PCA run --> WITHOUT OC_shr variable:
#par(mfrow=c(1,1))
#DataT <- AllnewData[,match(c("Farm_size", "hhsize", "Hrd_ratio", "Ttl_Lab", "Ttl_Rev",
#                             "Grn_shr", "SV_shr" ,"SC_shr"), dimnames(AllnewData)[[2]])]
#Data.pca <- dudi.pca(DataT, center=T, scale=T, scannf=T, nf=5)
#
#Data.pca$eig

#cumsum(Data.pca$eig) / sum(Data.pca$eig)

#par(mfrow=c(1,2))
#s.corcircle(Data.pca$co, xax=1, yax=2 )
#s.corcircle(Data.pca$co, xax=1, yax=3 )

#Data.pca$co

#s.label(Data.pca$li, xax=1, yax=2)
#s.label(Data.pca$li, xax=1, yax=3)

##cluster analysis
Data.cah <- hclust(dist(Data.pca$li), method="ward.D")

par(mfrow=c(1,2))

barplot(Data.cah$height)
plot(Data.cah)


Data.type <- cutree(Data.cah, k=5)

par(mfrow=c(1,2))
s.corcircle(Data.pca$co, xax=1, yax=2)
s.class(Data.pca$li, fac = as.factor(Data.type))

s.corcircle(Data.pca$co, xax=1, yax=3)
s.class(Data.pca$li, xax=1, yax=3, fac = as.factor(Data.type))

s.corcircle(Data.pca$co, xax=1, yax=4)
s.class(Data.pca$li, xax=1, yax=4, fac = as.factor(Data.type))

FinalData <- AllData2 %>%
        filter(folio != 9) %>% 
        filter(folio != 92) %>%
        filter(folio != 154) 

FinalData$typo <- Data.type

write.csv(FinalData, file="csv_files/Final_cluster.csv") 
