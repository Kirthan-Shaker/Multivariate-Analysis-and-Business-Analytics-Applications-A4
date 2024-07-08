# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Call the function
install_and_load(packages)
setwd('/Users/kirthanshaker/Desktop/SCMA 631 Data Files ')
survey_df<-read.csv('/Users/kirthanshaker/Desktop/SCMA 631 Data Files /Survey.csv',header=TRUE) 
dim(survey_df) 
names(survey_df) 
head(survey_df) 
str(survey_df)


#A)Do principal component analysis and factor analysis and identify the dimensions in the data. 

is.na(survey_df) 
sum(is.na(survey_df)) 
sur_int=survey_df[,20:46] 
str(sur_int) 
dim(sur_int) 
library(GPArotation) 
pca <- principal(sur_int,5,n.obs =162, rotate ="promax") 
pca 

om.h<-omega(sur_int,n.obs=162,sl=FALSE) 
op<-par(mfrow=c(1,1)) 
om<-omega(sur_int,n.obs=162) 
library(FactoMineR) 
pca<-PCA(sur_int,scale.unit = TRUE) 
summary(pca) 
biplot(pca, scale = 0) 
str(sur_int) 
dim(sur_int) 
show(sur_int) 



# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("cluster", "FactoMineR", "factoextra", "pheatmap")

install_and_load(packages)
setwd('/Users/kirthanshaker/Desktop/SCMA 631 Data Files ')
survey_df<-read.csv('/Users/kirthanshaker/Desktop/SCMA 631 Data Files /Survey.csv',header=TRUE) 
sur_int=survey_df[,20:46] 


#B) Carry our cluster analysis and characterize the respondents based on their background variables. 
library(cluster) 
library(factoextra) 
show(sur_int) 
fviz_nbclust(sur_int,kmeans,method = "gap_stat") 
set.seed(123) 
km.res<-kmeans(sur_int,4,nstart = 25) 
fviz_cluster(km.res,data=sur_int,palette="jco", 
             ggtheme = theme_minimal()) 
res.hc <- hclust(dist(sur_int), method = "ward.D2") 
fviz_dend(res.hc,cex=0.5,k=4,palette = "jco") 
library(pheatmap) 
pheatmap(t(sur_int),cutree_cols = 4) 


#C) Do multidimensional scaling and interpret the results. 

icecream_df<-read.csv('E:\\Code\\Notebooks\\Classes\\Icecream.csv',header=TRUE)
dim(icecream_df)

names(icecream_df) 

ice<-subset(icecream_df,select = -c(Brand)) 
distance_matrix<-dist(ice) 

mds_result<-cmdscale(distance_matrix,k=2) 

plot(mds_result[,1],mds_result[,2],pch=16,xlab="Dimension1",ylab="Dimension2",main="MDS plot") 




# Function to auto-install and load packages
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

# List of packages to install and load
packages <- c("dplyr", "psych", "tidyr", "GPArotation", "FactoMineR", "factoextra", "pheatmap")

# Call the function
install_and_load(packages)

survey_df<-read.csv('E:\\Code\\Notebooks\\Classes\\Survey.csv',header=TRUE) 
sur_int=survey_df[,20:46] 

#Factor Analysis 

factor_analysis<-fa(sur_int,nfactors = 4,rotate = "varimax") 
names(factor_analysis) 
print(factor_analysis$loadings,reorder=TRUE) 
fa.diagram(factor_analysis) 
print(factor_analysis$communality) 
print(factor_analysis$scores) 