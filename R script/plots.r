install.packages('DataExplorer') 
library(DataExplorer)
library(dlookr)
library(Hmisc)
library(dplyr)
path="./Tabelle to be consegnate/Results/"

derma<-read.csv2( file = paste0(path, "db_tot.csv"),header = T,stringsAsFactors = F)
db_tot<-read.csv2( file = paste0(path, "db_totale.csv"),header = T,stringsAsFactors = F)
db_tot$V1<-factor(db_tot$V1)
plot_str(derma)

plot_missing(db_tot)
plot_histogram(db_tot$V1 ,ncol = 5, nrow = 5,ggtheme = theme_minimal(),geom_histogram_args  = list(aes(fill=db_tot$Category),alpha=0.4))
plot_histogram(db_tot$V1)
plot_density(db_tot$V1,ncol = 5, nrow = 5,ggtheme = theme_minimal())
plot_bar(data = db_tot$V1%>%na.omit(.),ggtheme = theme_minimal(),theme_config = list())

plot_correlation(derma)
library(tidyverse)
plot_density(derma$Ranking,ggtheme = theme_minimal(),geom_density_args = list(aes(fill=derma$Category),alpha=0.4))

plot_boxplot(derma$Ranking, by = 'derma$Category')


describe(derma$Ranking)


