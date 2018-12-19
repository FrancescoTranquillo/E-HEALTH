install.packages('DataExplorer') 
library(DataExplorer)
library(dlookr)
library(Hmisc)
library(dplyr)


df3<-read.csv2("./Tabelle to be consegnate/Results/Dermatology_top60.csv", header=T, stringsAsFactors=F)

## Rename
names(df3) <- local({
  x <- names(df3)
  m <- match(x = x, table = c("X", "X.1"))
  replace(x, !is.na(m), c("Subcategory Function", "Albrecht.Category")[m[!is.na(m)]])
})

## Set Data Type
df3 <- within(df3, {
  URL <- as.character(URL)
  Name <- as.character(Name)
  ID <- as.integer(ID)
  Category <- as.character(Category)
  `Subcategory Function` <- as.factor(`Subcategory Function`)
  Albrecht.Category <- as.factor(Albrecht.Category)
  Description <- as.character(Description)
  Keywords <- as.character(Keywords)
  Version <- as.character(Version)
  Age.Rating <- as.integer(Age.Rating)
  Language.s. <- as.character(Language.s.)
  Developer.ID <- as.integer(Developer.ID)
  Developer.Name <- as.character(Developer.Name)
  Developer.URL <- as.character(Developer.URL)
  Price <- as.numeric(Price)
  Currency <- as.character(Currency)
  Size <- as.numeric(Size)
  Last.Update.Date <- as.Date(Last.Update.Date)
  Release.Date <- as.Date(Release.Date)
  Average.user.rating <- as.numeric(Average.user.rating)
  Number.of.user.ratings <- as.integer(Number.of.user.ratings)
  X..of.user.ratings.with.5.stars <- as.integer(X..of.user.ratings.with.5.stars)
  X..of.user.ratings.with.4.stars <- as.integer(X..of.user.ratings.with.4.stars)
  X..of.user.ratings.with.3.stars <- as.integer(X..of.user.ratings.with.3.stars)
  X..of.user.ratings.with.2.stars <- as.integer(X..of.user.ratings.with.2.stars)
  X..of.user.ratings.with.1.star <- as.integer(X..of.user.ratings.with.1.star)
  Date <- as.Date(Date)
  NC <- as.integer(NC)
  Across <- as.integer(Across)
  V1 <- as.factor(V1)
  V2 <- as.integer(V2)
  V3 <- as.integer(V3)
  Norma.average.user.rating <- as.numeric(Norma.average.user.rating)
  Norma.number.of.user.ratings <- as.numeric(Norma.number.of.user.ratings)
  Medical_Device <- as.integer(Medical_Device)
  Norma.length.description <- as.numeric(Norma.length.description)
  diff_in_days <- as.numeric(diff_in_days)
  urlcheck <- as.integer(urlcheck)
  Ranking <- as.numeric(Ranking)
})## Aggregate
df3.aggregate <- aggregate(formula = cbind(n = 1:nrow(df3)) ~ Albrecht.Category + `Subcategory Function`, data = df3, FUN = length)

df3$`Subcategory Function` <- factor(df3$`Subcategory Function`,
                                               levels = c(1,2,3,4,5,6),
                                               labels = c("provision of information",
                                                          "data acquisition, processing and evaluation",
                                                          "administrative use",
                                                          "calendar and appointment related apps",
                                                          "support",
                                                          "other"))

library(viridis)
df3a<-count(df3, a=df3$`Subcategory Function`,sort = T)

ggplot(df3a ,aes(x=reorder(a, -n),n))+
         geom_bar(stat="identity",  width=0.7, color="black",fill=viridis(6), size=1)+
  theme_minimal(base_size = 12)+
  coord_flip()+
  geom_text(aes(label=n), hjust=-0.5, color="black", size=5)+
  labs(y = "count", size=2,x = "Subcategory Function")
  labs(x = "Subcategory Function", size=2)
  



