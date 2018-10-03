url<-"https://itunes.apple.com/us/app/sprout-period-fertility-ovulation-tracker/id1003306557?mt=8"

page<-read_html(url)


##prova pegi
pegiiii<-page%>%
  html_nodes(".badge--product-title")%>%
  html_text(trim = TRUE)    

df<-read.csv2("app_M_H&F_ENGLISH_ONLY.csv" )
head(df)


str(df)
