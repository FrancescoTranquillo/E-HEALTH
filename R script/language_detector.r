library(rvest)
library(textcat)
url<- "https://itunes.apple.com/us/app/sykepleierappen/id975038867?mt=8"
page<- read_html(url)
appinfo<-page%>%
  rvest::html_nodes(".section__description .we-clamp__contents")%>%
  rvest::html_text()%>%
  strsplit(split = "\n")
appinfo
textcat(appinfo)
