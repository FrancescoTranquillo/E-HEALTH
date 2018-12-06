dfA<-read.csv2(
  "Preferred_Across_classe.csv"
)

setwd("~/GitHub/E-HEALTH/R script")
mesh <- read.csv2("mesh.csv", header = T, stringsAsFactors = F)
names(mesh)[1] <- "specialty"
mesh <- mesh[, 1:2]
mesh <- mesh[!duplicated(mesh),]
mesh <- mesh[!apply(is.na(mesh) | mesh == "", 1, all),]

dffine<-setdiff(dfA$Candidate_Preferred,mesh$terms)


mesh_across<-tibble("specialty"="Across", "terms"=dffine)%>%
  .[4:239,]

mesh<-rbind(mesh,mesh_across)

write.csv2(mesh, "mesh.csv",row.names = F)
