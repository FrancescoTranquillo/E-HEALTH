df <-
  read.csv2(
    "~/Documents/GitHub/E-HEALTH/R script/25_FRA.csv",
    stringsAsFactors = F,
    header = T
  )

df <- df[,c(1,4)]


write.table(
  df,
  "~/Documents/GitHub/E-HEALTH/R script/25_FRA.txt",
  append = F,
  dec = ".",
  col.names = F,
  row.names = F,
  sep = "\n---\n"
)
