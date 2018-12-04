df <-
  read.csv2(
    "25_FRA.csv",
    stringsAsFactors = F,
    header = T
  )

df <- df[,4]


write.table(
  df,
  "25_FRA.txt",
  append = F,
  dec = ".",
  col.names = F,
  row.names = F,
  #sep = \n",
  eol = "\n\n"
)