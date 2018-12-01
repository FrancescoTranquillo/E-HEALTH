df <-
  read.csv2(
    "Training_metamap.csv",
    stringsAsFactors = F,
    header = T
  )

df <- df[,4]


write.table(
  df,
  "Training_metamap.txt",
  append = F,
  dec = ".",
  col.names = F,
  row.names = F,
  #sep = \n",
  eol = "\n\n"
)

