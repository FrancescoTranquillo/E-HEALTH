df <-
  read.csv2(
    "Test_metamap.csv",
    stringsAsFactors = F,
    header = T
  )

df <- df[,4]


write.table(
  df,
  "Test_metamap.txt",
  append = F,
  dec = ".",
  col.names = F,
  row.names = F,
  #sep = \n",
  eol = "\n\n"
)


