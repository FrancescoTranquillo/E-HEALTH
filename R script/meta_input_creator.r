df <-
  read.csv2(
    "test_set.csv",
    stringsAsFactors = F,
    header = T
  )
df <- df[!apply(is.na(df) | df == "", 1, all),]
df <- df[,4]


write.table(
  df,
  "metanostro.txt",
  append = F,
  dec = ".",
  col.names = F,
  row.names = F,
  #sep = \n",
  eol = "\n\n"
)