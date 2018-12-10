df <-
  read.csv2(
    "database_preprocessed_english_nc_1.csv",
    stringsAsFactors = F,
    header = T
  )

df <- df[,5]


write.table(
  df,
  "database_preprocessed_english_nc_1.csv.txt",
  append = F,
  dec = ".",
  col.names = F,
  row.names = F,
  #sep = \n",
  eol = "\n\n"
)
