df <-
  read.csv2(
    "Across_Specialties_nostro.csv",
    stringsAsFactors = F,
    header = T
  )

df <- df[,4]


write.table(
  df,
  "Across_Specialties_nostro.txt",
  append = F,
  dec = ".",
  col.names = F,
  row.names = F,
  #sep = \n",
  eol = "\n\n"
)
