df <-
  read.csv2(
<<<<<<< HEAD
    "25_FRA.csv",
=======
    "Test_metamap.csv",
>>>>>>> parent of 2c00c88... x
    stringsAsFactors = F,
    header = T
  )

df <- df[,4]


write.table(
  df,
<<<<<<< HEAD
  "25_FRA.txt",
=======
  "Test_metamap.txt",
>>>>>>> parent of 2c00c88... x
  append = F,
  dec = ".",
  col.names = F,
  row.names = F,
  #sep = \n",
  eol = "\n\n"
)
