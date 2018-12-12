df <-
  read.csv2(
    "database_preprocessed_english_nc_1_across.csv",
    stringsAsFactors = F,
    header = T
  )

df_across_0<- subset(df, df$Across==0)

df_across_0_descriptions <- df_across_0[,5]


write.table(
  df_across_0_descriptions,
  "database_preprocessed_english_nc_1_across_0.txt",
  append = F,
  dec = ".",
  col.names = F,
  row.names = F,
  #sep = \n",
  eol = "\n\n"
)
