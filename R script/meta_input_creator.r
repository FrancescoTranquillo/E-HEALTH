df <-
  read.csv2(
    "./Tabelle to be consegnate/75_NC_across.csv",
    stringsAsFactors = F,
    header = T
  )

df_across_0<- subset(df, df$across_predicted==0)

df_across_0_descriptions <- df_across_0[,4]


write.table(
  df_across_0_descriptions,
  "./Tabelle to be consegnate/metamap_75.txt",
  append = F,
  dec = ".",
  col.names = F,
  row.names = F,
  #sep = \n",
  eol = "\n\n"
)
