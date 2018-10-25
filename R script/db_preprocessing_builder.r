filenames <-
  list.files(path = ".",
             pattern = "_completed.csv$",
             recursive = T)

df.list <- lapply(filenames,
                  read.csv2,
                  header = TRUE,
                  stringsAsFactors = FALSE)


my.df <- bind_rows(df.list)


write.csv2(my.df, "Database_preprocessing.csv", row.names = FALSE)
