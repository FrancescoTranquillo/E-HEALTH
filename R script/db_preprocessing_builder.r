filenames <-
  list.files(path = ".",
             pattern = "_completed.csv$",
             recursive = T)

my.df <-
  do.call("rbind",
          lapply(
            filenames,
            read.csv2,
            header = TRUE,
            stringsAsFactors = FALSE
          ))

write.csv2(my.df, "Database_preprocessing.csv", row.names = FALSE)
