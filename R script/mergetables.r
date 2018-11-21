
#Lista dei file con estensione ".csv"
filenames <- list.files(path = ".", pattern='^.*\\.csv$')

#Unione dei file tramite il comando rbind e usando lapply per applicare il comando  di bind agli elementi del
#vettore "filenames"
my.df <- do.call("rbind", lapply(filenames, read.csv, header = TRUE, sep=";"))

#Esistono elementi doppi nel dataframe?
anyDuplicated.data.frame(my.df)

#Sembra di no, per sicurezza si eliminano i duplicati
my.new.df <- my.df[!duplicated(paste(my.df$Name, my.df$URL, my.df$ID, my.df$X)),]

#(guardando la struttura dei dataframe si ha la prova che non esistono duplicati perchè in entrambi
#ci sono lo stesso numero di osservazioni, cioè righe)

#ordino il dataframe in ordine alfabetico (i primi elementi sono le app che iniziano per un simbolo)
my.new.df <- my.new.df[order(my.new.df$Name),]

#eliminazione della prima colonna, inutile, generata al momento della costruzione delle due tabelle precedenti
my.new.df <- my.new.df[,-1]

#scrittura del nuovo csv
write.csv2(my.new.df, "app_M_H&F.csv",row.names=FALSE)
