addattributes <- function(url) {
  print(url)
  Sys.sleep(delay)
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e)
      e
  )
  
  if (inherits(possibleError, "error")) {
    attrs <- data.frame(
      "Description" = NA,
      "Keywords" = NA,
      "Version" = NA,
      "Age rating" = NA,
      "Language(s)" = NA,
      "Developer ID" = NA,
      "Developer Name" = NA,
      "Developer URL" = NA,
      "Price" = NA,
      "Currency" = NA,
      "Size" = NA,
      "Last update date" =  NA,
      "Release Date" = NA,
      "Average user rating" = NA,
      "Number of user ratings" = NA,
      "% of user ratings with 5 stars" = NA,
      "% of user ratings with 4 stars" =  NA,
      "% of user ratings with 3 stars" =  NA,
      "% of user ratings with 2 stars" = NA,
      "% of user ratings with 1 star" =  NA,
      "Date" = anydate(Sys.Date()),
      "URL" = url,
      stringsAsFactors = FALSE
    )
    names(attrs) <-
      c(
        "Description",
        "Keywords" ,
        "Version",
        "Age Rating",
        "Language(s)",
        "Developer ID",
        "Developer Name",
        "Developer URL",
        "Price",
        "Currency",
        "Size",
        "Last Update Date",
        "Release Date",
        "Average user rating",
        "Number of user ratings",
        "% of user ratings with 5 stars",
        "% of user ratings with 4 stars",
        "% of user ratings with 3 stars",
        "% of user ratings with 2 stars",
        "% of user ratings with 1 star",
        "Date",
        "URL"
      )
    
    
  } else {
    page <- possibleError
    
    
    #Descrizione
    
    description <- page %>%
      html_node(".section__description .we-clamp__contents") %>%
      html_text(trim = TRUE)
    
    if (is.na(description)) {
      attrs <- data.frame(
        "Description" = "x",
        "Keywords" = "x",
        "Version" = "x",
        "Age rating" = "x",
        "Language(s)" = "x",
        "Developer ID" = "x",
        "Developer Name" = "x",
        "Developer URL" = "x",
        "Price" = "x",
        "Currency" = "x",
        "Size" = "x",
        "Last update date" =  "x",
        "Release Date" = "x",
        "Average user rating" = "x",
        "Number of user ratings" = "x",
        "% of user ratings with 5 stars" = "x",
        "% of user ratings with 4 stars" =  "x",
        "% of user ratings with 3 stars" =  "x",
        "% of user ratings with 2 stars" = "x",
        "% of user ratings with 1 star" =  "x",
        "Date" = anydate(Sys.Date()),
        "URL" = url,
        stringsAsFactors = FALSE
      )
      names(attrs) <-
        c(
          "Description",
          "Keywords" ,
          "Version",
          "Age Rating",
          "Language(s)",
          "Developer ID",
          "Developer Name",
          "Developer URL",
          "Price",
          "Currency",
          "Size",
          "Last Update Date",
          "Release Date",
          "Average user rating",
          "Number of user ratings",
          "% of user ratings with 5 stars",
          "% of user ratings with 4 stars",
          "% of user ratings with 3 stars",
          "% of user ratings with 2 stars",
          "% of user ratings with 1 star",
          "Date",
          "URL"
        )
      
    } else {
      ## Keywords
      keywords <- page %>%
        html_nodes('meta') %>%
        html_attr('content') %>%
        .[8]
      
      ## Lingue ####
      lang <- page %>%
        html_nodes(
          ".l-row:nth-child(8) .large-6 , .l-row:nth-child(7) .large-6 , .l-row:nth-child(6) .large-6 , .large-6 .we-clamp__contents , .l-row:nth-child(2) .large-6 , .large-6 .link , .information-list__item:nth-child(1) .large-6"
        ) %>%
        html_text(trim = TRUE) %>%
        .[5]
      
      
      ## Size ####
      patternnum <- "\\sGB|KB|MB"
      size <- page %>%
        html_nodes(
          ".l-row:nth-child(8) .large-6 , .l-row:nth-child(7) .large-6 , .l-row:nth-child(6) .large-6 , .large-6 .we-clamp__contents , .l-row:nth-child(2) .large-6 , .large-6 .link , .information-list__item:nth-child(1) .large-6"
        ) %>%
      html_text(trim = TRUE) %>%
      .[2]
      
      num <- gsub(pattern = patternnum, "", size)
      num <- as.double(num)
      if (grepl("GB", size) == TRUE) {
        size <- num * 1024
      } else if (grepl("KB|kB", size) == TRUE) {
        size <- num / 1024
      } else if (grepl("MB|mB", size) == TRUE) {
        size <- num
      } else {
        size<-NA
      }
      size <- signif(size, digits = 3)
      ## Ratings per star (INTEGER)####
      ratingsxstar <- page %>%
        html_nodes(".we-star-bar-graph .we-star-bar-graph__bar__foreground-bar") %>%
        html_attr("style") %>%
        str_extract_all(., "\\d+") %>%
        as.numeric(.)
      
      if (length(ratingsxstar) == "0") {
        ratingsxstar <- NA
      }
      
      onestar <- last(ratingsxstar)
      twostar <- ratingsxstar[2]
      threestar <- ratingsxstar[3]
      fourstar <- ratingsxstar[4]
      fivestar <- first(ratingsxstar)
      
      
      
      ##Url dev ####
      urldev <- page %>%
        html_node(".app-header__identity") %>%
        html_nodes(".link") %>%
        html_attr("href")
      
      
      
      ##ID dev ####
      IDs1 <- gsub("(?<=)(.*)(id)", "", urldev, perl = TRUE)
      IDs2 <- gsub("(?<=[id])(\\d+)", "", IDs1, perl = TRUE)
      iddev <- gsub("\\?mt=8", "", IDs2)
      
      
      
      
      
      
      # #Punteggio medio (DOUBLE)####
      avgrating <- page %>%
        html_nodes(".we-customer-ratings__averages__display") %>%
        html_text(trim = TRUE) %>%
        as.numeric(.)
      if (length(avgrating) == "0") {
        avgrating <- NA
      }
      
      #Punteggio totale (DOUBLE)####
      ratings <- page %>%
        html_node(".we-customer-ratings__count") %>%
        html_text(trim = TRUE) %>%
        gsub(" Ratings", "", .)
      ratings <- as.numeric(sub("K", "e3", ratings))
      if (length(ratings) == "0") {
        ratings <- NA
      }
      
      #PEGI (DOUBLE)####
      
      pegipattern <- "\\d+"
      
      pegi <- page %>%
        html_node(".l-row:nth-child(6) .large-6") %>%
        html_text(trim = TRUE) %>%
        str_extract(., pattern = pegipattern) %>%
        as.numeric()
      
      
      
      
      #Data ultimo update e release date (DATA)####
      
      lastupdatedates <- page %>%
        html_nodes(".version-history__item__release-date") %>%
        html_text(trim = TRUE) %>%
        gsub(",", "", .)
      
      if (length(lastupdatedates) == "0") {
        lastupdatedates <- NA
      }
      
      lastupdate <- first(lastupdatedates)
      releasedate <- last(lastupdatedates)
      
      #Ultima versione app (STRINGA) ####
      
      version <- page %>%
        html_node(".version-history__item__version-number") %>%
        html_text()
      
      
      #Developer (STRINGA)####
      
      devname <- page %>%
        html_node(".information-list__item:nth-child(1) .large-6") %>%
        html_text(trim = TRUE)
      
      
      #Prezzo (DOUBLE) e valuta (STRINGA) ####
      
      currencypattern <- "^\\W"
      price <- page %>%
        html_node(".inline-list__item--bulleted:nth-child(1)") %>%
        html_text(trim = TRUE)
      
      if(is.na(price)==T){
        price<-NA
        currency<-NA
      } else{
      
      if (price == "Free") {
        price <- 0
        currency <- NA
      } else {
        currency <- str_extract(price, currencypattern)
        price <- gsub(currencypattern, "", price)
        price <- as.numeric(price)
      }
      }
      
      attrs <- data.frame(
        "Description" = description,
        "Keywords" = keywords,
        "Version" = version,
        "Age rating" = pegi,
        "Language(s)" = lang,
        "Developer ID" = iddev,
        "Developer Name" = devname,
        "Developer URL" = urldev,
        "Price" = price,
        "Currency" = currency,
        "Size" = size,
        "Last update date" =  anytime(parse_date_time(lastupdate, orders = "b d Y")),
        "Release Date" = anytime(parse_date_time(releasedate, orders = "b d Y")),
        "Average user rating" = avgrating,
        "Number of user ratings" = ratings,
        "% of user ratings with 5 stars" = fivestar,
        "% of user ratings with 4 stars" = fourstar,
        "% of user ratings with 3 stars" = threestar,
        "% of user ratings with 2 stars" = twostar,
        "% of user ratings with 1 star" = onestar,
        "Date" = anydate(Sys.Date()),
        "URL" = url,
        stringsAsFactors = FALSE
      )
    }
    names(attrs) <-
      c(
        "Description",
        "Keywords" ,
        "Version",
        "Age Rating",
        "Language(s)",
        "Developer ID",
        "Developer Name",
        "Developer URL",
        "Price",
        "Currency",
        "Size",
        "Last Update Date",
        "Release Date",
        "Average user rating",
        "Number of user ratings",
        "% of user ratings with 5 stars",
        "% of user ratings with 4 stars",
        "% of user ratings with 3 stars",
        "% of user ratings with 2 stars",
        "% of user ratings with 1 star",
        "Date",
        "URL"
      )
    
    
    return(attrs)
  }
  
  return(attrs)
}
tictoc::tic()
g <- pblapply(list_url, addattributes)
tictoc::toc()
