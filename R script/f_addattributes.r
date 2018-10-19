



addattributes <- function(url) {
  
  possibleError <- tryCatch(
    read_html(url),
    error = function(e)
      e
  )
  
  if (inherits(possibleError, "error")) {
    attrs <- data.frame(matrix(NA, nrow = 1, ncol = 21)) %>%
      cbind(., url)

  } else {
    page <- possibleError
  
  
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
  }
  
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
  
  
  #Descrizione (STRINGA)####
  
  description <- page %>%
    html_node(".section__description .we-clamp__contents") %>%
    html_text(trim = TRUE)
  
  
  #Data ultimo update e release date (DATA)####
  
  lastupdate <- page %>%
    html_nodes(".version-history__item__release-date") %>%
    html_text(trim = TRUE) %>%
    gsub(",", "", .)
  
  if (length(lastupdate) == "0") {
    lastupdate <- NA
  }
  
  lastupdate <- first(lastupdate)
  releasedate <- last(lastupdate)
  
  #Ultima versione app (STRINGA) ####
  
  version <- page %>%
    html_node(".version-history__item__version-number") %>%         ##non ? un numero ? una stringa, va beneeeeeee??? non
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
  if (length(price) == "0") {
    price <- NA
  }
  
  if (price == "Free") {
    price <- 0
    currency <- NA
  } else {
    currency <- str_extract(price, currencypattern)
    price <- gsub(currencypattern, "", price)
    price <- as.numeric(price)
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
    #anytime::anydate(applastupdatedate),
    "Release Date" = anytime(parse_date_time(releasedate, orders = "b d Y")),
    #anytime::anydate(appreleasedate),
    "Average user ratings-current" = avgrating,
    "Number of user ratings-current" = ratings,
    # "Average user ratings-all" = appallavgr,
    # "Number of user ratings-all" = appallnorating,
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
      "Average user ratings-current",
      "Number of user ratings-current",
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

g <- lapply(list_url, addattributes)

# cl <- makeCluster(detectCores() - 1)
# 
# clusterEvalQ(cl, {
#   library(Rcrawler)
#   library(plyr)
#   library(dplyr)
#   library(tidyverse)
#   library(rvest)
#   library(stringr)
#   library(rebus)
#   library(urltools)
#   library(RCurl)
#   library(gdata)
#   library(anytime)
#   library(dplyr)
#   library(lubridate)
#   library(progress)
#   library(httr)
#   library(jsonlite)
#   library(purrrlyr)
#   library(parallel)
#   library(microbenchmark)
#   library(tictoc)
# })
# 
# clusterExport(cl, "addattributes")
# 
# g <- parLapply(cl, list_url, addattributes)