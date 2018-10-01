
  getlanguage<-function(df, df$URL){
    
  df_subset_lang <- df %>% rowwise() %>%     # Evaluate each row (URL) separately
    mutate(df$URL = as.character(URL),    # Convert factors to character for read_html
           languages = possibly(~.x %>% read_html() %>%    # Try to take a URL, read it,
                                  html_nodes(".section__description .we-clamp__contents")%>%
                                  html_text(trim=TRUE)%>%
                                  textcat(),
                                NA)(URL))