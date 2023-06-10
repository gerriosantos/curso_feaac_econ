
# Funcao de limpeza das bases
formata_nome <- function(x) {
  x<-tolower(x); x<-gsub("[áàãâ]", "a", x); x<-gsub("[éê]", "e", x); x<-gsub("[í]", "i", x)
  x<-gsub("[õôó]", "o", x); x<-gsub("[ú]", "u", x); x<-gsub("[ç]", "c", x); #x<-gsub("[ /]", "_", x)
  x<-gsub("[%(),]", "", x); x <- gsub('-', '', x); x <- gsub('_', ' ', x);
  x <- gsub("\'", '', x);
  x <- gsub("\`", '', x);
  x <- gsub("\\?", '', x); x <- gsub(":", '', x); x <- gsub("\\...", '', x);
  x <- gsub("[0-9]+", '', x);
  #x <- x<-gsub("\\*", "", x);
  x <- gsub(paste0('\\b', tm::stopwords("portuguese"), '\\b', collapse = '|'), '', x);
  x <- abjutils::rm_accent(x);
  x <- stringr::str_squish(x)
  return(x)
}
