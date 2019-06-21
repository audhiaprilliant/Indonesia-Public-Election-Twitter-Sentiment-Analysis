library(tm)
library(xml2)
library(stringr)
library(dplyr)
library(katadasaR)

removeURL <- function(x){
  gsub("http[^[:space:]]*", "", x)
}

removeMention <- function(x){
  gsub("@\\w+", "", x)
}

removeCarriage <- function(x){
  gsub("[\r\n]", "", x)
}

removeEmoticon <- function(x){
  gsub("[^\x01-\x7F]", "", x)
}

removeInvoice <- function(x){
  gsub("inv/[0-9]+/+[xvi]+/[xvi]+/[0-9]+", "", x, ignore.case = T)
}

unescapeHTML <- function(str) {
  xml2::xml_text(xml2::read_html(paste0("<x>", str, "</x>")))
}

toSpace <- content_transformer(function(x, pattern){
  gsub(pattern, " ", x)
})

# Spell Normalization Function
spell.correction = content_transformer(function(x, dict){
  words = sapply(unlist(str_split(x, "\\s+")),function(x){
    if(is.na(spell.lex[match(x, dict$slang),"formal"])){
      x = x
    } else{
      x = spell.lex[match(x, dict$slang),"formal"]
    }
  })
  x = paste(words, collapse = " ")
})

# Stemming Words
stemming = function(x){
  paste(sapply(unlist(str_split(x,'\\s+')),katadasar),collapse = " ")
}