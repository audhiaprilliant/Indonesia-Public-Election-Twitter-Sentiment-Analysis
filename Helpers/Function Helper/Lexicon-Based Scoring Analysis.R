library(tm)
library(xml2)
library(stringr)
library(dplyr)

scores.sentiment = function(df.tweet, senti, negasi, .progress='none') {
  require(plyr)
  require(stringr)
  scores <-  laply(df.tweet, function(tweet, senti, negasi) {
    list.words <- str_split(tweet, '\\s+')
    words <- unlist(list.words)
    word.matches <- match(words, senti$words)
    prev.word <- words[which(!is.na(word.matches))-1]
    next.word <- words[which(!is.na(word.matches))+1]
    true.match <- c(words[which(!is.na(word.matches))])
    word.check <- if(length(prev.word) == 0){
      senti[senti$words %in% true.match, "score"]
    }else if(prev.word %in% negasi){
      -(senti[senti$words %in% true.match , "score"])
    }else if(prev.word %in% booster$words | next.word %in% booster$words & 
             senti[senti$words %in% true.match, "score"] > 0){
      senti[senti$words %in% true.match , "score"] + booster[booster$words %in% words, "score"]
    }else if(prev.word %in% booster$words | next.word %in% booster$words & 
             senti[senti$words %in% true.match, "score"] < 0){
      senti[senti$words %in% true.match , "score"] - booster[booster$words %in% words, "score"]
    }else if(c(words[which(!is.na(word.matches))]) %in% senti$words){
      senti[senti$words %in% true.match, "score"]
    }else{
      0
    }
    score <- sum(word.check)
    return(score)
  }, senti, negasi, .progress=.progress )
  scores.df <- data.frame(score=scores, text=df.tweet)
  return(scores.df)
}