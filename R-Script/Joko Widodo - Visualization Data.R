# ===== TWITTER DATA ANALYSIS VISUALIZATION - JOKO WIDODO =====

library(ggplot2)
library(lubridate)

# ===== LOAD THE DATA
data.jokowi.df = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Indonesia Public Election - Twitter Sentiment Analysis/Datasets/Data Joko Widodo.csv',
                          header = TRUE,
                          sep = ',')
results = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Indonesia Public Election - Twitter Sentiment Analysis/Datasets/Sentiment Data Joko Widodo.csv',
                   header = TRUE,
                   sep = ',')

# ===== HISTOGRAM OF SENTIMENT SRORE
df.score = subset(results,
                  class == c('Negative','Positive'))
colnames(df.score) = c('Score',
                       'Text',
                       'Sentiment')
ggplot(df.score)+
  geom_density(aes(x = Score,
                   fill = Sentiment),
               alpha = 0.75)+
  xlim(c(-11,11))+
  labs(title = 'Density Plot of Sentiment Scores',
       subtitle = 'Joko Widodo',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Score')+ 
  ylab('Density')+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')+
  theme(legend.position = 'bottom',
        legend.title = element_blank())

# ===== BARPLOT OF SENTIMENT
df.sentiment = as.data.frame(table(results$class))
colnames(df.sentiment) = c('Sentiment',
                           'Freq')
ggplot(df.sentiment)+
  geom_bar(aes(x = Sentiment,
               y = Freq,
               fill = I('red')),
           stat = 'identity',
           alpha = 0.75,
           show.legend = FALSE)+
  labs(title = 'Barplot of Sentiments',
       subtitle = 'Joko Widodo',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')
# PIECHART OF SENTIMENT
df.pie = df.sentiment
df.pie$Prop = df.pie$Freq/sum(df.pie$Freq)
# Manipulate the Data
df.pie = df.pie %>%
  arrange(desc(Sentiment)) %>%
  mutate(lab.ypos = cumsum(Prop) - 0.5*Prop)

ggplot(df.pie,
       aes(x = 2,
           y = Prop,
           fill = Sentiment))+
  geom_bar(stat = 'identity',
           col = 'white',
           alpha = 0.75,
           show.legend = TRUE)+
  coord_polar(theta = 'y', 
              start = 0)+
  geom_text(aes(y = lab.ypos,
                label = Prop),
            color = 'white',
            fontface = 'italic',
            size = 4)+
  labs(title = 'Piechart of Sentiments',
       subtitle = 'Joko Widodo',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlim(c(0.5,2.5))+
  theme_void()+
  scale_fill_brewer(palette = 'Dark2')+
  theme(legend.title = element_blank(),
        legend.position = 'right')

# ===== BARPLOT OF SENTIMENT SCORE
df.sentiment.score = data.frame(table(results$score))
colnames(df.sentiment.score) = c('Score',
                                 'Freq')
df.sentiment.score$Score = as.character(df.sentiment.score$Score)
df.sentiment.score$Score = as.numeric(df.sentiment.score$Score)
Score1 = df.sentiment.score$Score
sign(df.sentiment.score[1,1])
for (i in 1:nrow(df.sentiment.score)) {
  sign.row = sign(df.sentiment.score[i,'Score'])
  for (j in 1:ncol(df.sentiment.score)) {
    df.sentiment.score[i,j] = df.sentiment.score[i,j] * sign.row
  }
}
df.sentiment.score$Label = c(letters[1:nrow(df.sentiment.score)])
df.sentiment.score$Sentiment = ifelse(df.sentiment.score$Freq < 0,
                                      'Negative','Positive')
df.sentiment.score$Score1 = Score1

ggplot(df.sentiment.score)+
  geom_bar(aes(x = Label,
               y = Freq,
               fill = Sentiment),
           stat = 'identity',
           show.legend = FALSE)+
  # Positive Sentiment
  geom_hline(yintercept = mean(abs(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Positive'),'Freq'])),
             col = I('black'),
             size = 1)+
  geom_text(aes(fontface = 'italic',
                label = paste('Average Freq:',
                              ceiling(mean(abs(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Positive'),'Freq'])))),
                x = 10,
                y = mean(abs(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Positive'),'Freq']))+30),
            hjust = 'right',
            size = 4)+
  # Negative Sentiment
  geom_hline(yintercept = mean(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Negative'),'Freq']),
             col = I('black'),
             size = 1)+
  geom_text(aes(fontface = 'italic',
                label = paste('Average Freq:',
                              ceiling(mean(abs(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Negative'),'Freq'])))),
                x = 5,
                y = mean(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Negative'),'Freq'])-15),
            hjust = 'left',
            size = 4)+
  labs(title = 'Barplot of Sentiments',
       subtitle = 'Joko Widodo',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Score')+
  scale_x_discrete(limits = df.sentiment.score$Label,
                   labels = df.sentiment.score$Score1)+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')

# ===== BARPLOT OF TOTAL TWEET
data.jokowi.df$created = ymd_hms(data.jokowi.df$created,
                                  tz = 'Asia/Jakarta')
summary(data.jokowi.df$created)
# Another way to make 'Date' and 'Hour' variables
data.jokowi.df$date = date(data.jokowi.df$created)
data.jokowi.df$hour = hour(data.jokowi.df$created)
table(data.jokowi.df$date)
table(data.jokowi.df$hour)
# Date 2019-05-29
data.jokowi.date1 = subset(x = data.jokowi.df,
                           date == '2019-05-29')
unique(data.jokowi.date1$date)
data.hour.date1 = data.frame(table(data.jokowi.date1$hour))
colnames(data.hour.date1) = c('Hour',
                              'Total.Tweets')
ggplot(data.hour.date1)+
  geom_bar(aes(x = Hour,
               y = Total.Tweets,
               fill = I('blue')),
           stat = 'identity',
           alpha = 0.75,
           show.legend = FALSE)+
  geom_hline(yintercept = mean(data.hour.date1$Total.Tweets),
             col = I('black'),
             size = 1)+
  geom_text(aes(fontface = 'italic',
                label = paste('Average:',
                              ceiling(mean(data.hour.date1$Total.Tweets)),
                              'Tweets per hour'),
                x = 8,
                y = mean(data.hour.date1$Total.Tweets)+20),
            hjust = 'left',
            size = 4)+
  labs(title = 'Total Tweets per Hours - Joko Widodo',
       subtitle = '28 May 2019',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Time of Day')+
  ylab('Total Tweets')+
  scale_fill_brewer(palette = 'Dark2')+
  theme_bw()
  
