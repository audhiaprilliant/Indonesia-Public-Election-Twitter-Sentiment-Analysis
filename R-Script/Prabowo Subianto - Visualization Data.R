# ===== TWITTER DATA ANALYSIS VISUALIZATION - PRABOWO SUBIANTO =====

library(ggplot2)
library(lubridate)

# ===== LOAD THE DATA
data.prabowo.df = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Indonesia Public Election - Twitter Sentiment Analysis/Datasets/Data Prabowo Subianto.csv',
                           header = TRUE,
                           sep = ',')
results = read.csv(file = 'D:/Audhi Aprilliant/IPB Works/Statistics Department/Data Science Practices/Projects/Indonesia Public Election - Twitter Sentiment Analysis/Datasets/Sentiment Data Prabowo Subianto.csv',
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
       subtitle = 'Prabowo Subianto',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Density')+ 
  ylab('Score')+
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
           alpha = 0.6,
           show.legend = FALSE)+
  labs(title = 'Barplot of Sentiments',
       subtitle = 'Prabowo Subianto',
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
       subtitle = 'Prabowo Subianto',
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
                x = 11,
                y = mean(abs(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Positive'),'Freq']))+20),
            hjust = 'right',
            size = 4)+
  # Negative Sentiment
  geom_hline(yintercept = mean(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Negative'),'Freq']),
             col = I('black'),
             size = 1)+
  geom_text(aes(fontface = 'italic',
                label = paste('Average Freq:',
                              ceiling(mean(abs(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Negative'),'Freq'])))),
                x = 9,
                y = mean(df.sentiment.score[which(df.sentiment.score$Sentiment == 'Negative'),'Freq'])-10),
            hjust = 'left',
            size = 4)+
  labs(title = 'Barplot of Sentiments',
       subtitle = 'Prabowo Subianto',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Score')+
  scale_x_discrete(limits = df.sentiment.score$Label,
                   labels = df.sentiment.score$Score1)+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')

# ===== BARPLOT OF TOTAL TWEET
data.prabowo.df$created = ymd_hms(data.prabowo.df$created,
                                  tz = 'Asia/Jakarta')
summary(data.prabowo.df$created)
# Another way to make 'Date' and 'Hour' variables
data.prabowo.df$date = date(data.prabowo.df$created)
data.prabowo.df$hour = hour(data.prabowo.df$created)
table(data.prabowo.df$date)
table(data.prabowo.df$hour)
# Date 2019-05-28
data.prabowo.date1 = subset(x = data.prabowo.df,
                            date == '2019-05-28')
unique(data.prabowo.date1$date)
data.hour.date1 = data.frame(table(data.prabowo.date1$hour))
colnames(data.hour.date1) = c('Hour',
                              'Total.Tweets')
ggplot(data.hour.date1)+
  geom_bar(aes(x = Hour,
               y = Total.Tweets,
               fill = I('red')),
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
                x = 6.5,
                y = mean(data.hour.date1$Total.Tweets)+5),
            hjust = 'left',
            size = 4)+
  labs(title = 'Total Tweets per Hours - Prabowo Subianto',
       subtitle = '28 May 2019',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Time of Day')+
  ylab('Total Tweets')+
  ylim(c(0,100))+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')

  # Date 2019-05-29
data.prabowo.date2 = subset(x = data.prabowo.df,
                            date == '2019-05-29')
unique(data.prabowo.date2$date)
data.hour.date2 = data.frame(table(data.prabowo.date2$hour))
colnames(data.hour.date2) = c('Hour',
                              'Total.Tweets')
ggplot(data.hour.date2)+
  geom_bar(aes(x = Hour,
               y = Total.Tweets,
               fill = I('red')),
           stat = 'identity',
           alpha = 0.75,
           show.legend = FALSE)+
  geom_hline(yintercept = mean(data.hour.date2$Total.Tweets),
             col = I('black'),
             size = 1)+
  geom_text(aes(fontface = 'italic',
                label = paste('Average:',
                              ceiling(mean(data.hour.date2$Total.Tweets)),
                              'Tweets per hour'),
                x = 1,
                y = mean(data.hour.date2$Total.Tweets)+6),
            hjust = 'left',
            size = 4)+
  labs(title = 'Total Tweets per Hours - Prabowo Subianto',
       subtitle = '29 May 2019',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Time of Day')+
  ylab('Total Tweets')+
  ylim(c(0,100))+
  theme_bw()+
  scale_fill_brewer(palette = 'Dark2')

# Date 2019-05-28 and Date 2019-05-29
data.hour.date3 = rbind(data.hour.date1,
                        data.hour.date2)
data.hour.date3$Date = c(rep(x = '2019-05-28',
                             len = nrow(data.hour.date1)),
                         rep(x = '2019-05-29',
                             len = nrow(data.hour.date2)))
data.hour.date3$Labels = c(letters,'A','B')
str(data.hour.date3)
data.hour.date3$Hour = as.character(data.hour.date3$Hour)
data.hour.date3$Hour = as.numeric(data.hour.date3$Hour)
for (i in 1:nrow(data.hour.date3)) {
  if (i%%2 == 0) {
    data.hour.date3[i,'Hour'] = ''
  }
  if (i%%2 == 1) {
    data.hour.date3[i,'Hour'] = data.hour.date3[i,'Hour']
  }
}
data.hour.date3$Hour = as.factor(data.hour.date3$Hour)

ggplot(data.hour.date3)+
  geom_bar(aes(x = Labels,
               y = Total.Tweets,
               fill = Date),
           stat = 'identity',
           alpha = 0.75,
           show.legend = TRUE)+
  geom_hline(yintercept = mean(data.hour.date3$Total.Tweets),
             col = I('black'),
             size = 1)+
  geom_text(aes(fontface = 'italic',
                label = paste('Average:',
                              ceiling(mean(data.hour.date3$Total.Tweets)),
                              'Tweets per hour'),
                x = 5,
                y = mean(data.hour.date3$Total.Tweets)+6),
            hjust = 'left',
            size = 3.8)+
  scale_x_discrete(limits = data.hour.date3$Labels,
                   labels = data.hour.date3$Hour)+
  labs(title = 'Total Tweets per Hours - Prabowo Subianto',
       subtitle = '28 - 29 May 2019',
       caption = 'Twitter Crawling 28 - 29 May 2019')+
  xlab('Time of Day')+
  ylab('Total Tweets')+
  ylim(c(0,100))+
  theme_bw()+
  theme(legend.position = 'bottom',
        legend.title = element_blank())+
  scale_fill_brewer(palette = 'Dark2')
