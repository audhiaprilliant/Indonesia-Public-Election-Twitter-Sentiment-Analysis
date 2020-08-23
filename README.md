## Analisis Sentimen Pasca Pemilihan Umum (PEMILU) Indonesia Tahun 2019 Mellui Platform Twitter dengan Naive Bayes

### Enviromental Development
In this research, author used personal computer or laptop with hardwares spesification described below:
- Processor AMD A8 – 7410
- Memory 4GB
- VGA Radeon (TM) R5 Graphics  
  
And several softwares:
- Operating system Microsoft Windows Pro 10 (64-bit) and Ubuntu Bionic Breaver
- R with packages twitteR, tm, dplyr, stringr, katadasaR, wordcloud, and ggplot2
- Microsoft Excel 2016 as tools for data editor, data merging, and data transforming

### Flowchart
Steps in this research entitled *Analisis Sentimen Pasca Pemilihan Umum (PEMILU) Indonesia Tahun 2019 melalui Platform Twitter* consists of data crawling, data pre-processing (cleansing, case folding, tokenizing, data normalization, filtering, and stemming), term weighting using TF-IDF, sentiment mining using Indonesian's corpora, and data visualization for sentiment (negative and positive) using wordcloud and graphs. Systematically, steps of the whole research is displayed in Figure 1:

<img src='img/Screenshot from 2020-06-04 00-33-58.png' alt='uptodate' class='center'>

### Dataset
For this project, we use primary data of Twitter by crawling on 28th - 29th of May 2019. Further, the data is in csv format (comma delimited). It refers into two topics, the data of Joko Widodo which contains the keyword "Joko Widodo" and the other one is the data of Prabowo Subianto that has the keyword "Prabowo Subianto".

| Variables      | Type       | Information                                                             |
| -------------- | ---------- | ----------------------------------------------------------------------- |
| Text           | Character  | Tweet of user in Twitter                                                |
| Create         | Date       | Date and time that tweet is made                                        |
| Truncated      | Logical    | Value 1 if the tweet is truncated and 0 if the tweet is not truncated   |
| ID             | Factor     | User ID                                                                 |
| StatusSource   | Factor     | URL to access the tweet online                                          |
| ScreenName     | Factor     | Username of user                                                        |
| RetweetCount   | Number     | Number of retweet                                                       |
| IsRetweet      | Logical    | The value 1 if user's tweet is the retweet of other user and vice versa |
| Retweet        | Numeric    | The value 1 if other user retweet user's tweet and vice versa           |
