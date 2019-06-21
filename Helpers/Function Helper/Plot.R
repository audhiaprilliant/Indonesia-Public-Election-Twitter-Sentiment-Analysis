library(ggplot2)

theme.tokopedia <- theme(panel.background = element_blank(),
                         panel.grid.minor = element_blank(),
                         axis.ticks.x = element_blank(),
                         panel.grid.major.y = element_line(colour = "grey"))

hourly <- function(df){
  library(pacman)
  p_load(ggplot2, dplyr)
  df <- df %>%
    mutate(time = as.factor(time)) %>%
    group_by(time) %>%
    dplyr::summarise(count = n())
  return(ggplot(df, aes(x=time, y=count)) + 
    geom_col(fill = "#40af47") +
    labs(title = "Total Tweets per Hour",
         subtitle = "Retrieved from @tokopediacare twitter account",
         caption = "15 November 2018",
         x = "Time of Day",
         y= "Total Tweets") +
    theme.tokopedia)
  }

daily <- function(df){
  library(pacman)
  p_load(ggplot2, dplyr)
  df %<>%
    mutate(dayofweek = wday(date, label = T, abbr = F)) %>%
    group_by(dayofweek) %>%
    summarise(count = n())
  ggplot(df, aes(x=dayofweek, y=count)) + 
    geom_col(fill = "#40af47") +
    labs(title = "Total Tweets per Day",
         subtitle = "Retrieved from @tokopediacare twitter account",
         caption = "15 November 2018",
         x = "Day of The Week",
         y= "Total Tweets") +
    theme.tokopedia
}