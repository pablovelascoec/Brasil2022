
setwd("/Users/Acer/Desktop/dashboardelectoral/shiny_workshops/Brasil2022/Brasil2022")

library(ggplot2)
library(readxl) 
library(ggrepel)
library(tidyverse)
library(ggthemes)
library(formattable)


surveysbr <- read_excel("surveysbr.xlsx")
surveysbr$Undecided_ <- surveysbr$Outro_ + surveysbr$BN_ + surveysbr$NR_
  
surveysbrnew <- surveysbr %>%
  #selecting relevant variables
  select(date,survey,Lula_DaSilva,Jair_Bolsonaro,Sergio_Moro,Ciro_Gomes,Joao_Doria,Undecided_,N) %>%
  #pivoteando
  pivot_longer(cols = contains("_"),
               names_to = "candidate", values_to = "votes")
surveysbrnew$date <- as.Date(surveysbrnew$date) 
surveysbrnew$voteper <- percent(surveysbrnew$votes)
surveysbrnew <- surveysbrnew[order(surveysbrnew$date), ]
surveysbrnew$error <- 1.96*sqrt(surveysbrnew$votes*(1-surveysbrnew$votes)/surveysbrnew$N)
surveysbrnew$errsup <- surveysbrnew$votes + surveysbrnew$error
surveysbrnew$errinf <- surveysbrnew$votes - surveysbrnew$error

mean_ends <-surveysbrnew %>%
  #selecting the last surveys
  filter(date>"2022-05-01") %>% 
  group_by(candidate)
data_ends <-aggregate(mean_ends$votes, list(mean_ends$candidate), FUN=mean, na.rm = TRUE)
data_ends$voteper <- percent(data_ends$x)
data_ends$candidate <- data_ends$Group.1
data_ends$date <- as.Date("2022-05-26") 


###this plot shows a smooth tendency and specific error margins calculated by the program 
surveysbrnew %>%
  ggplot(aes(x = date, y = voteper, color=candidate, na.rm=TRUE)) +
  geom_smooth( se=FALSE, size=1.1) +
  stat_smooth(aes(fill=votes),method = "loess", size=1, alpha=0.3)+
  scale_color_manual(values=c('green4', 'darkblue', 'darkgoldenrod2', 'firebrick2', 'cornsilk4', 'grey')) +
  scale_fill_manual(values=c('green4', 'darkblue', 'darkgoldenrod2', 'firebrick2', 'cornsilk4', 'grey')) +
  scale_linetype_manual(values=c('green4', 'darkblue', 'darkgoldenrod2', 'firebrick2', 'cornsilk4', 'grey')) +
    geom_point() +
  geom_text_repel(
    aes(label = voteper), data = data_ends,
    fontface ="plain", color = "black", size = 3, 
    force=1, point.padding=unit(1,'lines'),
    vjust=1,
    direction='y',
    nudge_x=0.1,
    segment.size=0.2,
    na.rm = TRUE
  ) + 
  geom_vline(xintercept = as.numeric(as.Date("2022-10-02"))
             , color = "black",
             linetype="dashed",
             alpha=0.8,
             size=0.8)+
  xlab(as.expression(expression( paste("Date") ))) +
  ylab("Voting intention") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  theme_fivethirtyeight() + 
  labs(title = "Voting intention - Brazil 2022", 
       subtitle = "Consolidated evolution based on national surveys (Datafolha, Quaest, IPESPE)", 
       caption = "OPALC - Sciences Po, by Pablo Velasco Oña") 


  


#####Now sponteneous voting 


spontbr <- read_excel("spontbr.xlsx")
spontbr$Undecided_ <- spontbr$BN_ + spontbr$NR_

spontbrnew <- spontbr %>%
  #selecting relevant variables
  select(date,survey,Lula_DaSilva,Jair_Bolsonaro,Outro_, Undecided_,N) %>%
  #pivoteando
  pivot_longer(cols = contains("_"),
               names_to = "candidate", values_to = "votes")
spontbrnew$date <- as.Date(spontbrnew$date) 
spontbrnew$voteper <- percent(spontbrnew$votes)
spontbrnew <- spontbrnew[order(spontbrnew$date), ]
spontbrnew$error <- 1.96*sqrt(spontbrnew$votes*(1-spontbrnew$votes)/spontbrnew$N)
spontbrnew$errsup <- spontbrnew$votes + spontbrnew$error
spontbrnew$errinf <- spontbrnew$votes - spontbrnew$error

mean_ends2 <-spontbrnew %>%
  #selecting the last surveys
  filter(date>"2022-05-01") %>% 
  group_by(candidate)
data_ends2 <-aggregate(mean_ends2$votes, list(mean_ends2$candidate), FUN=mean, na.rm = TRUE)
data_ends2$voteper <- percent(data_ends2$x)
data_ends2$candidate <- data_ends2$Group.1
data_ends2$date <- as.Date("2022-05-26") 


###this plot shows a smooth tendency and specific error margins calculated by the program 
spontbrnew %>%
  ggplot(aes(x = date, y = voteper, color=candidate, na.rm=TRUE)) +
  geom_smooth( se=FALSE, size=1.1) +
  stat_smooth(aes(fill=votes),method = "loess", size=1, alpha=0.1)+
  scale_color_manual(values=c('darkblue', 'firebrick2', 'darkgoldenrod2', 'cornsilk4')) +
  scale_fill_manual(values=c('darkblue', 'firebrick2', 'darkgoldenrod2', 'cornsilk4')) +
  scale_linetype_manual(values=c('darkblue', 'firebrick2', 'darkgoldenrod2', 'cornsilk4')) +
  geom_point() +
  geom_text_repel(
    aes(label = voteper), data = data_ends2,
    fontface ="plain", color = "black", size = 3, 
    force=1, point.padding=unit(1,'lines'),
    vjust=1,
    direction='y',
    nudge_x=0.1,
    segment.size=0.2,
    na.rm = TRUE
  ) + 
  geom_vline(xintercept = as.numeric(as.Date("2022-10-02"))
             , color = "black",
             linetype="dashed",
             alpha=0.8,
             size=0.8)+
  xlab(as.expression(expression( paste("Date") ))) +
  ylab("Voting intention") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  theme_fivethirtyeight() + 
  labs(title = "Voting intention (spontaneous) - Brazil 2022", 
       subtitle = "Consolidated evolution based on national surveys (Datafolha, Quaest, IPESPE)", 
       caption = "OPALC - Sciences Po, by Pablo Velasco Oña") 

    
    
    
    
    
    




