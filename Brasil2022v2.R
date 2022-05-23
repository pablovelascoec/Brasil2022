
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
  filter(date>"2022-04-24") %>% 
  group_by(candidate)
data_ends <-aggregate(mean_ends$votes, list(mean_ends$candidate), FUN=mean, na.rm = TRUE)
data_ends$voteper <- percent(data_ends$x)
data_ends$candidate <- data_ends$Group.1
data_ends$date <- as.Date("2022-05-18") 


###this plot shows a smooth tendency and specific error margins calculated by the program 
surveysbrnew %>%
  ggplot(aes(x = date, y = voteper, color=candidate, na.rm=TRUE)) +
  geom_smooth( se=FALSE, size=1.1) +
  stat_smooth(aes(fill=votes),method = "loess", size=1, alpha=0.2)+
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
  labs(title = "Voting intention - Brasil 2022", 
       subtitle = "Consolidated evolution based on national surveys (Datafolha, Quaest, IPESPE)", 
       caption = "OPALC - Sciences Po, by Pablo Velasco Oña") 




  scale_color_manual(values=c('firebrick2', 'darkblue', 'darkgoldenrod2', 'green4', 'cornsilk4')) +
  scale_fill_manual(values=c('firebrick2', 'darkblue', 'darkgoldenrod2', 'green4', 'cornsilk4')) +
  scale_linetype_manual(values=c('firebrick2', 'darkblue', 'darkgoldenrod2', 'green4', 'cornsilk4')) +
  scale_x_continuous(
  breaks = 1:2,
  expand = expansion(mult = 0.5)) +
  
  


    
    
    
    
    
    

#####alternative way of calling db
surveybr <- read_excel("surveybr.xlsx")
#order the values
surveysbrnew <- surveybr                                         
surveysbrnew$date <- as.Date(surveysbrnew$date) 
surveysbrnew$voteper <- percent(surveysbrnew$votes)
surveysbrnew <- surveysbrnew[order(surveysbrnew$date), ]
surveysbrnew$error <- 1.96*sqrt(surveysbrnew$votes*(1-surveysbrnew$votes)/surveysbrnew$N)
surveysbrnew$errsup <- surveysbrnew$votes + surveysbrnew$error
surveysbrnew$errinf <- surveysbrnew$votes - surveysbrnew$error
data_ends <- surveysbrnew %>% filter(date == "2022-05-08")


###this first plot shows a linear tendency and specific error margins are calculated according 
###to a manual formula taking into account only the size of the sample
surveysbrnew %>%
  ggplot(aes(x = date, y = votes, color=candidate, na.rm=TRUE)) +
  geom_line(na.rm=TRUE) +
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
             alpha=0.8)+
  geom_ribbon(aes(ymin=errinf, ymax = errsup , color = candidate), alpha=0.1) +
  xlab(as.expression(expression( paste("Date") ))) +
  ylab("Voting intention") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  theme_fivethirtyeight() + 
  labs(title = "Voting intention - Brasil 2022", 
       subtitle = "Consolidated evolution based on national surveys (Datafolha, Quaest, IPESPE)", 
       caption = "OPALC - Sciences Po, by Pablo Velasco Oña")

scale_color_manual(values=c('firebrick2', 'darkblue', 'darkgoldenrod2', 'green4', 'cornsilk4')) +
  




