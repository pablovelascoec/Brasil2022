#https://stackoverflow.com/questions/54852379/display-data-values-on-geom-line
setwd("/Users/Acer/Desktop/dashboardelectoral/shiny_workshops/Brasil2022")

library(ggplot2)
library(readxl) 
library(ggrepel)
library(tidyverse)

surveybr <- read_excel("surveybr.xlsx")
#order the values
surveysbrnew <- surveybr                                         
surveysbrnew$date <- as.Date(surveysbrnew$date) 
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
      scale_color_manual(values=c('firebrick2', 'darkblue', 'darkgoldenrod2', 'green4', 'cornsilk4')) +
      geom_point() +
      geom_text_repel(
        aes(label = votes), data = data_ends,
        fontface ="plain", color = "black", size = 3, box.padding = 0.8
      ) + 
      geom_ribbon(aes(ymin=errinf, ymax = errsup , color = candidate), alpha=0.1) +
      xlab(as.expression(expression( paste("Date") ))) +
      ylab("Voting intention") +
      scale_x_date(date_labels = "%Y-%m-%d") +
      theme_classic() + 
      labs(title = "Voting intention - Brasil 2022", 
           subtitle = "Consolidated evolution based on national surveys (Datafolha, Quaest, IPESPE)", 
           caption = "OPALC - Sciences Po, by Pablo Velasco Oña")



###this second plot shows a smooth tendency and specific error margins calculated by the program 
surveysbrnew %>%
  ggplot(aes(x = date, y = votes, color=candidate, na.rm=TRUE)) +
  geom_smooth( se=FALSE, size=1.1) +
  stat_smooth(aes(fill=votes),method = "loess", size=1, alpha=0.2)+
  scale_color_manual(values=c('firebrick2', 'darkblue', 'darkgoldenrod2', 'green4', 'cornsilk4')) +
  scale_fill_manual(values=c('firebrick2', 'darkblue', 'darkgoldenrod2', 'green4', 'cornsilk4')) +
  scale_linetype_manual(values=c('firebrick2', 'darkblue', 'darkgoldenrod2', 'green4', 'cornsilk4')) +
  geom_point() +
  geom_text_repel(
    aes(label = votes), data = data_ends,
    fontface ="plain", color = "black", size = 3, box.padding = 0.8
  ) + 
  xlab(as.expression(expression( paste("Date") ))) +
  ylab("Voting intention") +
  scale_x_date(date_labels = "%Y-%m-%d") +
  theme_classic() + 
  labs(title = "Voting intention - Brasil 2022", 
       subtitle = "Consolidated evolution based on national surveys (Datafolha, Quaest, IPESPE)", 
       caption = "OPALC - Sciences Po, by Pablo Velasco Oña")





