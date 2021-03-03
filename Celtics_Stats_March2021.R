library(tidyverse)
library(hexbin)
library(jsonlite)
library(httr)
library(scales)
library(ggExtra)   # marginal plots
library(ggtext)    # color your text
library(patchwork) # combine multiple plots
library(paletteer) # get all the color palettes
library(scales) 
library(ggplot2)
library(ggrepel)
library(gt)

Jaylen <- read_csv("Jaylen_Adv.csv")
Tatum <- read_csv("Tatum_Adv.csv")
Celtics_Adv <- read_csv("Celtics_Adv.csv")
Celtics_Adv <-Celtics_Adv %>% 
  rename("ThreeAR" = `3PAr`)%>%
  rename(W_L = "W/L")

Celtics_Adv_f <- Celtics_Adv %>% 
  filter(W_L == "L")
Celtics_Adv_W <- Celtics_Adv %>% 
  filter(W_L == "W")

ggplot(Celtics_Adv_f, aes(x=Pace, y= ThreeAR))+
  geom_point()

ggplot(Celtics_Adv_W, aes(x=Pace, y= ThreeAR))+
  geom_point()

ggplot(Celtics_Adv_f, aes(x=ORtg, y= DRtg))+
  geom_point()

ggplot(Celtics_Adv_W, aes(x=ORtg, y= DRtg))+
  geom_point()



ggplot(Celtics_Adv, aes(x=AST, y = oeFG))+
  geom_point(aes(color = ifelse(W_L == "W", "red", "green")))+
  theme(legend.position = "none")

ggplot(Celtics_Adv, aes(x=ORtg, y=DRtg))+
  geom_point(aes(color= ifelse(W_L == "W", "red", "green")),
             size = 2)+
  scale_x_continuous(breaks = seq(75,140,5))+
  scale_y_continuous(breaks = seq(95,135,5))+
  geom_hline(yintercept = 110, linetype = "dashed")+
  geom_vline(xintercept = 115, linetype = "dashed")+
  labs(x = "Offensive Rating",
       y = "Defensive Rating",
       title = "Celtics Offensive Rating vs Defensive Rating (Mar 1)",
       caption = "data:nba.com; plot: @_ThomasT")+
  theme(legend.position = "none")



ggplot(Celtics_Adv, aes(x=DRB, y=ORB))+
  geom_point(aes(color= ifelse(W_L == "W", "red", "green")),
             size = 2)+
  scale_x_continuous(breaks = seq(66,92,2))+
  scale_y_continuous(breaks = seq(14,36,2))+
  theme(legend.position = "none")






Jaylen %>%
  ggplot(aes(x = G, y= TS))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = seq(1,30,1))+
  scale_y_continuous(breaks = seq(.4,1,.1))

Tatum %>%
  ggplot(aes(x = G, y= TS))+
  geom_line()+
  geom_point()+
  scale_x_continuous(breaks = seq(1,30,1))+
  scale_y_continuous(breaks = seq(.2,1,.1))


Jaylen %>%
  ggplot(aes(x = G, y= TOV))+
  geom_line()+
  geom_point()
  #scale_x_continuous(breaks = seq(1,30,1))+
  #scale_y_continuous(breaks = seq(.4,1,.1))

Tatum %>%
  ggplot(aes(x = G, y= AST))+
  geom_line()+
  geom_point()


Celtics_Adv %>% ggplot(aes(x= G, y = eFG))+
  geom_line()+
  geom_point()

Celtics_Adv %>% ggplot(aes(x = G, y = ThreeAR))+
  geom_line()+
  geom_point()

Celtics_Adv %>% ggplot(aes(x = G, y = DRtg))+
  geom_line()+
  geom_point()


NBA_Rankings <- read_csv("NBA_Adv_Rankings.csv")

AST_Rank <- NBA_Rankings%>%
  select(TEAM, AST) %>% 
  arrange(-AST)#29 Cs

Dreb <- NBA_Rankings %>% 
  select(TEAM, DREB) %>% 
  arrange(-DREB) #14 Cs

TOV <- NBA_Rankings %>% 
  select(TEAM, TOV) %>% 
  arrange(TOV) #22 Cs

EFG_Rank <- NBA_Rankings %>% 
  select(TEAM, EFG) %>% 
  arrange(-EFG) #18 Cs

TS_RANK <- NBA_Rankings %>% select(TEAM, TS) %>% arrange(-TS) #20 Cs

name <- c("TS", "EFG", "TOV", "Dreb", "AST", "PITP", "FBPTS")
rank <- c(20,18,22,14,29, 19, 19)
Cs_Rank <- data.frame(name, rank)

ggplot(Cs_Rank, aes(x = rank, y = reorder(name, rank),))+
  geom_col(aes(fill = if_else(rank >= 0, " dark green","red")))+
  coord_flip()+
  labs( x = "Category",
        y = "Ranks",
        title = "Celtics Rankings by Category (Mar 1)",
        caption = "data:nba, plot: @_ThomasT")+
  scale_fill_identity()



Iso_PT <- read_csv("Iso_PT.csv")
Iso_PT %>% ggplot(aes(x = FREQ, PPP))+
  geom_point(aes(color = if_else(TEAM == "Boston Celtics", "green", "black"),
                 size = 1.5))+
  geom_text_repel(aes(label = if_else(TEAM == "Boston Celtics", "Bos", "")))+
  labs(x = "Frequency",
       y = "Points Per Possession",
       title = "NBA Isolation Plays Ranking(March 1st)",
       caption = "data:nba", "plot: @_ThomasT")+
  scale_x_continuous(breaks = seq(0,0.11,.01))+
theme(legend.position = "none")


Transition_Pt <- read_csv("Transition_PT.csv")
Transition_Pt <- Transition_Pt %>% 
  select(-PERCENTILE)
Transition_Pt%>% ggplot(aes(x = FREQ, PPP))+
  geom_point(aes(color = if_else(TEAM == "Boston Celtics", "green", "black"),
                 size = 1.5))+
  geom_text_repel(aes(label = if_else(TEAM == "Boston Celtics", "Bos", "")))+
  labs(
       title = "NBA Transition Plays Ranking(March 1st)",
       caption = "data:nba", "plot: @_ThomasT")+
  scale_x_continuous(name = "Frequency", breaks = seq(.1,0.21,.01))+
  scale_y_continuous(name = "Points Per Possession", breaks = seq(1.02,1.25,0.01))+
  theme(legend.position = "none")

PT_combine <- merge(Transition_Pt, Iso_PT, by = "TEAM")
