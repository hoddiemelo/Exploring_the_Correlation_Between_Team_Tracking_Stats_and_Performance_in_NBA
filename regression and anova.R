library(ggplot2)
library(dplyr)
library(MASS)
library(Metrics)
library(usdm)

data<- read.csv(file="~/Sports Analytics/team_tracking_per100.csv")

data<-
  data %>%
  mutate(win_pct = (W / GP))

cor(data$win_pct, data$OFFRTG)
hist(data$OFFRTG)

vif(data[,c("passes_made_100","secondary_ast_100","potential_ast_100","Ast.Adj.p100","touches_100",
            "avg_dribbles_touch_100","dribbles_100","post_ups_100","drives_100","drives_pass_rate","dist_miles_off")])

vif(data[,c("passes_made_100","secondary_ast_100","potential_ast_100","Ast.Adj.p100","touches_100",
            "dribbles_100","post_ups_100","drives_100","drives_pass_rate","dist_miles_off")])

vif(data[,c("passes_made_100","secondary_ast_100","potential_ast_100","Ast.Adj.p100","avg_dribbles_touch_100",
            "post_ups_100","drives_100","drives_pass_rate","dist_miles_off")])

# All with touches and dribbles
reg.lm<- lm(OFFRTG ~ passes_made_100 + secondary_ast_100 + potential_ast_100 + Ast.Adj.p100 + touches_100 +
            dribbles_100 + post_ups_100 + drives_100 + drives_pass_rate + dist_miles_off, data = data)
summary(reg.lm)
par(mfrow=c(2,2))
plot(reg.lm)
par(mfrow=c(1,1))

# All with avg touches per dribble - this is best model
reg.lm<- lm(OFFRTG ~ passes_made_100 + secondary_ast_100 + potential_ast_100 + Ast.Adj.p100 + avg_dribbles_touch_100 +
              post_ups_100 + drives_100 + drives_pass_rate + dist_miles_off, data = data)
summary(reg.lm)
par(mfrow=c(2,2))
plot(reg.lm)
par(mfrow=c(1,1))

# Removing adj ast
reg.lm<- lm(OFFRTG ~ passes_made_100 + secondary_ast_100 + potential_ast_100 + avg_dribbles_touch_100 +
              post_ups_100 + drives_100 + drives_pass_rate + dist_miles_off, data = data)
summary(reg.lm)
par(mfrow=c(2,2))
plot(reg.lm)
par(mfrow=c(1,1))

clusters<- read.csv(file="~/Sports Analytics/team_tracking_clustered_2.csv")

offrtg.aov<- aov(clusters$OFFRTG ~ clusters$play_style_clusters)
summary(offrtg.aov)
TukeyHSD(offrtg.aov)

clusters<-
  clusters %>%
  mutate(win_pct = (W / GP))

wins.aov<- aov(clusters$win_pct ~ clusters$play_style_clusters)
summary(wins.aov)
TukeyHSD(wins.aov)
