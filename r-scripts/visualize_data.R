#dependencies
library(plyr)
library(reshape2)
library(ggplot2)
source(build_database.R)

#user-defined options
team <- "LA Galaxy"
stage <- "Regular"

#subset team data
data.1 <- subset(data, team.1 == team)[,c("team.1", "year", "date", "stage", "result.1",
                                          "score.1", "score.2")]
data.2 <- subset(data, team.2 == team)[,c("team.2", "year", "date", "stage", "result.2",
                                          "score.1", "score.2")]

names(data.1) <- c("team", "year", "date", "stage", "result", "goals.for", "goals.against")
names(data.2) <- c("team", "year", "date", "stage", "result", "goals.against", "goals.for")

data.1$home.away <- "home"
data.2$home.away <- "away"

team.data <- rbind(data.1, data.2)
team.data <- subset(team.data, stage == stage)

#summarize stats by year
win.sum <- ddply(team.data, .(year, result), summarise,
                 sum=length(year))

win.sum <- dcast(win.sum, year~result, value.var="sum")

win.sum$tot <- win.sum[,2] + win.sum[,3] + win.sum[,4]
win.sum$win.pct <- win.sum$win / win.sum$tot

wins.plot <- ggplot(win.sum, aes(x=year, y=win.pct)) + 
  geom_bar(stat="identity") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90)) +
  ggtitle(team) + 
  xlab(element_blank()) + 
  ylab(element_text("Win Percentage"))

print(wins.plot)

gol.sum <- ddply(team.data, .(year), summarise,
                 goals.for=mean(goals.for),
                 goals.against=-mean(goals.against))

gol.sum <- melt(gol.sum, id.vars=c("year"), 
                meas.vars=c("goals.for", "goals.against"),
                variable.name="goal.group", value.name="goals")

gol.plot <- ggplot(gol.sum, aes(x=year, y=goals, fill=goal.group)) + 
  geom_bar(stat="identity") +
  theme_dark() +
  theme(axis.text.x = element_text(angle = 90, vjust=0.5)) +
  xlab(element_blank()) +
  ylab(element_text("Goals per game")) +
  guides(fill=guide_legend(title=NULL)) +
  scale_fill_manual(labels=c("Goals For", "Goals Against"), values=c("green", "red")) +
  ggtitle(team)
  
print(gol.plot)
