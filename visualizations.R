##run after aggregated data has been generated in wc18mcsim.R
#requires team.prob.df and/or unmirrored.team.prob.df

library(tidyverse)
library(RColorBrewer)

####visualizations of simulated estimates####
#customizing ggplot theme
custom_theme <- function() {
  palette <- brewer.pal("Greys", n=9)
  color.background = palette[1]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]
  
  theme_bw(base_size=9) +
    theme(panel.background=element_rect(fill=color.background, color=color.background)) +
    theme(plot.background=element_rect(fill=color.background, color=color.background)) +
    theme(panel.border=element_rect(color=color.background)) +
    theme(panel.grid.major=element_line(color=color.grid.major,size=0.25)) +
    theme(panel.grid.minor=element_blank()) +
    theme(axis.ticks=element_blank()) +
    theme(legend.background = element_rect(fill=color.background)) +
    theme(legend.text = element_text(size=7,color=color.axis.title)) +
    theme(plot.title=element_text(color=color.title, size=10, vjust=1.25)) +
    theme(axis.text.x=element_text(size=7,color=color.axis.text)) +
    theme(axis.text.y=element_text(size=7,color=color.axis.text)) +
    theme(axis.title.x=element_text(size=8,color=color.axis.title, vjust=0)) +
    theme(axis.title.y=element_text(size=8,color=color.axis.title, vjust=1.25)) +
    theme(plot.margin = unit(c(0.35, 0.2, 0.3, 0.35), "cm"))
}

#tileplot of match ups
prob.plot<-ggplot(team.prob.df, aes(y=team,x=opponent,fill=rel.freq)) + 
  geom_tile() + 
  geom_text(aes(label=label),size=2) + 
  scale_y_discrete(limits= rev(levels(team.prob.df$team))) +
  scale_fill_continuous(low="lightgreen",high="darkgreen",na.value = NA) +
  custom_theme() + 
  theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="",y="", title="2018 FIFA World Cup draw", subtitle="Approximate probabilities of every possible match-up in the group stage draw") +
  theme(plot.title = element_text(size = 15, face = "bold"))
prob.plot


#unmirrored equivalent (subjectively prettier)
prob.plot2<-ggplot(unmirrored.team.prob.df, aes(y=team,x=opponent,fill=rel.freq)) + 
  geom_tile() + 
  geom_text(aes(label=label),size=2) +
  scale_y_discrete(limits= rev(levels(unmirrored.team.prob.df$team))) + 
  scale_fill_continuous(low="lightgreen", high="darkgreen", na.value = NA) +
  custom_theme() + 
  theme(legend.position="none") + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  labs(x="",y="", title="2018 FIFA World Cup draw", subtitle="Approximate probabilities of every possible match-up in the group stage draw") +
  theme(plot.title = element_text(size = 15, face = "bold"))
prob.plot2