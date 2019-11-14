####creating data frame and filling pre-existing information####

teams = c("Russia", "Germany", "Brazil", "Portugal", "Argentina", "Belgium", "Poland", "France",
          "Spain", "Peru", "Switzerland", "England", "Colombia", "Mexico", "Uruguay", "Croatia",
          "Denmark", "Iceland", "Costa Rica", "Sweden", "Tunisia", "Egypt", "Senegal", "Iran",
          "Serbia", "Nigeria", "Australia", "Japan", "Morocco", "Panama", "South Korea", "Saudi Arabia")

df <- data.frame(index=c(rep(1:8,times=4)), team = teams)
df$team<-as.character(df$team)
df$seed <- as.factor(rep(1:4, each=8))

df$confed <- as.factor(c(1, 1, 2, 1, 2, 1, 1, 1,
                         1, 2, 1, 1, 2, 3, 2, 1, 
                         1, 1, 3, 1, 4, 4, 4, 5, 
                         1, 4, 5, 5, 4, 3, 5, 5))

levels(df$confed) <- c("UEFA", "COMNEBOL", "CONCACAF", "CAF", "AFC")

df$group <- NA
df$group[df$team=="Russia"]=1

####function to simulate draw pot by pot####
sim.groups<-function(x){
  k=TRUE
  g=x
  while(k==TRUE) {
    #allocate teams randomly
    g$group[g$seed==1 & g$team!="Russia"] <- sample(g$index[g$seed==1 & g$team!="Russia"], 7)
    g$group[g$seed==2] <- sample(g$index[g$seed==2], 8)
    g$group[g$seed==3] <- sample(g$index[g$seed==3], 8)
    g$group[g$seed==4] <- sample(g$index[g$seed==4], 8)
    
    #check if restrictions are met or re-do
    #max 2 UEFA teams are allowed in one group, max 1 from remaining regions are allowed in one group
    k=ifelse(sum(table(g$confed,g$group)[1,]<3)==8,
             ifelse(sum(table(g$confed,g$group)[2,]<2)==8,
                    ifelse(sum(table(g$confed,g$group)[3,]<2)==8,
                           ifelse(sum(table(g$confed,g$group)[4,]<2)==8,
                                  ifelse(sum(table(g$confed,g$group)[5,]<2)==8,FALSE,TRUE),TRUE),TRUE),TRUE),TRUE)
  }
  return(g)
}


####simulate data####

#number of simulations (this is quite computational heavy, don't run >100k sims if extreme precision is not of importance)
nsims=10000
sims <- vector("list",nsims)

#loop to simulate
for(i in 1:nsims){
  sims[[i]]<-sim.groups(df)
  print(i)
}


####probability by team####

#calculate frequency of each group member in the team's group
team.prob=vector("list",length(teams))
team.prob.dflist=vector("list",length(teams))
for(i in 1:length(teams)){
  for(k in 1:length(sims)){
    #extract vector of group members in team i's group
    gr<-sims[[k]][sims[[k]]$team %in% teams[i],5]
    team.prob[[i]]<-c(team.prob[[i]],sims[[k]][sims[[k]]$group==gr,2])
    print(i);print(k)
  }
  #calculate frequency of each group member in the team's group
  team.prob.dflist[[i]]<-data.frame(team=teams[[i]],table(team.prob[[i]])/length(sims))
  team.prob=vector("list",length(teams))
}


#bind to one data frame
team.prob.df <- do.call("rbind",team.prob.dflist)
colnames(team.prob.df) <- c("team","opponent","rel.freq")
#reorder factor levels of opponent to fit team order (for plotting convenience)
team.prob.df$opponent<-factor(team.prob.df$opponent, levels=teams)

#set rel.freq to NA if team==opponent (actual rel.freq=100%)
team.prob.df$rel.freq<-ifelse(team.prob.df$team==team.prob.df$opponent,NA,team.prob.df$rel.freq)
#add text label for plotting
team.prob.df$label <- paste0(sprintf("%.1f", team.prob.df$rel.freq*100), "%")
#set label to NA if team==opponent
team.prob.df$label<-ifelse(team.prob.df$team==team.prob.df$opponent,NA,team.prob.df$label)

#fill out the grif with all missing combinations of team~opponent w rel.freq = NA
for(i in 1:length(teams)){
  for(k in 1:length(teams)){
    temp<-if(nrow(team.prob.df[team.prob.df$team==teams[i] & team.prob.df$opponent==teams[k],])==0) c(teams[i],teams[k],NA,NA,NA) else NULL
    team.prob.df<-rbind(team.prob.df,temp)
  }
}

team.prob.df$rel.freq<-as.numeric(team.prob.df$rel.freq)

unmirrored.team.prob.df<-team.prob.df
#remove mirrored results
for(i in 1:length(teams)){
  for(k in 1:length(teams)){
    if(i>k) unmirrored.team.prob.df$rel.freq[unmirrored.team.prob.df$team==teams[i] & unmirrored.team.prob.df$opponent==teams[k]]=NA
    if(i>k) unmirrored.team.prob.df$label[unmirrored.team.prob.df$team==teams[i] & unmirrored.team.prob.df$opponent==teams[k]]=NA
  }
}

#drop unused factor levels
unmirrored.team.prob.df<-unmirrored.team.prob.df[complete.cases(unmirrored.team.prob.df),]
unmirrored.team.prob.df$opponent<-droplevels(unmirrored.team.prob.df$opponent)
unmirrored.team.prob.df$team<-droplevels(unmirrored.team.prob.df$team)


#furthermore the data can be used to extract probabilities of higher orders of combinations than one-to-one probabilities, 

#example using sweden
grps=NULL
for(k in 1:length(sims)){
  gr<-sims[[k]][sims[[k]]$team %in% "Sweden",5]
  grps=rbind(grps,sims[[k]][sims[[k]]$group==gr,2])
  print(k)
}
grps<-data.frame(grps)
grps$ones<-rep(1,length(grps$X1))

#per group
grps.agg<-aggregate(grps$ones, by=as.list(grps),FUN=sum)
grps.agg<-grps.agg[order(-grps.agg$x),]
grps.agg$rel.freq <- grps.agg$x/nsims
grps.agg

#then probabilities for any combination including sweden and 1-3 other teams can be extracted, e.g.
grps.agg[grps.agg$X1 %in% "Argentina" & grps.agg$X2 %in% "England" & grps.agg$X4 %in% "Nigeria",]

#...however this was not deemed useful for my purpose, so it will not be expanded upon or explored more thoroughly