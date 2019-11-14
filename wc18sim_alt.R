#less computational-intensive function to generate data, however not as easily generalizable if applied to future draws
#(because slot allocation, restrictions, distribution of confederations among pots vary)

####create data#####

teams = c("Russia", "Germany", "Brazil", "Portugal", "Argentina", "Belgium", "Poland", "France",
          "Spain", "Peru", "Switzerland", "England", "Colombia", "Mexico", "Uruguay", "Croatia",
          "Denmark", "Iceland", "Costa Rica", "Sweden", "Tunisia", "Egypt", "Senegal", "Iran",
          "Serbia", "Nigeria", "Australia", "Japan", "Morocco", "Panama", "South Korea", "Saudi Arabia")

confed = c("UEFA", "UEFA", "COMNEBOL", "UEFA", "COMNEBOL", "UEFA", "UEFA", "UEFA", 
           "UEFA", "COMNEBOL", "UEFA", "UEFA", "COMNEBOL", "CONCACAF", "COMNEBOL", "UEFA",
           "UEFA", "UEFA", "CONCACAF", "UEFA", "CAF", "CAF", "CAF", "AFC", 
           "UEFA", "CAF", "AFC", "AFC", "CAF", "CONCACAF", "AFC", "AFC")

df <- data.frame(index=c(rep(1:8,times=4)), 
                 team = teams,
                 confed = confed,
                 stringsAsFactors = FALSE)

####function to simulate draw####
sim_draw <- function(x) {
  group_names <- LETTERS[1:8]
  groups <- data.frame(matrix("", 8, 4), row.names = group_names, stringsAsFactors = FALSE)
  names(groups) <- paste0("Round_", 1:4)
  
  #mirror the draw with confed memberships
  confeds <- groups
  
  #round 1 
  groups$Round_1[1] <- x$team[1]
  round_1 <- sample(2:8)
  groups$Round_1[-1] <- x$team[round_1]
  confeds$Round_1 <- x$confed[match(groups$Round_1, x$team)]
  
  #round 2 
  #first, deal with the COMNEBOL teams
  r2_COMN <- x$team[9:16][x$confed[9:16] %in% "COMNEBOL"]
  #they can only go into these groups:
  r2_COMN_OK <- group_names[confeds$Round_1 != "COMNEBOL"]
  #so make the draw for these teams
  round_2_COMNEBOL <- sample(r2_COMN_OK, length(r2_COMN))
  groups[round_2_COMNEBOL, "Round_2"] <- r2_COMN
  
  #make the draw for the other teams
  r2_OTHER <- x$team[9:16][!x$confed[9:16] %in% "COMNEBOL"]
  r2_OTHER_OK <- group_names[!group_names %in% round_2_COMNEBOL]
  round_2_OTHER <- sample(r2_OTHER_OK)
  groups[round_2_OTHER, "Round_2"] <- r2_OTHER
  confeds$Round_2 <- x$confed[match(groups$Round_2, x$team)]
  
  #round 3
  #first, deal with the UEFA teams
  r3_UEFA <- x$team[17:24][x$confed[17:24] %in% "UEFA"]
  r3_UEFA_OK <- group_names[(confeds$Round_1 == "UEFA") + (confeds$Round_2 == "UEFA") != 2]
  round_3_UEFA <- sample(r3_UEFA_OK, length(r3_UEFA))
  groups[round_3_UEFA, "Round_3"] <- r3_UEFA
  
  #deal with the CONCACAF team
  round_3_LEFT <- group_names[groups$Round_3 == ""]
  r3_CONCACAF <- x$team[17:24][x$confed[17:24] %in% "CONCACAF"]
  r3_CONCACAF_OK <- group_names[(confeds$Round_2 != "CONCACAF") & (groups$Round_3 == "")]
  round_3_CONCACAF <- sample(r3_CONCACAF_OK, length(r3_CONCACAF))
  groups[round_3_CONCACAF, "Round_3"] <- r3_CONCACAF  
  
  #deal with the remainders
  r3_OTHER <- x$team[17:24][!x$confed[17:24] %in% c("UEFA", "CONCACAF")]
  r3_OTHER_OK <- group_names[groups$Round_3 == ""]
  round_3_OTHER <- sample(r3_OTHER_OK)
  groups[round_3_OTHER, "Round_3"] <- r3_OTHER
  
  confeds$Round_3 <- x$confed[match(groups$Round_3, x$team)]
  
  #round 4 
  #first, deal with the UEFA team
  r4_UEFA <- x$team[25:32][x$confed[25:32] %in% "UEFA"]
  r4_UEFA_OK <- group_names[(confeds$Round_1 == "UEFA") + (confeds$Round_2 == "UEFA") + (confeds$Round_3 == "UEFA") != 2]
  round_4_UEFA <- sample(r4_UEFA_OK, length(r4_UEFA))
  groups[round_4_UEFA, "Round_4"] <- r4_UEFA
  
  #deal with the CAF teams
  r4_CAF <- x$team[25:32][x$confed[25:32] %in% "CAF"]
  r4_CAF_OK <- group_names[(confeds$Round_3 != "CAF") & (groups$Round_4 == "")]
  round_4_CAF <- sample(r4_CAF_OK, length(r4_CAF))
  groups[round_4_CAF, "Round_4"] <- r4_CAF
  
  #then deal with the CONCACAF team
  #(a special case is if the group with the CAF team has a spot open. the CONCACAF team must get this spot)
  r4_CONCACAF <- x$team[25:32][x$confed[25:32] %in% "CONCACAF"]
  r3_AFC <- which(confeds$Round_3 == "AFC")
  if (r3_AFC %in% which(groups$Round_4 == "")) {
    r4_CONCACAF_OK <- group_names[r3_AFC]
  } else {
    r4_CONCACAF_OK <- group_names[(confeds$Round_2 != "CONCACAF") & (confeds$Round_3 != "CONCACAF") & (groups$Round_4 == "")]
  }
  round_4_CONCACAF <- sample(r4_CONCACAF_OK, length(r4_CONCACAF))
  groups[round_4_CONCACAF, "Round_4"] <- r4_CONCACAF
  
  #finally, deal with the other AFC teams
  r4_AFC <- x$team[25:32][x$confed[25:32] %in% "AFC"]
  r4_AFC_OK <- group_names[groups$Round_4 == ""]
  round_4_AFC <- sample(r4_AFC_OK, length(r4_AFC))
  groups[round_4_AFC, "Round_4"] <- r4_AFC
  
  return(groups)
}

n_sim <- 100000
sims=vector("list",n_sim)
sims <- replicate(n_sim, sim_draw(df), simplify = FALSE)

