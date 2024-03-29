run_sim <- function(rate, children_number, immunization, immunization_rate, days_spread){
  children_temp <- data.frame(
    "number"          = 1:children_number,
    "infected_s"      = rep(0, children_number),
    "infected_e"      = rep(0, children_number),
    "day"             = rep(0, children_number),
    "infected_from"   = rep("", children_number),
    "rate"            = rep(rate, children_number),
    "day_infected"    = rep("", children_number),
    "days_infected"   = rep(0, children_number)
  )
  
  children_temp$infected_s[which(children_temp$number == 1)] <- 1
  children_temp$infected_e[which(children_temp$number == 1)] <- 1
  # children_temp$days_infected[which(children_temp$number == 1)] <- 1
  
  if(immunization){
    for(i in 2:nrow(children_temp)){
      if(sample(1:100, 1) <= immunization_rate){
        # Adjusted Vaccine effectiveness (%) 36 [CI (21, 48)]
        # Adjust rate of infection by Adjusted VE
        children_temp$rate[i] <- ((children_temp$rate[i]/100) * .36)
      }
    }
  }
  
  children <- data.frame()
  day <- 0
  # browser()
  spreading <- (length(which(children_temp$rate == rate)) != 
    length(which(children_temp$infected_s == 1)))
  # loop until all children are infected
  while(spreading){
    # print(day)
    # loop through each child to the next - if infect can spread
    for(i in 1:nrow(children_temp)){
      # can child1 infect child2
      if(children_temp$infected_s[i] == 0 | 
         children_temp$days_infected[i] > days_spread | 
         children_temp$rate[i] == -1){next}
      # child is infected 
      children_temp$days_infected[i] <- children_temp$days_infected[i]+1
      for(j in 1:nrow(children_temp)){
        # already infected
        if(children_temp$infected_s[j] == 1 | children_temp$rate[j] == -1){next}
        # child cannot spread from itself to itself
        if(children_temp$number[i] == children_temp$number[j]){next}
        # random number generation from 1-100 with 2 as the threshold of being able to infect
        if(sample(1:100, 1) <= children_temp$rate[j]){
          children_temp$infected_e[j] <- 1
          children_temp$infected_from[j] <- i
          children_temp$day_infected[j] <- day
        }
      }
    }
    children <- rbind(children, children_temp)
    children_temp$infected_s <- children_temp$infected_e
    day <- day + 1
    children_temp$day <- day
    # browser()
    spreading <- (length(which(children_temp$rate == rate)) != 
      length(which(children_temp$infected_s == 1)))
    if(length(which(children_temp$days_infected %in% 1:days_spread)) == 0){
      spreading <- FALSE
    }
  }
  return(children)
}


getNodes <- function(infected){
  nodes <- data.frame(
    "id"             = seq(1:length(infected)),
    "shape"          = rep("dot", length(infected)),
    "shadow"         = rep(TRUE, length(infected)),
    "title"          = paste0("Student ", seq(1:length(infected)),": ", ifelse(infected == 1, "Infected", "Not Infected")),
    "label"          = 1:length(infected),
    "size"           = rep(10, length(infected)),
    "borderWidth"    = rep(2, length(infected)),
   # "color"          = ifelse(infected == 1, "red", "gray"),
    "group"          = ifelse(infected == 1, "Infected", "Not Infected")
  )
  return(nodes)
}

getEdges <- function(df){
  df <- df[which(df$infected_from != ""),]
  edges <- data.frame(
    "from"     = df$number,
    "to"       = df$infected_from,
    "color"    = rep("red", nrow(df))
  )
  return(edges)
}


getEV <- function(rate, children_number, immunization) {
  if(immunization) {
    return(((rate/100)*.5) +((rate/100) *.36 *.5))
  } 
  else {
    return((rate/100))
  }
  
  
  
}