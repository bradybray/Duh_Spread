df <- read.csv('pickem.csv', stringsAsFactors = FALSE)

df$pts1ats <- apply(df, 1, function(x) {
  as.numeric(x['pts1']) + as.numeric(x['spread1'])
})

df$pts2ats <- apply(df, 1, function(x) {
  as.numeric(x['pts2']) + as.numeric(x['spread2'])
})

matchup <-  function(data = df, team = team) {
  subset(df, subset = (home == team | away == team))
}

teams <- unique(df$home)

home.mov <- function(x) {
  
  home <- df[df$home == x,]
  
  p.diff <- home$pts2 - home$pts1
  
  list(x,mean(p.diff))
}

away.mov <- function(x) {
  
  away <- df[df$away == x,]
  
  p.diff <- away$pts1 - away$pts2
  
  list(x, mean(p.diff))
}

a.mov <- do.call(rbind, lapply(teams, away.mov))

h.mov <- do.call(rbind, lapply(teams, home.mov))

cbind(team = a.mov[,1], away = a.mov[,2], home = h.mov[,2])
