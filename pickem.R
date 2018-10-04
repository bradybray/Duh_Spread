df <- read.csv('pickem.csv', stringsAsFactors = FALSE)

df$pts1ats <- apply(df, 1, function(x) {
  as.numeric(x['pts1']) + as.numeric(x['spread1'])
})

df$pts2ats <- apply(df, 1, function(x) {
  as.numeric(x['pts2']) + as.numeric(x['spread2'])
})

df$atsdiff <- apply(df, 1, function(x) {
  abs(as.numeric(x['pts1ats']) - as.numeric(x['pts2ats']))
})

df$ptsdiff <- apply(df, 1, function(x) {
  abs(as.numeric(x['pts1']) - as.numeric(x['pts2']))
})

matchup <-  function(data = df, team = team) {
  subset(df, subset = (home == team | away == team))
}

teams <- unique(df$away)

league.summary <-lapply(teams, function(x) {
  home <- df[df$home == x,]
  away <- df[df$away == x,]
  loss.h <- dim(home[home$pts2 < home$pts1,])[1]
  loss.a <- dim(away[away$pts1 < away$pts2,])[1]
  loss.h.ats <- dim(home[home$pts2ats < home$pts1,])[1]
  loss.a.ats <- dim(away[away$pts1ats < away$pts2,])[1]
  games <- tail(df, n = 1)$week
  pf <- sum(home$pts2) + sum(away$pts1)
  pa <- sum(home$pts1) + sum(away$pts2)
  pfats <- sum(home$pts2ats) + sum(away$pts1ats)
  paats <- sum(home$pts1ats) + sum(away$pts2ats)
  atsdiff <- pfats - paats
  avgspread = round((sum(home$spread2) + sum(away$spread1))/games, digits=3)
  w.a <- paste(games-(loss.h + loss.a),'-',(loss.h+loss.a))
  w.a.ats <- paste(games-(loss.h.ats + loss.a.ats),'-',(loss.h.ats+loss.a.ats))
  mov <- as.numeric(sum((away$pts1 - away$pts2) + (home$pts2 - home$pts1))/games)
  c(team = x, record = w.a, record.ats = w.a.ats, points.for = pf,points.against=pa,
    point.diff =pf-pa, pfats = pfats, paats = paats, avgspread = avgspread,atsdiff = atsdiff,
    mov = (pf-pa)/games, movats = (pfats - paats)/games)
})

l.summary <- as.data.frame(do.call(rbind, league.summary))

cols <- c("points.for","points.against","point.diff","pfats","paats","atsdiff")

l.summary[cols] <- sapply(l.summary[cols], function(x) {
  as.numeric(as.character(x))
})

write.csv(l.summary[order(l.summary$atsdiff, decreasing = TRUE),], 'week3-summary.csv', row.names = FALSE)

write.csv(df, 'pickem.csv', row.names = FALSE)
