top_ <- function(x, y, top = 3){
  paste0(head(x, top), " (", round(100*head(y, top)/sum(y), 1), "%)", collapse = ", ")
}
top_categories <- function(d, cat1, cat2, var, top = 3){
  d$cat1 <- d[[cat1]]
  d$cat2 <- d[[cat2]]
  d$var <- d[[var]]
  tmp <- d[!is.na(cat1) & !is.na(cat2), .(
    Variable = sum(var, na.rm = T)
  ), keyby = .(cat1, cat2)][order(Variable, decreasing = T)]
  tmp <- tmp[, .(`Top` = top_(cat2, Variable, top))
             , keyby = cat1]
  setkey(tmp, "cat1")
  setnames(x = tmp, old = "cat1", new = cat1)
  setnames(x = tmp, old = "Top", new = paste("Top", cat2))
  tmp
}