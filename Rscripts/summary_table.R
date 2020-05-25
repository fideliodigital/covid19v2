
# source("top_categories.R")
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

summary_numerical <- function(d, cat1, var1){
  d$cat1 <- d[[cat1]]
  d$var1 <- d[[var1]]
  t1 <- d[!is.na(cat1), .(
    `min` = min(var1, na.rm = T)
    , `mean` = mean(var1, na.rm = T)
    , `median` = median(var1, na.rm = T)
    , `max` = max(var1, na.rm = T)
    , `sum` = sum(var1, na.rm = T)
  ), keyby = cat1]
  setnames(t1, old = "min", new = paste0("min_", var1))
  setnames(t1, old = "mean", new = paste0("mean_", var1))
  setnames(t1, old = "median", new = paste0("median_", var1))
  setnames(t1, old = "max", new = paste0("max_", var1))
  setnames(t1, old = "sum", new = paste0("sum_", var1))
  setnames(t1, old = "cat1", new = cat1)
  setkeyv(x = t1, cols = cat1)
  t1
}

summary_table <- function(d, category_, categoricals, numerical, order_by_ = "N", top = 3){ # variable ="payment_method"
  d$category_ <- d[[category_]]
  
  # Top Categorical Variables  
  t1 <- d[!is.na(category_), .N, keyby = category_]
  
  for(category in categoricals){ # category = "Departamento"
    t2 <- top_categories(d, cat1 = category_, cat2 = category, var = numerical, top = top)
    t1 <- t1[t2]
  }
  
  # Numerical variable
  t2 <- summary_numerical(d = d, cat1 = category_, var1 = numerical)
  t1 <- t1[t2]
  
  t1$order_by_ <- t1[[order_by_]]
  t1 <- t1[order(order_by_, decreasing = T)]
  t1$order_by_ <- NULL
  
  setnames(t1, old = "category_", new = category_)
  setkeyv(x = t1, cols = category_)
  t1
}

