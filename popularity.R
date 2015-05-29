profile.3604 <- readRDS("profile_3604_20150526_0_1000.rds")
profile.4648 <- readRDS("profile_4648_20150526_0_1000.rds")


response.3604 <- profile.3604$response %>% filter(purchased.url != "http://www.alexandani.com")
purchases.3604 <- profile.3604$purchases
purchases.3604$date <- as.Date(as.POSIXct(as.numeric(purchases.3604$purchase.time)/1000, origin="1970-01-01"))

items.3604 <- profile.3604$items
items.3604. <- items.3604 %>% filter(url != "http://www.alexandani.com")
items.3604$date <- as.Date(as.POSIXct(as.numeric(items.3604$purchase.time)/1000, origin="1970-01-01"))


eval_df <- response.3604 %>%
filter(substr(purchased.url, nchar(purchased.url) - 3, nchar(purchased.url)) != '.com') %>%
  mutate(fold.id = dense_rank(profile.id) %% 10) %>% 
  sample_n(1000)


items.3604.latest <- items.3604 %>% filter(date >= max(items.3604$date) - 30 & url != "http://www.alexandani.com")
items.3604.latest.folds <- inner_join(eval_df, items.3604.latest, by="profile.id")

#output <- expand.grid(profile.id=eval_df$profile.id, url=response.3604$purchased.url)

predict.pop.fold <- function(fold, eval.data, purchases){
  users <- filter(eval.data, fold.id == fold)
  
  popularity <- purchases %>% 
                filter(fold.id != fold) %>%
                group_by(url) %>%
                summarize(purchases = n()) %>%
                arrange(desc(purchases)) %>%
                head(10)
  popularity$purchased.prob <- popularity$purchases/sum(popularity$purchases)
  popularity$fold.id = fold
  combined <- inner_join(users, popularity, by="fold.id") %>%
              mutate(did.purchase=ifelse(url == purchased.url, 1, 0)) %>%
              select(profile.id, url, fold.id, purchased.prob, did.purchase)
    
  return(combined)  
}

predict.pop.cheat <- function(eval.data, purchases){
  users <- eval.data
  users$fake = 1
  
  popularity <- purchases %>% 
    group_by(url) %>%
    summarize(purchases = n()) %>%
    arrange(desc(purchases))
  popularity$purchased.prob <- popularity$purchases/sum(popularity$purchases)
  popularity <- head(popularity, 10)
  popularity$fake = 1
  combined <- inner_join(users, popularity, by="fake") %>%
    mutate(did.purchase=ifelse(url == purchased.url, 1, 0)) %>%
    select(profile.id, url, fold.id, purchased.prob, did.purchase)
  
  return(combined)  
}



all.predictions <- lapply(0:9, predict.pop.fold,eval_df, items.3604.latest.folds) %>% bind_rows
all.predictions.cheat <- predict.pop.cheat(eval_df, items.3604.latest)

write.csv(all.predictions, "popularity_cv.csv", row.names=F)
write.csv(all.predictions.cheat, "popularity_cheat.csv", row.names=F)

