library(dplyr)

data <- read.csv("data.csv", stringsAsFactors = FALSE)


sapply(data, function(x) {length(unique(x))})

data$team_id <- NULL
data$team_name <- NULL

data$DateTime <- scale(as.numeric(as.POSIXct(data$game_date)))

data$zerosecs <- 1*(data$seconds_remaining == 1)
data$lastmin <- 1*(data$minutes_remaining == 0)

data %>% count(action_type) %>%
    arrange(desc(n)) %>% filter(n < 20) -> actions
data$action_type[data$action_type %in% actions$action_type] <- "Other"

data$old <- 1*(data$season %in% c("2015-16", "2014-15")) #old kobe not as good
data$away <- 1*grepl("@", data$matchup)


data$ref_dist <- sqrt(data$loc_x^2 + data$loc_y^2)/10 #key
data$close <- 1*(data$ref_dist < 4)



train <- data[!is.na(data$shot_made_flag),]
test <- data[is.na(data$shot_made_flag),]


train$dist <- sqrt(abs(train$lat)*abs(train$lon))
test$dist <- sqrt(abs(test$lat)*abs(test$lon))

train$shot_made_flag <- as.factor(train$shot_made_flag)

save.image("data.Rdata")


