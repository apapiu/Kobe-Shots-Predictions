library(ggplot2)
library(dplyr)
library(plotly)
load("data.Rdata")

train$shot_made_flag <- factor(train$shot_made_flag, levels = c("1", "0"))

pplot <- function(feat) {
    feat <- substitute(feat)
    ggplot(data = train, aes_q(x = feat)) +
        geom_bar(aes(fill = shot_made_flag), stat = "count", position = "fill") +
    scale_fill_brewer(palette = "Set1", direction = -1)
}

#time
pplot(minutes_remaining)
pplot(period)
pplot(seconds_remaining) #matters when it's zero
pplot(zerosecs)
pplot(season)
pplot(old)

#distance
pplot(shot_distance)#key
pplot(combined_shot_type)
pplot(shot_type)
pplot(shot_zone_area) #meh
pplot(shot_zone_basic) #important

pplot(opponent) #meh

pplot(action_type) + coord_flip()#important when bin smaller ones.

ggplot(data = train, aes(x = action_type)) +
    geom_bar(aes(fill = shot_made_flag), stat = "count") #position = "fill")


#cleveland/ accuracy line:
prop.table(table(train$action_type, train$shot_made_flag),1) -> temp
as.data.frame.matrix(temp) -> temp
temp$shot <- rownames(temp)
ggplot(temp, aes(x = reorder(shot, `1`), y = 1)) +
    geom_point(aes(y = `1`), size = 3, color = " dark blue", stat = "identity") +
    coord_flip()


prop.table(table(train$shot_zone_basic, train$shot_made_flag),1) -> temp
as.data.frame.matrix(temp) -> temp
temp$shot <- rownames(temp)
ggplot(temp, aes(x = reorder(shot, `1`), y = 1)) +
    geom_point(aes(y = `1`), size = 3, color = " dark blue", stat = "identity") +
    coord_flip()

prop.table(table(train$seconds_remaining, train$shot_made_flag),1) -> temp
as.data.frame.matrix(temp) -> temp
temp$shot <- rownames(temp)

ggplot(temp, aes(x = shot, y = `1`)) +
    geom_point()

# court stuff:
ggplotly(
#how to plot these on a grid
train %>% 
    filter(action_type != "Jump Shot") %>% 
ggplot(aes(x = lon, y = lat)) +
    geom_point(aes(color = shot_made_flag), alpha = 0.7, size = 4) +
    ylim(c(33.7, 34.0883)) +
    #scale_color_brewer(palette = "Dark2") +
    theme_void()
    #geom_hex(alpha = 0.8)
)

#and make the hexagons accuracy colors:
ggplot(train, aes(x = lon, y = lat)) +
    stat_bin2d(bins = 40)


