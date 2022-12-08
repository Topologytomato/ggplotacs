# code of plot
# data preparation
library(readxl)
library(ggplot2)
library(tidyverse)
raw_data <- read_excel("/Users/willowwu/Downloads/all.households.xlsx")
list_variables <- c("Householder", "Ownership", "Family Household", 
                    "Family Poverty","Monthly Owner Costs","Monthly Gross Rent",
                    "Overcrowding", "Median Household Income")

#####################################################################

# plot 1distribution map (gif)
# Variable type: 1 kind of type
# recommend variable: Householder
library(gganimate)
library(transformr)
library(gifski)
library(usmap)

# variables setting
country_need <- "Argentina" # lower case or upper case both work
variables_need <- "Householder"

# plot
data_need <- raw_data[grep(variables_need, raw_data$Type), ] %>% 
  filter(tolower(country) == tolower(country_need))
data_plot <- pivot_longer(data_need,cols = colnames(data_need)[4:dim(data_need)[2]])
data_plot$name <- state.name[as.numeric(data_plot$name)]
data_plot$year <- as.integer(data_plot$year)
data_state <- statepop[,1:3]
data_plot_1 <- left_join(data_state, data_plot, by = c("full" = "name"))

plot1 <- plot_usmap(data = data_plot_1, values = "value", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = variables_need, label = scales::comma
  ) +
  theme(legend.position = "right") +
  labs(title = paste(variables_need, country_need, "in","Year:{frame_time}")) + 
  transition_time(year) +
  ease_aes("linear")

animate(plot1, duration = 5, fps = 20, width = 800, height = 800, renderer = gifski_renderer())
anim_save(paste("type1", country_need, variables_need,".gif", sep = '_'))

##################################################################
# plot 2 barplot in n top states(gif)
# Variable type: both single or multiple
# recommend variables: all

library(gganimate)
library(transformr)
library(gifski)

# variables setting
country_need <- "Argentina" # lower case or upper case both work
variables_need <- "Family Poverty"
n <- 10 # the states number you need, and the example is 10

# plot
householder_data <- raw_data[which(raw_data$Type == "Householder"),]
householder_data <- pivot_longer(householder_data,cols = colnames(data_need)[4:dim(data_need)[2]])
arrange_householder_data<- householder_data %>% 
  group_by(name) %>% summarize(sum = sum(value)) %>% arrange(sum)

state_used <- arrange_householder_data$name[1:n]

data_need <- raw_data[grep(variables_need, raw_data$Type), ] %>% 
  filter(tolower(country) == tolower(country_need))
data_plot <- pivot_longer(data_need,cols = colnames(data_need)[4:dim(data_need)[2]]) %>%
  filter(name %in% state_used)
data_plot$name <- state.name[as.numeric(data_plot$name)]
data_plot$year <- as.integer(data_plot$year)

plot2 <- ggplot(data_plot, aes(fill=Type, y=value, x=name)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(legend.position = "top") +
  labs(title = paste(variables_need, country_need, "in","Year:{frame_time}")) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 20L),
    axis.title.y = element_text(size = 13L),
    axis.title.x = element_text(size = 13L),
    legend.position = "top"
  ) + 
  transition_time(year) +
  ease_aes("linear")

animate(plot2, duration = 10, fps = 20, width = 1000, height = 600, renderer = gifski_renderer())
anim_save(paste("type2", country_need, variables_need,".gif", sep = '_'))

################################################################
# plot 3 distribution map (jpg)
library(usmap)

# variables setting
country_need <- "Argentina" # lower case or upper case both work
variables_need <- "Householder"
year_need <- "2005"

# plot
data_need <- raw_data[grep(variables_need, raw_data$Type), ] %>% 
  filter(tolower(country) == tolower(country_need))
data_plot <- pivot_longer(data_need,cols = colnames(data_need)[4:dim(data_need)[2]])
data_plot$name <- state.name[as.numeric(data_plot$name)]
data_plot$year <- as.integer(data_plot$year)
data_plot <- data_plot %>% filter(year == year_need)
data_state <- statepop[,1:3]
data_plot_3 <- left_join(data_state, data_plot, by = c("full" = "name"))

plot3 <- plot_usmap(data = data_plot_1, values = "value", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = variables_need, label = scales::comma
  ) +
  theme(legend.position = "right") +
  labs(title = paste(variables_need, country_need, "in",year_need))

png(file= paste("type3", country_need, variables_need,year_need, ".png", sep = '_'),
    width = 800, height = 800)
plot3
dev.off()

##################################################################
# plot 4 barplot in n top states(jpg)
# Variable type: both single or multiple
# recommend variables: all

# variables setting
country_need <- "Argentina" # lower case or upper case both work
variables_need <- "Family Poverty"
n <- 10 # the states number you need, and the example is 10
year_need <- "2018"

# plot
householder_data <- raw_data[which(raw_data$Type == "Householder"),]
householder_data <- pivot_longer(householder_data,cols = colnames(householder_data)[4:dim(householder_data)[2]])
arrange_householder_data<- householder_data %>% 
  group_by(name) %>% summarize(sum = sum(value)) %>% arrange(sum)
state_used <- arrange_householder_data$name[1:n]


data_need <- raw_data[grep(variables_need, raw_data$Type), ] %>% 
  filter(tolower(country) == tolower(country_need))
data_plot <- pivot_longer(data_need,cols = colnames(data_need)[4:dim(data_need)[2]]) %>%
  filter(name %in% state_used)
data_plot <- data_plot %>% filter(year == year_need)
data_plot$name <- state.name[as.numeric(data_plot$name)]
data_plot$year <- as.integer(data_plot$year)

plot4 <- ggplot(data_plot, aes(fill=Type, y=value, x=name)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(legend.position = "top") +
  labs(title = paste(variables_need, country_need, "in",year_need)) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 20L),
    axis.title.y = element_text(size = 13L),
    axis.title.x = element_text(size = 13L),
    legend.position = "top"
  )

png(file= paste("type4", country_need, variables_need, year_need, ".png", sep = '_'),
    width = 1000, height = 600)
plot4
dev.off()


################################################################
# plot 5 bar plot with year
# Variable type: both single or multiple
# recommend variables: all

# variables setting
country_need <- "Argentina" # lower case or upper case both work
variables_need <- "Family Poverty"

# plot
data_need <- raw_data[grep(variables_need, raw_data$Type), ] %>% 
  filter(tolower(country) == tolower(country_need))
data_plot <- pivot_longer(data_need,cols = colnames(data_need)[4:dim(data_need)[2]])
data_plot$name <- state.name[as.numeric(data_plot$name)]
data_plot$year <- as.integer(data_plot$year)


plot5 <- ggplot(data_plot, aes(fill=Type, y=value, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(legend.position = "top") +
  labs(title = paste(variables_need,"in", country_need)) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 20L),
    axis.title.y = element_text(size = 13L),
    axis.title.x = element_text(size = 13L),
    legend.position = "top"
)

png(file= paste("type5", country_need, variables_need,".png", sep = '_'),
    width = 1000, height = 600)
plot5
dev.off()

################################################################
# plot 6 line plot with year
# Variable type: both single or multiple
# recommend variables: all

# variables setting
country_need <- "Argentina" # lower case or upper case both work
variables_need <- "Family Poverty"
state_need <- "Colorado"

# plot
data_need <- raw_data[grep(variables_need, raw_data$Type), ] %>% 
  filter(tolower(country) == tolower(country_need))
data_plot <- pivot_longer(data_need,cols = colnames(data_need)[4:dim(data_need)[2]])
data_plot$name <- state.name[as.numeric(data_plot$name)]
data_plot$year <- as.integer(data_plot$year)
data_plot <- data_plot %>% filter(tolower(name) == tolower(state_need))

plot6 <- ggplot(data = data_plot,aes(x=year, y=value,group = Type, color=Type, shape=Type))+
  geom_point()+
  geom_line(linewidth = 1.2)+
  theme_classic()+
  theme(
    plot.title = element_text(size = 20L),
    axis.title.y = element_text(size = 16L),
    axis.title.x = element_text(size = 16L),
    legend.position = "top"
  )

png(file= paste("type6", country_need, variables_need,state_need,".png", sep = '_'),
    width = 1000, height = 600)
plot6
dev.off()

