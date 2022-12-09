# code of plot
# data preparation
library(readxl)
library(ggplot2)
library(tidyverse)
raw_data <- read_excel("/Users/willowwu/Downloads/all.households.xlsx")
list_variables <- c("Householder", "Ownership", "Family Household", 
                    "Family Poverty","Monthly Owner Costs","Monthly Gross Rent",
                    "Overcrowding", "Median Household Income")

list_state <- read_excel("list_state.xlsx")

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

#!!!!!
names(data_plot)[4] <- "fips"
data_plot$fips <- sprintf("%02d", as.numeric(data_plot$fips))
data_plot$year <- as.integer(data_plot$year)
#!!!!!

#!!!!!
plot1 <- plot_usmap(data = data_plot, values = "value", color = "red") + 
  scale_fill_continuous(
    low = "white", high = "red", name = variables_need, label = scales::comma
  ) +
  theme(
    plot.title = element_text(size = 20L),
    legend.key.size = unit(1, 'cm'), #change legend key size
    legend.key.height = unit(1, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.text = element_text(size = 14L),
    legend.position = "right") +
  labs(title = paste(variables_need, country_need, "in","Year:{frame_time}")) + 
  transition_time(year) +
  ease_aes("linear")
#!!!!

#!!!!
animate(plot1, duration = 10, fps = 4, width = 800, height = 800, renderer = gifski_renderer())
#!!!!

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
n <- 5 # the states number you need, and the example is 5

# plot
householder_data <- raw_data[which(raw_data$Type == "Householder"),]

#!!!!
householder_data <- pivot_longer(householder_data,cols = colnames(raw_data)[4:dim(raw_data)[2]])
arrange_householder_data<- householder_data %>% 
  group_by(name) %>% summarize(sum = sum(value)) %>% arrange(sum)
#!!!!

state_used <- arrange_householder_data$name[1:n]

data_need <- raw_data[grep(variables_need, raw_data$Type), ] %>% 
  filter(tolower(country) == tolower(country_need))
data_plot <- pivot_longer(data_need,cols = colnames(data_need)[4:dim(data_need)[2]]) %>%
  filter(name %in% state_used)

#!!!!!!!
names(data_plot)[4] <- "fips"
data_plot <- data_plot %>% left_join(list_state, by = c("fips" = "FIPS Code"))
data_plot$fips <- sprintf("%02d", as.numeric(data_plot$fips))
#!!!!!!!

data_plot$year <- as.integer(data_plot$year)

#!!!!!!!
plot2 <- ggplot(data_plot, aes(fill=Type, y=value, x=`State Name`)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(legend.position = "top") +
  labs(title = paste(variables_need, country_need, "in","Year:{frame_time}")) +
  theme_classic()+
  theme(
    plot.title = element_text(size = 20L),
    axis.title.y = element_text(size = 16L),
    axis.title.x = element_text(size = 16L),
    axis.text.x = element_text(size = 15L),
    axis.text.y = element_text(size = 15L),
    legend.text = element_text(size = 12L),
    legend.position = "top"
  ) + 
  transition_time(year) +
  ease_aes("linear")
#!!!!!!!!!!!!!!!!!

animate(plot2, duration = 10, fps = 4, width = 1000, height = 600, renderer = gifski_renderer())
anim_save(paste("type2", country_need, variables_need,".gif", sep = '_'))

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
data_plot$year <- as.integer(data_plot$year)

plot5 <- ggplot(data_plot, aes(fill=Type, y=value, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(legend.position = "top") +
  labs(title = paste(variables_need,"in", country_need)) +
  theme_classic()+
  #!!!!!!!!!!!1
  theme(
    plot.title = element_text(size = 24L),
    axis.title.y = element_text(size = 18L),
    axis.title.x = element_text(size = 18L),
    axis.text.x = element_text(size = 16L),
    axis.text.y = element_text(size = 16L),
    legend.key.size = unit(1, 'cm'), #change legend key size
    legend.key.height = unit(1, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.text = element_text(size = 16L),
    legend.position = "top"
  )
#!!!!!!!!!!!!!!!

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
  labs(title = paste(country_need," ", variables_need,"in", state_need)) +
  theme_classic()+
#!!!!!!!!!!!1
  theme(
    plot.title = element_text(size = 24L),
    axis.title.y = element_text(size = 18L),
    axis.title.x = element_text(size = 18L),
    axis.text.x = element_text(size = 16L),
    axis.text.y = element_text(size = 16L),
    legend.key.size = unit(2, 'cm'), #change legend key size
    legend.key.height = unit(2, 'cm'), #change legend key height
    legend.key.width = unit(2, 'cm'), #change legend key width
    legend.text = element_text(size = 16L),
    legend.position = "top"
  )
#!!!!!!!!!!!!!!!
png(file= paste("type6", country_need, variables_need,state_need,".png", sep = '_'),
    width = 1000, height = 600)
plot6
dev.off()

