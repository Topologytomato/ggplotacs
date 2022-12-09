# code of plot
# data preparation
library(readxl)
library(ggplot2)
library(tidyverse)
raw_data_pop <- read_excel("C:/Users/Jingy/×ÀÃæ/Jingyu/Fall 2022/MA675/BPDA Project/population data/all.population.xlsx")
list_variables_pop <- c("Pop", "Age", "Gender Distribution", "Marital Status" ,"Citizenship",
                        "Year of Entry", "Educational Attainment of population aged 25 or over", 
                        "Unemployment by Gender","Employment Type", "Employment by Industry",
                        "Employment by Occupation" ,"Poverty Status","Median Personal Earnings")

list_state <- read_excel("list_state.xlsx")

#####################################################################

# plot 1distribution map (gif)
# Variable type: 1 kind of type
# recommend variable: Pop
library(gganimate)
library(transformr)
library(gifski)
library(usmap)

# variables setting
country_need_pop <- "Colombia" # lower case or upper case both work
variables_need_pop <- "Pop"

# plot
data_need_pop <- raw_data_pop[grep(variables_need_pop, raw_data_pop$category), ] %>% 
  filter(tolower(country) == tolower(country_need_pop))
data_plot_pop <- pivot_longer(data_need_pop,cols = colnames(data_need_pop)[5:dim(data_need_pop)[2]])

#!!!!!
names(data_plot_pop)[5] <- "fips"
data_plot_pop$fips <- sprintf("%02d", as.numeric(data_plot_pop$fips))
data_plot_pop$year <- as.integer(data_plot_pop$year)
#!!!!!

#!!!!!
plot_pop1 <- plot_usmap(data = data_plot_pop, values = "value", color = "blue") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = variables_need_pop, label = scales::comma
  ) +
  theme(
    plot.title = element_text(size = 20L),
    legend.key.size = unit(1, 'cm'), #change legend key size
    legend.key.height = unit(1, 'cm'), #change legend key height
    legend.key.width = unit(1, 'cm'), #change legend key width
    legend.text = element_text(size = 14L),
    legend.position = "right") +
  labs(title = paste(variables_need_pop, country_need_pop, "in","Year:{frame_time}")) + 
  transition_time(year) +
  ease_aes("linear")
#!!!!

#!!!!
animate(plot_pop1, duration = 10, fps = 4, width = 800, height = 800, renderer = gifski_renderer())
#!!!!

anim_save(paste("type1", country_need_pop, variables_need_pop,".gif", sep = '_'))

##################################################################
# plot 2 barplot in n top states(gif)
# Variable type: both single or multiple
# recommend variables: all

library(gganimate)
library(transformr)
library(gifski)

# variables setting
country_need_pop <- "Argentina" # lower case or upper case both work
variables_need_pop <- "Marital Status"
n <- 5 # the states number you need, and the example is 5

# plot
population_data <- raw_data_pop[which(raw_data_pop$category == "Pop"),]

#!!!!
population_data <- pivot_longer(population_data,cols = colnames(raw_data_pop)[5:dim(raw_data_pop)[2]])
arrange_population_data <- population_data %>% 
  group_by(name) %>% summarize(sum = sum(value)) %>% arrange(sum)
#!!!!

state_used_pop <- arrange_population_data$name[1:n]

data_need_pop <- raw_data_pop[grep(variables_need_pop, raw_data_pop$category), ] %>% 
  filter(tolower(country) == tolower(country_need_pop))
data_plot_pop <- pivot_longer(data_need_pop,cols = colnames(data_need_pop)[5:dim(data_need_pop)[2]]) %>%
  filter(name %in% state_used_pop)

#!!!!!!!
names(data_plot_pop)[5] <- "fips"
data_plot_pop <- data_plot_pop %>% left_join(list_state, by = c("fips" = "FIPS Code"))
data_plot_pop$fips <- sprintf("%02d", as.numeric(data_plot_pop$fips))
#!!!!!!!

data_plot_pop$year <- as.integer(data_plot_pop$year)

#!!!!!!!
plot_pop2 <- ggplot(data_plot_pop, aes(fill=type, y=value, x=`State Name`)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(legend.position = "top") +
  labs(title = paste(variables_need_pop, country_need_pop, "in","Year:{frame_time}")) +
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

animate(plot_pop2, duration = 10, fps = 4, width = 1000, height = 600, renderer = gifski_renderer())
anim_save(paste("type2", country_need_pop, variables_need_pop,".gif", sep = '_'))

################################################################
# plot 5 bar plot with year
# Variable type: both single or multiple
# recommend variables: all

# variables setting
country_need_pop <- "Argentina" # lower case or upper case both work
variables_need_pop <- "Marital Status"

# plot
data_need_pop <- raw_data_pop[grep(variables_need_pop, raw_data_pop$category), ] %>% 
  filter(tolower(country) == tolower(country_need_pop))
data_plot_pop <- pivot_longer(data_need_pop,cols = colnames(data_need_pop)[5:dim(data_need_pop)[2]])
data_plot_pop$year <- as.integer(data_plot_pop$year)

plot_pop5 <- ggplot(data_plot_pop, aes(fill=type, y=value, x=year)) + 
  geom_bar(position="dodge", stat="identity") +
  theme(legend.position = "top") +
  labs(title = paste(variables_need_pop,"in", country_need_pop)) +
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

png(file= paste("type5", country_need_pop, variables_need_pop,".png", sep = '_'),
    width = 1000, height = 600)
plot_pop5
dev.off()

################################################################
# plot 6 line plot with year
# Variable type: both single or multiple
# recommend variables: all

# variables setting
country_need_pop <- "Argentina" # lower case or upper case both work
variables_need_pop <- "Marital Status"
state_need_pop <- "Colorado"

# plot
data_need_pop <- raw_data_pop[grep(variables_need_pop, raw_data_pop$category), ] %>% 
  filter(tolower(country) == tolower(country_need_pop))
data_plot_pop <- pivot_longer(data_need_pop,cols = colnames(data_need_pop)[5:dim(data_need_pop)[2]])
data_plot_pop$name <- state.name[as.numeric(data_plot_pop$name)]
data_plot_pop$year <- as.integer(data_plot_pop$year)
data_plot_pop <- data_plot_pop %>% filter(tolower(name) == tolower(state_need_pop))

plot_pop6 <- ggplot(data = data_plot_pop,aes(x=year, y=value, group = type, color=type, shape=type))+
  geom_point()+
  geom_line(linewidth = 1.2)+
  labs(title = paste(country_need_pop," ", variables_need_pop,"in", state_need_pop)) +
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
png(file= paste("type6", country_need_pop, variables_need_pop,state_need_pop,".png", sep = '_'),
    width = 1000, height = 600)
plot_pop6
dev.off()

