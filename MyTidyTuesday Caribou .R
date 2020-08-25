# MyTidyTuesday Caribou

# Importing the dataset
individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

# Loading the library
library(tidyverse)
library(lubridate)

# Exploring the datasets
summary(individuals)
names(individuals)
summary(locations)
names(locations)

# How many males/females?
individuals %>% group_by(sex) %>% View()

individuals %>% group_by(sex) %>% tally # 286 caribous?, only 4 males?
individuals %>% distinct(animal_id) %>% tally # 260 unique caribous
individuals %>% group_by(sex) %>% distinct(animal_id) %>% tally # 4 males
sum(is.na(individuals$sex)) # No NAs

individuals %>% filter(sex=='m') # The 4 males 
# BP_car022 KE_car026 NA_car055 BP_car115 

sum(is.na(individuals$with_calf)) #202 NAs
individuals %>% filter(with_calf==TRUE) %>% tally # 18 caribous with calf for sure
individuals %>% count(pregnant) # 16 pregnant 

locations %>% arrange(timestamp) %>% 
  filter(animal_id=='BP_car022')

# Locating caribous with calf
individuals %>% filter(deploy_on_latitude>40) %>% 
  count(with_calf, pregnant, study_site, deploy_on_longitude, deploy_on_latitude, sort = TRUE) %>% 
  ggplot(aes(deploy_on_longitude, deploy_on_latitude, size=n, 
             color=with_calf==TRUE))+
  geom_point()+
  ggthemes::theme_map()

# Makes more sense with color by study site
individuals %>% filter(deploy_on_latitude>40) %>% 
  count(study_site, deploy_on_longitude, deploy_on_latitude, 
        sort = TRUE) %>%
  ggplot(aes(deploy_on_longitude, deploy_on_latitude, size=n, color=study_site))+
  geom_point() + 
  ggthemes::theme_map()

locations %>% ggplot(aes(longitude, latitude, color=study_site)) + geom_point()
                      


