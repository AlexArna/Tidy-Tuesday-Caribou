# Tidy Tuesday

# Loading the readr library
library(readr)
library(tidyverse)
library(lubridate)

# Importing the datasets from github website
individuals <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/individuals.csv')
locations <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-06-23/locations.csv')

# Exploring the data
str(individuals)
summary(individuals)
names(individuals) # 14 columns

str(locations)
summary(locations)
names(locations) # 7 columns

View(individuals) # 286 caribous
# same as individuals %>% View

# Using summarize(across) to calculate the mean without NAs
individuals %>% summarize(across(sex:study_site, list(~mean(!is.na(.)))))

# checking counts of pregnant and life_stage
individuals %>% count(pregnant) # lots of NAs
individuals %>% count(life_stage) # lots of NAs, around 25% data available
individuals %>% count(with_calf) # lots of NAs, around 25% data available
individuals %>% count(death_cause) # lots of unknowns
individuals %>% count(deploy_off_type, sort = TRUE)

individuals %>% filter(deploy_off_type=='dead') %>% count(death_cause, sort = TRUE)

# Graphing the counts based on latitude and longitude
individuals %>% count(study_site, deploy_on_longitude, deploy_on_latitude, sort = TRUE) %>% 
  ggplot(aes(deploy_on_longitude, deploy_on_latitude, size=n))+geom_point() 
# There is one outlier> bug? out of Canada

# Filtering to remove the outlier
individuals %>% filter(deploy_on_latitude>40) %>% count(study_site, deploy_on_longitude,
                                                        deploy_on_latitude, sort = TRUE) %>% 
  ggplot(aes(deploy_on_longitude, deploy_on_latitude, size=n))+geom_point()
# The graph showing the repartition of caribou by site looks more like Canada map

# Same graph with different colour by study_site 
individuals %>% filter(deploy_on_latitude>40) %>% count(study_site, 
                      deploy_on_longitude, deploy_on_latitude, sort = TRUE) %>% 
  ggplot(aes(deploy_on_longitude, deploy_on_latitude, size=n, color=study_site))+
  geom_point()

# theme_get() To get current theme

# Creating a map with theme_map 
individuals %>% filter(deploy_on_latitude>40) %>% count(study_site, 
                                                        deploy_on_longitude, deploy_on_latitude, sort = TRUE) %>% 
  ggplot(aes(deploy_on_longitude, deploy_on_latitude, size=n, color=study_site))+
  geom_point() + 
  ggthemes::theme_map()

# Removing the legend for the size (n) with scale_size_continuous(guide=FALSE)
individuals %>% filter(deploy_on_latitude>40) %>% count(study_site, 
                                                        deploy_on_longitude, deploy_on_latitude, sort = TRUE) %>% 
  ggplot(aes(deploy_on_longitude, deploy_on_latitude, size=n, color=study_site))+
  geom_point() + 
  scale_size_continuous(guide = FALSE)+
  ggthemes::theme_map()

# Adding world map borders with borders()

individuals %>% filter(deploy_on_latitude>40) %>% count(study_site, 
                                                        deploy_on_longitude, deploy_on_latitude, sort = TRUE) %>% 
  ggplot(aes(deploy_on_longitude, deploy_on_latitude, size=n, color=study_site))+
  borders('world', regions = 'canada')+
  geom_point() + 
  scale_size_continuous(guide = FALSE)+
  ggthemes::theme_map()

# The points are all grouped together, need to focus map on relevant area
# > didnt manage to import a province map, it's not a big deal

# Back to previous map 

# counting animal_id

individuals %>% count(animal_id, sort = TRUE) 
# duplicates for same animal_id > animal moving tracked at different time/site

individuals %>% filter(animal_id=='MO_car150') # same animal at different time

# Exploring locations 

# Plotting the location points based on longitude and latitude
locations %>% ggplot(aes(longitude, latitude, color=study_site))+ geom_point()

# Checking the number of points per animal from start of tracking to end
locations %>% group_by(animal_id, study_site) %>% 
  summarise(start=min(timestamp),end=max(timestamp), num_points=n()) %>% 
  ungroup() %>% arrange(desc(num_points))
            
# Exploring the data for a single animal randomly selected
locations %>% filter(animal_id==sample(animal_id, 1))

# Even better randomization with unique (to account for most frequent animals)
locations %>% filter(animal_id==sample(unique(animal_id), 1))

# Plotting with geom-path to get the animal's journey!!
locations %>% filter(animal_id==sample(unique(animal_id), 1)) %>% 
  ggplot(aes(longitude, latitude)) + geom_path()

# Even better if we arrange by timestamp first! 
# We store the particular animal into example_animal

example_animal <- locations %>% arrange(timestamp) %>% 
  filter(animal_id==sample(unique(animal_id), 1)) 

# Adding color based on timestamp (year)
example_animal %>% 
  ggplot(aes(longitude, latitude, color=year(timestamp))) +
  geom_point()+ # adding the points too
  geom_path()

# Plotting with factor()
example_animal %>% 
  ggplot(aes(longitude, latitude, color=factor(floor_date(timestamp, 'quarter')))) +
  geom_point()+ # adding the points too
  geom_path()+
  labs(color='Quarter', # to replace factor(floor_date(timestamp, 'quarter')) as label
  title='One caribou over time')

# Same as above without factor
example_animal %>% 
  ggplot(aes(longitude, latitude, color=floor_date(timestamp, 'quarter'))) +
  geom_point()+ 
  geom_path()+
  labs(color='Quarter', 
       title='One caribou over time')

# Same as above without floor_date and with alpha
example_animal %>% 
  ggplot(aes(longitude, latitude, color=timestamp)) +
  geom_point(alpha= .5)+ # alpha adds some transparency> see different points
  geom_path(alpha= .5)+
  labs(title='One caribou over time')

# Using quarter and facet_wrap
example_animal %>% 
  mutate(quarter=as.Date(floor_date(timestamp, 'quarter'))) %>% 
  ggplot(aes(longitude, latitude, color=timestamp)) +
  geom_point(alpha= .5)+ # alpha adds some transparency> see different points
  geom_path(alpha= .5)+
  facet_wrap(~ quarter)+
  labs(title='One caribou over time')
# We see where the caribou spent most of his time per quarter!

# Exploring the gap (time) between 2 timestamps

example_animal %>% arrange(timestamp) %>% 
  mutate(gap=round(difftime(timestamp, lag(timestamp), units = 'hours'))) %>% 
  count(gap, sort = TRUE) 
# there is an NA because of the 1st value

# Same as above without NA
example_animal %>% arrange(timestamp) %>% 
  mutate(gap=round(difftime(timestamp, lag(timestamp), units = 'hours'))) %>% 
  filter(!is.na(gap)) %>% 
  count(gap, sort = TRUE) 

# What about for all animals? Exploring gap/window as above

locations %>% arrange(timestamp) %>% 
  group_by(animal_id) %>% 
  mutate(gap=round(difftime(timestamp, lag(timestamp), units='hours'))) %>% 
  ungroup() %>% 
  filter(!is.na(gap)) %>% 
  count(gap, sort = TRUE) 

# Same as above but filtering window<=24 
# Plotting the results with a histogram

locations %>% arrange(timestamp) %>% 
  group_by(animal_id) %>% 
  mutate(gap=round(difftime(timestamp, lag(timestamp), units='hours'))) %>% 
  ungroup() %>% 
  filter(!is.na(gap)) %>% 
  #count(gap, sort = TRUE) %>% 
  filter(gap<=24) %>% 
  ggplot(aes(gap))+
  geom_histogram(binwidth = 2)

# The caribous are tracked for around 4-8 hours in general

# 1st attempt by David Robinson
locations %>% group_by(animal_id) %>% 
  mutate(last_longitude=lag(longitude), 
         last_latitude=lag(latitude),
    hours=as.numeric(difftime(timestamp, lag(timestamp), units = 'hours')),
    distance=sqrt((longitude-last_longitude)^2 +
      (latitude-last_latitude)^2),
    speed= distance/ hours) %>% View


# 2nd attempt with geosphere
library(geosphere)
locations_with_gaps <- 
locations %>% group_by(animal_id) %>% 
  mutate(last_lon=lag(longitude), 
         last_lat=lag(latitude),
         hours=as.numeric(difftime(timestamp, lag(timestamp), units = 'hours')),
         km=distHaversine(cbind(longitude, latitude), cbind(last_lon, last_lat))/1000,
         kph= km/ hours) %>% 
  ungroup

# Plotting (basic)

locations_with_gaps %>% 
  ggplot(aes(kph))+
  geom_histogram()+
  scale_x_log10()+
  labs(title = 'On average, how fast do caribous travel?')

# Plotting but filtering with hours (to remove sleeping caribous)

locations_with_gaps %>% 
  filter(hours<=8) %>% 
  ggplot(aes(kph))+
  geom_histogram()+
  scale_x_log10(labels=scales::comma)+ # to avoid kph expressed in 1e-03
  labs(title = 'On average, how fast do caribous travel?')

# By animal
by_animal <- locations_with_gaps %>% 
  group_by(animal_id, study_site) %>% 
  summarise(start=min(timestamp), end=max(timestamp), num_points=n(), 
            avg_speed=mean(kph[hours<=8], na.rm = TRUE)) %>% 
  ungroup() %>%
  arrange(desc(num_points))

# Checking distribution of average speed
by_animal %>% ggplot(aes(avg_speed))+geom_histogram()
# Most caribous move slowly, less than 0.5km ph

# Same as by_animal but filtering straight away to simplify
by_animal2 <- 
locations_with_gaps %>% 
  filter(hours<=8) %>% 
  group_by(animal_id, study_site) %>% 
  summarise(start=min(timestamp), end=max(timestamp), num_points=n(), 
            avg_speed=mean(kph, na.rm = TRUE)) %>% 
  ungroup() %>%
  arrange(desc(num_points))

# Plotting the result
by_animal2 %>% 
  filter(num_points>=10) %>% # at least 10 obs 
  ggplot(aes(num_points, avg_speed))+
  geom_point() +
  scale_x_log10() # changes scale of x axis to get first 1000 obs
# 2 groups are way faster than the crowd

# Which is the fastest caribou?
by_animal2 %>% filter(num_points>=10) %>% 
  arrange(desc(avg_speed))
# SC_car171 has 2684 obs over 3 years

locations_with_gaps %>% filter(animal_id=='SC_car171') %>% 
  arrange(desc(kph)) %>% View
# There seems to be an issue with the data. Multiple points with hours almost
# the same. Time between 2 points is just a fraction of a second

# Let's remove points/obs that are too close to each others with hours>=0.5

by_animal3 <- 
  locations_with_gaps %>% 
  filter(hours<=8, hours>=0.5) %>% 
  group_by(animal_id, study_site) %>% 
  summarise(start=min(timestamp), end=max(timestamp), num_points=n(), 
            avg_speed=mean(kph, na.rm = TRUE)) %>% 
  ungroup() %>%
  arrange(desc(num_points))

# Plotting the result
by_animal3 %>%filter(num_points>=10) %>% # at least 10 obs 
  ggplot(aes(num_points, avg_speed))+
  geom_point() +
  scale_x_log10() 
# There is no more outliers

# Using expand_limits(y=0) to expand y axis
by_animal3 %>%filter(num_points>=10) %>% # at least 10 obs 
  ggplot(aes(num_points, avg_speed))+
  geom_point() +
  scale_x_log10()+
  expand_limits(y=0)

# Fastest caribou?
by_animal3 %>% filter(num_points>=10) %>% 
  arrange(desc(avg_speed))
# QU_car107

# Tracking QU_car107

locations_with_gaps %>% filter(animal_id=='QU_car107') %>% 
  mutate(quarter=as.Date(floor_date(timestamp, 'quarter'))) %>% 
  ggplot(aes(longitude, latitude, color=timestamp)) +
  geom_point(alpha= .5)+ # alpha adds some transparency> see different points
  geom_path(alpha= .5)+
  facet_wrap(~ quarter)+
  labs(title='QU_car107 over time')

# Same but coloring based on speed

locations_with_gaps %>% filter(animal_id=='QU_car107') %>% 
  mutate(quarter=as.Date(floor_date(timestamp, 'quarter'))) %>% 
  ggplot(aes(longitude, latitude, color=kph)) +
  geom_point(alpha= .5)+ # alpha adds some transparency> see different points
  geom_path(alpha= .5)+
  facet_wrap(~ quarter)+
  labs(title='QU_car107 over time')

# Plotting speed of QU_car107
locations_with_gaps %>% filter(animal_id=='QU_car107') %>% 
  ggplot(aes(kph))+
  geom_histogram()+
  scale_x_log10()
# Median quite high, some extremes values

# Exploring speed per season (month)
locations_with_gaps %>% 
  filter(hours<=8, hours>=0.5) %>% 
  group_by(month=month(timestamp, label = TRUE)) %>% 
  summarise(avg_speed=mean(kph)) %>% 
  ggplot(aes(month, avg_speed, group=1))+
  geom_line()+
  expand_limits(y=0)+
  labs(title = 'Seasonal trend in caribou speed')

# animals seem slower in winter spring and faster in summer


# Speed by season and study site
locations_with_gaps %>% 
  filter(hours<=8, hours>=0.5) %>% 
  group_by(month=month(timestamp, label = TRUE), study_site) %>% 
  summarise(avg_speed=mean(kph), n=n()) %>% 
  ggplot(aes(month, avg_speed, group=study_site, color=study_site))+
  geom_line()+
  geom_point(aes(size=n))+
  expand_limits(y=0)+
  labs(title = 'Seasonal trend in caribou speed')

# Same as above but with facet_wrap and without legend


locations_with_gaps %>% 
  filter(hours<=8, hours>=0.5) %>% 
  group_by(month=month(timestamp, label = TRUE), study_site) %>% 
  summarise(avg_speed=mean(kph), n=n()) %>% 
  ggplot(aes(month, avg_speed, group=study_site, color=study_site))+
  geom_line()+
  geom_point(aes(size=n))+
  expand_limits(y=0)+
  facet_wrap(~ study_site)+
  theme(legend.position = 'none')+
  labs(y='Average speed (kph)', title = 'Seasonal trend in caribou speed')

