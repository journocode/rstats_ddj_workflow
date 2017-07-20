########
# Head #
########
needs(readxl,tidyr,dplyr,ggplot2)

# Load the two data sheets into two different data variables:
# Example if it's in a CSV: file <- read.csv("file.csv", sep=",", dec=";", fileEncoding="latin1", na.strings="#")
age_male <- read_excel("age_data.xlsx", sheet=1)
age_female <- read_excel("age_data.xlsx", sheet=2)

################# 
# Preprocessing #
#################
head(age_male) # check out the first rows of the male age data
head(age_female) # check out the first rows of the female age data

# checking the number of rows
nrow(age_male)
nrow(age_female)
any(duplicated(age_female)) # checking for duplicates in age_female
age_female <- unique(age_female) # deleting duplicates
nrow(age_female) # check rownumber again

# merging
age_data <- merge(age_male, age_female[c(1,4)], by.x="district_id", by.y="dist_id")
head(age_data) # looks good!

# tidyverse
age_data <- gather(age_data, key=key, value=age, 4:5)
head(age_data) # this is what I call tidy!

############
# Analysis #
############
# Summarize the data to get the average age in Germany
age_data %>% summarize(mean=mean(age))
# Now group by the column key to get the average age for males and females in Germany
age_data %>% group_by(key) %>% summarize(mean=mean(age))
# In the same way, we can group by other columns, for example:
age_data %>% group_by(city_county) %>% summarize(mean=mean(age))


# Now calculate the average age per district but instead of summarizing, save the resulat in a new column.
# This time, save the new dataset as a variable in the R environment
age2 <- age_data %>% group_by(district_id) %>% mutate(district_mean=mean(age))
head(age2)
# We now want to find the youngest cities of Germany.
# We won't need the columns key and age for that. We deselect those rows and then reduce the dataset to the unique left rows
age2 %<>% select(-c(4,5)) %>% unique()
head(age2)
# Now filter to only retain the city data and arrange the dataset descending to the district_mean
youngest_cities <- age2 %>% filter(city_county %in% "city") %>% arrange(district_mean)
head(youngest_cities)
# Next, we only want to have a look at bavarian cities, whose district ids all start with "09". A great base function
# called startsWith() can easily find all district ids that start with certain entries:
youngest_cities %>% filter(startsWith(district_id, "09"))
# Let's find the oldest bavarian city
youngest_cities %>% filter(startsWith(district_id, "09")) %>% arrange(desc(district_mean))
# Let’s find the youngest city for every german state
youngest_cities %>% group_by(state=substr(district_id, 1, 2)) %>%
  filter(district_mean %in% min(district_mean)) %>% arrange(district_mean)

# ggplot choropleth
needs(rgdal,broom)
krs_shape <- readOGR(dsn="krs_shape/krs_shape_germany.shp", layer="krs_shape_germany", stringsAsFactors=FALSE, encoding="utf-8")
# untidy shape
head(krs_shape)
head(krs_shape@data)
head(krs_shape@polygons)
# tidy shape
head(tidy(krs_shape))

# get tidy shape ids
shape_district_ids <- as.numeric(krs_shape$KRS)
age2 %<>% arrange(match(as.numeric(district_id), shape_district_ids))
age2$id <- 0:401

plot_data <- merge(tidy(krs_shape), age2, by="id", all.x=T) %>% arrange(id) 
head(plot_data) # final plot data

# plot 
cols <- c("#e5f5e0","#c7e9c0","#a1d99b","#74c476","#41ab5d","#238b45", "#006d2c","#00441b") # set indiviual color scheme

ggplot(data=plot_data, aes(x=long, y=lat, group=group)) + # never forget the grouping aestethic!
  geom_polygon(aes(fill=district_mean)) +
  theme_void() + # clean background theme
  ggtitle("Average Age of Germanys districts") +
  theme(plot.title = element_text(face="bold", size=12, hjust=0, color="#555555")) +
  scale_fill_gradientn(colors=cols, space = "Lab", na.value = "#bdbdbd", name=" ") +
  coord_map() # change projection