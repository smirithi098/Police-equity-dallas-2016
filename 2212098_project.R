setwd("S:/Data visualization")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(parsedate)
library(ggmap)
library(viridis)
library(plotly)
library(leaflet)


dallas_data <- read.csv("37-00049_UOF-P_2016_prepped.csv")
copy_data <- dallas_data

glimpse(dallas_data)

head(dallas_data)

######## Pre-processing #########

# format the date columns
date_format = "%d-%m-%y"

dallas_data <- dallas_data %>%
  mutate(INCIDENT_DATE = parse_date(INCIDENT_DATE),
         OFFICER_HIRE_DATE = parse_date(OFFICER_HIRE_DATE))

# Pivot the `type_of_force` columns into a single one
cols_to_pivot <- c("TYPE_OF_FORCE_USED1", "TYPE_OF_FORCE_USED2", "TYPE_OF_FORCE_USED3",
                   "TYPE_OF_FORCE_USED4", "TYPE_OF_FORCE_USED5", "TYPE_OF_FORCE_USED6",
                   "TYPE_OF_FORCE_USED7", "TYPE_OF_FORCE_USED8", "TYPE_OF_FORCE_USED9",
                   "TYPE_OF_FORCE_USED10")

dallas_data <- dallas_data %>%
  pivot_longer(cols = cols_to_pivot,
               names_to = "TYPE_OF_FORCE_USED",
               values_to = "FORCE_NAME")

# Remove rows with empty cells in `FORCE_NAME`
dallas_data <- dallas_data %>%
  filter(!(FORCE_NAME == ""))

# extract values of `force_effective` corresponding to force used
# function to extract the number from the string
f1 <- function(v1) {
  return(as.numeric(str_extract(v1, "\\d+")))
}

# function to split the string by ',' and access the value respective to extracted index
f2 <- function(v2, number) {
  return(str_trim(str_split(v2, ",")[[1]][number]))
}

# extract the number values from all the string values of `TYPE_OF_FORCE_USED`
index <- sapply(dallas_data[,c(37,38)]$TYPE_OF_FORCE_USED, f1, simplify = TRUE)

# get the correct values of force effective corresponding to the number got from the previous step
val_fe <- mapply(f2, dallas_data[,c(37,38)]$FORCE_EFFECTIVE, index)

# get the correct values of uof no. corresponding to the number got from the previous step
val_uof <- mapply(f2, dallas_data[,3]$UOF_NUMBER, index)
val_uof <- read.table(text=val_uof, col.names = c("UOF_ID"))

# add the extracted values back to the original dataset
dallas_data <- dallas_data %>%
  mutate(FORCE_EFFECTIVE = val_fe,
         UOF_NUMBER = val_uof$UOF_ID)

# fill in the missing latitudes and longitudes
lat_na_indices <- which(is.na(dallas_data$LOCATION_LATITUDE))

lat_lon <- geocode(dallas_data[lat_na_indices, ]$LOCATION_FULL_STREET_ADDRESS_OR_INTERSECTION)


dallas_data <- rbind(dallas_data %>%
  filter(is.na(LOCATION_LATITUDE)) %>%
  mutate(LOCATION_LATITUDE = lat_lon$lat,
         LOCATION_LONGITUDE = lat_lon$lon),
  dallas_data %>%
    filter(!is.na(LOCATION_LATITUDE))
)

# trim white spaces from all columns
dallas_data <- dallas_data %>%
  mutate(across(where(is.character), str_trim))


# Convert categorical columns to factor type from character
dallas_data <- dallas_data %>%
  mutate_at(c(5,6,9,10,11,13:18,23,34,35,37:38), as.factor)

# Drop non-relevant columns from the dataset
dallas_data <- dallas_data %>%
  select(-c('STREET_NUMBER', 'STREET_NAME', 'STREET_DIRECTION', 'STREET_TYPE',
            'LOCATION_CITY', 'LOCATION_STATE', 'NUMBER_EC_CYCLES'))

########### EDA #############

# calculate the proportion of officers gender wise
distribution_officer_gender <- dallas_data %>%
  group_by(OFFICER_GENDER) %>%
  summarise(Count = n()) %>%
  mutate(percentage_freq = (Count/sum(Count))*100)

# plot pie chart to show the distribution of officer gender
distribution_officer_gender %>%
  ggplot(aes(x="", y=Count, fill=OFFICER_GENDER)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  geom_text(aes(label=paste(round(percentage_freq, 2), "%")), 
            position = position_stack(vjust=0.45)) +
  theme_void() +
  scale_fill_manual(values = c("brown1", "dodgerblue1")) +
  labs(title = "Distribution of gender in officers")

# distribution of the number of female and male in each race
distribution_officer_race <- dallas_data %>%
  group_by(OFFICER_GENDER, OFFICER_RACE) %>%
  count() %>%
  arrange(desc(OFFICER_RACE))

# plot the distribution facet wrapping race for both genders
distribution_officer_race %>%
  ggplot(aes(x=reorder(OFFICER_RACE, -n), y=n, fill=OFFICER_GENDER)) +
  geom_bar(position = "dodge", stat = "identity") +
  geom_text(aes(label = n, vjust=-0.5, hjust=0.5), position = position_dodge(width=0.9),
            size=2.2) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top",
    legend.direction = "horizontal",
    axis.line = element_line(colour = "black")
  ) +
  scale_fill_manual(values = colorspace::diverge_hcl(2)) +
  scale_y_continuous(breaks = seq(0,10000,500)) +
  labs(title = "Count of officers per race for Male and Female",
       x = "Classes of race",
       y = "Count")


# plot the distribution between race and years on force
ggplotly(dallas_data %>%
  select(c("OFFICER_RACE", "OFFICER_YEARS_ON_FORCE")) %>%
  group_by(OFFICER_RACE, OFFICER_YEARS_ON_FORCE) %>%
  ggplot(aes(x=OFFICER_RACE, y=OFFICER_YEARS_ON_FORCE)) +
  geom_boxplot(show.legend = F, color="dodgerblue3", fill="dodgerblue3", alpha=0.35) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_y_continuous(breaks = seq(0, 45, 5)) +
  labs(title = "Distribution of years of experience in the force in each race",
       x = "Race", y = "Years on force"))


# distribution of subject types across race
subject_description_count <- dallas_data %>%
  filter(SUBJECT_RACE != "NULL") %>%
  group_by(SUBJECT_DESCRIPTION) %>%
  count() %>%
  arrange(desc(n))

# plot the frequency of subject descriptions
subject_description_count %>%
  ggplot(aes(x=reorder(SUBJECT_DESCRIPTION, n), 
             y=n,
             fill=ifelse(n > 900, "Highlighted", "Normal"))) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_fill_manual(name = "Count", values = c("royalblue3", "grey79")) +
  coord_flip() +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(title = "Frequency of types of subjects based on description",
       x = "Subject description", 
       y = "Count")

ggplotly(dallas_data %>%
  filter(SUBJECT_WAS_ARRESTED == "Yes") %>%
  group_by(INCIDENT_DATE) %>%
  count() %>%
  ggplot(aes(x=INCIDENT_DATE, y=n)) +
  geom_area(alpha=0.35, fill = "indianred3") +
  geom_line(color = "indianred3", group = 1) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(title = "Number of arrests over the time in 2016",
       x = "Date",
       y = "Number of arrests"))


to_match <- dallas_data %>%
  filter(SUBJECT_WAS_ARRESTED == "Yes") %>%
  distinct(OFFICER_ID, OFFICER_YEARS_ON_FORCE) %>%
  filter(OFFICER_ID != 0) %>%
  arrange(OFFICER_ID)

distinct_ids <- to_match[!duplicated(to_match$OFFICER_ID), ]

corr_arrested_exp <- cbind(
  dallas_data %>%
    group_by(OFFICER_ID, SUBJECT_WAS_ARRESTED) %>%
    summarise(count = n()) %>%
    filter(OFFICER_ID != 0, SUBJECT_WAS_ARRESTED == "Yes"),
  distinct_ids)

print(cor(corr_arrested_exp$count, corr_arrested_exp$OFFICER_YEARS_ON_FORCE))

plot_sc <- corr_arrested_exp %>%
  ggplot(aes(x=count, y=OFFICER_YEARS_ON_FORCE)) +
  geom_point(color = "rosybrown2") +
  geom_smooth(method = lm, color = "maroon", se = F) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  labs(title = "Correlation between years on force and the number of arrests",
       x = "Number of arrests",
       y = "Years of experience")
  
ggplotly(plot_sc)

plot_b <- dallas_data %>%
  filter(SUBJECT_OFFENSE %in% c("Assault/FV", "APOWW", "Warrant/Hold")) %>%
  group_by(SUBJECT_OFFENSE, TYPE_OF_FORCE_USED, SUBJECT_RACE) %>%
  count() %>%
  filter(SUBJECT_RACE != "NULL") %>%
  ggplot(aes(x=TYPE_OF_FORCE_USED, y=n, fill=SUBJECT_RACE)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(vars(SUBJECT_OFFENSE)) +
  scale_x_discrete(labels = c("Force1", "Force2", "Force3", "Force4", "Force5",
                              "Force6", "Force7", "Force8")) +
  scale_fill_manual(values = colorspace::diverge_hsv(4)) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  labs(title = "Number of forces used for 3 types of offenses on subjects based on race",
       x = "Forces used", y = "Count of subjects")

ggplotly(plot_b)


new_dallas_data <- dallas_data %>%
  mutate(day = format(INCIDENT_DATE, "%d"), month = format(INCIDENT_DATE, '%m'))


incident_count <- new_dallas_data %>%
  filter(SUBJECT_INJURY == "Yes") %>%
  group_by(day, month, INCIDENT_REASON) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(INCIDENT_REASON, month) %>%
  mutate(prop = round((n()/sum(count))*100, 2))

plot_bubble <- incident_count %>%
  mutate(text = paste("Day: ", day,
                      "\nMonth: ", month,
                      "\nIncident type: ", INCIDENT_REASON,
                      "\nCount: ", count,
                      "\nIncident proportion: ", prop)) %>%
  ggplot(aes(x=day, y=count, color=month, size = prop, text = text)) +
  geom_point(alpha=1) +
  theme(
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_color_viridis(discrete=TRUE, guide=FALSE) +
  labs(title = "Number of incidents reported per day with proportions of incident types",
       x = "Day",
       y = "Number of incidents")

ggplotly(plot_bubble, tooltip = "text")


location_data <- dallas_data %>%
  filter(!is.na(LOCATION_LATITUDE), SUBJECT_WAS_ARRESTED == "Yes") %>%
  group_by(SUBJECT_ID, LOCATION_LATITUDE, LOCATION_LONGITUDE, DIVISION) %>%
  summarise(count = n()) %>%
  filter(SUBJECT_ID != 0, DIVISION != "NULL")

my_palette <- colorFactor(palette = viridis(7),
                          domain = location_data$DIVISION,
                          na.color = "transparent")

leaflet(location_data) %>%
  setView(lng = -96.808891, lat = 32.779167, zoom = 10) %>%
  addProviderTiles(providers$Stamen.TonerLite) %>%
  addCircleMarkers(
    lng = ~LOCATION_LONGITUDE,
    lat = ~LOCATION_LATITUDE,
    fillColor = ~my_palette(location_data$DIVISION),
    fillOpacity = 0.8,
    color="white",
    radius=8,
    stroke=F,
    label=~paste("Arrested: ", count),
    popup=~paste("Arrested: ", count, "\nDIVISION: ", DIVISION)
  ) %>%
  addLegend(pal=my_palette, values = ~DIVISION, title = "Regions", 
            position = "bottomleft",
            opacity = 1)
  

subject_injury_on_force <- dallas_data %>%
  filter(SUBJECT_WAS_ARRESTED == "Yes") %>%
  group_by(SUBJECT_INJURY, FORCE_EFFECTIVE, TYPE_OF_FORCE_USED, SUBJECT_GENDER) %>%
  count()

ggplotly(subject_injury_on_force %>%
  filter(!is.na(TYPE_OF_FORCE_USED), SUBJECT_GENDER != "NULL", SUBJECT_GENDER != "Unknown") %>%
  ggplot(aes(x=reorder(TYPE_OF_FORCE_USED, n), y=n)) +
  geom_bar(aes(fill=SUBJECT_INJURY), 
           stat = "identity", 
           position = "dodge", 
           show.legend = F) +
  scale_x_discrete(labels = c("Force1", "Force2", "Force3", "Force4", "Force5",
                              "Force6", "Force7", "Force8", "Force9", "Force10")) +
  scale_fill_manual(values = c("palegreen3", "palevioletred3")) +
  coord_flip() +
  facet_wrap(vars(SUBJECT_GENDER), scales = "free") +
  theme_classic() +
  labs(title = "Subjects injured due to force used on each gender",
       x = "Force used",
       y = "Number of subjects injured")
)
