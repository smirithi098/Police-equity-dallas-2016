setwd("S:/Data visualization")

library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(parsedate)
library(ggmap)
library(viridis)
library(ggforce)
library(plotly)

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

# separate the rows with multiple columns into individual rows
dallas_data <- dallas_data %>% 
  separate_longer_delim(UOF_NUMBER, delim = ",")

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

# extract the number values from all the string values of `TYPE_OF_FORCE_USED`
index <- sapply(dallas_data[,c(37,38)]$TYPE_OF_FORCE_USED, f1, simplify = TRUE)

# get the correct values corresponding to the number got from the previous step
val <- mapply(f2, dallas_data[,c(37,38)]$FORCE_EFFECTIVE, index)

# function to extract the number from the string
f1 <- function(v1) {
  return(as.numeric(str_extract(v1, "\\d+")))
}

# function to split the string by ',' and access the value respective to extracted index
f2 <- function(v2, number) {
  return(str_trim(str_split(v2, ",")[[1]][number]))
}

# add the extracted values back to the original dataset
dallas_data <- dallas_data %>%
  mutate(FORCE_EFFECTIVE = val)

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
  scale_fill_manual(values = c("plum3", "slategray3")) +
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
dallas_data %>%
  select(c("OFFICER_RACE", "OFFICER_YEARS_ON_FORCE")) %>%
  group_by(OFFICER_RACE, OFFICER_YEARS_ON_FORCE) %>%
  ggplot(aes(x=OFFICER_RACE, y=OFFICER_YEARS_ON_FORCE)) +
  geom_violin() +
  geom_sina(aes(color = OFFICER_RACE, group = OFFICER_RACE), 
            alpha=0.2, size = 0.6,
            show.legend = F) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_y_continuous(breaks = seq(0, 45, 5)) +
  labs(title = "Distribution of years of experience in the force in each race",
       x = "Race", y = "Years on force")

# calculate the count of subject gender in each race
subject_distribution_count <- dallas_data %>%
  filter(SUBJECT_GENDER != "NULL", SUBJECT_GENDER != "Unknown",
         SUBJECT_RACE != "NULL") %>%
  group_by(SUBJECT_GENDER, SUBJECT_RACE) %>%
  summarise(Count = n(), .groups = "drop")


# plot the distribution of subjects facet wrapping race for both genders
subject_distribution_count %>%
  ggplot(aes(x=SUBJECT_GENDER, y=Count, fill=SUBJECT_GENDER)) +
  geom_bar(position = "dodge", stat = "identity") +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.line = element_line(colour = "black")
  ) +
  scale_fill_manual(values = c("#FF6969", "#A6D0DD")) +
  facet_wrap(vars(SUBJECT_RACE), scales = "free") +
  labs(title = "Count of subjects per race for Male and Female",
       x = "Classes of race",
       y = "Count")

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
             fill=ifelse(n > 2500, "Highlighted", "Normal"))) +
  geom_bar(stat = "identity", show.legend = F) +
  scale_fill_manual(name = "Count", values = c("cornflowerblue", "grey65")) +
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

plot_l <- dallas_data %>%
  filter(SUBJECT_WAS_ARRESTED == "Yes") %>%
  group_by(INCIDENT_DATE, SUBJECT_RACE) %>%
  count() %>%
  ggplot(aes(x=INCIDENT_DATE, y=n, fill=SUBJECT_RACE, text=SUBJECT_RACE)) +
  geom_area(alpha=0.6) +
  theme(
    panel.background = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line = element_line(colour = "black")
  ) +
  scale_fill_brewer(palette = "Spectral") +
  labs(title = "Number of arrests over the time in 2016",
       x = "Date",
       y = "Number of arrests")

ggplotly(plot_l)

dallas_data %>%
  select(c("SUBJECT_ID", "REASON_FOR_FORCE", "FORCE_EFFECTIVE", "TYPE_OF_FORCE_USED",
           "SUBJECT_INJURY", "SUBJECT_GENDER", "SUBJECT_RACE")) %>%
  distinct() %>%
  group_by(SUBJECT_ID, SUBJECT_RACE) %>%
  summarise(Count = n())
  
# add_count(SUBJECT_ID, name = "forces_used_count")
