install.packages("readxl")
install.packages("data.table")

library(data.table)
library(readxl)
library(readr)
library(dplyr)
library(ggplot2)

maternalmortality <- read_excel("C:\\Users\\ilcb\\Documents\\DATASET_1_GLOBAL_MATERNAL_MORTALITY_1990_2011.xlsx")
femaleliteracy <- read_excel("C:\\Users\\ilcb\\Documents\\DATASET_2_ADULT_LITERACY_FEMALE.xlsx")

# Reconcile differences in column names and values
setnames(femaleliteracy, "Entity", "Country")

# Function to reconcile the verbiage for World and Global
global_df <- function(df) {
  df[] <- lapply(df, function(x) {
    if (is.character(x)) {
      return(gsub("World", "GLOBAL", x, ignore.case = TRUE))  # Replace in character columns
    } else {
      return(x)  # Leave non-character columns unchanged
    }
  })
  return(df)
}

femaleliteracy_mod <- global_df(femaleliteracy)

# Merge reconciled data sets
merged <- merge(maternalmortality, femaleliteracy_mod, by = c("Country", "Year"))

# Filter data to focus on relevant years
merged_filtered <- merged %>% 
  filter(Year >= 1990 & Year <= 2011)

# Create the scatter plot
scatter_plot <- ggplot(merged_filtered, aes(x = `Literacy rate, adult female (% of females ages 15 and above)`,
                                            y = `Maternal mortality ratio (MMR)`)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Maternal Mortality vs. Female Literacy Rates",
       x = "Female Literacy Rate (%)",
       y = "Maternal Mortality Ratio (MMR)") +
  theme_classic()

# Display the plot
scatter_plot

# Calculate average maternal mortality and female literacy rates by year
averages <- merged_filtered %>% 
  group_by(Year) %>% 
  summarise(avg_maternal_mortality = mean(`Maternal mortality ratio (MMR)`),
            avg_female_literacy = mean(`Literacy rate, adult female (% of females ages 15 and above)`))

# Create a time series plot
time_series_plot <- ggplot(averages, aes(x = Year)) +
  geom_line(aes(y = avg_maternal_mortality, color = "Maternal Mortality")) +
  geom_line(aes(y = avg_female_literacy, color = "Female Literacy")) +
  labs(title = "Average Maternal Mortality and Female Literacy Rates Over Time",
       x = "Year",
       y = "Rate") +
  scale_color_manual(name = "Indicator", values = c("blue", "red")) +
  theme_classic()

# Display the plot
time_series_plot