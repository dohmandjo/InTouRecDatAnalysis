
# Import Tidyverse package
install.packages("tidyverse")

# load tidyverse, readr, and scales packages to manipulate and build charts
library(tidyverse)
library(readr)
library(scales)

# Load the data
tourist <- read_csv("tourism-recipts.csv")
# Get information about the dataset 
# (number of rows and columns, datatypes for each column)
glimpse(tourist)
# Convert the "year" column to integer
tourist$year <- as.integer(tourist$year)
# View the data
tourist


# How have international tourism receipts changed over time? ####
# Analyze the trend in total receipts year by year and identify
# any significant patterns or fluctuations. 
# You can create visualizations such as line plots or bar charts
# to illustrate the changes over time.

# Create a plot to visualize revenue growth over the years 
tourist_plot<-ggplot(tourist, aes(x=year, y=`value_$`,color=`value_$`)) + 
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(title="Tourism Receipts Trend Income per Year",
       x = "Year", y= "Revenue In USD", color="Revenue In USD")

# Display the plot
tourist_plot


# Which countries or regions have the highest tourism receipts? ####
# Explore the dataset to identify the top countries or regions 
# that generate the most tourism receipts. You can calculate 
# the total receipts for each country/region and visualize 
# the results using a bar chart.

# Create a new dataframe that sums the revenue total by country and create
# a new column that hold the total revenue numbers not scientifically 
total_revenue_per_country <- tourist %>%
  group_by(code) %>%
  summarize(total_revenue=sum(`value_$`, na.rm = TRUE)) %>%
  mutate(normal_total_revenue=format(total_revenue,scientific= FALSE,big.mark=","))

# Create a dataframe with the revenue from 400,000,000,000
highest_total_revenue <- total_revenue_per_country %>%
  filter(total_revenue > 400000000000)
# Plot the revenue the highest_total_revenue by country
CountriesTotal_revenue_plot <- ggplot(highest_total_revenue,aes(x=code, y=total_revenue, color=code)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Countries With Highest Tourism Revenue", x="Country", y="Revenue in USD", color= "Country Code")

# Display the plot and add total revenue for each country on top of each bar vertically
CountriesTotal_revenue_plot + 
  geom_text(aes(label=normal_total_revenue), vjust=.5, angle = 90, hjust =-.02) +
  theme(axis.text.y = element_text(angle = 0))

# Ghana vs Ivory Coast Tourism Revenue ####

# Create a dataframe that filters out Ghana and Côte d'Ivoire tourism revenue
tourism_GHAvsCIV <- filter(tourist, name %in% c("Ghana","Côte d'Ivoire")) %>%
  mutate(normal_revenue=format(`value_$`,scientific=FALSE,big.mark=","))

# Create a plot that visually compares both countries revenue per year
GHAvsCIV_plot <- ggplot(tourism_GHAvsCIV,aes(x=year, y=`value_$`, color= name)) +
  geom_line() +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Tourism Revenue Ghana vs Côte d'Ivoire", 
       x="Year", y="Revenue In USD", color = "Countries") 
# Display chart
GHAvsCIV_plot

# Create a dataframe that sum total revenue for both countries
GHAvsCIVtotal_revenue <- tourist %>%
  filter(name %in% c("Ghana","Côte d'Ivoire")) %>%
  group_by(name) %>%
  summarize(total_revenue=sum(`value_$`, na.rm = TRUE)) %>%
  mutate(normal_total_revenue=format(total_revenue,scientific=FALSE,big.mark=","))

# Create plot for GHAvsCIVtotal_revenue dataframe
GHAvsCIVtotal_revenue_plot <- ggplot(GHAvsCIVtotal_revenue,aes(x=name, y=`total_revenue`)) +
  geom_bar(stat = "identity") +
  scale_y_continuous(labels = scales::comma) +
  labs(title = "Tourism Total Revenue Ghana vs Côte d'Ivoire", x="Country", y="Revenue In USD")

# Display the plot and add total revenue for each country on top of each bar horizontally 
GHAvsCIVtotal_revenue_plot + geom_text(aes(label=normal_total_revenue),vjust=-.5)

