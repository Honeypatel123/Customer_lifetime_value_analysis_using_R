install.packages("dplyr")
library(dplyr)
installed.packages("tidyr")
library(tidyr)
install.packages("plotly")
library(plotly)

config(plotly.template = list(
  layout = list(
    paper_bgcolor = "white",
    plot_bgcolor = "white"
  )
))
library(readr)

# Read the CSV file
data <- read_csv("C:\\Users\\Honey Patel\\Downloads\\acquisition_data\\customer_acquisition_data.csv")

# Print the first 5 rows of the data
print(head(data))

fig <- plot_ly(data, x = ~cost, nbinsx = 20, type = "histogram")

# Set the title
fig <- fig %>% layout(title = "Distribution of Acquisition Cost")

# Show the plot
fig

fig <- plot_ly(data, x = ~revenue, nbinsx = 20, type = "histogram")

# Set the title
fig <- fig %>% layout(title = "Distribution of Revenue")

# Show the plot
fig


cost_by_channel <- data %>%
  group_by(channel) %>%
  summarise(mean_cost = mean(cost)) %>%
  ungroup()

# Create the bar plot
fig <- plot_ly(cost_by_channel, x = ~channel, y = ~mean_cost, type = "bar")

# Set the plot title
fig <- fig %>%
  layout(title = "Customer Acquisition Cost by Channel")

# Display the plot
fig


conversion_by_channel <- data %>%
  group_by(channel) %>%
  summarise(mean_conversion_rate = mean(conversion_rate)) %>%
  ungroup()

# Create the bar plot
fig <- plot_ly(conversion_by_channel, x = ~channel, y = ~mean_conversion_rate, type = "bar")

# Set the plot title
fig <- fig %>%
  layout(title = "Conversion Rate by Channel")

# Display the plot
fig



# Calculate the revenue by channel
revenue_by_channel <- data %>%
  group_by(channel) %>%
  summarise(total_revenue = sum(revenue)) %>%
  ungroup()


# Group the data by channel and calculate the sum of revenue
revenue_by_channel <- data %>%
  group_by(channel) %>%
  summarize(total_revenue = sum(revenue))

# Create the donut pie chart
fig <- plot_ly(
  revenue_by_channel,
  labels = ~channel,
  values = ~total_revenue,
  type = "pie",
  hole = 0.6,
  insidetextfont = list(size = 12),
  textposition = "inside",
  textinfo = "value+percent",
  marker = list(colors = c("#B3E5FC", "#FFD1DC", "#FFB6C1", "#B2D8D8", "#AEEAEC"))
)

# Set the chart title and layout
fig <- fig %>%
  layout(
    title = list(
      text = "Total Revenue by Channel",
      font = list(
        family = "Arial",
        size = 18,
        color = "black"
      )
    ),
    paper_bgcolor = "white",
    plot_bgcolor = "white"
  )

# Display the chart
fig

# Calculate ROI
data$roi <- data$revenue / data$cost

# Calculate ROI by channel
roi_by_channel <- aggregate(roi ~ channel, data, mean)

# Plot ROI by channel
library(ggplot2)
ggplot(roi_by_channel, aes(x = channel, y = roi)) +
  geom_bar(stat = "identity") +
  labs(title = "Return on Investment (ROI) by Channel",
       x = "Channel",
       y = "ROI") +
  theme_minimal()

data$cltv <- (data$revenue - data$cost) * data$conversion_rate / data$cost

channel_cltv <- data %>%
  group_by(channel) %>%
  summarise(cltv = mean(cltv))

ggplot(channel_cltv, aes(x = channel, y = cltv, fill = channel)) +
  geom_col(width = 0.7, color = "white") +
  scale_fill_brewer(palette = "Pastel1") +
  labs(title = "Customer Lifetime Value by Channel",
       x = "Channel",
       y = "CLTV") +
  theme_minimal()


# Subset the data
subset <- data[data$channel %in% c("social media", "referral"), ]

# Create the box plot
library(ggplot2)
ggplot(subset, aes(x = channel, y = cltv, fill = channel)) +
  geom_boxplot() +
  labs(title = "CLTV Distribution by Channel",
       x = "Channel",
       y = "CLTV",
       fill = "Channel") +
  theme_minimal()