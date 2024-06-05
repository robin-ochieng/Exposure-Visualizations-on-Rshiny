# Load necessary libraries
library(ggplot2)
library(tidyr)
library(dplyr)
library(ggrepel)  # For better text placement

# Assuming Merged_Risk_Premium is your merged data frame
# Transforming data from wide to long format
data_long <- Merged_Risk_Premium %>%
  pivot_longer(
    cols = matches("Freq_|Sev_"),
    names_to = c(".value", "year"),
    names_pattern = "(Freq_|Sev_)(\\d+)"
  )

# Generate the plot with facets for each year
p <- ggplot(data_long, aes(x = Sev_, y = Freq_, color = Statutory_Class)) +
  geom_point(size = 2.5, alpha = 0.8) +  # Adjusted point size and transparency
  geom_text_repel(aes(label = Statutory_Class), size = 3, 
                  box.padding = unit(0.35, "lines"),
                  point.padding = unit(0.5, "lines")) +  # Improved label placement
  scale_color_viridis_d(begin = 0.2, end = 0.8, option = "D") +  # Using a diverging color scale
  facet_wrap(~year, scales = "free_y") +  # Allowing free scales if data range varies
  labs(
    title = "Scatter Plot of Severity vs Frequency by Year",
    x = "Severity",
    y = "Frequency"
  ) +
  theme_minimal() +
  theme(legend.position = "right",  # Adjust legend position
        plot.margin = unit(c(1, 1, 1, 1), "cm"),  # Adjust plot margin
        strip.text.x = element_text(size = 12, face = "bold"),  # Facet label styling
        axis.text = element_text(size = 10))  # Axis text styling

# Print the plot
print(p)
