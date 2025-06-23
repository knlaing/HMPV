
# RSV Case Data Retrieval and Visualization Pipeline --------------------

library(rvest)
library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)

# Load and clean data ----
Weekly_total_case_count <- read.csv("250409_Respir_Weekly_Samples_New_Haven.csv")

RSV_total_case_count <- Weekly_total_case_count %>%
  select(Week_Ending, RSV) # This is total RSV cases

Weekly_sample_inventory <- read.csv("250409_Sample_Inventory.csv")

Weekly_sample_inventory <- Weekly_sample_inventory[, c("Pathogen", "Collection_Date")]
Weekly_sample_inventory <- Weekly_sample_inventory[grepl("RSV", Weekly_sample_inventory$Pathogen, ignore.case = TRUE), ]

Weekly_sample_inventory <- Weekly_sample_inventory %>%
  mutate(Collection_Date = as.Date(Collection_Date)) %>%
  mutate(Week_Ending = Collection_Date + (6 - as.integer(format(Collection_Date, "%w"))))

Weekly_sample_counts <- Weekly_sample_inventory %>%
  count(Week_Ending, name = "Sample_Count") %>%
  filter(Week_Ending >= as.Date("2023-10-28") & Week_Ending <= as.Date("2025-01-18"))

RSV_total_case_count <- RSV_total_case_count %>%
  mutate(Week_Ending = as.Date(Week_Ending, format = "%Y.%m.%d")) %>%
  filter(Week_Ending >= as.Date("2023-10-28") & Week_Ending <= as.Date("2025-01-18"))

# Merge weekly data ----
colnames(Weekly_sample_counts)[2] <- "Inventory_Cases"
colnames(RSV_total_case_count)[2] <- "YNNH_Total_Cases"

combined_RSV_cases <- merge(RSV_total_case_count, Weekly_sample_counts, by = "Week_Ending", all = TRUE)
combined_RSV_cases[is.na(combined_RSV_cases)] <- 0
# Calculate sampling density & monthly aggregation ----
combined_RSV_cases <- combined_RSV_cases %>%
  mutate(Sampling_Density = ifelse(YNNH_Total_Cases == 0, 0, Inventory_Cases / YNNH_Total_Cases),
         Month = as.Date(paste0(format(Week_Ending, "%Y-%m"), "-01")))

monthly_case_counts <- combined_RSV_cases %>%
  group_by(Month) %>%
  summarise(
    YNNH_Total_Cases = sum(YNNH_Total_Cases, na.rm = TRUE),
    Inventory_Cases = sum(Inventory_Cases, na.rm = TRUE),
    Sampling_Density = sum(Inventory_Cases, na.rm = TRUE) / sum(YNNH_Total_Cases, na.rm = TRUE)
  ) %>%
  arrange(Month) %>%
  mutate(Sampling_Density_Smoothed = zoo::rollmean(Sampling_Density, k = 3, fill = NA, align = "center"))

# Calculate overall max for scaling
max_cases <- max(monthly_case_counts$YNNH_Total_Cases, na.rm = TRUE)

# Calculate average sampling density across the full dataset
avg_sampling_density <- mean(monthly_case_counts$Sampling_Density, na.rm = TRUE)

avg_df <- data.frame(
  avg_line = avg_sampling_density * max_cases,
  label = "Yearly Average Sampling Density"
)

# Plot ----
ggplot(monthly_case_counts, aes(x = Month)) +
  geom_col(aes(y = YNNH_Total_Cases, fill = "YNNH Total Cases"), position = "identity") +
  geom_col(aes(y = Inventory_Cases, fill = "Inventory Cases"), position = "identity", alpha = 0.7) +
  geom_line(aes(y = Sampling_Density_Smoothed * max_cases, color = "Monthly Sampling Density"),
            size = 1, na.rm = TRUE) +
  geom_hline(data = avg_df,
             aes(yintercept = avg_line, color = label),
             linetype = "dashed", size = 1) +
  scale_fill_manual(
    name = "Case Counts",
    values = c("YNNH Total Cases" = "lightblue", "Inventory Cases" = "darkblue")
  ) +
  scale_color_manual(
    name = "Sampling Density",
    values = c("Monthly Sampling Density" = "darkred", "Yearly Average Sampling Density" = "red")
  ) +
  scale_y_continuous(
    name = "Case Count",
    sec.axis = sec_axis(~ . / max_cases, name = "Sampling Density")
  ) +
  scale_x_date(
    expand = c(0, 0),
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = range(monthly_case_counts$Month),
    breaks = function(x) {
      x[!format(x, "%Y-%m") %in% c("2023-09", "2025-01", "2025-02")]
    }
  ) +
  labs(
    title = "RSV Sampling Density and Total Case Counts",
    x = "Month"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Save as SVG
if (!requireNamespace("svglite", quietly = TRUE)) install.packages("svglite")
ggsave("RSV_sampling_density.svg", plot = last_plot(), device = "svg", width = 10, height = 6)
