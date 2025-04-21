# YNNH Case Data Retrieval  -----------------------------------------------
library(rvest)
library(tidyverse)
library(dplyr)
library(lubridate)
library(tidyr)
library(ggplot2)
library(zoo)

# Pull / Clean data ----
Weekly_total_case_count <- read.csv("250409_Respir_Weekly_Samples_New_Haven.csv")

hMPV_total_case_count <- Weekly_total_case_count %>%
  select(Week_Ending, HMPV) # This is total hMPV cases

Weekly_sample_inventory <- read.csv("250409_Sample_Inventory.csv")

Weekly_sample_inventory <- Weekly_sample_inventory[, c("Pathogen", "Collection_Date")]
Weekly_sample_inventory <- Weekly_sample_inventory[grepl("hMPV", Weekly_sample_inventory$Pathogen, ignore.case = TRUE), ]

# Assign week ending to our sample inventory ----

Weekly_sample_inventory <- Weekly_sample_inventory %>%
  mutate(Collection_Date = as.Date(Collection_Date))

# Assign the next Saturday (week ending) for each date
Weekly_sample_inventory <- Weekly_sample_inventory %>%
  mutate(
    Week_Ending = Collection_Date + (6 - as.integer(format(Collection_Date, "%w")))
  )


# Count weekly samples in inventory
hMPV_inventory_case_counts <- Weekly_sample_inventory %>%
  count(Week_Ending, name = "Sample_Count") # This is our inventory hMPV cases








# BAR GRAPH by week ---------------------------------------------------------------



Weekly_sample_counts <- Weekly_sample_inventory %>%
  count(Week_Ending, name = "Sample_Count")

# Make sure both have proper Date formats
hMPV_total_case_count <- hMPV_total_case_count %>%
  mutate(Week_Ending = as.Date(Week_Ending, format = "%Y.%m.%d")) %>%
  filter(Week_Ending >= as.Date("2023-10-28") & Week_Ending <= as.Date("2025-01-18"))

Weekly_sample_counts <- Weekly_sample_inventory %>%
  count(Week_Ending, name = "Sample_Count") %>%
  filter(Week_Ending >= as.Date("2023-10-28") & Week_Ending <= as.Date("2025-01-18"))

ggplot() +
  # Total hMPV case counts (red)
  geom_col(
    data = hMPV_total_case_count,
    aes(x = Week_Ending, y = HMPV),
    fill = "red"
  ) +
  # Weekly sample counts (blue)
  geom_col(
    data = Weekly_sample_counts,
    aes(x = Week_Ending, y = Sample_Count),
    fill = "steelblue",
    alpha = 0.7
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(as.Date("2023-10-28"), as.Date("2025-01-18"))
  ) +
  labs(
    title = "Weekly hMPV Case Counts and Sample Submissions",
    x = "Week Ending",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# BAR GRAPH by month ------------------------------------------------------

# Create Month column (first day of the month)
hMPV_total_case_count_monthly <- hMPV_total_case_count %>%
  mutate(Month = floor_date(Week_Ending, unit = "month")) %>%
  group_by(Month) %>%
  summarise(HMPV = sum(HMPV), .groups = "drop")

Weekly_sample_counts_monthly <- Weekly_sample_counts %>%
  mutate(Month = floor_date(Week_Ending, unit = "month")) %>%
  group_by(Month) %>%
  summarise(Sample_Count = sum(Sample_Count), .groups = "drop")

# Plot monthly aggregated values
ggplot() +
  # Total case counts (red bars)
  geom_col(
    data = hMPV_total_case_count_monthly,
    aes(x = Month, y = HMPV),
    fill = "red"
  ) +
  # Sample counts (blue bars, semi-transparent)
  geom_col(
    data = Weekly_sample_counts_monthly,
    aes(x = Month, y = Sample_Count),
    fill = "steelblue",
    alpha = 0.7
  ) +
  scale_x_date(
    date_breaks = "1 month",
    date_labels = "%b %Y",
    limits = c(as.Date("2023-10-01"), as.Date("2025-01-31"))
  ) +
  labs(
    title = "Monthly hMPV Case Counts and Sample Submissions",
    x = "Month",
    y = "Count"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Merge weekly data ----
colnames(Weekly_sample_counts)[2] <- "Inventory_Cases"
colnames(hMPV_total_case_count)[2] <- "YNNH_Total_Cases"

combined_hMPV_cases <- merge(hMPV_total_case_count, Weekly_sample_counts, by = "Week_Ending", all = TRUE)
combined_hMPV_cases[is.na(combined_hMPV_cases)] <- 0




# Adding sampling density -------------------------------------------------


# Data 
combined_hMPV_cases <- combined_hMPV_cases %>%
  mutate(Sampling_Density = ifelse(YNNH_Total_Cases == 0, 0, Inventory_Cases / YNNH_Total_Cases)) %>%
  mutate(Month = format(Week_Ending, "%Y-%m")) %>%
  mutate(Month = as.Date(paste0(Month, "-01")))

# Aggregate months 
monthly_case_counts <- combined_hMPV_cases %>%
  group_by(Month) %>%
  summarise(
    YNNH_Total_Cases = sum(YNNH_Total_Cases, na.rm = TRUE),
    Inventory_Cases = sum(Inventory_Cases, na.rm = TRUE),
    Sampling_Density = sum(Inventory_Cases, na.rm = TRUE) / sum(YNNH_Total_Cases, na.rm = TRUE)
  ) %>%
  arrange(Month)

max_cases <- max(monthly_case_counts$YNNH_Total_Cases, na.rm = TRUE)

# average sampling density for the period October 2023 to October 2024
avg_sampling_density <- monthly_case_counts %>%
  filter(Month >= as.Date("2023-10-01") & Month <= as.Date("2024-10-01")) %>%
  summarise(avg_density = mean(Sampling_Density, na.rm = TRUE)) %>%
  pull(avg_density)

# Plot average sampling density
ggplot(monthly_case_counts, aes(x = Month)) +

  geom_col(aes(y = YNNH_Total_Cases, fill = "YNNH Total Cases"), position = "identity") +

  geom_col(aes(y = Inventory_Cases, fill = "Inventory Cases"), position = "identity", alpha = 0.7) +
 
  geom_hline(yintercept = avg_sampling_density * max_cases, linetype = "dashed", color = "red") +
  scale_fill_manual(
    name = "Case Counts",
    values = c("YNNH Total Cases" = "lightblue", "Inventory Cases" = "darkblue")
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
      # Example: remove some specific months if needed (adjust as appropriate)
      x[!format(x, "%Y-%m") %in% c("2023-09", "2025-01", "2025-02")]
    }
  ) +
  labs(
    title = "hMPV Density of Samples Processed vs Total Cases",
    x = "Month"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))




# Horizontal and monthly density ------------------------------------------



# Data 
combined_hMPV_cases <- combined_hMPV_cases %>%
  mutate(Sampling_Density = ifelse(YNNH_Total_Cases == 0, 0, Inventory_Cases / YNNH_Total_Cases)) %>%
  mutate(Month = format(Week_Ending, "%Y-%m")) %>%
  mutate(Month = as.Date(paste0(Month, "-01")))

# Aggregate months
monthly_case_counts <- combined_hMPV_cases %>%
  group_by(Month) %>%
  summarise(
    YNNH_Total_Cases = sum(YNNH_Total_Cases, na.rm = TRUE),
    Inventory_Cases = sum(Inventory_Cases, na.rm = TRUE),
    Sampling_Density = sum(Inventory_Cases, na.rm = TRUE) / sum(YNNH_Total_Cases, na.rm = TRUE)
  ) %>%
  arrange(Month) %>%

  
  mutate(Sampling_Density_Smoothed = zoo::rollmean(Sampling_Density, k = 3, fill = NA, align = "center"))

# Scaling factor to map sampling density 
max_cases <- max(monthly_case_counts$YNNH_Total_Cases, na.rm = TRUE)

# average sampling density for  October 2023 to October 2024
avg_sampling_density <- monthly_case_counts %>%
  summarise(avg_density = mean(Sampling_Density, na.rm = TRUE)) %>%
  pull(avg_density)


#  horizontal average line 
avg_df <- data.frame(
  avg_line = avg_sampling_density * max_cases, 
  label = "Yearly Average Sampling Density"
)

# Plot with bars, sliding window line, and horizontal average line
ggplot(monthly_case_counts, aes(x = Month)) +
  # Bars for case counts
  geom_col(aes(y = YNNH_Total_Cases, fill = "YNNH Total Cases"), position = "identity") +
  geom_col(aes(y = Inventory_Cases, fill = "Inventory Cases"), position = "identity", alpha = 0.7) +
  # Sliding window line for monthly sampling density 
  geom_line(aes(y = Sampling_Density_Smoothed * max_cases, color = "Monthly Sampling Density"), 
            size = 1, na.rm = TRUE) +
  # Horizontal dashed line for the average sampling density 
  geom_hline(data = avg_df, 
             aes(yintercept = avg_line, color = label), 
             linetype = "dashed", size = 1) +
  # Scale for case counts
  scale_fill_manual(
    name = "Case Counts",
    values = c("YNNH Total Cases" = "lightblue", "Inventory Cases" = "darkblue")
  ) +
  # scale for colors for sampling density lines
  scale_color_manual(
    name = "Sampling Density",
    values = c("Yearly Average Sampling Density" = "red", "Monthly Sampling Density" = "darkred")
  ) +
 
  scale_y_continuous(
    name = "Case Count",
    sec.axis = sec_axis(~ . / max_cases, name = "Sampling Density")
  ) +
  # Format x-axis with monthly breaks and date labels
  scale_x_date(
    expand = c(0, 0), 
    date_breaks = "1 month", 
    date_labels = "%b %Y",
    limits = range(monthly_case_counts$Month),
    breaks = function(x) {  
      # remove months 
      x[!format(x, "%Y-%m") %in% c("2023-09", "2025-01", "2025-02")]
    }
  ) +
  labs(
    title = "hMPV Sampling Density and Total Case Counts",
    x = "Month"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






# Save as SVG -------------------------------------------------------------

if (!requireNamespace("svglite", quietly = TRUE)) {
  install.packages("svglite")
}


ggsave("250410_hMPV_avg_sampling_density.svg", plot = last_plot(), device = "svg", width = 10, height = 6)
