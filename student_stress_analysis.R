# Project Topic: Student Stress Analysis

# The program follows common R coding practices:
# logical organization of code
# meaningful and consistent naming conventions
# proper indentation and spacing
# basic error handling
# modular function design
# keeping line length readable 
# consistent syntax and formatting

# libraries
library(dplyr)
library(ggplot2)
library(gridExtra)

# load dataset (update file path as needed)
file_path <- "~/Documents/Sem/R programs/StressLevelDataset.csv"

# check file exists
if(!file.exists(file_path)){
  stop("Dataset file is not found! Check the file path.")
}

# read data
data <- read.csv(file_path)

# check dataset is not empty
if(nrow(data) == 0) {
  stop("Dataset is empty!")
}

# check required columns exist
required_columns <- c(
  "anxiety_level",
  "study_load", 
  "stress_level", 
  "sleep_quality", 
  "academic_performance", 
  "extracurricular_activities"
)
missing_columns <- setdiff(required_columns, colnames(data))
if (length(missing_columns) > 0) {
  stop(paste("Missing columns:", paste(missing_columns, collapse = ", ")))
}

# check missing values
if (any(is.na(data))) {
  warning("Dataset has missing values. Calculatins will ignore NA values.")
}

# view sample data
cat("\nDataset:\n")
head(data)

# view column names
cat("\nColumns:\n")
colnames(data) 

# view data types
cat("\nColumn Datatypes:\n")
str(data)

# select columns
selected_data <- data %>%
  select(
    anxiety_level,
    study_load, 
    stress_level, 
    sleep_quality, 
    academic_performance, 
    extracurricular_activities
  )

# check selected data
cat("\nSelected Data:\n")
head(selected_data) 

# custom data structures - dataset
create_data <- function(data) {
  structure(
    list(
      dataset = data,
      entries = nrow(data)
    ),
    class = "Data"
  )
}

# custom data structures - compute statistics
create_statistics <- function(data) {
  summary_stats <- data %>%
    summarise(
      # descriptive statistics - mean scores 
      avg_stress_level = mean(stress_level, na.rm = TRUE),
      avg_anxiety_level = mean(anxiety_level, na.rm = TRUE),
      avg_study_load = mean(study_load, na.rm = TRUE),
      avg_sleep_quality = mean(sleep_quality, na.rm = TRUE),
      avg_performance = mean(academic_performance, na.rm = TRUE),
      
      # correlation analysis - relationships b/w variables
      cor_anxiety_stress = cor(anxiety_level, stress_level, 
                               use = "complete.obs"),
      cor_study_stress = cor(study_load, stress_level, use = "complete.obs"),
      cor_sleep_performance = cor(sleep_quality, academic_performance, 
                                  use = "complete.obs")
    )
  
  # categorical analysis - frequency of extracurricular activities
  extracurricular_count <- data %>%
    count(extracurricular_activities, name = "count")
  
  stats_list <- as.list(summary_stats)
  stats_list$extracurricular_count <- extracurricular_count
      
  structure(stats_list, class = "Statistics")
}

# create objects
stress_details <- create_data(selected_data)
stress_stats <- create_statistics(selected_data)

# view dataset
cat("\nDataset:\n")
head(stress_details$dataset)

cat("\n ----- Statistical Analysis ----- \n")
cat("\nTotal Entries:", stress_details$entries, "\n")
cat("\nAverage Stress Level:", 
    sprintf("%.2f", stress_stats$avg_stress_level), "\n")
cat("\nAverage Anxiety Level:", 
    sprintf("%.2f", stress_stats$avg_anxiety_level), "\n")
cat("\nAverage Study Load:", 
    sprintf("%.2f", stress_stats$avg_study_load), "\n")
cat("\nAverage Sleep Quality:", 
    sprintf("%.2f", stress_stats$avg_sleep_quality), "\n")
cat("\nAverage Academic Performance:", 
    sprintf("%.2f", stress_stats$avg_performance), "\n")
cat("\nCorrelation between Anxiety Level and Stress Level:", 
    sprintf("%.2f", stress_stats$cor_anxiety_stress), "\n")
cat("\nCorrelation between Study Load and Stress Level:", 
    sprintf("%.2f", stress_stats$cor_study_stress), "\n")
cat("\nCorrelation between Sleep Quality and Academic Performance:", 
    sprintf("%.2f", stress_stats$cor_sleep_performance), "\n")
cat("\nFrequency of Extracurricular Activities:\n")
stress_stats$extracurricular_count %>%
  mutate(line = paste(extracurricular_activities, ":", count)) %>%
  pull(line) %>%
  cat(sep = "\n")
cat("\n")

# Data Visualization
# Chart 1: Anxiety by Stress Level
plot1 <- stress_details$dataset %>%
  ggplot(aes(x = factor(stress_level),y = anxiety_level)) +
  geom_boxplot(fill = "#9F86C0") +
  labs(
    title = "Anxiety by Stress Level",
    x = "Stress Level",
    y = "Anxiety Level"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#231942"),
    axis.title.x = element_text(color = "#231942"),
    axis.title.y = element_text(color = "#231942"),
    axis.text.x = element_text(color = "#231942"),
    axis.text.y = element_text(color = "#231942"),
    panel.grid.major = element_line(color = "#E7E7FF"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Chart 2: Study Load vs Stress Level
plot2 <- stress_details$dataset %>%
  ggplot(aes(x = factor(study_load), y = stress_level)) +
  geom_count(color = "#9F86C0", alpha = 0.8) +
  labs(
    title = "Study Load vs Stress Level",
    x = "Study Load",
    y = "Stress Level",
    size = "Students"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#231942"),
    axis.title.x = element_text(color = "#231942"),
    axis.title.y = element_text(color = "#231942"),
    axis.text.x = element_text(color = "#231942"),
    axis.text.y = element_text(color = "#231942"),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6),
    panel.grid.major = element_line(color = "#E7E7FF"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Chart 3: Academic Performance Distribution
plot3 <- stress_details$dataset %>%
  ggplot(aes(x = academic_performance)) +
  geom_density(fill = "#9F86C0", alpha = 0.4) +
  labs(
    title = "Academic Performance Distribution",
    x = "Academic Performance",
    y = "Density"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#231942"),
    axis.title.x = element_text(color = "#231942"),
    axis.title.y = element_text(color = "#231942"),
    axis.text.x = element_text(color = "#231942"),
    axis.text.y = element_text(color = "#231942"),
    panel.grid.major = element_line(color = "#E7E7FF"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# Chart 4: Extracurricular Activities Distribution
plot4 <- stress_details$dataset %>%
  ggplot(aes(x = factor(extracurricular_activities))) +
  geom_bar(fill = "#9F86C0", width = 0.6) +
  labs(
    title = "Extracurricular Activities Distribution",
    x = "Number of Activities",
    y = "Number of Students"
  ) +
  theme_minimal() + 
  theme(
    plot.title = element_text(face = "bold", size = 12, color = "#231942"),
    axis.title.x = element_text(color = "#231942"),
    axis.title.y = element_text(color = "#231942"),
    axis.text.x = element_text(color = "#231942"),
    axis.text.y = element_text(color = "#231942"),
    panel.grid.major = element_line(color = "#E7E7FF"),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 10, 10, 10)
  )

# display plots
grid.arrange(plot1, plot2, plot3, plot4, ncol = 2)


