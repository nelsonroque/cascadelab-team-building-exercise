# Load necessary libraries
library(dplyr)

# Step 1: Load the questions file to get the correct attribute labels
load_attribute_labels <- function(file_path) {
  # Read the CSV file containing the labels
  df_labels <- read.csv(file_path)
  
  # Extract the "Label" column to use as attribute labels
  labels <- df_labels$Label
  
  return(labels)
}

# Step 2: Load and clean the Qualtrics data
load_and_clean_data <- function(file_path, labels) {
  # Read the CSV file (Qualtrics usually has metadata rows, so we skip the first 2 rows)
  df <- read.csv(file_path, skip = 2)
  
  # Use the second row for the concise labels
  concise_labels <- df[1, ]
  
  # Skip the first two rows (metadata and labels) to get the actual data
  df <- read.csv(file_path, skip = 3)
  
  # Assign the concise labels to the columns
  colnames(df) <- concise_labels
  
  # Select relevant columns based on the attribute labels (use select from tidyverse)
  df_clean <- df %>%
    select(any_of(labels)) %>%
    mutate(across(everything(), as.numeric))  # Convert all columns to numeric
  
  return(df_clean)
}

# Step 3: Define roles and calculate role scores
get_role_scores <- function(df, labels) {
  # Dynamically extract role-specific attributes from the Label list
  data_analyst_attributes <- c("Data Analysis", "Problem Solving", "Technical Skills", "Attention to Detail", "Adaptability")
  team_leader_attributes <- c("Leadership", "Communication", "Teamwork", "Creativity", "Innovation")
  
  # Ensure that we only select existing columns using `select()`
  analyst_scores <- df %>%
    select(any_of(data_analyst_attributes))  # Selects existing columns
  leader_scores <- df %>%
    select(any_of(team_leader_attributes))  # Selects existing columns
  
  # Calculate composite scores by averaging the columns for each role
  df <- df %>%
    rowwise() %>%
    mutate(
      `Data Analyst Score` = mean(c_across(all_of(data_analyst_attributes)), na.rm = TRUE),
      `Team Leader Score` = mean(c_across(all_of(team_leader_attributes)), na.rm = TRUE)
    )
  
  # Rank the team members based on their role scores
  df <- df %>%
    mutate(
      `Data Analyst Rank` = rank(-`Data Analyst Score`),  # Higher score gets a better rank
      `Team Leader Rank` = rank(-`Team Leader Score`)
    )
  
  # Select and return only the relevant columns (scores and ranks)
  return(df %>% select(`Data Analyst Score`, `Team Leader Score`, `Data Analyst Rank`, `Team Leader Rank`))
}

# Main execution
main <- function() {
  # File paths
  labels_file_path <- "questions.csv"
  data_file_path <- "your_qualtrics_file.csv"
  
  # Step 1: Load the attribute labels
  labels <- load_attribute_labels(labels_file_path)
  
  # Step 2: Load and clean the Qualtrics data
  attribute_data <- load_and_clean_data(data_file_path, labels)
  
  # Step 3: Compute role scores
  role_scores <- get_role_scores(attribute_data, labels)
  
  # Step 4: Output the role scores
  print(role_scores)
  
  # Optionally, save the role scores to a CSV file
  write.csv(role_scores, "role_scores.csv", row.names = FALSE)
}

# Run the main function
main()

# File paths
labels_file_path <- "questions.csv"
data_file_path <- "your_qualtrics_file.csv"

# Step 1: Load the attribute labels
labels <- load_attribute_labels(labels_file_path)

# Step 2: Load and clean the Qualtrics data
attribute_data <- load_and_clean_data(data_file_path, labels)
