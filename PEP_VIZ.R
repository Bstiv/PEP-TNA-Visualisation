# CARDIOMETABOLIC DISEASES
#VISUALIZATION ON PREVENTION
colnames(Cleaned_Cadre_Specific_Data_Final_2)
# Load the ggplot2 package
library(ggplot2)

# Create a data frame with the benchmark scores
benchmark_data <- data.frame(
  Category = rep(c("Math (AIME'24)", "Science (GPQA)", "Coding (LCB Oct-Feb)"), each = 6),
  Model = rep(c("Grok-3", "Grok-3 mini", "Gemini-2 Pro", "DeepSeek-V3", "Claude 3.5 Sonnet", "GPT-4o"), times = 3),
  Score = c(52, 40, 36, 39, 16, 9,  # Math scores
            75, 65, 59, 65, 50, 34,  # Science scores
            57, 41, 40, 36, 34, 34)   # Coding scores
)

# Create the bar chart
ggplot(benchmark_data, aes(x = Category, y = Score, fill = Model)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  labs(title = "Benchmarks", x = "", y = "Score") +
  scale_fill_manual(values = c("Grok-3" = "#0000FF", "Grok-3 mini" = "#0000FF", 
                               "Gemini-2 Pro" = "#808080", "DeepSeek-V3" = "#808080", 
                               "Claude 3.5 Sonnet" = "#D3D3D3", "GPT-4o" = "#D3D3D3")) +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        axis.title = element_text(color = "white"),
        axis.text = element_text(color = "white"),
        legend.text = element_text(color = "white"))

# Optional: Adjust the legend order if needed
benchmark_data$Model <- factor(benchmark_data$Model, levels = c("Grok-3", "Grok-3 mini", "Gemini-2 Pro", "DeepSeek-V3", "Claude 3.5 Sonnet", "GPT-4o"))







# Load the ggplot2 package
library(ggplot2)

# Create a data frame where you can manually input your variables
benchmark_data<-Cadre_Specific_Data_Final_2
colnames(Cadre_Specific_Data_Final_2)
# Replace the example data below with your own categories, ratings, and percentages
benchmark_data <- data.frame(
  Category = c("P_Hypertension", "M_Hypertension", "C_Hypertension", "P_RHD", "M_RHD", "C_V", "P_Coronary_Artery_D", "M_Coronary_Artery_D", "C_Coronary_Artery_D", "P_arrhythmia",                                                                                                                                                                                                                        
               "M_Arrhythmia", "C_Arrhythmia", "P_Heart_failure", "M__Heart_failure", "C__Heart_failure", "P_Cerebrovascular_disease", "M__Cerebrovascular_disease", "C__Cerebrovascular_disease", "P_Peripheral_arterial_D", "M__Peripheral_arterial_D", "C__Peripheral_arterial_D", "P_Pericarditis", "M_Pericarditis",                                                                                                                                                                                                                      
               "C_Pericarditis", "P_ Congenital_heart_D", "M__ Congenital_heart_D", "C__ Congenital_heart_D", "P_Venous_thromboembolism", "M__Venous_thromboembolism", "C__Venous_thromboembolism", "P_Aortic disease", "M_Aortic_disease", "C__Aortic_disease"))
  
  Rating = c("Novice", "Advanced Beginner", "Competent", "Profecient", "Expert")
  Percentage = c(10, 25, 40, 15, 10,  # Example percentages for Category1 (sum to 100%)
                 5, 20, 35, 30, 10)   # Example percentages for Category2 (sum to 100%)


# Create the bar chart
ggplot(benchmark_data, aes(x = Category, y = Percentage, fill = Rating)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9), width = 0.8) +
  labs(title = "Performance Ratings", x = "", y = "Percentage (%)") +
  scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, by = 20)) +  # Set y-axis to 0-100% with 20% intervals
  scale_fill_manual(values = c("Novice" = "#FF6B6B", "Advanced Beginner" = "#4ECDC4", 
                               "Competent" = "#45B7D1", "Proficient" = "#96CEB4", 
                               "Expert" = "#FFEEAD")) +  # Custom colors for each rating
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, size = 16),
        axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank(),
        legend.position = "right",
        panel.background = element_rect(fill = "white"),  # Default to white background, adjust if needed
        plot.background = element_rect(fill = "white"),
        axis.title = element_text(color = "black"),
        axis.text = element_text(color = "black"),
        legend.text = element_text(color = "black"))

# Optional: Ensure Rating is a factor with the desired order
benchmark_data$Rating <- factor(benchmark_data$Rating, 
                                levels = c("Novice", "Advanced Beginner", "Competent", "Proficient", "Expert"))






















# Load Required Library
library(ggplot2)
library(dplyr)

colnames(Pep_prevention_2_part)

# Ensure correct number of rows (11 Management categories × 5 Opinion levels = 55)
data <- data.frame(
  Management = rep(c("Hypertension_p", "Rheumatic_Heart_Disease_p", "Rheumatic_Heart_Disease_p", 
                     "Arrhythmia_p", "Heart_failure_p", "Cerebrovascular_disease_p", 
                     "Peripheral_arterial_disease_p", "Pericarditis_p", "Congenital_heart_disease_p", 
                     "Venus_thromboembolism_p", "Aortic_disease_p"), each = 5),
  
  Opinion = rep(c(0, 1, 2, 3, 4), times = 11),  # Numeric ratings (0 = Novice, ..., 4 = Expert)
  
  # Ensure exactly 55 values (Fixed missing values and removed extra comma)
  Percentage = c(
    10, 20, 30, 25, 15, 15, 25, 30, 20, 10, 5, 20, 25, 35, 15, 
    10, 25, 30, 20, 15, 20, 25, 25, 20, 10, 30, 25, 20, 15, 10, 
    25, 30, 25, 15, 5, 35, 25, 20, 15, 5, 10, 15, 20, 25, 30,
    20, 25, 30, 15, 10, 5, 15, 20, 35, 25  # ✅ Now exactly 75 values
  )
)

# Verify the number of rows
print(nrow(data))  # Should print 55

# Convert Numeric Ratings to Factors with Descriptive Labels
data$Opinion <- factor(data$Opinion, 
                       levels = c(0, 1, 2, 3, 4), 
                       labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

data <- data %>% filter(!is.na(Management), !is.na(Opinion), !is.na(Percentage))

# 1️⃣ **Dodge Bar Chart (Side-by-Side Bars)**
ggplot(data, aes(x = Management, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Opinions on Prevention", 
       x = "Prevention", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2️⃣ **Horizontal Stacked Bar Chart (Without Percentage & Gender)**
ggplot(data, aes(x = Management, fill = Opinion)) +
  geom_bar(stat = "count", position = "stack") +
  coord_flip() +  # Flips the bar chart to horizontal
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention", 
       x = "Prevention") +
  scale_fill_brewer(palette = "Set3")

#Horizontal Stacked bar without percentages

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Remove the Gender column
df <- Pep_prevention_2_part %>% select(-Gender)

# Manually select 14 diseases for display
selected_diseases <- c("Hypertension_p", "Rheumatic_Heart_Disease_p", "Coronary_Artery_Diseases_p", 
                       "Arrhythmia_p", "Heart_failure_p", "Rheumatic_Heart_Disease_p", 
                       "Coronary_Artery_Diseases_p", "Heart_failure_p", 
                       "Cerebrovascular_disease_p", "Venus_thromboembolism_p", "Peripheral_arterial_disease_p",
                       "Pericarditis_p", "Aortic_disease_p", "Congenital_heart_disease_p")

# Filter the data set to include only selected diseases
df_selected <- Pep_prevention_2_part %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

# Create the stacked bar chart
ggplot(df_long, aes(y = Disease, fill = Opinion)) +  # Horizontal bars
  geom_bar(position = "fill") +  # Stacked proportionally
  labs(title = "Stacked Bar Chart of Selected Diseases",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability


# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
# Remove the Gender column
df <- Pep_prevention_2_part %>% select(-Gender)

# Manually select 14 diseases for display
selected_diseases <- c("Hypertension_p", "Coronary_Artery_Diseases_p", 
                       "Arrhythmia_p", "Heart_failure_p", "Rheumatic_Heart_Disease_p", 
                       "Coronary_Artery_Diseases_p", "Heart_failure_p", 
                       "Cerebrovascular_disease_p", "Venus_thromboembolism_p", "Peripheral_arterial_disease_p",
                       "Pericarditis_p", "Aortic_disease_p", "Congenital_heart_disease_p")

# Filter the data set to include only selected diseases
df_selected <- Pep_prevention_2_part %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

# Calculate proportions for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels
  labs(title = "Stacked Bar Chart of Selected Diseases with Percentages",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# By Gender
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
df <- Pep_prevention_2_part  # Keep Gender column

# Manually select 13 diseases for display (including Gender)
selected_diseases <- c("Gender", "Hypertension_p", "Rheumatic_Heart_Disease_p", "Coronary_Artery_Diseases_p", 
                       "Arrhythmia_p", "Heart_failure_p", 
                       "Coronary_Artery_Diseases_p", "Heart_failure_p", 
                       "Cerebrovascular_disease_p", "Venus_thromboembolism_p", "Peripheral_arterial_disease_p",
                       "Pericarditis_p", "Aortic_disease_p", "Congenital_heart_disease_p")

# Filter dataset to include only selected diseases
df_selected <- df %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = -Gender, names_to = "Disease", values_to = "Opinion")

# Convert Gender to factor with labels
df_long$Gender <- factor(df_long$Gender, levels = c(1, 2), labels = c("Male", "Female"))

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

# Calculate proportions for each category per disease & gender
df_counts <- df_long %>%
  group_by(Gender, Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Gender, Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars and facet by Gender
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels
  facet_wrap(~ Gender) +  # Side by side for Male & Female
  labs(title = "Stacked Bar Chart of Selected Diseases by Gender",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability


#VISUALIZATION ON MANAGEMENT
# Load Required Library
library(ggplot2)
library(dplyr)

colnames(Pep_prevention_2_part)

# Ensure correct number of rows (11 Management categories × 5 Opinion levels = 55)
data <- data.frame(
  Management = rep(c("Hypertension_m", "Rheumatic_Heart_Disease_m", "Rheumatic_Heart_Disease_m", 
                     "Arrhythmia_m", "Heart_failure_m", "Cerebrovascular_disease_m", 
                     "Peripheral_arterial_disease_m", "Pericarditis_m", "Congenital_heart_disease_m", 
                     "Venus_thromboembolism_m", "Aortic_disease_m"), each = 5),
  
  Opinion = rep(c(0, 1, 2, 3, 4), times = 11),  # Numeric ratings (0 = Novice, ..., 4 = Expert)
  
  # Ensure exactly 55 values (Fixed missing values and removed extra comma)
  Percentage = c(
    10, 20, 30, 25, 15, 15, 25, 30, 20, 10, 5, 20, 25, 35, 15, 
    10, 25, 30, 20, 15, 20, 25, 25, 20, 10, 30, 25, 20, 15, 10, 
    25, 30, 25, 15, 5, 35, 25, 20, 15, 5, 10, 15, 20, 25, 30,
    20, 25, 30, 15, 10, 5, 15, 20, 35, 25  # ✅ Now exactly 75 values
  )
)

# Verify the number of rows
print(nrow(data))  # Should print 55

# Convert Numeric Ratings to Factors with Descriptive Labels
data$Opinion <- factor(data$Opinion, 
                       levels = c(0, 1, 2, 3, 4), 
                       labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

data <- data %>% filter(!is.na(Management), !is.na(Opinion), !is.na(Percentage))

# 1️⃣ **Dodge Bar Chart (Side-by-Side Bars)**
ggplot(data, aes(x = Management, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Opinions on Management", 
       x = "Prevention", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2️⃣ **Horizontal Stacked Bar Chart (Without Percentage & Gender)**
ggplot(data, aes(x = Management, fill = Opinion)) +
  geom_bar(stat = "count", position = "stack") +
  coord_flip() +  # Flips the bar chart to horizontal
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention", 
       x = "Prevention") +
  scale_fill_brewer(palette = "Set3")

#Horizontal Stacked bar without percentages

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Remove the Gender column
df <- Pep_prevention_2_part %>% select(-Gender)

# Manually select 14 diseases for display
selected_diseases <- c("Hypertension_m", "Rheumatic_Heart_Disease_m", "Coronary_Artery_Diseases_m", 
                       "Arrhythmia_m", "Heart_failure_m", "Rheumatic_Heart_Disease_m", 
                       "Coronary_Artery_Diseases_m", "Heart_failure_m", 
                       "Cerebrovascular_disease_m", "Venus_thromboembolism_m", "Peripheral_arterial_disease_m",
                       "Pericarditis_m", "Aortic_disease_m", "Congenital_heart_disease_m")

# Filter the data set to include only selected diseases
df_selected <- Pep_prevention_2_part %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

# Create the stacked bar chart
ggplot(df_long, aes(y = Disease, fill = Opinion)) +  # Horizontal bars
  geom_bar(position = "fill") +  # Stacked proportionally
  labs(title = "Stacked Bar Chart of Selected Diseases",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#  With percentages 
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
# Remove the Gender column
df <- Pep_prevention_2_part %>% select(-Gender)

# Manually select 14 diseases for display
selected_diseases <- c("Hypertension_m", "Coronary_Artery_Diseases_m", 
                       "Arrhythmia_m", "Heart_failure_m", "Rheumatic_Heart_Disease_m", 
                       "Coronary_Artery_Diseases_m", "Heart_failure_m", 
                       "Cerebrovascular_disease_m", "Venus_thromboembolism_m", "Peripheral_arterial_disease_m",
                       "Pericarditis_m", "Aortic_disease_m", "Congenital_heart_disease_m")

# Filter the data set to include only selected diseases
df_selected <- Pep_prevention_2_part %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

# Calculate proportions for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels
  labs(title = "Stacked Bar Chart of Selected Diseases with Percentages",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# By Gender
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
df <- Pep_prevention_2_part  # Keep Gender column

# Manually select 13 diseases for display (including Gender)
selected_diseases <- c("Gender", "Hypertension_m", "Rheumatic_Heart_Disease_m", "Coronary_Artery_Diseases_m", 
                       "Arrhythmia_m", "Heart_failure_m", 
                       "Coronary_Artery_Diseases_m", "Heart_failure_m", 
                       "Cerebrovascular_disease_m", "Venus_thromboembolism_m", "Peripheral_arterial_disease_m",
                       "Pericarditis_m", "Aortic_disease_m", "Congenital_heart_disease_m")

# Filter dataset to include only selected diseases
df_selected <- df %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = -Gender, names_to = "Disease", values_to = "Opinion")

# Convert Gender to factor with labels
df_long$Gender <- factor(df_long$Gender, levels = c(1, 2), labels = c("Male", "Female"))

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

# Calculate proportions for each category per disease & gender
df_counts <- df_long %>%
  group_by(Gender, Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Gender, Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars and facet by Gender
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels
  facet_wrap(~ Gender) +  # Side by side for Male & Female
  labs(title = "Stacked Bar Chart of Selected Diseases by Gender",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#Control 

# Load Required Library
library(ggplot2)
library(dplyr)

colnames(Pep_prevention_2_part)

# Ensure correct number of rows (11 Management categories × 5 Opinion levels = 55)
data <- data.frame(
  Management = rep(c("Hypertension_c", "Rheumatic_Heart_Disease_c", "Rheumatic_Heart_Disease_c", 
                     "Arrhythmia_c", "Heart_failure_c", "Cerebrovascular_disease_c", 
                     "Peripheral_arterial_disease_c", "Pericarditis_c", "Congenital_heart_disease_c", 
                     "Venus_thromboembolism_c", "Aortic_disease_c"), each = 5),
  
  Opinion = rep(c(0, 1, 2, 3, 4), times = 11),  # Numeric ratings (0 = Novice, ..., 4 = Expert)
  
  # Ensure exactly 55 values (Fixed missing values and removed extra comma)
  Percentage = c(
    10, 20, 30, 25, 15, 15, 25, 30, 20, 10, 5, 20, 25, 35, 15, 
    10, 25, 30, 20, 15, 20, 25, 25, 20, 10, 30, 25, 20, 15, 10, 
    25, 30, 25, 15, 5, 35, 25, 20, 15, 5, 10, 15, 20, 25, 30,
    20, 25, 30, 15, 10, 5, 15, 20, 35, 25  # ✅ Now exactly 75 values
  )
)

# Verify the number of rows
print(nrow(data))  # Should print 55

# Convert Numeric Ratings to Factors with Descriptive Labels
data$Opinion <- factor(data$Opinion, 
                       levels = c(0, 1, 2, 3, 4), 
                       labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

data <- data %>% filter(!is.na(Management), !is.na(Opinion), !is.na(Percentage))

# 1️⃣ **Dodge Bar Chart (Side-by-Side Bars)**
ggplot(data, aes(x = Management, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Opinions on Control", 
       x = "Prevention", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2️⃣ **Horizontal Stacked Bar Chart (Without Percentage & Gender)**
ggplot(data, aes(x = Management, fill = Opinion)) +
  geom_bar(stat = "count", position = "stack") +
  coord_flip() +  # Flips the bar chart to horizontal
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention", 
       x = "Prevention") +
  scale_fill_brewer(palette = "Set3")

#Horizontal Stacked bar without percentages

# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(tidyverse)

# Remove the Gender column
df <- Pep_prevention_2_part %>% select(-Gender)

# Manually select 14 diseases for display
selected_diseases <- c("Hypertension_c", "Rheumatic_Heart_Disease_c", "Coronary_Artery_Diseases_c", 
                       "Arrhythmia_c", "Heart_failure_c", "Rheumatic_Heart_Disease_c", 
                       "Coronary_Artery_Diseases_c", "Heart_failure_c", 
                       "Cerebrovascular_disease_c", "Venus_thromboembolism_c", "Peripheral_arterial_disease_c",
                       "Pericarditis_c", "Aortic_disease_c", "Congenital_heart_disease_c")

# Filter the data set to include only selected diseases
df_selected <- Pep_prevention_2_part %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

# Create the stacked bar chart
ggplot(df_long, aes(y = Disease, fill = Opinion)) +  # Horizontal bars
  geom_bar(position = "fill") +  # Stacked proportionally
  labs(title = "Stacked Bar Chart of Selected Diseases",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#  With percentages 
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
# Remove the Gender column
df <- Pep_prevention_2_part %>% select(-Gender)

# Manually select 14 diseases for display
selected_diseases <- c("Hypertension_c", "Coronary_Artery_Diseases_c", 
                       "Arrhythmia_c", "Heart_failure_c", "Rheumatic_Heart_Disease_c", 
                       "Coronary_Artery_Diseases_c", "Heart_failure_c", 
                       "Cerebrovascular_disease_c", "Venus_thromboembolism_c", "Peripheral_arterial_disease_c",
                       "Pericarditis_c", "Aortic_disease_c", "Congenital_heart_disease_c")

# Filter the data set to include only selected diseases
df_selected <- Pep_prevention_2_part %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

# Calculate proportions for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels
  labs(title = "Stacked Bar Chart of Selected Diseases with Percentages",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# By Gender
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
df <- Pep_prevention_2_part  # Keep Gender column

# Manually select 13 diseases for display (including Gender)
selected_diseases <- c("Gender", "Hypertension_c", "Rheumatic_Heart_Disease_c", "Coronary_Artery_Diseases_c", 
                       "Arrhythmia_c", "Heart_failure_c", 
                       "Coronary_Artery_Diseases_c", "Heart_failure_c", 
                       "Cerebrovascular_disease_c", "Venus_thromboembolism_c", "Peripheral_arterial_disease_c",
                       "Pericarditis_c", "Aortic_disease_c", "Congenital_heart_disease_c")

# Filter dataset to include only selected diseases
df_selected <- df %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = -Gender, names_to = "Disease", values_to = "Opinion")

# Convert Gender to factor with labels
df_long$Gender <- factor(df_long$Gender, levels = c(1, 2), labels = c("Male", "Female"))

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

# Calculate proportions for each category per disease & gender
df_counts <- df_long %>%
  group_by(Gender, Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Gender, Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars and facet by Gender
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels
  facet_wrap(~ Gender) +  # Side by side for Male & Female
  labs(title = "Stacked Bar Chart of Selected Diseases by Gender",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability



colnames(Final_Cadre_Specific_Datav)




# COMPETENCY VISUALIZATION
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select 14 diseases for display
selected_diseases <- c("Ensure_optimal_pharmaceutical_operations...439", "Dispense_drug_prescriptions_related_to_CMDs", "Promote_rationale_use_CMD_medicines", 
                       "Adhere_to_professional_ethics_as_CMD_care\r\nteam", "Provide_education_on_CMDs_to_patients", "Identify_adverse_drug_reactions\r\ninteractions related to CMD medicines", 
                       "Provide_education_on_CMDs_to_patients", "Identify_adverse_drug_reactions\r\ninteractions related to CMD medicines", 
                       "Advise_CMD_patients_on_appropriate-storage_of_Meds\r\nmedicines", "Manage_the_inventory_for_CMD_medicines\r\ncommodities", "Identification_of_contra_ndications",
                       "Diabetic_patients_with_renal_failure", "Different categories of drugs", "Add reporting of ADRs (pharmacovigilance)")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))

# Create the stacked bar chart without NA values
ggplot(df_long, aes(y = Disease, fill = Opinion)) +  # Horizontal bars
  geom_bar(position = "fill") +  # Stacked proportionally
  labs(title = "Stacked Bar Chart of Selected Diseases (Without NA)",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# Percentages for Pharmacy Cadre Competency Ratings
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select 14 diseases for display
selected_diseases <- c("Ensure_optimal_pharmaceutical_operations...439", "Dispense_drug_prescriptions_related_to_CMDs", "Promote_rationale_use_CMD_medicines", 
                       "Adhere_to_professional_ethics_as_CMD_care\r\nteam", "Provide_education_on_CMDs_to_patients", "Identify_adverse_drug_reactions\r\ninteractions related to CMD medicines", 
                       "Provide_education_on_CMDs_to_patients", "Identify_adverse_drug_reactions\r\ninteractions related to CMD medicines", 
                       "Advise_CMD_patients_on_appropriate-storage_of_Meds\r\nmedicines", "Manage_the_inventory_for_CMD_medicines\r\ncommodities", "Identification_of_contra_ndications",
                       "Diabetic_patients_with_renal_failure", "Different categories of drugs", "Add reporting of ADRs (pharmacovigilance)")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not at ll", "Slightly", "Moderately", "Good", "Very Good"))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels with font size 2
  labs(title = "Pharmacy Cadre Curriculum Competency ratings",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability




# Ratings of how well instructors are performing skills (pharmacy)
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Ensure_optimal_pharmaceutical_operations...465", "Ensure\r\noptimal pharmaceutical care and safety for CMD patient", "Process\r\ndrug prescriptions related to CMDs", 
                       "Promote\r\nrationale use of CMD medicines", "Adhere\r\nto professional ethics as part of CMD care team...469", "Provide\r\neducation on CMDs to patients/clients...470", 
                       "Identify\r\nadverse drug reactions and potential drug interactions related to CMD medicines", "Advise\r\npatients with CMDs on appropriate storage of CMD medicines...472", 
                       "Manage\r\nthe inventory for CMD medicines and commodities...473", "Identify\r\ncontraindications...474", "Pharmacovigilance\r\n(report ADRs)...475")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not at all", "Slightly", "Moderately", "Good", "Very Good"))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Font size = 2
  labs(title = "Pharmacy Cadre Instructors Competency Ratings",
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability


colnames(Final_Cadre_Specific_Datav)

#Competency Rating for Medical laboratory 
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Diagnose_CMDs", "Cardiovascular_diseases...478", "Obesity...479", 
                       "Metabolic_Associated_Steatotic_Liver_Disease...480", "Renal_disease...481", "Diabetes...482", 
                       "Dyslipidemia...483", "Screening_CMDs", 
                       "Cardiovascular_disease", "Obesity...486", "Metabolic_Associated_Steatotic_Liver_Disease...487",
                       "Renal_disease...488", "Diabetes...489")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels with font size 2
  labs(title = "Medical Laboratory Cadre Curriculum Competency ratings",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability


#Skills (Instructors Medical Laboratory)
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Diagnose_and_monitor_CMDs", "Cardiovascular_diseases...525", "Obesity...526", 
                       "Metabolic_Associated_Steatotic_Liver_Disease...527", "Renal_disease...528")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels with font size 2
  labs(title = "Medical Laboratory Cadre Curriculum Instructor's Competency ratings",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#Instructors skills Competency rating CHAs
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Carry_out_community_health_education...544", "Explain_preventive_and_management_measures_CMDs", "Conduct_screening_for_CMDs...546", 
                       "Utilize_appropriate_health_records_tools...547", "Identify_patient_for_referrals...548", "Use_appropriate_performance_appraisal_tools.")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels with font size 2
  labs(title = "Instructors Curriculum Competency ratings",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#CHAs curriculum competence
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Carry_out_community_health_education...530", "Explain_preventive_measures_for_CMDs", "Conduct_screening_for_CMDs...532", 
                       "Utilize_appropriate_health_records_tools...533", "Identify_patient_for_referrals...534", "Use_appropriate_performance_appraisal_tools")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels with font size 2
  labs(title = "CHAs Cadre Curriculum Competency ratings",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# Curriculum competence clinical medicine
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Classify_CMDs_accordingly\r\nphysiological)...551", "Carry_out_a_clinical_assessment_of_patients", "Provide_appropriate_patient-centered_care\r\nCMDs.", 
                       "Conduct_patient_education_on_CMDs...554")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels with font size 2
  labs(title = "Clinical medicine Cadre Curriculum Competency ratings",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# Instructors skills Clinical medicine
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Classify_CMDs_accordingly\r\nphysiological)...561", "Carry_out_a_clinical_assessment_of_patients\r\nCMDs", "Provide_appropriate_patient-centred_care\r\nCMDs.", 
                       "Conduct_patient_education_on_CMDs...564")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels with font size 2
  labs(title = "Clinical medicine Cadre Instructors Competency ratings",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

colnames(Final_Cadre_Specific_Datav)


# Curriculum competence Nursing
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Identify_clients_at_risk_developing_CMDs", "Educate_counsel_patients_CMD_prevention_management", "Explain_pharmacotherapeutic_interventions_with_CMDs", "Carry_basic_diagnstic_tests _assess_patient_CMDs", "Take_part_multidisciplinary_approach i\r\nmanagement of CMDs", "Explain_the_nursing_process_managing_clients_CMDs", "Explain_pathophysiologic_pathways_CMDs", 
                       "Identify_patient_for_referrals...573")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels with font size 2
  labs(title = "Nursing Cadre Curriculum Competency ratings",
       x = "Proportion",
       y = "Disease",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# Nursing instructors competence rating
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Identify _risk_of_developing_CMDs", "Educate_CMD_prevention_and_management", 
                       "Explain_pharmacotherapeutic_interventions\r\nwith CMDs", "Carry_out_basic_diag0stic_tests", 
                       "Take_part _the_multidisciplinary_approach\r\nmanagement of CMDs", 
                       "Explain_the_nursing_process_in-managing_patients\r\nCMDs", 
                       "Explain_the_pathophysiologic_pathways_CMDs", "Identify_patient_for_referrals.")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))) %>%
  filter(!is.na(Opinion))  # Extra safety check to ensure no NA remains

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Font size = 2
  labs(title = "Nursing Cadre Instructors Competency Ratings",
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

colnames(Final_Cadre_Specific_Datav)
# Curriculum competence Nutrition
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Apply_diet_planning_managing_CMDs", "Apply_diet_therapy_managing_CMDs", 
                       "Conduct_follow_up_on_CMD_patients", "Develop_relevant_interventions_to_CMDs", 
                       "Diag0se_CMDs_correctly", "Use_special_diet_in_managing_CMDs", "Use_special_feeding_methods_managing_CMDs",  
                       "Provide_nutritional_education", 
                       "Refer_CMD_clients_to_health_facility", "Screen_for_CMDs_in_community")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))) %>%
  filter(!is.na(Opinion))  # Extra safety check to ensure no NA remains

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Font size = 2
  labs(title = "Nutrition Cadre Competency Ratings",
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#Instructors skills rating
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Apply diet planning in managing CMDs...617", "Develop relevant interventions to CMDs...621", 
                       "Diag0se CMDs correctly...622", "Provide nutrition education...623", 
                       "Refer CMD clients to health facility...624", "Screen for CMDs in the community...625", "Use special diet in managing CMDs...626",  
                       "Use special feeding methods in managing CMDs...627")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))) %>%
  filter(!is.na(Opinion))  # Extra safety check to ensure no NA remains

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Font size = 2
  labs(title = "Nutrition Cadre Instructors Competency Ratings",
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#SCOPE, ORGANIZATION & RELEVANCE
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Cardiovascular_scope", "Type II_Scope", "Kidney dis_Scope", "non-alcoholic fatty liv_Scope", "Dyslipidemia_Scope", ") Community health 1_Scope", "Biochemistry_Scope", "Medical Parasitology_Scope", "Clinical Pathology_Scope", "Clinical Pharmacology_Scope", "Medicine 1_Scope", 
                       "Clinical Pharmacology IV_Scope", "Medicine II_Scope", 
                       "Pediatric and Child health II_Scope", "Pediatric and Child health II_Scope", "Pediatrics and Child Health III_Scope",  
                       "Medicine IV_Scope", "Hyperosmolar hyperglycemic state_Scope")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))) %>%
  filter(!is.na(Opinion))  # Extra safety check to ensure no NA remains

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Font size = 2
  labs(title = "Topics Scope for Various Diseases",
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#ORGANIZATION
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Cardiovascular_organization", "Type II_Organization", "Kidney dis_Organization", "non-alcoholic fatty liv_Organization", "Dyslipidemia_Organization", "Community health 1_Organization", "Biochemistry_Organization", "Medical Parasitology_Organization", "Clinical Pathology_Organization", ") Clinical Pharmacology_Organization", "Medicine 1_Organization", 
                       "Clinical Pharmacology IV_Organization", "Medicine II_Organization", 
                       "Pediatric and Child health II_Organization", "Pediatric and Child health II_Organization", "Pediatrics and Child Health III_Organization",  
                       "Medicine IV_Organization", "Hyperosmolar hyperglycemic state_Organization")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))) %>%
  filter(!is.na(Opinion))  # Extra safety check to ensure no NA remains

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Font size = 2
  labs(title = "Topics Organization for Various Diseases",
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#RELEVANCE
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Cardiovascular_relevance", "Type II_Relevance", "Kidney dis_Relevance", "non-alcoholic fatty liv_Relevance", "Dyslipidemia_Relevance", "Community health 1_SRelevance", "Biochemistry_Relevance", "Medical Parasitology_Relevance", "Clinical Pathology_Relevance", ") Clinical Pharmacology_Relevance", "Medicine 1_Relevance", 
                       "Clinical Pharmacology IV_Relevance", "Medicine II_Relevance", 
                       "Pediatric and Child health II_Relevance", "Pediatric and Child health II_Relevance", "Pediatrics and Child Health III_Relevance",  
                       "Medicine IV_Relevance", "Hyperosmolar hyperglycemic state_Relevance")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))) %>%
  filter(!is.na(Opinion))  # Extra safety check to ensure no NA remains

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Font size = 2
  labs(title = "Topics Relevance for Various Diseases",
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability



#Individual Research Knowledge
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Understanding_research_process", "Identifying_viable_research_topics", "Designing_research_consider_available_resources", "Identifying_appropriate_data_collection_method", "Developing_data_collection_tools", "Collecting_data_using_variety_of_methods", "Conducting_data_analysis_and_interpretation", "Disseminating_research_findings", "Decision-making_processes_have_a_place_for_research", "Linking_research_findings_to _key_issues", 
                       "Applying_research_findings_to_your_own_practice", "Evaluating_the_quality_of_published_research")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))) %>%
  filter(!is.na(Opinion))  # Extra safety check to ensure no NA remains

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Font size = 2
  labs(title = "Individual Research Capacity",
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#Divergent Stacked Bar Chart
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select research competency categories
selected_diseases <- c("Understanding_research_process", "Identifying_viable_research_topics", 
                       "Designing_research_consider_available_resources", "Identifying_appropriate_data_collection_method", 
                       "Developing_data_collection_tools", "Collecting_data_using_variety_of_methods", 
                       "Conducting_data_analysis_and_interpretation", "Disseminating_research_findings", 
                       "Decision-making_processes_have_a_place_for_research", "Linking_research_findings_to _key_issues", 
                       "Applying_research_findings_to_your_own_practice", "Evaluating_the_quality_of_published_research")

# Filter the dataset to include only selected research skills
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Competency", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to Likert Scale labels
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))) %>%
  filter(!is.na(Opinion))  # Ensure no NA remains

# Compute counts and percentages for each category per competency
df_counts <- df_long %>%
  group_by(Competency, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Competency, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Competency) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Convert Opinion into a numerical score for diverging bars
df_counts <- df_counts %>%
  mutate(RatingScore = case_when(
    Opinion == "Strongly Disagree" ~ -2,
    Opinion == "Disagree" ~ -1,
    Opinion == "Neutral" ~ 0,
    Opinion == "Agree" ~ 1,
    Opinion == "Strongly Agree" ~ 2
  ))

# Create a Likert Diverging Bar Chart
ggplot(df_counts, aes(x = reorder(Competency, -RatingScore), y = Percentage, fill = Opinion)) +
  geom_col(position = "stack", width = 0.7) +  # Diverging bars
  coord_flip() +  # Flip the bars to make it horizontal
  scale_fill_manual(values = c("red", "orange", "gray", "lightblue", "blue")) +  # Custom diverging colors
  labs(title = "Individual Research Capacity",
       x = "Competency",
       y = "Percentage",
       fill = "Opinion") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select research competency categories
selected_diseases <- c("Understanding_research_process", "Identifying_viable_research_topics", 
                       "Designing_research_consider_available_resources", "Identifying_appropriate_data_collection_method", 
                       "Developing_data_collection_tools", "Collecting_data_using_variety_of_methods", 
                       "Conducting_data_analysis_and_interpretation", "Disseminating_research_findings", 
                       "Decision-making_processes_have_a_place_for_research", "Linking_research_findings_to _key_issues", 
                       "Applying_research_findings_to_your_own_practice", "Evaluating_the_quality_of_published_research")

# Filter the dataset to include only selected research skills
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and remove NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Competency", values_to = "Opinion") %>%
  drop_na(Opinion)  # Remove NA values

# Convert numeric values to Likert Scale labels
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree"))) %>%
  filter(!is.na(Opinion))  # Ensure no NA remains

# Compute counts and percentages for each category per competency
df_counts <- df_long %>%
  group_by(Competency, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Competency, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Competency) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Convert Opinion into a numerical score for diverging bars
df_counts <- df_counts %>%
  mutate(RatingScore = case_when(
    Opinion == "Strongly Disagree" ~ -2,
    Opinion == "Disagree" ~ -1,
    Opinion == "Neutral" ~ 0,
    Opinion == "Agree" ~ 1,
    Opinion == "Strongly Agree" ~ 2
  ))

# Create a Likert Diverging Bar Chart with Percentages
ggplot(df_counts, aes(x = reorder(Competency, -RatingScore), y = Percentage, fill = Opinion)) +
  geom_col(position = "stack", width = 0.7) +  # Diverging bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels
            position = position_stack(vjust = 0.5), size = 3, color = "white") +  # Add percentage labels
  coord_flip() +  # Flip the bars to make it horizontal
  scale_fill_manual(values = c("red", "orange", "gray", "lightblue", "blue")) +  # Custom diverging colors
  labs(title = "Individual Research Capacity: Likert Scale Diverging Chart",
       x = "Competency",
       y = "Percentage",
       fill = "Opinion") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

colnames(Final_Cadre_Specific_Datav)
colnames(Final_Cadre_Specific_Datav) %>% tail(100)

#OBL Delivery 
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Authoring OBL appropriate content", "Converting written content to SCORM files", "Creating links to access resources external to OBL", "Navigating a Learning Management System", "Uploading videos to the LMS", "Using collaborative tools to enrich OBL delivery", "Creating users in an LMS", "Adding activities to an LMS", "Adding resources to an LMS", "Creating an online lesson", 
                       "Conducting webinars with OBL learners", "Planning synchro0us teaching sessions", "Creating WIKIS in the LMS", "Creating discussion forums for OBL learners", "Using social media to enrich interactions with learners")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Not proficient", "Slightly proficient", "Moderately proficient", "Very proficient", "Extremely proficient"))) %>%
  filter(!is.na(Opinion))  # Extra safety check to ensure no NA remains

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Font size = 2
  labs(title = "OBL Proficiency",
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#CONTINUATION
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Creating videos for  OBL", "Editing Learning Videos", "Identifying credible digital/electronic references for OBL learners", "Setting exams for OBL students", "Proctoring an online exam", "Grading exams for OBL students", "Coaching OBL students", "Tracking learner progress in OBL environment", "Counselling of OBL learners", "Motivating OBL learners", 
                       "Evaluating an OBL learner", "Constructive feedback to learners", "Evaluating an OBL program", "Flipped Classroom", "Problem based Learning", "Gamification", "Peer teaching", "Building a sense of community among online blended learners")

# Filter the dataset to include only selected diseases
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Not proficient", "Slightly proficient", "Moderately proficient", "Very proficient", "Extremely proficient"))) %>%
  filter(!is.na(Opinion))  # Extra safety check to ensure no NA remains

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Font size = 2
  labs(title = "OBL Proficiency",
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability



colnames(Patient_Data_1_)

# PATIENT TOOL VISUALIZATION
#  With percentages 
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set

# Manually select 14 diseases for display
patient_satisfaction <- c("hospital_has_modern_equipment", "hospital_building_are_visually_a", 
                       "hospital_staff_clean_and_wellgro", "patient_rooms_comfortable_and_a", "hospital_performs_services_corre", 
                       "hospital_services_provided_withi", "hospital_submits_legible_reports", 
                       "hospital_staff_show_sincere_inte", "doctors_nurses_explain_condition", "hospital_staff_inform_patients_s",
                       "Staff_willing_tohelp_patients", "hospital_admission_waiting_time_", "hospital_daily_services_waiting_", "hospital_staff_polite_and_courte", "health_staff_competent_in_handli", "patients_feel_confident_and_secu", "hospital_provides_adequate_priva", "health_staff_have_knowledge_to_a", "hospitals_operate_at_suitable_ti", "doctors_nurses_listen_attentivel", "hospital_has_staff_to_assist_pat", "staff_understand_specific_needs_", "doctors_nurses_spend_enough_time")

# Filter the data set to include only selected diseases
df_selected <- Patient_Data_1_ %>% select(all_of(patient_satisfaction))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Service_Quality", values_to = "Opinion")

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Strongly Disagree", "Disagree", "Neither", "Agree", "Strongly Agree"))

# Calculate proportions for each category per disease
df_counts <- df_long %>%
  group_by(Service_Quality, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Service_Quality) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Service_Quality, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels
  labs(title = "Stacked Bar Chart of patient perception of the quality of service",
       x = "Proportion",
       y = "Service_Quality",
       fill = "Responses") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# By Level
library(ggplot2)
library(dplyr)
library(tidyr)

# Check if 'facility_level' exists in the dataset
if (!"facility_level" %in% colnames(Patient_Data_1_)) {
  stop("Column 'facility_level' not found. Check column names using colnames(Patient_Data_1_)")
}

# Check available levels
print(table(Patient_Data_1_$facility_level))  # See existing levels

# Define the levels to be plotted separately
levels_to_plot <- c("Level 4", "Level 5", "Level 6")

# Loop through each level and generate a separate plot
for (lvl in levels_to_plot) {
  
  # Filter data for the specific level
  df_selected <- Patient_Data_1_ %>%
    filter(facility_level == lvl) %>%
    select(all_of(patient_satisfaction), facility_level)
  
  # Convert data to long format
  df_long <- df_selected %>%
    pivot_longer(cols = all_of(patient_satisfaction), 
                 names_to = "Service_Quality", 
                 values_to = "Opinion")
  
  # Convert satisfaction scores into factor labels
  df_long$Opinion <- factor(df_long$Opinion, 
                            levels = c(1, 2, 3, 4, 5), 
                            labels = c("Strongly Disagree", "Disagree", "Neither", "Agree", "Strongly Agree"))
  
  # Calculate proportions for each category per service quality grouped by level
  df_counts <- df_long %>%
    group_by(Service_Quality, Opinion) %>%
    summarise(Count = n(), .groups = "drop") %>%
    group_by(Service_Quality) %>%
    mutate(Percentage = Count / sum(Count) * 100)
  
  # Create separate chart for each level
  plot <- ggplot(df_counts, aes(y = Service_Quality, x = Percentage, fill = Opinion)) +
    geom_col(position = "fill", width = 0.7) +  # Stacked bars
    geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
              position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Percentage labels
    labs(title = paste("Patient Satisfaction -", lvl),
         x = "Proportion",
         y = "Service Quality",
         fill = "Responses") +
    theme_minimal() +
    theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability
  
  # Print the plot for the current level
  print(plot)
}

















































# Ensure Opinion is a factor variable
data$Opinion <- factor(data$Opinion, levels = c("Novice", "Advanced Beginner", "Competent", "Proficient", "Expert"))

# Horizontal Stacked Bar Chart (With Percentages for Novice, Competent, Expert)
ggplot(data, aes(x = Management, fill = Opinion)) +
  geom_bar(stat = "count", position = "stack") +
  geom_text(
    aes(
      label = ifelse(after_stat(count) > 0 & Opinion %in% c("Novice", "Competent", "Expert"), 
                     paste0(after_stat(count), "%"), "")),  
    stat = "count",
    position = position_stack(vjust = 0.5),  # Centers text inside bars
    color = "black",  # Text color
    size = 3,  # Font size
    fontface = "bold"  # Bold text for clarity
  ) +
  coord_flip() +  # Flip to horizontal bar chart
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention", 
       x = "Prevention", 
       fill = "Opinion") +
  scale_fill_brewer(palette = "Set3")















































































































































































# Load Required Library
colnames(Pep_prevention_2_part)
# Load Required Library
library(ggplot2)

# Ensure correct number of rows (11 Control categories × 3 Opinion levels = 33)
data <- data.frame(
  Control = rep(c("Hypertension_p", "Rheumatic_Heart_Disease_p", "Coronary_Artery_Diseases_p", 
                  "Arrhythmia_p", "Heart_failure_p", "Cerebrovascular_disease_p", 
                  "Peripheral_arterial_disease_p", "Pericarditis_p", "Congenital_heart_disease_p", 
                  "Venus_thromboembolism_p", "Aortic_disease_p"), each = 3),
  
  Opinion = rep(c(1, 2, 3), times = 11),  # Numeric ratings (1 = Novice, ..., 3 = Expert)
  
  # Ensure exactly 33 values (Fixed missing values and removed extra comma)
  Percentage = c(
    10, 20, 30, 25, 15, 15, 25, 30, 20, 10, 5, 20, 25, 35, 15, 
    10, 25, 30, 20, 15, 20, 25, 25, 20, 10, 30, 25, 20, 15, 10, 
    25, 30, 25 # ✅ Now exactly s
  )
)

# Verify the number of rows
print(nrow(data))  # Should print 33

# Convert Numeric Ratings to Factors with Descriptive Labels
data$Opinion <- factor(data$Opinion, 
                       levels = c(1, 2, 3), 
                       labels = c("Novice", "Competent", "Expert"))

# 1️⃣ **Dodge Bar Chart (Side-by-Side Bars)**
ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Opinions on Prevention", 
       x = "Management", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load required libraries
library(ggplot2)

# Create the horizontal stacked bar chart with percentages inside bars
ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),  # Centers text inside bars
            color = "black", 
            size = 2) +  # Adjust font size
  coord_flip() +  # Flips the bar chart to horizontal
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.title.y = element_blank(),  # Hide y-axis title
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.text.x = element_blank())  # Hide x-axis labels

# For splitting the charts into Male and Female

# Load required libraries
library(ggplot2)

# Create the horizontal stacked bar chart with percentages and gender split
str(data)  # Check the structure of your data set
data$Gender <- sample(c(1, 2), nrow(data), replace = TRUE)  # Example if missing

# Load required libraries
library(ggplot2)

ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),  # Centers text inside bars
            color = "black", 
            size = 2) +  # Adjust font size
  coord_flip() +  # Flips the bar chart to horizontal
  facet_wrap(~ Gender, labeller = as_labeller(c("1" = "Male", "2" = "Female"))) +  # Split by Gender
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention by Gender", 
       x = "Control", 
       y = "Percentage",
       fill = "Opinion") +
  scale_fill_brewer(palette = "Set3")


ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),  # Centers text inside bars
            color = "black", 
            size = 2) +  # Adjusted font size (smaller text)
  coord_flip() +  # Flips the bar chart to horizontal
  facet_wrap(~ Gender, labeller = as_labeller(c("1" = "Male", "2" = "Female"))) +  # Split by Gender
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention by Gender", 
       x = "Control", 
       y = "Percentage",
       fill = "Opinion") +
  scale_fill_brewer(palette = "Set3")






# 3️⃣ **Vertical Stacked Bar Chart**
ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Distribution of Curriculum Competency on Prevention", 
       x = "Prevention", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

# **Charts for Prevention**
ggplot(data, aes(x = Opinion, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Control) +  # Creates separate charts per prevention category
  theme_minimal() +
  labs(title = "Individual Bar Charts for Each Disease Control", 
       x = "Opinion", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3")


# CMDs MANAGEMENT

# Load Required Library
library(ggplot2)

# Ensure correct number of rows (11 Control categories × 3 Opinion levels = 33)
data <- data.frame(
  Control = rep(c("Hypertension_m", "Rheumatic_Heart_Disease_m", "Coronary_Artery_Diseases_m", 
                  "Arrhythmia_m", "Heart_failure_m", "Cerebrovascular_disease_m", 
                  "Peripheral_arterial_disease_m", "Pericarditis_m", "Congenital_heart_disease_m", 
                  "Venus_thromboembolism_m", "Aortic_disease_m"), each = 3),
  
  Opinion = rep(c(1, 2, 3), times = 11),  # Numeric ratings (1 = Novice, ..., 3 = Expert)
  
  # Ensure exactly 33 values (Fixed missing values and removed extra comma)
  Percentage = c(
    10, 20, 30, 25, 15, 15, 25, 30, 20, 10, 5, 20, 25, 35, 15, 
    10, 25, 30, 20, 15, 20, 25, 25, 20, 10, 30, 25, 20, 15, 10, 
    25, 30, 25 # ✅ Now exactly s
  )
)

# Verify the number of rows
print(nrow(data))  # Should print 33

# Convert Numeric Ratings to Factors with Descriptive Labels
data$Opinion <- factor(data$Opinion, 
                       levels = c(1, 2, 3), 
                       labels = c("Novice", "Competent", "Expert"))

# 1️⃣ **Dodge Bar Chart (Side-by-Side Bars)**
ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Opinions on Management", 
       x = "Management", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load required libraries
library(ggplot2)

# Create the horizontal stacked bar chart with percentages inside bars
ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),  # Centers text inside bars
            color = "black", 
            size = 4) +  # Adjust font size
  coord_flip() +  # Flips the bar chart to horizontal
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Management") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.title.y = element_blank(),  # Hide y-axis title
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.text.x = element_blank())  # Hide x-axis labels

# For splitting the charts into Male and Female

# Load required libraries
library(ggplot2)

# Create the horizontal stacked bar chart with percentages and gender split
str(data)  # Check the structure of your data set
data$Gender <- sample(c(1, 2), nrow(data), replace = TRUE)  # Example if missing

# Load required libraries
library(ggplot2)

ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),  # Centers text inside bars
            color = "black", 
            size = 4) +  # Adjust font size
  coord_flip() +  # Flips the bar chart to horizontal
  facet_wrap(~ Gender, labeller = as_labeller(c("1" = "Male", "2" = "Female"))) +  # Split by Gender
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Management by Gender", 
       x = "Control", 
       y = "Percentage",
       fill = "Opinion") +
  scale_fill_brewer(palette = "Set3")

#CMDs CONTROL
# Load Required Library
library(ggplot2)

# Ensure correct number of rows (11 Control categories × 3 Opinion levels = 33)
data <- data.frame(
  Control = rep(c("Hypertension_c", "Rheumatic_Heart_Disease_c", "Coronary_Artery_Diseases_c", 
                  "Arrhythmia_c", "Heart_failure_c", "Cerebrovascular_disease_c", 
                  "Peripheral_arterial_disease_c", "Pericarditis_c", "Congenital_heart_disease_c", 
                  "Venus_thromboembolism_c", "Aortic_disease_c"), each = 3),
  
  Opinion = rep(c(1, 2, 3), times = 11),  # Numeric ratings (1 = Novice, ..., 3 = Expert)
  
  # Ensure exactly 33 values (Fixed missing values and removed extra comma)
  Percentage = c(
    10, 20, 30, 25, 15, 15, 25, 30, 20, 10, 5, 20, 25, 35, 15, 
    10, 25, 30, 20, 15, 20, 25, 25, 20, 10, 30, 25, 20, 15, 10, 
    25, 30, 25 # ✅ Now exactly s
  )
)

# Verify the number of rows
print(nrow(data))  # Should print 33

# Convert Numeric Ratings to Factors with Descriptive Labels
data$Opinion <- factor(data$Opinion, 
                       levels = c(1, 2, 3), 
                       labels = c("Novice", "Competent", "Expert"))

# 1️⃣ **Dodge Bar Chart (Side-by-Side Bars)**
ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Opinions on Control", 
       x = "Management", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Load required libraries
library(ggplot2)

# Create the horizontal stacked bar chart with percentages inside bars
ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),  # Centers text inside bars
            color = "black", 
            size = 4) +  # Adjust font size
  coord_flip() +  # Flips the bar chart to horizontal
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Control") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.title.y = element_blank(),  # Hide y-axis title
        axis.title.x = element_blank(),  # Hide x-axis title
        axis.text.x = element_blank())  # Hide x-axis labels

# For splitting the charts into Male and Female

# Load required libraries
library(ggplot2)

# Create the horizontal stacked bar chart with percentages and gender split
str(data)  # Check the structure of your data set
data$Gender <- sample(c(1, 2), nrow(data), replace = TRUE)  # Example if missing

# Load required libraries
library(ggplot2)

ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),  # Centers text inside bars
            color = "black", 
            size = 4) +  # Adjust font size
  coord_flip() +  # Flips the bar chart to horizontal
  facet_wrap(~ Gender, labeller = as_labeller(c("1" = "Male", "2" = "Female"))) +  # Split by Gender
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Control by Gender", 
       x = "Control", 
       y = "Percentage",
       fill = "Opinion") +
  scale_fill_brewer(palette = "Set3")




#Competency ratings

View(Final_Cadre_Specific_Datav)
colnames(Final_Cadre_Specific_Datav)
# Load Required Library
library(ggplot2)

# Ensure correct number of rows (6 Control categories × 5 Opinion levels = 30)
data <- data.frame(
  Control = rep(c(
    "Create community health awareness",
    "Promote prevention of cardio metabolic diseases in the community",
    "Screen clients...156",
    "Monitor client progress...157",
    "Make referrals...158",
    "Provides supervision...159"
  ), each = 5),  # Corrected repetition structure
  
  Opinion = rep(c(0,1,2,3,4), times = 6),  # Numeric ratings (0 = Not at all, ..., 4 = Very Good)
  
  # Ensure exactly 30 values
  Percentage = c(
    10, 20, 30, 25, 15, 15, 25, 30, 20, 10, 5, 20, 25, 35, 15, 
    10, 25, 30, 20, 15, 20, 25, 25, 20, 10, 30, 25, 20, 15, 10
  )
)

# Verify the number of rows
print(nrow(data))  # Should print 30

# Convert Numeric Ratings to Factors with Descriptive Labels
data$Opinion <- factor(data$Opinion, 
                       levels = c(0,1, 2, 3,4), 
                       labels = c("Not at all", "Slightly", "Moderately", "Good", "Very Good"))

# 1️⃣ **Dodge Bar Chart (Side-by-Side Bars)**
ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Competency Ratings", 
       x = "Management", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)

# Ensure correct number of rows (6 Control categories × 5 Opinion levels = 30)
data <- data.frame(
  Control = rep(c(
    "Create community health awareness",
    "Promote prevention of cardio metabolic diseases in the community",
    "Screen clients...156",
    "Monitor client progress...157",
    "Make referrals...158",
    "Provides supervision...159"
  ), each = 5),  # Corrected repetition structure
  
  Opinion = rep(c(0,1,2,3,4), times = 6),  # Numeric ratings (0 = Not at all, ..., 4 = Very Good)
  
  # Ensure exactly 30 values
  Percentage = c(
    10, 20, 30, 25, 15, 15, 25, 30, 20, 10, 5, 20, 25, 35, 15, 
    10, 25, 30, 20, 15, 20, 25, 25, 20, 10, 30, 25, 20, 15, 10
  )
)

# Verify the number of rows
print(nrow(data))  # Should print 30

# Convert Numeric Ratings to Factors with Descriptive Labels
data$Opinion <- factor(data$Opinion, 
                       levels = c(0,1, 2, 3,4), 
                       labels = c("Not at all", "Slightly", "Moderately", "Good", "Very Good"))

# Create horizontal stacked bar chart with percentages inside bars
ggplot(data, aes(x = Percentage, y = Control, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 4, color = "white") +  # Add percentage labels
  labs(title = "Percentage Distribution of Competency Ratings", 
       x = "Management Control",
       y = "Proportion",
       fill = "Competency Level") +
  scale_fill_brewer(palette = "Set3") +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 10),  # Keep axis text readable
        axis.text.y = element_text(size = 10, hjust = 1))  # Ensure Control names appear properly as rows







































# 1️⃣ **Dodge Bar Chart (Side-by-Side Bars)**
ggplot(data, aes(x = Prevention, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Opinions on Prevention", 
       x = "Prevention", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2️⃣ **Horizontal Stacked Bar Chart**
ggplot(data, aes(x = Prevention, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +  # Flips the bar chart to horizontal
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention", 
       x = "Prevention", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3")

# TO DISPLAY PERCENTAGES
# Load required libraries
library(ggplot2)

# Create the horizontal stacked bar chart with percentage labels inside bars
ggplot(data, aes(x = Prevention, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),  # Centers text inside bars
            color = "black", 
            size = 4) +  # Adjust font size
  coord_flip() +  # Flips the bar chart to horizontal
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention", 
       x = "Prevention", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3")

# 3️⃣ **Vertical Stacked Bar Chart**
ggplot(data, aes(x = Prevention, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Distribution of Curriculum Competency on Prevention", 
       x = "Prevention", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

# 4️⃣ **Faceted Bar Chart (Separate Chart for Each Disease Prevention)**
ggplot(data, aes(x = Opinion, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Prevention) +  # Creates separate charts per prevention category
  theme_minimal() +
  labs(title = "Individual Bar Charts for Each Disease Prevention", 
       x = "Opinion", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3")

#VISUALIZATION ON MANAGEMENT

# Load Required Library
library(ggplot2)

# Ensure correct number of rows (15 Management categories × 5 Opinion levels = 75)
data <- data.frame(
  Management = rep(c("Management_hyp", "Management_rhd", "Management_cad", 
                     "Management_arr", "Management_heart_f", "Management_cereb_d", 
                     "Management_per_art_diseas", "Management_per", "Management_cong_heart-d", 
                     "Management_venus_thro", "Management_aort_d", "Management_obes", 
                     "Management_class_1", "Management_class_2", "Management_class_3"), each = 5),
  
  Opinion = rep(c(0, 1, 2, 3, 4), times = 15),  # Numeric ratings (0 = Novice, ..., 4 = Expert)
  
  # Ensure exactly 75 values (Fixed missing values and removed extra comma)
  Percentage = c(
    10, 20, 30, 25, 15, 15, 25, 30, 20, 10, 5, 20, 25, 35, 15, 
    10, 25, 30, 20, 15, 20, 25, 25, 20, 10, 30, 25, 20, 15, 10, 
    25, 30, 25, 15, 5, 35, 25, 20, 15, 5, 10, 15, 20, 25, 30,
    20, 25, 30, 15, 10, 5, 15, 20, 35, 25, 10, 30, 20, 25, 15,
    10, 20, 15, 25, 30, 20, 10, 35, 25, 20, 15, 5, 30, 20, 25  # ✅ Now exactly 75 values
  )
)

# Verify the number of rows
print(nrow(data))  # Should print 75

# Convert Numeric Ratings to Factors with Descriptive Labels
data$Opinion <- factor(data$Opinion, 
                       levels = c(0, 1, 2, 3, 4), 
                       labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))

# 1️⃣ **Dodge Bar Chart (Side-by-Side Bars)**
ggplot(data, aes(x = Management, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  theme_minimal() +
  labs(title = "Percentage Distribution of Opinions on Management", 
       x = "Management", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# 2️⃣ **Horizontal Stacked Bar Chart**
ggplot(data, aes(x = Management, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  coord_flip() +  # Flips the bar chart to horizontal
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention", 
       x = "Prevention", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3")
ggplot(data, aes(x = Control, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = paste0(Percentage, "%")), 
            position = position_stack(vjust = 0.5),  # Centers text inside bars
            color = "white",  # White for better visibility
            size = 2,       # Smaller font size
            fontface = "bold") +  # Bold text for clarity
  coord_flip() +  # Flip to horizontal bar chart
  facet_wrap(~ Gender, labeller = as_labeller(c("1" = "Male", "2" = "Female"))) +  # Split by Gender
  theme_minimal() +
  labs(title = "Horizontal Stacked Bar Chart of Opinions on Prevention by Gender", 
       x = "Control", 
       y = "Percentage",
       fill = "Opinion") +
  scale_fill_brewer(palette = "Set3")











# 3️⃣ **Vertical Stacked Bar Chart**
ggplot(data, aes(x = Management, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Distribution of Curriculum Competency on Management", 
       x = "Prevention", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate labels for readability

# 4️⃣ **Faceted Bar Chart (Separate Chart for Each.n)**
ggplot(data, aes(x = Opinion, y = Percentage, fill = Opinion)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Management) +  # Creates separate charts per prevention category
  theme_minimal() +
  labs(title = "Individual Bar Charts for Each Disease Management", 
       x = "Opinion", 
       y = "Percentage") +
  scale_fill_brewer(palette = "Set3")






library(ggplot2)

# Ensure correct number of rows (15 Prevention categories × 5 Opinion levels = 75)
data <- data.frame(
  Prevention = rep(c("Prevention_hyp", "Prevention_rhd", "Prevention_cad", 
                     "Prevention_arr", "Prevention_heart_f", "Prevention_cereb_d", 
                     "Prevention_per_art_diseas", "Prevention_per", "Prevention_cong_heart_d", 
                     "Prevention_venus_thro", "Prevention_aort_d", "Prevention_obes", 
                     "Prevention_class_1", "Prevention_class_2", "Prevention_class_3"), each = 5),
  
  Opinion = rep(c(0, 1, 2, 3, 4), times = 15),  # Numeric ratings (0 = Novice, ..., 4 = Expert)
  
  # Ensure exactly 75 values (Fixed missing values and removed extra comma)
  Percentage = c(
    10, 20, 30, 25, 15, 15, 25, 30, 20, 10, 5, 20, 25, 35, 15, 
    10, 25, 30, 20, 15, 20, 25, 25, 20, 10, 30, 25, 20, 15, 10, 
    25, 30, 25, 15, 5, 35, 25, 20, 15, 5, 10, 15, 20, 25, 30,
    20, 25, 30, 15, 10, 5, 15, 20, 35, 25, 10, 30, 20, 25, 15,
    10, 20, 15, 25, 30, 20, 10, 35, 25, 20, 15, 5, 30, 20, 25  # ✅ Now exactly 75 values
  )
)

# Verify the number of rows
print(nrow(data))  # Should print 75

# Convert Numeric Ratings to Factors with Descriptive Labels
data$Opinion <- factor(data$Opinion, 
                       levels = c(0, 1, 2, 3, 4), 
                       labels = c("Novice", "Adv. Beginner", "Competent", "Proficient", "Expert"))














