#Final_VIZ Trials

library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
# Manually select all diseases for display

selected_diseases <- c("P_Hypertension", "M_Hypertension", "C_Hypertension", "P_RHD", "M_RHD", "C_V", "P_Coronary_Artery_D", "M_Coronary_Artery_D", "C_Coronary_Artery_D", "P_arrhythmia",                                                                                                                                                                                                                        
             "M_Arrhythmia", "C_Arrhythmia", "P_Heart_failure", "M__Heart_failure", "C__Heart_failure", "P_Cerebrovascular_disease", "M__Cerebrovascular_disease", "C__Cerebrovascular_disease", "P_Peripheral_arterial_D", "M__Peripheral_arterial_D", "C__Peripheral_arterial_D", "P_Pericarditis", "M_Pericarditis",                                                                                                                                                                                                                      
             "C_Pericarditis", "P_ Congenital_heart_D", "M__ Congenital_heart_D", "C__ Congenital_heart_D", "P_Venous_thromboembolism", "M__Venous_thromboembolism", "C__Venous_thromboembolism", "P_Aortic disease", "M_Aortic_disease", "C__Aortic_disease")

# Filter the data set to include only selected diseases
df_selected <- Pep_prevention_2_part %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Novice", "Adv. Beginner", "Competent", "Profecient", "Expert"))

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


#Trials 
#  With percentages 
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
# Remove the Gender column
df <- Cadre_Specific_Data_Final_2 %>% select(-Gender)

# Manually select  diseases for display
selected_diseases <- c(
  "P_Hypertension", "M_Hypertension", "C_Hypertension", 
  "P_RHD", "M_RHD", "C_RHD", 
  "P_Coronary_Artery_D", "M_Coronary_Artery_D", "C_Coronary_Artery_D", 
  "P_arrhythmia", "M_Arrhythmia", "C_Arrhythmia", 
  "P_Heart_failure", "M__Heart_failure", "C__Heart_failure", 
  "P_Cerebrovascular_disease", "M__Cerebrovascular_disease", "C__Cerebrovascular_disease", 
  "P_Peripheral_arterial_D", "M__Peripheral_arterial_D", "C__Peripheral_arterial_D", 
  "P_Pericarditis", "M_Pericarditis", "C_Pericarditis", 
  "P_ Congenital_heart_D", "M__ Congenital_heart_D", "C__ Congenital_heart_D", 
  "P_Venous_thromboembolism", "M__Venous_thromboembolism", "C__Venous_thromboembolism", 
  "P_Aortic disease", "M_Aortic_disease", "C__Aortic_disease"
)
# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Final_2 %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels

# convert it to a factor with the desired order:
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c("Novice", "Advanced Beginner", "Competent", "Profecient", "Expert"))

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


#3 Charts Sepparetely
#Trials 
#  With percentages 
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
# Remove the Gender column
df <- Cadre_Specific_Data_Final_2 %>% select(-Gender)

# Manually select  diseases for display
selected_diseases <- c(
  "P_Hypertension", "M_Hypertension", "C_Hypertension", 
  "P_RHD", "M_RHD", "C_RHD", 
  "P_Coronary_Artery_D", "M_Coronary_Artery_D", "C_Coronary_Artery_D", 
  "P_arrhythmia", "M_Arrhythmia", "C_Arrhythmia", 
  "P_Heart_failure", "M__Heart_failure", "C__Heart_failure", 
  "P_Cerebrovascular_disease", "M__Cerebrovascular_disease", "C__Cerebrovascular_disease", 
  "P_Peripheral_arterial_D", "M__Peripheral_arterial_D", "C__Peripheral_arterial_D", 
  "P_Pericarditis", "M_Pericarditis", "C_Pericarditis", 
  "P_ Congenital_heart_D", "M__ Congenital_heart_D", "C__ Congenital_heart_D", 
  "P_Venous_thromboembolism", "M__Venous_thromboembolism", "C__Venous_thromboembolism", 
  "P_Aortic disease", "M_Aortic_disease", "C__Aortic_disease"
)
# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Final_2 %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels

# convert it to a factor with the desired order:
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c("Novice", "Advanced Beginner", "Competent", "Profecient", "Expert"))

# Calculate proportions for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Subset data for diseases that begin with "P"
df_counts_P <- df_counts %>% filter(substr(trimws(Disease), 1, 1) == "P")

# Plot the stacked bar chart for P diseases
ggplot(df_counts_P, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Converts raw counts to proportions
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  labs(
       x = "Proportion",
       y = "Disease",
       fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Subset data for diseases that begin with "M"
df_counts_M <- df_counts %>% filter(substr(trimws(Disease), 1, 1) == "M")

# Plot the stacked bar chart for M diseases
ggplot(df_counts_M, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  labs(
       x = "Proportion",
       y = "Disease",
       fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Subset data for diseases that begin with "C"
df_counts_C <- df_counts %>% filter(substr(trimws(Disease), 1, 1) == "C")

# Plot the stacked bar chart for C diseases
ggplot(df_counts_C, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  labs(
       x = "Proportion",
       y = "Disease",
       fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))


# Grouped charts 
library(ggplot2)
library(dplyr)
library(tidyr)

# Remove the Gender column
df <- Cadre_Specific_Data_Final_2 %>% select(-Gender)

# Manually select diseases for display
selected_diseases <- c(
  "P_Hypertension", "M_Hypertension", "C_Hypertension", 
  "P_RHD", "M_RHD", "C_RHD", 
  "P_Coronary_Artery_D", "M_Coronary_Artery_D", "C_Coronary_Artery_D", 
  "P_arrhythmia", "M_Arrhythmia", "C_Arrhythmia", 
  "P_Heart_failure", "M__Heart_failure", "C__Heart_failure", 
  "P_Cerebrovascular_disease", "M__Cerebrovascular_disease", "C__Cerebrovascular_disease", 
  "P_Peripheral_arterial_D", "M__Peripheral_arterial_D", "C__Peripheral_arterial_D", 
  "P_Pericarditis", "M_Pericarditis", "C_Pericarditis", 
  "P_ Congenital_heart_D", "M__ Congenital_heart_D", "C__ Congenital_heart_D", 
  "P_Venous_thromboembolism", "M__Venous_thromboembolism", "C__Venous_thromboembolism", 
  "P_Aortic disease", "M_Aortic_disease", "C__Aortic_disease"
)
# Filter the dataset to include only selected diseases
df_selected <- df %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert Opinion to a factor with the desired order.
df_long$Opinion <- factor(df_long$Opinion,
                          levels = c("Novice", "Advanced Beginner", "Competent", "Profecient", "Expert"))

# Calculate counts and percentages for each opinion level per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Subset data for diseases that begin with "P"
df_counts_P <- df_counts %>% 
  filter(substr(trimws(Disease), 1, 1) == "P")

# Create grouped bar chart for P diseases
ggplot(df_counts_P, aes(x = Disease, y = Count, fill = Opinion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.25, size = 2, color = "black") +
  labs(title = "Grouped Bar Chart of Cardiovascular Diseases Prevention",
       x = "Disease",
       y = "Count",
       fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Subset data for diseases that begin with "M"
df_counts_M <- df_counts %>% 
  filter(substr(trimws(Disease), 1, 1) == "M")

# Create grouped bar chart for M diseases
ggplot(df_counts_M, aes(x = Disease, y = Count, fill = Opinion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.25, size = 2, color = "black") +
  labs(title = "Grouped Bar Chart of Cardiovascular Diseases Management",
       x = "Disease",
       y = "Count",
       fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Subset data for diseases that begin with "C"
df_counts_C <- df_counts %>% 
  filter(substr(trimws(Disease), 1, 1) == "C")

# Create grouped bar chart for C diseases
ggplot(df_counts_C, aes(x = Disease, y = Count, fill = Opinion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.25, size = 2, color = "black") +
  labs(title = "Grouped Bar Chart of Cardiovascular Diseases Control",
       x = "Disease",
       y = "Count",
       fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#OBL DELIVERY 
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
selected_diseases <- c("Authoring OBL appropriate content", "Converting written content to SCORM files", "Creating links to access resources external to OBL", "Navigating a Learning Management System", "Uploading videos to the LMS", "Using collaborative tools to enrich OBL delivery", "Creating users in an LMS", "Adding activities to an LMS", "Adding resources to an LMS", "Creating an online lesson", 
                       "Conducting webinars with OBL learners", "Planning synchro0us teaching sessions", "Creating WIKIS in the LMS", "Creating discussion forums for OBL learners", "Using social media to enrich interactions with learners", "Creating videos for  OBL", "Editing Learning Videos", "Setting exams for OBL students", "Proctoring an online exam")
colnames(Cadre_Specific_Data_Final_2)

# Filter the dataset to include only selected diseases
df_selected <- Cadre_Specific_Data_Final_2 %>% select(all_of(selected_diseases))

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
            position = position_fill(vjust = 0.5), size = 2, color = "black") +  # Font size = 2
  labs(
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
df_selected <- Cadre_Specific_Data_Final_2 %>% select(all_of(selected_diseases))

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
            position = position_fill(vjust = 0.5), size = 2, color = "black") +  # Font size = 2
  labs(
       x = "Proportion",
       y = "Competency",
       fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#SCOPE, RELEVANCE, ORGANIZATION

#Revised
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select variables for display (all three groups are included)
selected_diseases <- c(
  "Cardiovascular_scope", "Type II_Scope", "Kidney dis_Scope", "non-alcoholic fatty liv_Scope", "Dyslipidemia_Scope", ") Community health 1_Scope", "Biochemistry_Scope", "Medical Parasitology_Scope", "Clinical Pathology_Scope", "Clinical Pharmacology_Scope", "Medicine 1_Scope", 
  "Clinical Pharmacology IV_Scope", "Medicine II_Scope", 
  "Pediatric and Child health II_Scope", "Pediatric and Child health II_Scope", "Pediatrics and Child Health III_Scope",  
  "Medicine IV_Scope", "Hyperosmolar hyperglycemic state_Scope", 
  "Cardiovascular_organization", "Type II_Organization", "Kidney dis_Organization", "non-alcoholic fatty liv_Organization", "Dyslipidemia_Organization", "Community health 1_Organization", "Biochemistry_Organization", "Medical Parasitology_Organization", "Clinical Pathology_Organization", ") Clinical Pharmacology_Organization", "Medicine 1_Organization", 
  "Clinical Pharmacology IV_Organization", "Medicine II_Organization", 
  "Pediatric and Child health II_Organization", "Pediatric and Child health II_Organization", "Pediatrics and Child Health III_Organization",  
  "Medicine IV_Organization", "Hyperosmolar hyperglycemic state_Organization", 
  "Cardiovascular_relevance", "Type II_Relevance", "Kidney dis_Relevance", "non-alcoholic fatty liv_Relevance", "Dyslipidemia_Relevance", "Community health 1_SRelevance", "Biochemistry_Relevance", "Medical Parasitology_Relevance", "Clinical Pathology_Relevance", ") Clinical Pharmacology_Relevance", "Medicine 1_Relevance", 
  "Clinical Pharmacology IV_Relevance", "Medicine II_Relevance", 
  "Pediatric and Child health II_Relevance", "Pediatric and Child health II_Relevance", "Pediatrics and Child Health III_Relevance",  
  "Medicine IV_Relevance", "Hyperosmolar hyperglycemic state_Relevance"
)

# Filter the dataset to include only selected variables
df_selected <- Final_Cadre_Specific_Datav %>% select(all_of(selected_diseases))

# Convert data to long format and drop NA values
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)

# Convert Opinion to a factor with the desired order.
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not all", "Slightly", "Moderately", "Good", "Very Good"))) %>%
  filter(!is.na(Opinion))

# Compute counts and percentages for each category per variable
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all 5 levels appear
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Subset data for variables ending with "scope" or "Scope"
df_counts_scope <- df_counts %>% 
  filter(grepl("_scope$", Disease, ignore.case = TRUE))

ggplot(df_counts_scope, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bar (proportions)
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  
            position = position_fill(vjust = 0.5), size = 2, color = "white") +
  labs(title = "Topics Scope for Various Diseases",
       x = "Proportion",
       y = "Variable",
       fill = "Opinion") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Subset data for variables ending with "organization" or "Organization"
df_counts_org <- df_counts %>% 
  filter(grepl("_organization$", Disease, ignore.case = TRUE))

ggplot(df_counts_org, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  
            position = position_fill(vjust = 0.5), size = 2, color = "white") +
  labs(title = "Topics Organization for Various Diseases",
       x = "Proportion",
       y = "Variable",
       fill = "Opinion") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Subset data for variables ending with "relevance" or "Relevance"
df_counts_rel <- df_counts %>% 
  filter(grepl("_relevance$", Disease, ignore.case = TRUE))

ggplot(df_counts_rel, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  
            position = position_fill(vjust = 0.5), size = 2, color = "white") +
  labs(title = "Topics Relevance for Various Diseases",
       x = "Proportion",
       y = "Variable",
       fill = "Opinion") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

#PATIENT SATISFACTION
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set

colnames(Patient_Satisfaction_Data)

# Manually select  diseases for display
selected_diseases <- c(
  "hospital has well maintained and modern equipment", "Physical facilities are visually appealing", "Staff in are clean and well groomed", 
  "Patient rooms are comfortable and accord privacy", "Patient rooms are comfortable and accord privacy", "hospital performs services and procedures correctly the first time", 
  "hospital provides services within the time promised in charter", "hospital submits legible patient reports", "the staff show sincere interest to solve patient problems", 
  "doctors explains health conditions in understandable way", "hospital staff inform patients exactly when service will run", "Staff are willing to help patients", "Waiting time for admission in the hospital is short", "At the Hospital, staff are polite and courteous", "The hospitals operate at times suitable to patients", "Doctors spend enough time with each patient", "Staff are able to understand specific needs of patients", "Doctors and nurses listen to me and my issues attentively")
# Filter the data set to include only selected diseases
df_selected <- Patient_Satisfaction_Data %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Strongly Disagree", "Diagree", "Neutral", "Agree", "Strongly Agree")))
# Calculate proportions for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)
#Neutral to black
ggplot(df_counts, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Converts raw counts to proportions
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), 
            size = 2, color = "white") +
  labs(
  
    x = "Proportion",
    y = "Disease",
    fill = "Rating"
  ) +
  # Overwrite only "Neutral" to black, keep other default colors
  scale_fill_manual(
    values = c(
      "Strongly Disagree" = "#F8766D",  # Default first color
      "Diagree"           = "#A3A500",  # Default second color
      "Neutral"           = "black",    # Overridden to black
      "Agree"             = "#00B0F6",  # Default fourth color
      "Strongly Agree"    = "#E76BF3"   # Default fifth color
    )
  ) +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))






colnames(Patient_Satisfaction_Data)
#By type of ownership and levels
library(ggplot2)
library(dplyr)
library(tidyr)

# Check the column names to verify that "Hospital_Level" exists:
colnames(patient)

# Manually select patient satisfaction criteria (note: remove duplicates if needed)
#service_quality_expectations_scale
selected_criteria <- c(
  "Hospital_shouldhave_Wellmainta", "hospital_building_shouldbe_vis", "hospital_staff_shouldbe_clean", 
  "Patient_rooms_should_be_comfor", "hospitals_should_perform_servic", "hospitals_should_provide_service", 
  "hospitals_should_submit_legible_", "staff_should_show_sincere_intere", "doctors_nurses_should_explain_co", 
  "staff_should_inform_patients_ser", "staff_should_be_willing_to_help_", "waiting_time_for_admission_shoul", 
  "staff_should_be_polite_and_court", "Health_staff_shouldbe_competen", "patients_should_feel_confident_a", 
  "hospitals_should_provide_adequat", "Health_staff_should_have_knowled", "hospitals_should_operate_at_suit", 
  "doctors_nurses_should_listen_att", "staff_should_understand_patients", "doctors_nurses_should_spend_enou")

# Perception of service quality
selected_criteria <- c(
  "hospital_has_modern_equipment", "hospital_building_are_visually_a", "hospital_staff_clean_and_wellgro", 
  "patient_rooms_comfortable_and_a", "hospital_performs_services_corre", "hospital_services_provided_withi", 
  "hospital_submits_legible_reports", "hospital_staff_show_sincere_inte", "doctors_nurses_explain_condition", 
  "hospital_staff_inform_patients_s", "hospital_daily_services_waiting_", "hospital_staff_polite_and_courte", 
  "health_staff_competent_in_handli", "patients_feel_confident_and_secu", "hospital_provides_adequate_priva", 
  "health_staff_have_knowledge_to_a", "hospitals_operate_at_suitable_ti", "doctors_nurses_listen_attentivel", 
  "hospital_has_staff_to_assist_pat", "staff_understand_specific_needs_", "doctors_nurses_spend_enough_time")

#Health service delivery scale
selected_criteria <- c(
  "staff_well_trained_to_offer_serv", "Prescribed_drugs_readily_avail", "Lab_tests_done_within_the_hos", 
  "Hospitals_provide_privacy_during", "Health_service_charges_are_pocke", "staff_friendly_to_patients", 
  "hospital_maintains_patient_confi", "patient_waiting_time_within_serv")

# Filter the data set to include only selected criteria and the hospital level variable
df_selected <- patient %>% 
  select(all_of(selected_criteria), `facility_level`)

# Convert data to long format (pivot only the selected criteria, leave the hospital level intact)
df_long <- df_selected %>%
  pivot_longer(cols = all_of(selected_criteria), 
               names_to = "Condition", 
               values_to = "Opinion") %>%
  drop_na(Opinion)

# Convert numeric Opinion values to factor levels with labels
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")))

# Calculate counts and percentages for each category per criterion and hospital level
df_counts <- df_long %>%
  group_by(Condition, Opinion, `facility_level`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Condition, Opinion, `facility_level`, fill = list(Count = 0)) %>%
  group_by(Condition, `facility_level`) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# ---------------------------
# Plot for Hospital Level 4
# ---------------------------
df_counts_lvl4 <- df_counts %>% filter(`facility_level` == "Level 4")

ggplot(df_counts_lvl4, aes(y = Condition, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bar chart (proportions)
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), 
            size = 3, 
            color = "black") +
  labs(
       x = "Proportion",
       y = "Satisfaction Criteria",
       fill = "Rating") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))
# ---------------------------
# Plot for Hospital Level 5
# ---------------------------
df_counts_lvl5 <- df_counts %>% filter(`facility_level` == "Level 5")

ggplot(df_counts_lvl5, aes(y = Condition, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bar chart (proportions)
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), 
            size = 2, 
            color = "black") +
  labs(
       x = "Proportion",
       y = "Satisfaction Criteria",
       fill = "Rating") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# By type of ownership
#By type of ownership and levels
library(ggplot2)
library(dplyr)
library(tidyr)

# Check the column names to verify that "Hospital_Level" exists:
colnames(patient)

# Manually select patient satisfaction criteria (note: remove duplicates if needed)
selected_criteria <- c(
  "Hospital_shouldhave_Wellmainta", "hospital_building_shouldbe_vis", "hospital_staff_shouldbe_clean", 
  "Patient_rooms_should_be_comfor", "hospitals_should_perform_servic", "hospitals_should_provide_service", 
  "hospitals_should_submit_legible_", "staff_should_show_sincere_intere", "doctors_nurses_should_explain_co", 
  "staff_should_inform_patients_ser", "staff_should_be_willing_to_help_", "waiting_time_for_admission_shoul", 
  "staff_should_be_polite_and_court", "Health_staff_shouldbe_competen", "patients_should_feel_confident_a", 
  "hospitals_should_provide_adequat", "Health_staff_should_have_knowled", "hospitals_should_operate_at_suit", 
  "doctors_nurses_should_listen_att", "staff_should_understand_patients", "doctors_nurses_should_spend_enou")

# Perception of service quality
selected_criteria <- c(
  "hospital_has_modern_equipment", "hospital_building_are_visually_a", "hospital_staff_clean_and_wellgro", 
  "patient_rooms_comfortable_and_a", "hospital_performs_services_corre", "hospital_services_provided_withi", 
  "hospital_submits_legible_reports", "hospital_staff_show_sincere_inte", "doctors_nurses_explain_condition", 
  "hospital_staff_inform_patients_s", "hospital_daily_services_waiting_", "hospital_staff_polite_and_courte", 
  "health_staff_competent_in_handli", "patients_feel_confident_and_secu", "hospital_provides_adequate_priva", 
  "health_staff_have_knowledge_to_a", "hospitals_operate_at_suitable_ti", "doctors_nurses_listen_attentivel", 
  "hospital_has_staff_to_assist_pat", "staff_understand_specific_needs_", "doctors_nurses_spend_enough_time")

#Health service delivery scale
selected_criteria <- c(
  "staff_well_trained_to_offer_serv", "Prescribed_drugs_readily_avail", "Lab_tests_done_within_the_hos", 
  "Hospitals_provide_privacy_during", "Health_service_charges_are_pocke", "staff_friendly_to_patients", 
  "hospital_maintains_patient_confi", "patient_waiting_time_within_serv")

# Filter the data set to include only the selected criteria and Hospital_Level
df_selected <- patient %>% 
  select(all_of(selected_criteria), `facility_ownership`)
#To include hub
df_selected <- patient %>% 
  select(all_of(selected_criteria), `facility_ownership`, `hub`)  # Include hub

# Convert data to long format.
# Pivot only the selected criteria; leave the type of ownership intact
df_long <- df_selected %>%
  pivot_longer(cols = all_of(selected_criteria), 
               names_to = "Condition", 
               values_to = "Opinion") %>%
  drop_na(Opinion)

# Convert numeric Opinion values to factor levels with labels
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")))

# Calculate counts and percentages for each category per condition and type of ownership
df_counts <- df_long %>%
  group_by(Condition, Opinion, `facility_ownership`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Condition, Opinion, `facility_ownership`, fill = list(Count = 0)) %>%
  group_by(Condition, `facility_ownership`) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Facet Wrap Plot for Government & Faith-Based Ownership
# ---------------------------

ggplot(df_counts, aes(y = Condition, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bar chart (proportions)
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), 
            size = 2, 
            color = "black") +
  labs(
    x = "Proportion",
    y = "Satisfaction Criteria",
    fill = "Rating"
  ) +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8)) +
  facet_wrap(~ facility_ownership)  # Facet by facility ownership

# ---------------------------
# Plot for Government Ownership
# ---------------------------
df_counts_gov <- df_counts %>% filter(`facility_ownership` == "Goverment")

ggplot(df_counts_gov, aes(y = Condition, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bar chart (proportions)
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), 
            size = 2, 
            color = "black") +
  labs(
       x = "Proportion",
       y = "Satisfaction Criteria",
       fill = "Rating") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# ---------------------------
# Plot for Faith-Based Ownership
# ---------------------------
df_counts_fb <- df_counts %>% filter(`Type of ownership of the health facility` == "Faith based")

ggplot(df_counts_fb, aes(y = Condition, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bar chart (proportions)
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), 
            size = 2, 
            color = "white") +
  labs(title = "Patient Satisfaction: Faith-Based Ownership",
       x = "Proportion",
       y = "Satisfaction Criteria",
       fill = "Rating") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

#HEADS OF DEPARTMENT
#Gender pie chart
library(ggplot2)
library(dplyr)

# Check the column names to verify that "Gender" exists:
colnames(Final_Headsofdepartment_Data)

# Calculate counts for Gender
gender_counts <- Final_Headsofdepartment_Data %>%
  count(Gender)

# Calculate the percentage for each gender
gender_counts <- gender_counts %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the pie chart
ggplot(gender_counts, aes(x = "", y = n, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +  # Bar chart for pie
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 6) +  # Add percentages to the chart
  labs(title = "Gender Distribution Across Departments") +
  theme_void()  # Remove background gridlines

#Age distribution by gender

library(ggplot2)
library(dplyr)

# Check the column names to verify that "Age" and "Gender" exist:
colnames(Final_Headsofdepartment_Data)

# Assuming "Age" is a continuous variable, let's create age groups (e.g., 0-20, 21-40, etc.)
Head_of_department_Ages <- Final_Headsofdepartment_Data %>%
  mutate(Age = case_when(
    Age <= 20 ~ "0-20",
    Age <= 40 ~ "21-40",
    Age <= 60 ~ "41-60",
    Age <= 80 ~ "61-80",
    TRUE ~ "81+"  # For ages above 80
  ))

# Count the number of individuals in each age group by gender
age_gender_counts <- Head_of_department_Ages %>%
  count(Age, Gender)

# Create the bar chart
ggplot(age_gender_counts, aes(x = Age, y = n, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +  # Position dodge for side-by-side bars
  labs(title = "Age Distribution by Gender",
       x = "Age Group",
       y = "Count",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Institutions Research Capacity
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
colnames(Final_Headsofdepartment_Data)
# Manually select  diseases for display
selected_diseases <- c(
  "Decision-making_research", "Infrastructure support research", "Research mandate institution", 
  "Staff  research knowledge", "Staff involved research discussions", "Staff provided research incentives", 
  "budget  for ongoing research", "defined research priorities", "mechanisms communicate research_f", 
  "mechanisms to engage staff conduct_res", "mentorship to support staf_research", "training programs female_reseachers", 
  "training programs minority groups", "training programs focus on research", "central research support staff" 
  
)
# Filter the data set to include only selected diseases
df_selected <- Final_Headsofdepartment_Data %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")))

# Calculate proportions for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Proportional stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 2, color = "white") +
  labs(
       x = "Proportion",
       y = "Research Areas",
       fill = "Rating") +
  scale_fill_manual(values = c("Strongly Disagree" = "#E41A1C",  # Red
                               "Disagree" = "#FF7F00",           # Orange-yellow
                               "Neutral" = "black",              # Black for Neutral
                               "Agree" = "#4DAF4A",              # Green
                               "Strongly Agree" = "orange")) +   # Orange for Strongly Agree
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels
  labs(title = "Institutions Research Capacity",
       x = "Proportion",
       y = "Research areas",
       fill = "Rating") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# By type of ownership
library(ggplot2)
library(dplyr)
library(tidyr)

# Check the column names to verify that "Type_of_Ownership" exists
colnames(Final_Headsofdepartment_Data)

# Manually select  diseases for display
selected_diseases <- c(
  "Decision-making_research", "Infrastructure support research", "Research mandate institution", 
  "Staff  research knowledge", "Staff involved research discussions", "Staff provided research incentives", 
  "budget  for ongoing research", "defined research priorities", "mechanisms communicate research_f", 
  "mechanisms to engage staff conduct_res", "mentorship to support staf_research", "training programs female_reseachers", 
  "training programs minority groups", "training programs focus on research", "central research support staff" 
  
)
# Filter the data set to include only selected criteria and Type_of_Ownership
# Filter the data set to include only selected criteria and Type_of_Ownership
df_selected <- Final_Headsofdepartment_Data %>% 
  select(all_of(selected_diseases), `Select the type of ownership`)

# Convert data to long format, pivoting only the selected research criteria (excluding Type_of_Ownership)
df_long <- df_selected %>%
  pivot_longer(cols = all_of(selected_diseases), 
               names_to = "Disease", 
               values_to = "Opinion") %>%
  drop_na(Opinion)

# Convert numeric Opinion values to factor levels with labels
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")))

# Calculate counts and percentages for each category per disease and type of ownership
df_counts <- df_long %>%
  group_by(Disease, Opinion, `Select the type of ownership`) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease, `Select the type of ownership`) %>%
  mutate(Percentage = Count / sum(Count) * 100)
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels
  labs(
       x = "Proportion",
       y = "Research Areas",
       fill = "Rating") +
  facet_wrap(~ `Select the type of ownership`) +  # Facet by Type_of_Ownership (Government vs Faith-based)
  scale_fill_manual(values = c("Strongly Disagree" = "#E41A1C",   # Red
                               "Disagree" = "#FF7F00",            # Orange
                               "Neutral" = "black",               # Black for Neutral
                               "Agree" = "#4DAF4A",               # Green
                               "Strongly Agree" = "#377EB8")) +   # Blue
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# Create the stacked bar chart with percentages inside bars, and facet by Type_of_Ownership
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_fill(vjust = 0.5), size = 2, color = "white") +  # Add percentage labels
  labs(
       x = "Proportion",
       y = "Research Areas",
       fill = "Rating") +
  facet_wrap(~ `Select the type of ownership`) +  # Facet by Type_of_Ownership (Government vs Faith-based)
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability


#STUDENT
#Gender pie chart
library(ggplot2)
library(dplyr)

# Check the column names to verify that "Gender" exists:
colnames(Final_Students_Data)

# Calculate counts for Gender
gender_counts <- Final_Students_Data %>%
  count(Gender)

# Calculate the percentage for each gender
gender_counts <- gender_counts %>%
  mutate(Percentage = n / sum(n) * 100)

# Create the pie chart
ggplot(gender_counts, aes(x = "", y = n, fill = Gender)) +
  geom_bar(stat = "identity", width = 1) +  # Bar chart for pie
  coord_polar(theta = "y") +  # Convert bar chart to pie chart
  geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
            position = position_stack(vjust = 0.5), size = 6) +  # Add percentages to the chart
  labs(title = "Gender Distribution Across Departments") +
  theme_void()  # Remove background gridlines

#Age distribution by gender

library(ggplot2)
library(dplyr)

# Check the column names to verify that "Age" and "Gender" exist:
colnames(Final_Students_Data)

# Assuming "Age" is a continuous variable, let's create age groups (e.g., 0-20, 21-40, etc.)
Student_Ages <- Final_Students_Data %>%
  mutate(Age = case_when(
    Age <= 20 ~ "0-20",
    Age <= 40 ~ "21-40",
    Age <= 60 ~ "41-60",
    Age <= 80 ~ "61-80",
    TRUE ~ "81+"  # For ages above 80
  ))

# Count the number of individuals in each age group by gender
age_gender_counts <- Student_Ages %>%
  count(Age, Gender)

# Create the bar chart
ggplot(age_gender_counts, aes(x = Age, y = n, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +  # Position dodge for side-by-side bars
  labs(title = "Age Distribution by Gender",
       x = "Age Group",
       y = "Count",
       fill = "Gender") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for readability

# Grouped charts
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select two diseases for display (e.g., "P_Hypertension" and "M_Hypertension")
selected_diseases <- c("reliable internet access", "comfort using online learning platforms")

# Filter the data set to include only selected diseases
df_selected <- Final_Students_Data %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert Opinion to a factor with the desired order
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Not reliable at all", "Rarely reliable", "Sometimes reliable", "Always reliable", "Mostly reliable")))

# Calculate counts and percentages for each opinion level per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create grouped bar chart for the selected diseases
ggplot(df_counts, aes(x = Disease, y = Count, fill = Opinion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.25, size = 2, color = "black") +
  labs(title = "Grouped Bar Chart of Selected Variables",
       x = "Variables",
       y = "Count",
       fill = "Opinion Level") +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette from RColorBrewer
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

library(ggplot2)
library(dplyr)
library(tidyr)

# Check the column names to verify that the selected variables exist
colnames(Final_Students_Data)

# Manually select two variables for display
selected_diseases <- c("reliable internet access")

# Check the first few rows to ensure the data is correctly structured
head(Final_Students_Data)

# Check the unique values in the 'Opinion' column for the selected diseases
unique(Final_Students_Data$`reliable internet access`)

# Check for missing values in the selected columns
sum(is.na(Final_Students_Data$`reliable internet access`))

# Filter the data set to include only selected variables and remove rows with missing values
df_selected <- Final_Students_Data %>%
  select(all_of(selected_diseases)) %>%
  drop_na()  # Remove rows with missing values

# Ensure 'Opinion' values are treated as factors with desired levels
df_selected$`reliable internet access` <- as.factor(df_selected$`reliable internet access`)

# Convert data to long format (pivot only the selected criteria)
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert Opinion to a factor with the desired order
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c("Not reliable at all", "Rarely reliable", "Sometimes reliable", 
                                     "Always reliable", "Mostly reliable")))

# Calculate counts and percentages for each opinion level per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create grouped bar chart for the selected diseases
ggplot(df_counts, aes(x = Disease, y = Count, fill = Opinion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.25, size = 2, color = "black") +
  labs(title = "Internet Access",
       x = "Variables",
       y = "Count",
       fill = "Reliability Level") +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette from RColorBrewer
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


library(ggplot2)
library(dplyr)
library(tidyr)

# Check the column names to verify that the selected variables exist
colnames(Final_Students_Data)

# Manually select two variables for display
selected_diseases <- c("comfort using online learning platforms")

# Check the first few rows to ensure the data is correctly structured
head(Final_Students_Data)

# Check the unique values in the 'Opinion' column for the selected diseases
unique(Final_Students_Data$`comfort using online learning platforms`)

# Check for missing values in the selected columns
sum(is.na(Final_Students_Data$`comfort using online learning platforms`))

# Filter the data set to include only selected variables and remove rows with missing values
df_selected <- Final_Students_Data %>%
  select(all_of(selected_diseases)) %>%
  drop_na()  # Remove rows with missing values

# Ensure 'Opinion' values are treated as factors with desired levels
df_selected$`comfort using online learning platforms` <- as.factor(df_selected$`comfort using online learning platforms`)

# Convert data to long format (pivot only the selected criteria)
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert Opinion to a factor with the desired order
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c("Not comfortable", "Rarely comfortable", "Sometimes comfortable", 
                                     "Always comfortable", "Mostly comfortable")))

# Calculate counts and percentages for each opinion level per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Create grouped bar chart for the selected diseases
ggplot(df_counts, aes(x = Disease, y = Count, fill = Opinion)) +
  geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_dodge(width = 0.9),
            vjust = -0.25, size = 2, color = "black") +
  labs(title = "Using Online Learning Platforms",
       x = "Variables",
       y = "Count",
       fill = "Reliability Level") +
  scale_fill_brewer(palette = "Set3") +  # Use a color palette from RColorBrewer
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#OTHER CMDs
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Load the data set
# Remove the Gender column
df <- Cadre_Specific_Data_Final_2 %>% select(-Gender)
colnames(Cadre_Specific_Data_Final_2)

# Manually select  diseases for display
selected_diseases <- c(
  "P_Obesity", "M_Obesity", "C_Obesity", 
  "P_Class 1", "M_Class 1", "C_Class 1", 
  "P_Class 2", "M__Class 2", "C__Class 2", 
  "P_Class 3", "M_Class 3", "C_Class 3", 
  "P_NAFL", "M_NAFL", "C_NAFL", 
  "P_Non-alcoholic_steato", "M__Non-alcoholic_steato", "C__Non-alcoholic_steato", 
  "P_Glomerulonephritis", "M_Glomerulonephritis", "C_Glomerulonephritis", 
  "P_Kidney_stone", "M__Kidney_stone", "C_Kidney_stone", 
  "P_Analgesic_nephropathy", "M__Analgesic_nephropathy", "C__Analgesic_nephropathy", 
  "P_Chronic kidney d", "M_Chronic_kidney d", "C_Chronic_kidney d", 
  "P_Renal disease-prerenal dis", "M__Renal disease-prerenal dis", "C__Renal disease-prerenal dis"
)
# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Final_2 %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")


# convert it to a factor with the desired order:
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c("Novice", "Advanced Beginner", "Competent", "Profecient", "Expert"))

# Calculate proportions for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Subset data for diseases that begin with "P"
df_counts_P <- df_counts %>% filter(substr(trimws(Disease), 1, 1) == "P")

# Plot the stacked bar chart for P diseases
ggplot(df_counts_P, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Converts raw counts to proportions
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  labs(
    x = "Proportion",
    y = "Disease",
    fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Subset data for diseases that begin with "M"
df_counts_M <- df_counts %>% filter(substr(trimws(Disease), 1, 1) == "M")

# Plot the stacked bar chart for M diseases
ggplot(df_counts_M, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  labs(
    x = "Proportion",
    y = "Disease",
    fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Subset data for diseases that begin with "C"
df_counts_C <- df_counts %>% filter(substr(trimws(Disease), 1, 1) == "C")

# Plot the stacked bar chart for C diseases
ggplot(df_counts_C, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  labs(
    x = "Proportion",
    y = "Disease",
    fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))


# CMDs CONTINUATION
# Manually select  diseases for display
selected_diseases <- c(
  "P_Lupus_nephritis", "M__Lupus_nephritis", "C__Lupus_nephritis", 
  "P_UTI", "M_UTI", "C_UTI", 
  "P_Polycystic_kidney_disease", "M__Polycystic_kidney_disease", "C_Polycystic_kidney_disease", 
  "P_Acute_Kidney_Injury", "M_Acute_Kidney_Injury", "C_Acute_Kidney_Injury", 
  "P_Type_1_Diabetes", "M_Type_1_Diabetes", "C_Type_1_Diabetes", 
  "P_Type_2_Diabetes", "M_Type_2_Diabetes", "C_Type_2_Diabetes", 
  "P_Drug_Induced_Diabetes", "M_Drug_Induced_Diabetes", "C_Drug_Induced_Diabetes", 
  "P_ Gestational_Diabetes", "M_ Gestational_Diabetes", "C_ Gestational_Diabetes", 
  "P_Hyperlipidemia", "M_Hyperlipidemia", "C_Hyperlipidemia", 
  "P_Hypoalphalipoproteinemia", "M_Hypoalphalipoproteinemia", "C_Hypoalphalipoproteinemia"
)
# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Final_2 %>% select(all_of(selected_diseases))

# Convert data to long format
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion")

# Convert numeric values to factor levels with labels

# convert it to a factor with the desired order:
df_long$Opinion <- factor(df_long$Opinion, 
                          levels = c("Novice", "Advanced Beginner", "Competent", "Profecient", "Expert"))

# Calculate proportions for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Subset data for diseases that begin with "P"
df_counts_P <- df_counts %>% filter(substr(trimws(Disease), 1, 1) == "P")

# Plot the stacked bar chart for P diseases
ggplot(df_counts_P, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Converts raw counts to proportions
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  labs(
    x = "Proportion",
    y = "Disease",
    fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Subset data for diseases that begin with "M"
df_counts_M <- df_counts %>% filter(substr(trimws(Disease), 1, 1) == "M")

# Plot the stacked bar chart for M diseases
ggplot(df_counts_M, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  labs(
    x = "Proportion",
    y = "Disease",
    fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))

# Subset data for diseases that begin with "C"
df_counts_C <- df_counts %>% filter(substr(trimws(Disease), 1, 1) == "C")

# Plot the stacked bar chart for C diseases
ggplot(df_counts_C, aes(y = Disease, x = Count, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +
  geom_text(aes(label = paste0(round(Percentage, 1), "%")),
            position = position_fill(vjust = 0.5), size = 3, color = "black") +
  labs(
    x = "Proportion",
    y = "Disease",
    fill = "Competency Level") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))


colnames(Cadre_Specific_Data_Modified)

#Curriculum competency CHA

library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display

selected_diseases <- c("Create_comm_health_awareness.", "Promote_prevention_of_CMDs_com...106", "Screen_clients.", "Monitor_client_progress.", "Make_referrals.", "Provides_supervision.")


# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c("not at all", "slightly", "moderately", "good", "very good")))

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
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 2
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Competency Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability


# Perfoming
colnames(Cadre_Specific_Data_Modified)
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display

selected_diseases <- c("Create_community_awareness..1", "Promote_prevention_of_cardiome", "Screen_clients..2", "Monitor_client_progress..2", "Make_referrals..2", "Provides_supervision..2")


# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c("Not all well", "Less well", "Somehow well", "Well", "Very well")))

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
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 2
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#CLINICAL MEDICINE CURRICULUM 
colnames(Cadre_Specific_Data_Modified)
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display
#Manage diabetes
selected_diseases <- c("Type_1_disease_process.\r\n1_diab", "Type_2_disease_process.\r\n__2_di", "Type_1_risk_management\r\n__1___r", "Type_2_risk_management.\r\n2___ri", 
                       "Acute_complications.\r\ncomplicat", "Long_term_complications.\r\n__(va", "Insulin_therapy.\r\ntherapy", "Insulin_devices.\r\ndevices_and_g", 
                       "Foot_assesment.\r\n__assessment_a", "Psychosocial_complications.\r\n__", "Medical_nutrition_therapy.\r\nnut", "Interpreting_results.\r\n__result", 
                       "Gestational Diabetes Mellitus.", "Screen_for_Gestational_Diabete",
                       "Diagnose_Gestational_Diabetes_", "Oral_Antidiabetic_Drugs_(OAD).")

#Cardiovascular Diseases
selected_diseases <- c("Diagnose_Hypertension.",
                       "Treat__hypertension_patients_s", "Manage_co__morbidities_to_achi", "Treat_RHD_patients.\r\npatients_w", "Diagnose_RHD.\r\nHypertension", "Implement_time_sensitive_tream", "Confirm_diag_and_determine_str", "Coordinate_diagnostic_imaging.",
                       "Monitor_and_stabilize_vital_si", "Coordinate_diagnostic_imaging.", "Confirm_diag_and_determine_str", 
                       "Implement_time_sensitive_tream", "Diagnose_RHD.\r\nHypertension", "Treat_RHD_patients.\r\npatients_w", 
                       "Diagnosis_Coronary_Artery_Dise", "Treat_CHD_patients.\r\npatients_w", "Diagnosis_of_Manage_Rheumatic_",
                       "Treat_patients_with_Rheumatic_")



, "Manage_General_dyslipidemia.",
                       "Diagnose_Combined_hyperlipidem", "Treat_Combined_hyperlipidemia.", "Diagnose_Manage_NAFL.", "Treat_Manage_NAFL.")

#Continuation
selected_diseases <- c("Diagnose_Class_1.", "Treat_Class_1.", "Diagnose__Class_2.", "Treat__Class_2.", "Diagnose_Glomerulonephritis.", "Treat_Glomerulonephritis.", "Diagnose_Kidney_stones.", "Treat_Kidney_stones.", "Diagnose_Analgesic_nephropathy", "Treat_Analgesic_nephropathy.", "Diagnose_Chronic_kidney_diseas", "Treat_Chronic_kidney_disease.", "Diagnose_Lupus_nephritist.",
                       "Treat_Lupus_nephritis.", "Diagnose__Urinary_tract_infect", "Treat__Urinary_tract_infection", "Diagnose_Polycystic_kidney_dis", "Treat_Polycystic_kidney_diseas") 
                  
# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c("Not at all", "Slightly", "Moderately", "Good", "Very good")))
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
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 2
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#NEW LABLES
library(ggplot2)
library(dplyr)
library(tidyr)

#Cardiovascular Diseases
selected_diseases <- c("Diagnose_Hypertension.",
                       "Treat__hypertension_patients_s", "Manage_co__morbidities_to_achi", "Treat_RHD_patients.\r\npatients_w", "Diagnose_RHD.\r\nHypertension", "Implement_time_sensitive_tream", "Confirm_diag_and_determine_str", "Coordinate_diagnostic_imaging.",
                       "Monitor_and_stabilize_vital_si", "Coordinate_diagnostic_imaging.", "Confirm_diag_and_determine_str", 
                       "Implement_time_sensitive_tream", "Diagnose_RHD.\r\nHypertension", "Treat_RHD_patients.\r\npatients_w", 
                       "Diagnosis_Coronary_Artery_Dise", "Treat_CHD_patients.\r\npatients_w", "Diagnosis_of_Manage_Rheumatic_",
                       "Treat_patients_with_Rheumatic_")

# Filter the dataset to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c("Not at all", "Slightly", "Moderately", "Good", "Very good")))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Define the new disease/competency labels
new_labels <- c(
  "Treat_RHD_patients_patients_w" = "Treat RHD patients",
  "Treat_patients_with_Rheumatic_" = "Treat RHD using standardized protocol",
  "Treat_CHD_patients_patients_w" = "Treat CHD patients",
  "Treat__hypertension_patients_s" = "Treat hypertension patients",
  "Monitor_and_stabilize_vital_si" = "Monitor and stabilize vital signs",
  "Manage_co__morbidities_to_achi" = "Manage co-morbidities",
  "Implement_time_sensitive_tream" = "Implement time-sensitive treatment",
  "Diagnosis_of_Manage_Rheumatic_" = "Manage Rheumatic fever",
  "Diagnosis_Coronary_Artery_Dise" = "Diagnose CAD",
  "Diagnose_RHD_Hypertension" = "Diagnose hypertension",
  "Coordinate_diagnostic_imaging" = "Coordinate diagnostic imaging",
  "Confirm_diag_and_determine_str" = "Confirm diag. and determine type of stroke"
)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability
colnames(Cadre_Specific_Data_Modified)
# FOR OTHER PARTS
library(ggplot2)
library(dplyr)
library(tidyr)

#Dyslipidemia

selected_diseases <- c("Diagnose_dyslipidemia.", "Treat_dyslipidemia.", "Diagnose_Hyperlipidemia.", "Treat_Hyperlipidemia.", 
                       "Diagnose_Combined_hyperlipidem", "Treat_Combined_hyperlipidemia.", "Diagnose_Hypoalphalipoproteine", 
                       "Treat_Hypoalphalipoproteinemia")

# Filter the dataset to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c("Not at all", "Slightly", "Moderately", "Good", "Very good")))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Define the new disease/competency labels
new_labels <- c(
  "Diagnose_dyslipidemia." = "Diagnose dyslipidemia",
  "Treat_dyslipidemia." = "Treat dyslipidemis",
  "Diagnose_Hyperlipidemia." = "Diagnose hyperlipidemia",
  "Treat_Hyperlipidemia." = "Treat hyperlipidemia",
  "Diagnose_Combined_hyperlipidem" = "Diagnose combined hyperlipidemia",
  "Treat_Combined_hyperlipidemia." = "Treat combined hyperlipidemia ",
  "Diagnose_Hypoalphalipoproteine" = "Diagnose hypoalphaliproteinemia",
  "Treat_Hypoalphalipoproteinemia" = "Treat hypoalphaliproteinemia"
)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# MASLD & Obesity renamed
library(ggplot2)
library(dplyr)
library(tidyr)

colnames(Cadre_Specific_Data_Modified)

selected_diseases <- c("Diagnose_Manage_NAFL.", "Treat_Manage_NAFL.", "Diagnose_Nonalcoholic_steatohe", 
                       "Treat_Nonalcoholic_steatohepat") 
                       
selected_diseases <- c("Diagnose_Class_1.", "Treat_Class_1.", "Diagnose__Class_2.", 
                       "Treat__Class_2.")

# Filter the dataset to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA
df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c("Not at all", "Slightly", "Moderately", "Good", "Very good")))

# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Define the new disease/competency labels
new_labels <- c(
  "Diagnose_Manage_NAFL." = "Diagnose and manage NAFL",
  "Treat_Manage_NAFL." = "Treat NAFL",
  "Diagnose_Nonalcoholic_steatohe" = "Diagnose NASH",
  "Treat_Nonalcoholic_steatohepat" = "Treat NASH"
)

new_labels <- c(  
  "Diagnose_Class_1." = "Diagnose class 1",
  "Treat_Class_1." = "Treat class 1",
  "Diagnose__Class_2." = "Diagnose class 2",
  "Treat__Class_2." = "Treat class 2"
)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability


#NURSING CURRICULUM
#Rate competense
colnames(Cadre_Specific_Data_Modified)
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display

selected_diseases <- c("Assesing_for_risk_of_cardiovas", "Assesing_for_risk_of_obesity", "Assesing_for_risk_of_MASLD\r\nAss", 
                       "Assesing_for_risk_of_Renal\r\ndis", 
                       "Assesing_for_risk_of_diabetes", "Assesing_for_risk_of_dyslipide")

selected_diseases <- c("conduct_patient_education_card", 
                       "conduct_patient_education_obes", 
                       "conduct_patient_education_MASL", "conduct_patient_education_rena", "conduct_patient_education_diab", 
                       "conduct_patient_education_dysl")

selected_diseases <- c("Advise_on_pharmacotherapeutic_...284", "Advise_on_pharmacotherapeutic_...285",
                       "Advise_on_pharmacotherapeutic_...286", "Advise_on_pharmacotherapeutic_...287", 
                       "Advise_on_pharmacotherapeutic_...288",
                       "Advise_on_pharmacotherapeutic_...289")

#Continuation
selected_diseases <- c("multidisciplinary_approach_car", "multidisciplinary_approach_obe", "multidisciplinary_approach_MAS", 
                       "multidisciplinary_approach_ren", "multidisciplinary_approach_dia", "multidisciplinary_approach_dys")

selected_diseases <- c("Performance_of_basic_diagnosti...291", "Performance_of_basic_diagnosti...292", "Performance_of_basic_diagnosti...293", 
                       "Performance_of_basic_diagnosti...294", "Performance_of_basic_diagnosti...295", "Performance_of_basic_diagnosti...296") 

selected_diseases <- c("Make_appropriate_referrals_car",
                       "Make_appropriate_referrals_obe", "Make_appropriate_referrals_met", "Make_appropriate_referrals_ren", 
                       "Make_appropriate_referrals_dia", "Make_appropriate_referrals_dys") 

# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA

df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("not all well", "less well", "Somhow well", "well", "very well")))


# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Define the new disease/competency labels
new_labels <- c(
  "Assesing_for_risk_of_cardiovas" = "Assesing for risk of cardiovascular dis.",
  "Assesing_for_risk_of_obesity" = "Assesing for risk of obesity",
  "Assesing_for_risk_of_MASLD\r\nAss" = "Assesing for risk of MASLD",
  "Assesing_for_risk_of_Renal\r\ndis" = "Assesing for risk of Renal",
  "Assesing_for_risk_of_diabetes" = "Assesing for risk of diabetes",
  "Assesing_for_risk_of_dyslipide" = "Assesing for risk of dyslipidemia"
)

new_labels <- c(
  "conduct_patient_education_card" = "patient education on cardiovascular dis.",
  "conduct_patient_education_obes" = "patient education on obesity",
  "conduct_patient_education_MASL" = "patient education on MASL",
  "conduct_patient_education_rena" = "patient education on renal dis.",
  "conduct_patient_education_diab" = "patient education on diabetes",
  "conduct_patient_education_dysl" = "patient education on dyslipidemia"
)

new_labels <- c(
  "Advise_on_pharmacotherapeutic_...284" = "pharmacotherapeutic interventions Cardiovascular",
  "Advise_on_pharmacotherapeutic_...285" = "pharmacotherapeutic interventions Obesity",
  "Advise_on_pharmacotherapeutic_...286" = "pharmacotherapeutic interventions MASLD",
  "Advise_on_pharmacotherapeutic_...287" = "pharmacotherapeutic interventions Renal dis.",
  "Advise_on_pharmacotherapeutic_...288" = "pharmacotherapeutic interventions Diabetes",
  "Advise_on_pharmacotherapeutic_...289" = "pharmacotherapeutic interventions Dyslipidemuia"
)

new_labels <- c(
  "multidisciplinary_approach_car" = "multidisciplinary approach cardiovascular dis.",
  "multidisciplinary_approach_obe" = "multidisciplinary approach cardiovascular Obesity",
  "multidisciplinary_approach_MAS" = "multidisciplinary approach cardiovascular MASLD",
  "multidisciplinary_approach_ren" = "multidisciplinary approach cardiovascular Renal dis.",
  "multidisciplinary_approach_dia" = "multidisciplinary approach cardiovascular Diabetes",
  "multidisciplinary_approach_dys" = "multidisciplinary approach cardiovascular Dyslipidemia"
)

new_labels <- c(
  "Performance_of_basic_diagnosti...291" = "Perform basic diagnostic cardiovascular",
  "Performance_of_basic_diagnosti...292" = "Perform basic diagnostic obesity",
  "Performance_of_basic_diagnosti...293" = "Perform basic diagnostic MASLD",
  "Performance_of_basic_diagnosti...294" = "Perform basic diagnostic renal dis.",
  "Performance_of_basic_diagnosti...295" = "Perform basic diagnostic diabetes",
  "Performance_of_basic_diagnosti...296" = "Perform basic diagnostic Dyslipidemia"
)

new_labels <- c(
  "Make_appropriate_referrals_car" = "Make appropriate referrals cardiovascular",
  "Make_appropriate_referrals_obe" = "Make appropriate referrals obesity",
  "Make_appropriate_referrals_met" = "Make appropriate referrals MASLD",
  "Make_appropriate_referrals_ren" = "Make appropriate referrals renal dis.",
  "Make_appropriate_referrals_dia" = "Make appropriate referrals diabetes",
  "Make_appropriate_referrals_dys" = "Make appropriate referrals dyslipidemia"
)
# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# Curriculum importance 
colnames(Cadre_Specific_Data_Modified)
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display

selected_diseases <- c("Cardiovascular.\r\ndisease", "Obesity", "Metabolic\r\nAssociated_Steatotic...222", 
                       "Renal\r\ndisease", 
                       "Diabetes", "Dyslipidemia")

selected_diseases <- c("Cardiovascular_disease", 
                       "Obesity.1", 
                       "Metabolic_Associated_Steatotic...229", "Renal_disease", "Diabetes.1", 
                       "Dyslipidemia.1")

selected_diseases <- c("Cardiovascular\r\ndisease", "Obesity.2",
                       "Metabolic\r\nAssociated_Steatotic...236", "Renal\r\ndisease.1", 
                       "Diabetes.2",
                       "Dyslipidemia.2")

#Continuation
selected_diseases <- c("Cardiovascular\r\ndisease.1", "Obesity.3", "Metabolic\r\nAssociated_Steatotic...243", 
                       "Renal\r\ndisease.2", "Diabetes.3", "Dyslipidemia.3")

selected_diseases <- c("Cardiovascular\r\ndisease.2", "Obesity.4", "Metabolic\r\nAssociated_Steatotic...250", 
                       "Renal\r\ndisease.3", "Diabetes.4", "Dyslipidemia.4") 

selected_diseases <- c("Cardiovascular\r\ndisease.4",
                       "Obesity.6", "Metabolic\r\nAssociated_Steatotic...265", "Renal\r\ndisease.5", 
                       "Diabetes.6", "Dyslipidemia.6") 

# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA

df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("not all important", "less important", "Somehow", "important", "very important")))


# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Define the new disease/competency labels
new_labels <- c(
  "Cardiovascular.\r\ndisease" = "Assesing for risk of cardiovascular dis.",
  "Obesity" = "Assesing for risk of obesity",
  "Metabolic\r\nAssociated_Steatotic...222" = "Assesing for risk of MASLD",
  "Renal\r\ndisease" = "Assesing for risk of Renal",
  "Diabetes" = "Assesing for risk of diabetes",
  "Dyslipidemia" = "Assesing for risk of dyslipidemia"
)

new_labels <- c(
  "Cardiovascular_disease" = "patient education on cardiovascular dis.",
  "Obesity.1" = "patient education on obesity",
  "Metabolic_Associated_Steatotic...229" = "patient education on MASL",
  "Renal_disease" = "patient education on renal dis.",
  "Diabetes.1" = "patient education on diabetes",
  "Dyslipidemia.1" = "patient education on dyslipidemia"
)

new_labels <- c(
  "Cardiovascular\r\ndisease" = "pharmacotherapeutic interventions Cardiovascular",
  "Obesity.2" = "pharmacotherapeutic interventions Obesity",
  "Metabolic\r\nAssociated_Steatotic...236" = "pharmacotherapeutic interventions MASLD",
  "Renal\r\ndisease.1" = "pharmacotherapeutic interventions Renal dis.",
  "Diabetes.2" = "pharmacotherapeutic interventions Diabetes",
  "Dyslipidemia.2" = "pharmacotherapeutic interventions Dyslipidemuia"
)

new_labels <- c(
  "Cardiovascular\r\ndisease.2" = "multidisciplinary approach cardiovascular dis.",
  "Obesity.4" = "multidisciplinary approach cardiovascular Obesity",
  "Metabolic\r\nAssociated_Steatotic...250" = "multidisciplinary approach cardiovascular MASLD",
  "Renal\r\ndisease.3" = "multidisciplinary approach cardiovascular Renal dis.",
  "Diabetes.4" = "multidisciplinary approach cardiovascular Diabetes",
  "Dyslipidemia.4" = "multidisciplinary approach cardiovascular Dyslipidemia"
)

new_labels <- c(
  "Cardiovascular\r\ndisease.1" = "Perform basic diagnostic cardiovascular",
  "Obesity.3" = "Perform basic diagnostic obesity",
  "Metabolic\r\nAssociated_Steatotic...243" = "Perform basic diagnostic MASLD",
  "Renal\r\ndisease.2" = "Perform basic diagnostic renal dis.",
  "Diabetes.3" = "Perform basic diagnostic diabetes",
  "Dyslipidemia.3" = "Perform basic diagnostic Dyslipidemia"
)

new_labels <- c(
  "Cardiovascular\r\ndisease.4" = "Make appropriate referrals cardiovascular",
  "Obesity.6" = "Make appropriate referrals obesity",
  "Metabolic\r\nAssociated_Steatotic...265" = "Make appropriate referrals MASLD",
  "Renal\r\ndisease.5" = "Make appropriate referrals renal dis.",
  "Diabetes.6" = "Make appropriate referrals diabetes",
  "Dyslipidemia.6" = "Make appropriate referrals dyslipidemia"
)
# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Importance Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#NUTRITION
#Rate how well the curriculum is designed to enable the learner to acquire these  CMD competencies
colnames(Cadre_Specific_Data_Modified)
library(ggplot2)
library(dplyr)
library(tidyr)

# Manually select diseases for display

selected_diseases <- c("Assess_patient_nutritional_nee...355", "Assess_patient_nutritional_nee...356", 
                       "Assess_patient_nutritional_nee...357", "Assess_patient_nutritional_nee...358", 
                       "Assess_patient_nutritional_nee...359", "Assess_patient_nutritional_nee...360") 
                       
                       
selected_diseases <- c("Assess_patient_nutritional_sta...362", "Assess_patient_nutritional_sta...363", 
                       "Assess_patient_nutritional_sta...364", "Assess_patient_nutritional_sta...365", 
                       "Assess_patient_nutritional_sta...366", "Assess_patient_nutritional_sta...367")
                       
selected_diseases <- c("Advise_relevant_nutritional_in...369", "Advise_relevant_nutritional_in...370",
                       "Advise_relevant_nutritional_in...371", "Advise_relevant_nutritional_in...372", 
                       "Advise_relevant_nutritional_in...373",
                       "Advise_relevant_nutritional_in...374")

selected_diseases <- c("Identify_nutritional_related_C", "Identify_nutritional_related_O", 
                       "Identify_nutritional_related_M...378", "Identify_nutritional_related_r", 
                       "Identify_nutritional_related_M...380", "Identify_nutritional_related_D") 
                       

selected_diseases <- c("Nutritional_education__Cardiov", "Nutritional_education_Obesity", "Nutritional_education_MASLD", 
                       "Nutritional_education_Renal_di", "Nutritional_education_Diabetes", "Nutritional_education_Dyslipid"
                      ) 

# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA

df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("not all well", "less well", "Somehow well", "well", "very well")))


# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Define the new disease/competency labels

new_labels <- c(
  "Assess_patient_nutritional_nee...355" = "Patient nutritional needs cardiovascular dis.",
  "Assess_patient_nutritional_nee...356" = "Patient nutritional needs obesity",
  "Assess_patient_nutritional_nee...357" = "Patient nutritional needs MASLD",
  "Assess_patient_nutritional_nee...358" = "Patient nutritional needs renal dis.",
  "Assess_patient_nutritional_nee...359" = "Patient nutritional needs diabetes",
  "Assess_patient_nutritional_nee...360" = "Patient nutritional needs dyslipidemia"
)

new_labels <- c(
  "Assess_patient_nutritional_sta...362" = "Patient nutritional status cardiovascular dis.",
  "Assess_patient_nutritional_sta...363" = "Patient nutritional status obesity",
  "Assess_patient_nutritional_sta...364" = "Patient nutritional status MASLD",
  "Assess_patient_nutritional_sta...365" = "Patient nutritional status renal dis.",
  "Assess_patient_nutritional_sta...366" = "Patient nutritional status diabetes",
  "Assess_patient_nutritional_sta...367" = "Patient nutritional status dyslipidemia"
)

new_labels <- c(
  "Advise_relevant_nutritional_in...369" = "Nutritional intervention cardiovascular dis",
  "Advise_relevant_nutritional_in...370" = "Nutritional intervention obesity",
  "Advise_relevant_nutritional_in...371" = "Nutritional intervention MASLD",
  "Advise_relevant_nutritional_in...372" = "Nutritional intervention renal dis.",
  "Advise_relevant_nutritional_in...373" = "Nutritional intervention diabetes",
  "Advise_relevant_nutritional_in...374" = "Nutritional intervention dyslipidemia"
)

new_labels <- c(
  "Identify_nutritional_related_C" = "Identify nutritional related cardiovascular dis.",
  "Identify_nutritional_related_O" = "Identify nutritional related obesity",
  "Identify_nutritional_related_M...378" = "Identify nutritional related MASLD",
  "Identify_nutritional_related_r" = "Identify nutritional related renal dis.",
  "Identify_nutritional_related_M...380" = "Identify nutritional related diabetes ",
  "Identify_nutritional_related_D" = "Identify nutritional related dyslipidemia"
)

new_labels <- c(
  "Nutritional_education__Cardiov" = "Nutritional education cardiovascular dis.",
  "Nutritional_education_Obesity" = "Nutritional education obesity",
  "Nutritional_education_MASLD" = "Nutritional education MASLD",
  "Nutritional_education_Renal_di" = "Nutritional education renal dis.",
  "Nutritional_education_Diabetes" = "Nutritional education diabetes",
  "Nutritional_education_Dyslipid" = "Nutritional education dyslipidemia"
)
# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability


#PHARMACY
# Rate how well the curriculum is designed in terms of competencies to equip the learner on CMDs 
library(ggplot2)
library(dplyr)
library(tidyr)

selected_diseases <- c("Ensure_optimal_pharmaceutical_...390", "Ensure_optimal_pharmaceutical_...391", 
                       "Dispense_drug_prescriptions_re", "Promote_rationale_use_of_CMD_m...393", "Adhere_to_professional_ethics_", 
                       "Provide_education_on_CMDs_to_p", "Identify_adverse_drug_reaction...396", "Advise_CMD_patients_on_appropr", 
                       "Manage_the_inventory_for_CMD_m", "Identification_of_contra_indic", "Diabetic_patients_with_renal_f",
                       "Different_categories_of_drugs", "Add_reporting_of_ADRs_pharmaco"
) 

# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA

df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(1, 2, 3, 4, 5), 
                          labels = c("Not at all", "Slightly", "Moderately", "Good", "Very Good")))


# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)

# Define the new disease/competency labels

new_labels <- c(
  "Ensure_optimal_pharmaceutical_...390" = "Optimal pharmaceutical operations",
  "Ensure_optimal_pharmaceutical_...391" = "Optimal pharmaceutical care",
  "Dispense_drug_prescriptions_re" = "Dispense drug prescriptions",
  "Promote_rationale_use_of_CMD_m...393" = "Promote rationale use of medicine",
  "Adhere_to_professional_ethics_" = "Adhere to professional ethics",
  "Provide_education_on_CMDs_to_p" = "Education on CMD medicine",
  "Identify_adverse_drug_reaction...396" = "Identify adverse drug reactions",
  "Advise_CMD_patients_on_appropr" = "Advise on appropriate medicine storage",
  "Manage_the_inventory_for_CMD_m" = "Manage medicine inventory",
  "Identification_of_contra_indic" = "Identification of contra-indications",
  "Diabetic_patients_with_renal_f" = "Diabetic patients wih renal failure",
  "Different_categories_of_drugs" = "Different categories of drugs",
  "Add_reporting_of_ADRs_pharmaco" = "ADRs reporting (pharmacovigilance)"
)
# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability


# COURSE OUTCOMES 
#CHAs
colnames(Cadre_Specific_Data_Modified)
# Rate how well the curriculum is designed to enable the learner perform the following tasks 
library(ggplot2)
library(dplyr)
library(tidyr)

selected_diseases <- c("Carry_out_community_health_edu...481", "Explain_preventive_and_managem...482", 
                       "Conduct_screening_for_CMDs", "Utilize_appropriate_health_rec...484", 
                       "Identify_patient/client_for_re...485", "Use_appropriate_performance_ap...486") 

# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA

df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not at all", "Slightly", "Moderately", "Good", "Very Good")))


# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Define the new disease/competency labels

new_labels <- c(
  "Carry_out_community_health_edu...481" = "conduct community health education",
  "Explain_preventive_and_managem...482" = "Explain CMD management measures",
  "Conduct_screening_for_CMDs" = "Conduct CMD screening",
  "Utilize_appropriate_health_rec...484" = "Appropriate health record tools",
  "Identify_patient/client_for_re...485" = "Identify patient for refferals",
  "Use_appropriate_performance_ap...486" = "Appropriate performance appraisal tools"
)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability


# Clinical Medicine curriculum outcome
colnames(Cadre_Specific_Data_Modified)
# Rate how well the curriculum is designed to enable the learner perform the following tasks 
library(ggplot2)
library(dplyr)
library(tidyr)

selected_diseases <- c("Identify_clients_at_risk_of_de...517", "Educate_and_counsel_patients/_...518", "Explain_pharmacotherapeutic_in...519", 
                       "Carry_out_basic_diagnostic_tes...520", "Take_part_in_the_multidiscipli...521", "Explain_the_nursing_process_in...522", 
                       "Explain_the_pathophysiologic_p...523", "Identify_patient/client_for_re...524") 

selected_diseases <- c("Classify_CMDs_accordingly_(bio...502", "Carry_out_a_clinical_assessmen...503", 
                       "Provide_appropriate_patient_ce...504", "Conduct_patient_education_on_C...505")

# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA

df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not at all", "Slightly", "Moderately", "Good", "Very Good")))


# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Define the new disease/competency labels

new_labels <- c(
  "Classify_CMDs_accordingly_(bio...502" = "Classify CMDs accrdingly",
  "Carry_out_a_clinical_assessmen...503" = "Conduct clinical assesment of pati.",
  "Provide_appropriate_patient_ce...504" = "Provide patient-centred care",
  "Conduct_patient_education_on_C...505" = "Conduct patient education on CMDs"
  
)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability



# Nursing Outcomes 
# Rate how well the curriculum is designed to enable the learner perform the following tasks 
library(ggplot2)
library(dplyr)
library(tidyr)

selected_diseases <- c("Identify_clients_at_risk_of_de...517", "Educate_and_counsel_patients/_...518", "Explain_pharmacotherapeutic_in...519", 
                       "Carry_out_basic_diagnostic_tes...520", "Take_part_in_the_multidiscipli...521", "Explain_the_nursing_process_in...522", 
                       "Explain_the_pathophysiologic_p...523", "Identify_patient/client_for_re...524") 

# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA

df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not at all", "Slightly", "Moderately", "Good", "Very Good")))


# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Define the new disease/competency labels

new_labels <- c(
  "Identify_clients_at_risk_of_de...517" = "Identify clients at risk_dev. CMDs",
  "Educate_and_counsel_patients/_...518" = "Educate Clients prevention & mngt. CMDS",
  "Explain_pharmacotherapeutic_in...519" = "Explain pharmacotherapeutic interventions",
  "Carry_out_basic_diagnostic_tes...520" = "Conduct diagnostoc test assesing CMDs",
  "Take_part_in_the_multidiscipli...521" = "Contribute in multidisciplinary approach in CMDs",
  "Explain_the_nursing_process_in...522" = "Explain the nursing process mng. CMDs",
  "Explain_the_pathophysiologic_p...523" = "Identify patients for refferals",
  "Identify_patient/client_for_re...524" = "Explain pathophysiologic pathw. 6 CMDs"
)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

#Nutrition Outcomes
# Rate how well the curriculum is designed to enable the learner perform the following tasks 
library(ggplot2)
library(dplyr)
library(tidyr)

selected_diseases <- c("Apply_diet_planning_in_managin...544", "Apply_diet_therapy_in_managing...545", "Assess_the_following_CMDs_indi...546", 
                       "Conduct_follow_up_on_CMD_clien...547", "Develop_relevant_interventions...548", "Diagnose_CMDs_correctly", 
                       "Provide_nutritional_education", "Refer_CMD_clients_to_health_fa...551", "Screen_for_CMDs_in_the_communi...552", 
                       "Use_special_diet_in_managing_C...553", "Use_special_feeding_methods_in...554") 

# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA

df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not at all", "Slightly", "Moderately", "Good", "Very Good")))


# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Define the new disease/competency labels

new_labels <- c(
  "Apply_diet_planning_in_managin...544" = "Diet planning in managing CMDs",
  "Apply_diet_therapy_in_managing...545" = "Diet therapy in managing CMDs",
  "Assess_the_following_CMDs_indi...546" = "Asses CMDs indicators",
  "Conduct_follow_up_on_CMD_clien...547" = "Follow up on CMD patients",
  "Develop_relevant_interventions...548" = "Develop relevant CMDs intervention",
  "Diagnose_CMDs_correctly" = "Diagnose CMDs correctly",
  "Provide_nutritional_education" = "Provide nutritional education",
  "Refer_CMD_clients_to_health_fa...551" = "Refer CMD patients to facilities",
  "Screen_for_CMDs_in_the_communi...552" = "Screen for CMDs in communities",
  "Use_special_diet_in_managing_C...553" = "Special diet in managing CMDs",
  "Use_special_feeding_methods_in...554" = "Special feeding methods in manag. CMDs"
)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability

# Pharmacy Outcome
colnames(Cadre_Specific_Data_Modified)
library(ggplot2)
library(dplyr)
library(tidyr)

selected_diseases <- c("Explain_optimal_pharmaceutical...580", "Interpret_drug_prescriptions_r...581", "Explain_rationale_use_of_CMD_m", 
                       "Demonstrate_understanding_of_p...583", "Educate_clients/patients_on_CM...584", "Provide_information_on_adverse...585", 
                       "Explain_appropriate_storage_re", "Demonstrate_understanding_of_c...587", "Identify_contraindications", 
                       "Pharmacovigilance_(report_ADRs") 

# Filter the data set to include only selected diseases
df_selected <- Cadre_Specific_Data_Modified %>% select(all_of(selected_diseases))

# Convert data to long format and ensure NA values are dropped
df_long <- df_selected %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Opinion") %>%
  drop_na(Opinion)  # Ensure NA values are fully removed

# Convert numeric values to factor levels with labels, ensuring no NA

df_long <- df_long %>%
  mutate(Opinion = factor(Opinion, 
                          levels = c(0, 1, 2, 3, 4), 
                          labels = c("Not at all", "Slightly", "Moderately", "Good", "Very Good")))


# Compute counts and percentages for each category per disease
df_counts <- df_long %>%
  group_by(Disease, Opinion) %>%
  summarise(Count = n(), .groups = "drop") %>%
  complete(Disease, Opinion, fill = list(Count = 0)) %>%  # Ensure all five levels appear
  filter(!is.na(Opinion)) %>%  # Remove any NA entries introduced by complete()
  group_by(Disease) %>%
  mutate(Percentage = Count / sum(Count) * 100)
# Define the new disease/competency labels

new_labels <- c(
  "Explain_optimal_pharmaceutical...580" = "Explain pharmaceutical care & safety",
  "Interpret_drug_prescriptions_r...581" = "Interprete drug prescriptions",
  "Explain_rationale_use_of_CMD_m" = "Rationale use of CMD medicine",
  "Demonstrate_understanding_of_p...583" = "Understanding of professional ethics",
  "Educate_clients/patients_on_CM...584" = "Educate patients on CMD medicine",
  "Provide_information_on_adverse...585" = "Information on adverse drug reactions",
  "Explain_appropriate_storage_re" = "Appropriate storage requirements",
  "Demonstrate_understanding_of_c...587" = "Understanding of inventory components",
  "Identify_contraindications" = "Identify contraindications",
  "Pharmacovigilance_(report_ADRs" = "Pharmacovigilance"
)

# Create the stacked bar chart with percentages inside bars
ggplot(df_counts, aes(y = Disease, x = Percentage, fill = Opinion)) +
  geom_col(position = "fill", width = 0.7) +  # Stacked bars
  geom_text(aes(label = ifelse(Percentage > 0, paste0(round(Percentage, 1), "%"), "")),  # Hide 0% labels for clarity
            position = position_fill(vjust = 0.5), size = 3, color = "black") +  # Font size = 3
  labs(
    x = "Proportion",
    y = "Competency",
    fill = "Expertise Level") +
  scale_fill_brewer(palette = "Set2") +  # Adjust color scheme for better contrast
  scale_y_discrete(labels = new_labels) +  # ✅ Apply correct labels
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10))  # Adjust text size for readability








































