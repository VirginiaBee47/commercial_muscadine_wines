rm(list=ls())
library(tidyverse)

# Create helper functions
add_specificity_rename <- function(names) {
  for(i in 1:length(names)) {
    if(str_detect(names[i], "Q7__")) {
      names[i] <- paste0(names[i], "_Aroma")
    } else if(str_detect(names[i], "Q14__")) {
      names[i] <- paste0(names[i], "_Taste")
    }
  }
  return(names)
}

remove_question_id <- function(names) {
  for(i in 1:length(names)) {
    names[i] <- str_remove(names[i], "Q[0-9]+[_]+[0-9]+[_]+")
  }
  return(names)
}

# Load the data
# Wine sensory data
reds <- read_csv("../Data/Muscadine_Red_Wine_Raw_Data.csv") %>%
  rename( # Rename columns to remove '/' character
    Q7__1__Muscadine_Fruity = `Q7__1__Muscadine/Fruity`,
    Q7__3__Apple_Pear = `Q7__3__Apple/Pear`,
    Q7__9__Green_Grassy = `Q7__9__Green/Grassy`,
    Q14__10__Green_Grassy = `Q14__10__Green/Grassy`
  ) %>%
  rename_with(~ add_specificity_rename(.), .cols = everything()) %>%
  rename_with(~ remove_question_id(.), .cols = everything()) %>%
  mutate(
    Wine_Name = case_when(
      str_detect(Sample_Name, "Lakeridge Southern Red") ~ "Lakeridge Southern Red",
      TRUE ~ Sample_Name
    )
  )
# Duplicates Muscadine/Fruity, Red_Fruit, Oxidized, Herbaceous, Green/Grassy, Jammy  

whites <- read_csv("../Data/Muscadine_White_Wine_Raw_Data.csv") %>%
  rename_with(~ add_specificity_rename(.), .cols = everything()) %>%
  rename_with(~ remove_question_id(.), .cols = everything()) %>%
  mutate(
    Wine_Name = Sample_Name
  )
# Duplicates Muscadine_Fruity, Red_Fruit, Oxidized, Honey  

combined <- full_join(reds, whites) %>%
  rename(
    Pink_Color = Pink_Red,
    Green_Color = Green,
    Brown_Color = Brown,
    Orange_Color = Orange,
    Red_Color = Red,
    Yellow_Color = Yellow
  ) %>%
  rename(
   Color_Intensity = Color_Intensity_Color_Intensity,
   Color_Liking = Color_Liking_Color_Liking,
   Muscadine_Aroma_Intensity = Muscadine_Aroma_Intensity_Muscadine_Aroma_Intensity,
   Aroma_Liking = Aroma_Liking_Aroma_Liking,
   Taste_Liking = Taste_Liking_Taste_Liking,
   Muscadine_Flavor_Intensity = Muscadine_in_the_Mouth_Intensity_Muscadine_in_the_Mouth_Intensity,
   Sweetness_Intensity = Sweetness_Intensity_Sweetness_Intensity,
   Astringency_Intensity = Astringency_Intensity_Astringency_Intensity,
   Sourness_Intensity = Sourness_Intensity_Sourness_Intensity,
   Bitterness_Intensity = Bitterness_Intensity_Bitterness_Intensity,
   Overall_Quality_Liking = Overall_Quality_Overall_Quality_Liking
  )

# Summarize data by averaging relevant columns
average_scores <- combined %>%
  select(-c(Test_Name:Sample_Position), -c(Design_Position_Name:Prefer_not_to_say)) %>%
  group_by(Wine_Name, Sample_Number) %>%
  summarise(
    across(ends_with("_Color"), mean, .names = "{.col}_avg"),
    across(ends_with("_Aroma"), mean, .names = "{.col}_avg"),
    across(ends_with("_Taste"), mean, .names = "{.col}_avg"),
    across(ends_with("_Liking"), mean, .names = "{.col}_avg"),
    across(ends_with("_Intensity"), mean, .names = "{.col}_avg"),
  )

# Define the desired order
desired_order <- c("A1", "B1", "C1", "D1", "E1", "F1", "E2", "G1", "H1", "D2", "I1", "J1", "D3", 
                   "D4", "K1", "C2", "C3", "H2", "B2", "B3", "D5", "D6", "D7", "D8", "D9", "E3", 
                   "E4", "E5", "E6", "J2")

# Arrange the dataframe according to the desired order
final_ordered_data <- average_scores %>%
  mutate(Wine_ID = factor(Sample_Number, levels = desired_order)) %>%
  select(Wine_ID, everything(), -Sample_Number) %>%
  arrange(Wine_ID) %>%
  ungroup() %>%
  select(-Wine_Name)
  
# Write the final ordered data to a CSV file
write.csv(final_ordered_data, "final_ordered_wine_data.csv", row.names = FALSE)
