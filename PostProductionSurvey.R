#' @title PostProductionSurvey.R
#' @description Visualizing player feedback from the Holy Dead game survey.
#' @author Dawood-Parhiar
#' @keywords barplot, pie, boxplot, likert, game feedback, summary, mean, median, standard deviation, correlation
#' @seealso https://r4ds.had.co.nz/

# Clear Console
cat("\014")

# Load Packages -----------------------------------------------------------
if (!require("tidyverse")) install.packages("tidyverse")
library(tidyverse)

# Clear Console
cat("\014")

library(readxl)

# Load Data ---------------------------------------------------------------
survey_data <- read_excel("Data/Holy_Dead_Feedback_Form_Post_Production.xlsx")

# View column names
names(survey_data)


# Clean Column Names (Optional) -------------------------------------------
# Makes them easier to reference in code
survey_data <- janitor::clean_names(survey_data)

# Barplot: Enjoyability Rating --------------------------------------------
ggplot(survey_data, aes(x = as.factor(on_a_scale_of_1_5_how_enjoyable_was_the_game))) +
  geom_bar(fill = "steelblue") +
  labs(title = "Enjoyability of the Game",
       x = "Enjoyment Rating (1 = Low, 5 = High)",
       y = "Number of Responses")

# Barplot: Was Control Easy? ---------------------------------------------
ggplot(survey_data, aes(x = were_the_controls_easy_to_learn)) +
  geom_bar(fill = "darkgreen") +
  labs(title = "Ease of Learning Controls",
       x = "Response",
       y = "Count")

# Histogram: Control Responsiveness --------------------------------------
ggplot(survey_data, aes(x = how_responsive_did_the_controls_feel_1_laggy_5_very_responsive)) +
  geom_histogram(binwidth = 1, fill = "purple", color = "white") +
  labs(title = "Control Responsiveness",
       x = "Responsiveness Rating (1–5)",
       y = "Number of Players")

# Pie Chart: Did Players Feel Immersed -----------------------------------
immersion_table <- table(survey_data$did_the_game_world_feel_immersive)
pie(immersion_table,
    col = c("lightblue", "lightpink", "lightgreen"),
    main = "Did the Game World Feel Immersive?")

ggplot(survey_data, aes(x = did_you_encounter_any_bugs_or_glitches)) +
  geom_bar(fill = "red") +
  labs(title = "Bugs or Glitches Reported",
       x = "Reported Bugs/Glitches",
       y = "Count") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Boxplot: Enjoyment by Age Group -----------------------------------------
ggplot(survey_data, aes(x = what_is_our_age_group, 
                        y = as.numeric(on_a_scale_of_1_5_how_enjoyable_was_the_game))) +
  geom_boxplot(fill = "orange") +
  labs(title = "Game Enjoyment by Age Group",
       x = "Age Group",
       y = "Enjoyment Rating (1–5)")

# Save Plots (Optional) ---------------------------------------------------
# ggsave("enjoyment_plot.png", width = 6, height = 4)

