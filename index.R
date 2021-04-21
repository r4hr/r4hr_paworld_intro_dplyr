# Presentation url: https://how-to-clean-data-quickly.netlify.app/#1
# GitHub repo: "https://github.com/r4hr/r4hr_paworld_intro_dplyr/"
 

# Install tidyverse packages for the first time
install.packages("tidyverse")

# Load tiverse packages
library(tidyverse)


# A Tidyverse sequence --------------------
# hr_data %>%                               # Call the dataframe
#   select(position, salary) %>%            # Picks columns to analyse
#   filter(salary < 500000) %>%             # Filter salaries below $ 500.000
#   group_by(position) %>%                  # Groups the results by position
#   summarise(mean_salary = mean(salary))   # Calculate mean salary for position




# Code WITHOUT the pipe operator
# arrange(filter(select(salaries, role, base_salary, industry), role == "HRBP"), desc(base_salary))


# Code WITH the pipe operator
# salaries %>%
#   select(role, base_salary, industry) %>%
#   filter(role == "HRBP") %>%
#   arrange(desc(base_salary))



# The Analysis Process
# 1. Define an analysis goal or target.
# 2. Assemble and clean your data
# 3. Use the Ben Teusch approach: group, summarise, and plot.




# Load the dataset -------------------------------
library(tidyverse)

options(scipen = 999) # Changes the way numbers are plot in axis


# The url link to the location of the dataset
url_link <- "https://raw.githubusercontent.com/r4hr/r4hr_paworld_intro_dplyr/main/salary_survey_ar.csv"

# Load the csv file from GitHub
salaries <- read_delim(url_link,    #<<
                       delim = ";") #<<



# Let's explore the structure of the dataset
glimpse(salaries)




# Analyse distribution of base_salary

ggplot(salaries, 
       aes(x = base_salary)) +
  geom_histogram()

summary(salaries$base_salary)




# filter() ------------------------------------------

# Filter data of CHRO
salaries %>% 
  filter(role == "CHRO") #<<

# Pass all data BUT CHRO
salaries %>% 
  filter(role != "CHRO") 

# Passing two conditions to filter
salaries %>% 
  filter(industry == "Construction" & gender == "Female") 


# This doesn't work, why?
salaries %>% 
  filter(industry == "Media" & industry == "Chemical Industry") %>%  
  select(role, base_salary) 

# Because you work at one industry or the other (| OR operator )
salaries %>% 
  filter(industry == "Media" | industry == "Chemical Industry") %>%  
  select(role, base_salary)


# Best-practice: filter with a vector of selection
salaries %>% 
  filter(industry %in% c("Media", "Health", "Technology")) %>% 
  select(role, base_salary)


# Exercise for filtering -------------------------  
salaries_clean <- salaries %>% 
  filter(between(base_salary, # Variable to filter 
                 20000,       # Low filter 
                 500000),     # Top filter 
         hr_experience_years < 30) 



# A better look into salary distribution
ggplot(salaries_clean, aes(x = base_salary))+
  geom_histogram()



# Select() ----------------------------------------
# We can use select not only to choose variables to include, but also to exclude variables.
salaries_clean <- salaries_clean %>% 
  select(-age) # Exclude age variable


data.frame(
    "Currency" = c("Euro", "Dollar", "Pound Sterling", "Swiss Franc", "Yen", "Real", "Canadian Dollar" ),
    "Divide by" = c(112, 93, 127, 102, .86, 17, 74)
  )



# Mutate() ---------------------------------------

# Adding a new variable
salaries_clean <- salaries_clean %>%     # Overwrite the same dataframe
  mutate(int_salary = base_salary / 112) # Estimate salary in Euro 

glimpse(salaries_clean)


# Plot mean salary per role
salaries_clean %>% 
  group_by(role) %>% 
  summarise(mean_salary = mean(int_salary)) %>% 
  ggplot(aes(x = mean_salary, y = role)) +
  geom_col()

# Transforming a variable from chr to factor
salaries_clean <- salaries_clean %>% 
  mutate(role = factor(role,         # Overwrite the same variable #<<
                       # Define the order of the variable
                       levels = c("Administrative", "Analyst", "Supervisor",       #<<
                                  "HRBP", "Head", "Manager", "CHRO"))) #<<


# Plot with a more organized look
salaries_clean %>% 
  group_by(role) %>% 
  summarise(mean_salary = mean(int_salary)) %>% 
  ggplot(aes(x = mean_salary, y = role)) +
  geom_col()




# if_else -------------------
salaries_clean <- salaries_clean %>% 
  mutate(engaged = if_else(satisfaction >= 4, # Condition #<<
                           1,                 # Value if true #<<
                           0))                # Value if false #<<


# case_when---------------------------------------------
salaries_clean <- salaries_clean %>% 
  rename(number_employees = company_size) %>% # Another dplyr function!
  mutate(company_size = case_when(        
    number_employees < 500 ~ "Small",     
    number_employees < 2000 ~ "Medium",   
    TRUE ~ "Large"                        
           ))



# Group_by -----------------------------
salaries_clean %>%
  group_by(industry, role) %>% # You can group by more than one column
  summarise(mean_salary = mean(int_salary),
            sd_salary = sd(int_salary))


# Summarise -----------------------------
salaries_clean %>% 
  group_by(university_type) %>%              # Variable of interest to group
  summarise(mean_salary = mean(int_salary))  # Create a new variable with a summary statistics




gender_median_salary <- salaries_clean %>% 
  group_by(role, gender) %>% 
  summarise(median_salary = median(int_salary))

gender_median_salary


ggplot(gender_median_salary,   # Dataframe
       aes(x = role,           # Map role to the x axis
           y = median_salary,  # Map median_salary to the y axis
           fill = gender)) +   # Map gender to color the bars
  geom_col(position = "dodge")


# Create a vector of colours
gender_colors <- c("#8624F5", "#1FC3AA")

# Use it in your plot
ggplot(gender_median_salary,   # Dataframe
       aes(x = role,           # Map role to the x axis
           y = median_salary,  # Map median_salary to the y axis
           fill = gender)) +   # Map gender to color the bars
  geom_col(position = "dodge") +
  scale_fill_manual(values = gender_colors) + #<<
  theme_minimal()              # Adds a theme layer #<<



# Boxplot to see data distributions
ggplot(salaries_clean, 
       aes(x = role,
           y = int_salary,
           fill = gender)) +
  geom_boxplot() +
  scale_fill_manual(values = gender_colors) +
  theme_minimal()



# Dumbbell to plot salary gap --------------------
# Create a new dataframe
gap_analysis <- salaries_clean %>% 
  select(role, gender, int_salary)


# Calculate the mean salary for gender and for each position
gap <- gap_analysis %>% 
  group_by(gender, role) %>% 
  summarise(mean_salary = mean(int_salary))

gap


# Pivot data and estimate the salary gap
library(scales)

gap_wider <- gap %>% 
  pivot_wider(., # With the point we're using all the variables.
              names_from = gender,  # Names for the new columns
              values_from = mean_salary) %>% # Values for the new columns
  mutate(salary_gap = percent((Male-Female)/Male, 1),
         x = (Male + Female)/2) # The mean of both salaries, we need it for the plot

gap_wider



library(ggalt)

# Basic dummbell plot
p <- ggplot(gap_wider,                             # Save the plot in a object
       aes(x = Female, xend = Male, y = role,
           group = role, label = salary_gap)) +
   geom_dumbbell(color = "#808080",                # Make the dumb bell plot #<<
                size_x = 3, size_xend = 3, #<<
                colour_x = gender_colors[1], #<<
                colour_xend = gender_colors[2],
                show.legend = TRUE) #<<


p

p +
geom_text(data = gap_wider,
            aes(x, role, label = salary_gap), nudge_y = .2) +
  theme_minimal() +
  labs(title = "Gender Pay Gap in HR Positions",
       subtitle = "Data Salary from Argentina",
       x ="", y = "Role",
       caption = "Source: Club de R para RRHH \n For the People Analytics World 2021") +
  annotate("text",x = 1340, y = "Manager", label = "Female", color = "#8624F5",size = 4) +
  annotate("text",x = 1950, y = "Manager", label = "Male", color = "#1FC3AA", size = 4) 
  



