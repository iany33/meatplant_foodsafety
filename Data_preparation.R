
pacman::p_load(
  rio,          
  here,         
  skimr,        
  tidyverse,     
  gtsummary,    
  rstatix,      
  corrr,        
  janitor,      
  lubridate,     
  forcats      
)

audits <- import(here("data", "Audit_data_RTE_meat_plants.xlsx"))

plant_characteristics <- import(here("data", "RTE_plant_characteristics_clean.xlsx"))

# Combine the two datasets based on unique plant ID number
audits <- left_join(audits, plant_characteristics, by = "Plant_ID")

# Get overview of dataset, including variable types
skim(audits)

# Convert date variables to appropriate R date class
audits <- audits |>  
  mutate(across(.cols = where(is.POSIXct), .fns = as.Date))

# Create new/dichotomous versions of item result variable

audits <- audits |>
  mutate(Result2 = case_when(
    Result == "Pass"  ~ "Pass",
    Result == "Fail"  ~ "Fail",
    Result == "Not Applicable"  ~ NA_character_,)) |>
  mutate(Result2 = fct_relevel(Result2, "Pass", "Fail"))
table(audits$Result2, useNA = "always")

# Add count variables of audit item passes/fails and total items assessed (passes + fails) to dataset for GEE analysis

audits <- audits |> 
  group_by(Audit_num) |>
  mutate(
   items_assessed = sum(!is.na(Result2)),     
   n_pass  = sum(Result2 == "Pass", na.rm=T),  
   n_fail  = sum(Result2 == "Fail", na.rm=T),
   pass_rate = n_pass/items_assessed,
   fail_rate = n_fail/items_assessed)

# Create new dataset with the audit item stat variables created above and merge with wide dataset

audits <- audits |>
  distinct(Audit_num, items_assessed, n_pass, n_fail, fail_rate, pass_rate, .keep_all = TRUE)

audits <- audits |> 
  select(Plant_ID, Operation_type.x, Audit_num, Start_date, End_date,
         Rating, Num_employees, Seasonal_operations, Food_service_area,
         Food_service, Catering, Municipal_water, 
         Sausages, Blood_products, Dried_meats, Fermented_meats, 
         Jerky, Wet_cured, Warm_smoked, Hot_smoked,
         items_assessed, n_pass, n_fail, fail_rate, pass_rate) |> ungroup()

# Create new version of audit pass rating

audits <- audits |>
  mutate(Rating2 = case_when(
    Rating == "Pass" ~ 1,
    Rating == "Fail" | Rating == "Conditional-Pass"  ~ 0)) |>
  mutate(Rating2 = as.numeric(Rating2))

audits |> tabyl(Rating)
audits |> tabyl(Rating2)

audits |> group_by(Plant_ID) |> summarize(count = n())

# Create new year and day of week variables 

audits <- audits |> 
  mutate(year = as.factor(format(Start_date,'%Y'))) |> 
  mutate(week_day = as.factor(wday(Start_date))) 

audits <- audits |> mutate(year = fct_relevel(year, sort))

# Graphs of outcomes

ggplot(data = audits, mapping = aes(x = year, y = pass_rate, fill = year)) +
  geom_violin(width = 1) +
  geom_boxplot(width = 0.2) +  
  theme_minimal() +
  theme(legend.position = "none") +
  scale_fill_viridis_d() +
  labs(
    x = "Year",
    y = "Audit item pass rate" )

ggplot(data = audits, mapping = aes(x = Operation_type.x, y = pass_rate, fill = Operation_type.x)) +
  geom_violin(width = 1) +
  geom_boxplot(width = 0.2) +                    
  theme_minimal() +
  theme(legend.position = "none") +
  labs(
    x = "Establishment type",
    y = "Audit item pass rate" )


# Plot overall pass/conditional pass/fail proportion by year

ggplot(audits) +
  geom_bar(mapping = aes(x = year, fill = Rating)) +                    
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "Audit rating",
       x = "Year",
       y = "Number of audits")  +
  scale_fill_manual(values = c("Pass" = "#009E73",
                               "Conditional-Pass" = "#F0E442",
                               "Fail" = "#D55E00"))

# Express as percentage out of 100 for each bar instead for easier comparisons
rating_year <- audits |> 
  group_by(year, Rating) |> 
  summarise(count = n()) |> 
  mutate(perc = count/sum(count))

ggplot(rating_year) +
  geom_bar(mapping = aes(x = year, y = perc*100, fill = Rating), stat="identity") +                    
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "Audit rating",
    x = "Year",
    y = "Percent" ) +
  scale_fill_manual(values = c("Pass" = "#009E73",
                               "Conditional-Pass" = "#F0E442",
                               "Fail" = "#D55E00"))

# Plot audit pass proportion by establishment type
rating_type <- audits |> 
  group_by(Operation_type.x, Rating) |> 
  summarise(count = n()) |> 
  mutate(perc = count/sum(count))

ggplot(rating_type) +
  geom_bar(mapping = aes(x = Operation_type.x, y = perc*100, fill = Rating), stat="identity") +                    
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "Audit rating",
       x = "Establishment type",
       y = "Percent" ) +
  scale_fill_manual(values = c("Pass" = "#009E73",
                               "Conditional-Pass" = "#F0E442",
                               "Fail" = "#D55E00"))

ggplot(audits) +
  geom_bar(mapping = aes(x = Operation_type.x, fill = Rating)) +                    
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(fill = "Audit rating",
       x = "Establishment type",
       y = "Number of audits")  +
  scale_fill_manual(values = c("Pass" = "#009E73",
                               "Conditional-Pass" = "#F0E442",
                               "Fail" = "#D55E00"))


# Audit item pass rate distribution 

ggplot(audits) +
  geom_histogram(mapping = aes(x = pass_rate), binwidth = 0.003,
                 fill="#69b3a2", color="#e9ecef") +   
  theme_minimal() +
  labs(x = "Audit item pass rate",
       y = "Number of audits") 

ggplot(audits) +
  geom_histogram(mapping = aes(x = fail_rate), binwidth = 0.005,
                 fill="#69b3a2", color="#e9ecef") +   
  theme_minimal() +
  labs(x = "Audit item fail rate",
       y = "Number of audits") 


# Recode variables and create some combinations

cols <- c("Operation_type.x", "Seasonal_operations", "Food_service_area", 
          "Municipal_water", "Sausages", "Blood_products", 
          "Dried_meats", "Fermented_meats", "Jerky", 
          "Wet_cured", "Warm_smoked", "Hot_smoked") 

library(magrittr)
audits %<>% mutate_at(cols, factor)
remove(cols)

audits <- audits |>
  mutate(Food_service_catering = case_when(
    Food_service == "Yes" | Catering == "Yes" ~ "Yes",
    TRUE ~ "No")) |> 
  mutate(Food_service_catering = as.factor(Food_service_catering))

audits <- audits |>
  mutate(Smoked = case_when(
    Warm_smoked == "Yes" | Hot_smoked == "Yes" ~ "Yes",
    TRUE ~ "No")) |> 
  mutate(Smoked = as.factor(Smoked))

### Descriptive stats of selected plant characteristics ###

audits |> 
  select(Operation_type.x, Seasonal_operations, Food_service_area,
         Municipal_water, Sausages, Blood_products, 
         Dried_meats, Fermented_meats, Jerky, Wet_cured, Smoked) |>  
  tbl_summary(digits = list(all_categorical() ~ c(0, 1)), type = all_categorical() ~ "categorical") 

audits |> 
  get_summary_stats(Num_employees, pass_rate, fail_rate, items_assessed, type = "full")

# Obtain descriptive stats at plant level

audits |> distinct(Plant_ID, .keep_all=TRUE) |> 
  get_summary_stats(Num_employees)

audits |> distinct(Plant_ID, .keep_all=TRUE) |> 
  select(Operation_type.x, Seasonal_operations, Food_service_area,
         Municipal_water, Sausages, Blood_products, 
         Dried_meats, Fermented_meats, Jerky, Wet_cured, Smoked) |>  
  tbl_summary(digits = list(all_categorical() ~ c(0, 1)), type = all_categorical() ~ "categorical") 


# Center and standardize number of employees variables

audits <- audits |> 
  mutate(Num_employees_s = (Num_employees - mean(Num_employees, na.rm=TRUE)) / sd(Num_employees, na.rm=TRUE))

audits |> 
  get_summary_stats(Num_employees, Num_employees_s, type = "full")


# Export dataset for analysis

pacman::p_load(xlsx)
write.xlsx(audits, "audit_data_analysis.xlsx")

