library(tidyverse)
library(openxlsx)
library(lubridate)

encuesta_rh <- read_delim("https://raw.githubusercontent.com/r4hr/r4hr_introduccion_dplyr/main/Datos/encuesta_sueldos.csv", delim = ",")

salaries <- read_delim("salary_survey_ar.csv", delim = ";")

salaries <- encuesta_rh %>% 
  rename(gender = genero,
         age = edad,
         university_type = universidad,
         industry = rubro,
         company_size = empleados,
         company_type = origen_capital,
         hr_employees = empleados_rh,
         role = puesto,
         team_managed = personas_a_cargo,
         hr_experience_years = anios_rh,
         base_salary = sueldo_bruto,
         rise = aumento,
         satisfaction = satisfaccion,
         looking_for_job = buscando_trabajo) %>% 
  select(-erp, -rise) %>% 
  mutate_if(is.character, as.factor)

glimpse(salaries)

salaries <- salaries %>% 
  mutate(gender = fct_recode(gender, "Female" = "Femenino",
                             "Male" = "Masculino"),
         university_type = fct_recode(university_type, "Private" = "Universidad Privada",
                                      "Public" = "Universidad Pública"),
         company_type = str_replace(company_type, "acional", "ational")
         ) 

str(salaries)

salaries %>% 
  group_by(industry) %>% 
  tally() %>% 
  print(n = Inf)

salaries <- salaries %>% 
  mutate(industry = fct_recode(industry, "Farming" = "Agricultura; plantaciones,otros sectores rurales",
                               "Food and Beverages" = "Alimentación; bebidas; tabaco",
                               "Banking and Finantial Services" = "Bancos; banca online;",
                               "Banking and Finantial Services" = "Servicios financieros; seguros",
                               "Retail" = "Comercio",
                               "Construction" = "Construcción",
                               "Education" = "Educación",
                               "Goverment" = "Función pública",
                               "Hospitality" = "Hotelería, restauración, turismo",
                               "Chemical Industry" = "Industrias químicas",
                               "Media" = "Medios de comunicación; cultura; gráficos",
                               "Mining" = "Minería (carbón, otra minería)",
                               "Other" = "Otros",
                               "Other" = "Producción de metales básicos",
                               "Other" = "Silvicultura; madera; celulosa; papel",
                               "Oil and Gas" = "Petróleo y producción de gas; refinación de petróleo",
                               "Automotive" = "Terminales automotrices, fábricas autopartistas, y afines",
                               "Techonology" = "Tecnologías de Información, Sistemas, y afines",
                               "Transportation" = "Transporte (incluyendo aviación civil; ferrocarriles por carretera)",
                               "Transportation" = "Transporte marítimo; puertos;",
                               "Textile" = "Textiles; vestido; cuero; calzado",
                               "Consulting Services" = "Servicios profesionales",
                               "Consulting Services" = "Servicios de consultoría",
                               "Health" = "Servicios de salud",
                               "TelCo" = "Servicios de correos, y de telecomunicaciones",
                               "Public Services" = "Servicios públicos (agua;gas; electricidad)")) 

salaries %>% 
  group_by(role) %>% 
  tally()

salaries <- salaries %>% 
  mutate(role = fct_recode(role,
                           "Administrative" = "Administrativo",
                           "Analyst" = "Analista",
                           "CHRO" = "Director",
                           "Manager" = "Gerente",
                           "Head" = "Jefe",
                           "Supervisor" = "Responsable")) 

salaries %>% 
  group_by(looking_for_job) %>% 
  tally()

salaries <- salaries %>% 
  mutate(looking_for_job = fct_recode(looking_for_job,
                                      "Not looking for a job" = "No estoy buscando cambiar",
                                      "No but open to opportunities" = "No,  pero escucho ofertas",
                                      "Actively looking for a job" = "Sí, activamente"))

write_delim(x = salaries, file = "salary_survey_ar.csv", delim = ";")

glimpse(salaries)

arrange(filter(select(salaries, role, base_salary, industry), role == "HRBP"), desc(base_salary))


ggplot(salaries, aes(x = base_salary)) +
  geom_histogram()


salaries %>% 
  group_by(role) %>% 
  tally() %>% 
  arrange(n)

salaries %>% 
  filter(industry == "Construction" & gender == "Female")


salaries %>% 
  filter(industry == "Media") %>% 
  mutate(int_salary = base_salary / 74) %>% 
  select(base_salary, int_salary)
