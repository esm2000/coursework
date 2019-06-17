library(tidyverse)
View(table1)
View(table2)
View(table3)
View(table4a) # cases
View(table4b) # population

# -------------------------------------------------------------
#               P A G E   1 5 1 :   # 2
# -------------------------------------------------------------
# Extract the number of TB cases per country per year
tb_cases <- table2 %>%
  spread(type,count) 

# Extract the matching population per country per year
 pop <- table4b %>%
  gather('year','population','1999','2000')

cases <- table4a %>%
  gather('year','cases','1999','2000')

# Divide the cases by population, and multiple by 10,000

# With Table 2
rate_table <- tb_cases %>%
  mutate(rate = cases / population * 10000) 

View(rate_table)

# With Table 4
combined <- inner_join(cases, pop, by = c('country','year'))

rate_table2 <- combined %>%
  mutate(rate = cases / population * 10000)

View(rate_table2)

# -------------------------------------------------------------
#               P A G E   1 5 6 :   # 1   &   # 3
# -------------------------------------------------------------

# Why are gather() and spread() not perfectly symmetrical?
# Carefully consider the following example:

stocks <- tibble(
  year = c(2015,2015,2016,2016),
  half = c(1, 2, 1, 2),
  return = c(1.88,0.59,0.92,0.17)
)

stocks %>%
  spread(year,return) %>%
  gather('year','return', '2015':'2016')

# It's not perfectly symmetrical because the order of the columns
# is not maintained. Also, because gather() cannot accept doubles
# as its arguments, the variable type of 'half' gets converted
# from character to double

# Why does spreading this tibble fail? How could you add a new 
# column to fix the problem?

people <- tribble(
  ~name,             ~key,    ~value,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)
View(people)

# Spreading this tibble fails because there's nothing to 
# differiate "Phillip Woods | Age"

people %>%
  mutate(row = c(1,1,2,1,1)) %>%
  spread(key,value)



