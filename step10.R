# Grad analysis

source("grad_text_function.R")

grads <- read.csv("grads.csv", stringsAsFactors = FALSE)

# create a new column for discipline
grads$Discipline <- ifelse(grepl("Family", grads$Organization), "FM",
                         ifelse(grepl("Emergency", grads$Organization), "EM", 
                                ifelse(grepl("Internal", grads$Organization), "IM", 
                                       ifelse(grepl("Obstet", grads$Organization), "OBGYN", 
                                              ifelse(grepl("Pediat", grads$Organization), "Peds",
                                                     ifelse(grepl("Child", grads$Organization), "Peds",
                                                            ifelse(grepl("IM", grads$Organization), "Combined Program",
                                                                   ifelse(grepl("Anest", grads$Organization), "Combined Program",NA))))))))

# group by discipline

grad_discipline <- grads %>%  
  group_by(Discipline, Y_N_Practice_in_CA) %>% 
  summarize(Total = n()) %>% 
  pivot_wider(names_from = Y_N_Practice_in_CA,
              values_from = Total)

# create a totals row
grad_discipline <- rbind(grad_discipline, data.frame(Discipline = "Total", No = sum(grad_discipline$No), Yes = sum(grad_discipline$Yes)))

# add in a percent column
grad_discipline <- grad_discipline %>% 
  mutate("Percent Yes" = round(Yes / (No + Yes), 2))

# group by county and discipline to get numbers of of grads

county_grads <- grads %>% 
  filter(In.Ca.or.OOS == " CA" &
           Y_N_Practice_in_CA == "Yes")

# get the number of grads that practice in cA but dont have a county
no_county <- sum(county_grads$County == " --Select One-- ")

# group by county and discipline
cali_grads <- county_grads %>% 
  filter(County != " --Select One-- ") %>% 
  group_by(County, Discipline) %>%
  summarize(n = n())

cali_grads$County <- str_remove(cali_grads$County, "County")
cali_grads$County <- str_trim(cali_grads$County, "both")

# group by county to get total grads

total_grads <- cali_grads %>% 
  group_by(County) %>% 
  summarize(n = sum(n))

# create table for grad text
counties <- unique(cali_grads$County)

gradtexts <- NULL

for (i in counties) {
  
  d2 <- filter(cali_grads, County == i)
  
  gradtexts[i] <- grad_text(d2)
}

df <- data.frame(County = counties, mytext = gradtexts)

rm(list = c("d2", "i", "gradtexts", "counties", "grads", "county_grads", "cali_grads"))

# merge total grads with sheet with text

grad_set <- left_join(total_grads, df, by = "County")

#read in cali map
cali <- counties("California", cb = TRUE)

colnames(cali)[6] <- "County"

# merge cali map with total_set

grads_cali <- left_join(cali, grad_set, by = "County")

rm(list = c("df", "grad_set", "total_grads"))





