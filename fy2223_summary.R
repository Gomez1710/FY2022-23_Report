library(tidyverse)
library(showtext)
library(tigris)

source("countyTest.R")

font_add_google("Montserrat", "Montserrat")

showtext_auto()

#read in files
fy2223 <- read.csv("awardees_nonAwardees.csv", stringsAsFactors = FALSE)

# Create a summary table of awarded applications
awardee_summary <- fy2223 %>% 
  group_by(Discipline) %>% 
  summarize("# of Awardees" = sum(Awarded. == "Awarded"),
            "Positions Funded" = sum(Positions.Awarded),
            "Funds Awarded" = sum(Funds.Awarded)) %>% 
  bind_rows(summarise_all(., ~if(is.numeric(.)) sum(.) else "Totals"))

awardee_summary$`Funds Awarded` <- scales::dollar(awardee_summary$`Funds Awarded`)

# Create a map summarizing awardee summary by county #################################################################################
#remove the word "county"
fy2223$PC <- str_remove_all(fy2223$PC, " County")
fy2223$PC <- str_trim(fy2223$PC, side = "both")

awardees <- fy2223 %>% filter(Awarded. == "Awarded")

list_of_awardees <- awardees %>% select(-4)

list_of_awardees$Funds.Awarded <- scales::dollar(list_of_awardees$Funds.Awarded)

colnames(list_of_awardees)[c(1:12)] <- c("Organziation",
                                         "Grant ID",
                                         "Discipline",
                                         "HPSA Score", 
                                         "Priority County Score", 
                                         "County", 
                                         "Payor Mix Score",
                                         "Program Type Score",
                                         "Graduate Total Score",
                                         "Final Score",
                                         "Awarded Amount",
                                         "Positions Funded")

#assign tier level
awardees$Tier.Level <- ifelse(awardees$PC_Score == 1, "Tier 3",
                            ifelse(awardees$PC_Score == 2, "Tier 2",
                                   ifelse(awardees$PC_Score == 3, "Tier 1", "")))

# create a summary for county and tiers to be merged at the end
tiers <- awardees %>% 
  group_by(PC, Tier.Level) %>% 
  summarize(n = n(),
            funds = scales::dollar(sum(Funds.Awarded)))

colnames(tiers)[1] <- "County"

#group by county, discipline and Tiers
pc_summary <- awardees %>% 
  group_by(PC, Discipline, Tier.Level) %>% 
  summarize(n = n(),
            funds = sum(Funds.Awarded))

# create a vector of unique counties
counties <- unique(pc_summary$PC)

# get the for loop ready
county_Text <- NULL

for (i in counties) {
  
  d2 <- filter(pc_summary, PC == i)
  
  county_Text[i] <- countyText(d2)
}

#create a new sheet with county and text
df <- data.frame(County = counties, mytext = county_Text)

#read in cali data for merging
cali <- counties("California", cb = TRUE)
colnames(cali)[6] <- "County"

#merge cali with df
awardee_map <- left_join(cali, df, by = "County")

# for counties that do not have awardees, provide some text
awardee_map$mytext[is.na(awardee_map$mytext)] <- "County has no awardees"

# make a summary to get county and tiers

awardee_map <- left_join(awardee_map, tiers, by = "County")


rm(awardees, d2, df, tiers, pc_summary)

# do some data cleaning ###############################################################################################################

# abbreviate discipline names
fy2223$Discipline <- str_replace_all(fy2223$Discipline, "Family Medicine", "FM")
fy2223$Discipline <- str_replace_all(fy2223$Discipline, "Internal Medicine", "IM")
fy2223$Discipline <- str_replace_all(fy2223$Discipline, "Pediatrics", "Peds")
fy2223$Discipline <- str_replace_all(fy2223$Discipline, "Obstetrics and Gynecology", "OBGYN")
fy2223$Discipline <- str_replace_all(fy2223$Discipline, "Emergency Medicine", "EM")

#remove the word county from counties
fy2223$PC <- str_remove_all(fy2223$PC, " County")
fy2223$PC <- str_trim(fy2223$PC, side = "both")

# filter for FM, IM, PEDS, OBGYN, and EM
fm <- fy2223 %>% filter(Discipline == "FM")
em <- fy2223 %>% filter(Discipline == "EM")
im <- fy2223 %>% filter(Discipline == "IM")
Peds <- fy2223 %>% filter(Discipline == "Peds")
ob <- fy2223 %>% filter(Discipline == "OBGYN")

#create boxplots for each discipline

# Family Medicine
fm_DOT <- ggplot(fm, aes(FinalScore, text = FinalScore, fill = Awarded.)) +
  geom_dotplot(binwidth = 1, stackdir = "center")

#get text position from dot plot
point.pos <- ggplot_build(fm_DOT)$data[[1]]

#order row
idx <- order(fm$FinalScore)
fm2 <- fm[idx,]

#scale fix
scale.fact <- 0.1
fm2$ytext <- point.pos$stackpos*scale.fact
fm2$xtext <- point.pos$x

#create dot plot of final score but only using the text
fm_FS <- ggplot() +
  geom_label(data = fm2, mapping = aes(x = FinalScore, y = ytext, label = FinalScore, fill = Awarded.),
             fontface = "bold",
             family = "Montserrat",
             size = 12) +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) + 
theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )


fm_FS
#plot dot plot for HPSA
fm_HPSA <- ggplot() +
  geom_label(data = fm2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(HPSA), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot dot plot for PC
fm_PC <- ggplot() +
  geom_label(data = fm2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PC_Score), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot payor mix score for fm
fm_PM <- ggplot() +
  geom_label(data = fm2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PM), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot program type score score for fm
fm_PT <- ggplot() +
  geom_label(data = fm2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PT), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

# plot grad score
fm_Grads <- ggplot() +
  geom_label(data = fm2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(Grads), fill = Awarded.),
             size = 12,
             family = "Montserrat",
             fontface = "bold") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )


# Internal Medicine ###############################################################################
im_DOT <- ggplot(im, aes(FinalScore, text = FinalScore, fill = Awarded.)) +
  geom_dotplot(binwidth = 1, stackdir = "center")

#get text position from dot plot
point.pos <- ggplot_build(im_DOT)$data[[1]]

#order row
idx <- order(im$FinalScore)
im2 <- im[idx,]

#scale fix
scale.fact <- 0.2
im2$ytext <- point.pos$stackpos*scale.fact
im2$xtext <- point.pos$x

#create dot plot of final score but only using the text
im_FS <- ggplot() +
  geom_label(data = im2, mapping = aes(x = FinalScore, y = ytext, label = FinalScore, fill = Awarded.),
             fontface = "bold",
             family = "Montserrat",
             size = 12) +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) + 
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )
im_FS
#plot dot plot for HPSA
im_HPSA <- ggplot() +
  geom_label(data = im2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(HPSA), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot dot plot for PC
im_PC <- ggplot() +
  geom_label(data = im2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PC_Score), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserra") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot payor mix score for fm
im_PM <- ggplot() +
  geom_label(data = im2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PM), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )
#plot program type score score for fm
im_PT <- ggplot() +
  geom_label(data = im2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PT), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )
# plot grad score
im_Grads <- ggplot() +
  geom_label(data = im2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(Grads), fill = Awarded.),
             size = 12,
             family = "Montserrat",
             fontface = "bold") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

# Pediatrics #########################################################################################

peds_DOT <- ggplot(Peds, aes(FinalScore, text = FinalScore, fill = Awarded.)) +
  geom_dotplot(binwidth = 1, stackdir = "center")

#get text position from dot plot
point.pos <- ggplot_build(peds_DOT)$data[[1]]

#order row
idx <- order(Peds$FinalScore)
peds2 <- Peds[idx,]

#scale fix
scale.fact <- 0.2
peds2$ytext <- point.pos$stackpos*scale.fact
peds2$xtext <- point.pos$x

#create dot plot of final score but only using the text
peds_FS <- ggplot() +
  geom_label(data = peds2, mapping = aes(x = FinalScore, y = ytext, label = FinalScore, fill = Awarded.),
             fontface = "bold",
             family = "Montserrat",
             size = 12) +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) + 
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

peds_FS
#plot dot plot for HPSA
peds_HPSA <- ggplot() +
  geom_label(data = peds2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(HPSA), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot dot plot for PC
peds_PC <- ggplot() +
  geom_label(data = peds2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PC_Score), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot payor mix score for fm
peds_PM <- ggplot() +
  geom_label(data = peds2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PM), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot program type score score for fm
peds_PT <- ggplot() +
  geom_label(data = peds2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PT), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )
# plot grad score
peds_Grads <- ggplot() +
  geom_label(data = peds2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(Grads), fill = Awarded.),
             size = 12,
             family = "Montserrat",
             fontface = "bold") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

# OBGYN #############################################################################################

ob_DOT <- ggplot(ob, aes(FinalScore, text = FinalScore, fill = Awarded.)) +
  geom_dotplot(binwidth = 1, stackdir = "center")

#get text position from dot plot
point.pos <- ggplot_build(ob_DOT)$data[[1]]

#order row
idx <- order(ob$FinalScore)
ob2 <- ob[idx,]

#scale fix
scale.fact <- 0.2
ob2$ytext <- point.pos$stackpos*scale.fact
ob2$xtext <- point.pos$x

#create dot plot of final score but only using the text
ob_FS <- ggplot() +
  geom_label(data = ob2, mapping = aes(x = FinalScore, y = ytext, label = FinalScore, fill = Awarded.),
             fontface = "bold",
             family = "Montserrat",
             size = 12) +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) + 
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

ob_FS
#plot dot plot for HPSA
ob_HPSA <- ggplot() +
  geom_label(data = ob2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(HPSA), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot dot plot for PC
ob_PC <- ggplot() +
  geom_label(data = ob2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PC_Score), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot payor mix score for fm
ob_PM <- ggplot() +
  geom_label(data = ob2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PM), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )
#plot program type score score for fm
ob_PT <- ggplot() +
  geom_label(data = ob2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PT), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )
# plot grad score
ob_Grads <- ggplot() +
  geom_label(data = ob2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(Grads), fill = Awarded.),
             size = 12,
             family = "Montserrat",
             fontface = "bold") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

# Emergency Medicine #################################################################################################

em_DOT <- ggplot(em, aes(FinalScore, text = FinalScore, fill = Awarded.)) +
  geom_dotplot(binwidth = 1, stackdir = "center")

#get text position from dot plot
point.pos <- ggplot_build(em_DOT)$data[[1]]

#order row
idx <- order(em$FinalScore)
em2 <- em[idx,]

#scale fix
scale.fact <- 0.2
em2$ytext <- point.pos$stackpos*scale.fact
em2$xtext <- point.pos$x

#create dot plot of final score but only using the text
em_FS <- ggplot() +
  geom_label(data = em2, mapping = aes(x = FinalScore, y = ytext, label = FinalScore, fill = Awarded.),
             fontface = "bold",
             family = "Montserrat",
             size = 12) +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) + 
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

em_FS
#plot dot plot for HPSA
em_HPSA <- ggplot() +
  geom_label(data = em2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(HPSA), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot dot plot for PC
em_PC <- ggplot() +
  geom_label(data = em2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PC_Score), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot payor mix score for fm
em_PM <- ggplot() +
  geom_label(data = em2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PM), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

#plot program type score score for fm
em_PT <- ggplot() +
  geom_label(data = em2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(PT), fill = Awarded.),
             size = 12,
             fontface = "bold",
             family = "Montserrat") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )

# plot grad score
em_Grads <- ggplot() +
  geom_label(data = em2,
             mapping = aes(x = FinalScore, y = ytext, label = factor(Grads), fill = Awarded.),
             size = 12,
             family = "Montserrat",
             fontface = "bold") +
  guides(fill = guide_legend(override.aes = aes(label = ""))) +
  scale_fill_manual(values = c("#10C637", "#FF982C")) +
  theme_bw() +
  labs(x = "Final Score",
       fill = "Applicants") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(size = 20,
                               family = "Montserrat",
                               face = "bold"),
    axis.title.x = element_text(size = 20,
                                family = "Montserrat"),
    legend.text = element_text(size = 18,
                               family = "Montserrat"),
    legend.title = element_text(size = 18,
                                family = "Montserrat")
  )
