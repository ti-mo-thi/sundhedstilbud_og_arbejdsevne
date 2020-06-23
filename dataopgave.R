library(readxl)
library(tidyverse)
library(cluster)  
library(corrplot)
library(ggplot2)


# Load data
data_raw<- read_excel("~/Documents/na18_tableau_resultater_Migrated Data.xls")


# Filter data
topics_list <- c("Arbejdsevne",
            "Arbejdsrelateret sygdom",
            "Begrænset i arbejdet pga. smerter" ,
            "Depressive symptomer",
            "Mental sundhed",
            "Selvvurderet helbred",
            "Tilbud om behandlingsordning",
            "Tilbud om motionsfaciliteter eller ugentlig motion",
            "Tilbud om rygestop",
            "Tilbud om små motionsaktiviteter i dagligdagen",
            "Tilbud om sund kost/kostvejledning",
            "Tilbud om sundhedstjek",
            "Vitalitet")

names(data_raw) <- gsub(" ", "_", names(data_raw)) # remove spaces
names(data_raw) <- tolower(names(data_raw)) # tranform to lower case column names
data <- data_raw %>% 
  filter(main_group == 'Branche' | main_group == 'Job') %>%
  filter(title %in% topics_list)
#%>%
  # mutate(score = case_when(
  #   hoej_score_godt == 1 ~ score,
  #   hoej_score_godt == 0 ~ score * (-1),
  #   TRUE ~ hoej_score_godt
  # ))


# From long to wide
data_wide <- data %>%
  select(group, question, score) %>%
  drop_na() %>%
  mutate(id=1:n()) %>%
  spread(question, score) %>%
  select(-id) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  group_by(group) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE)

summary(data_wide)
head(data_wide)

# Correlation analysis
scaled <- scale(data_wide[, -1])
cor <- cor(scaled)
corrplot(cor, method="color", addCoef.col = "black", diag=FALSE)


# Insights 
# Offering sports or general treatment opportunities shows a positive association with workability, work related sicknesses, and vitality
# Tilbud af motion og behandling er positivt associerert med vuderingen af arbejdsevnen, arbejds relaterede sygdome og vitalitet 

# Development over the years
data_wide_years <- data %>%
  select(question, year, score) %>%
  mutate(year = as.character(year)) %>%
  drop_na() %>%
  mutate(id=1:n()) %>%
  spread(question, score) %>%
  select(-id) %>%
  mutate_if(is.numeric, ~replace(., is.na(.), 0)) %>%
  group_by(year) %>%
  summarise_if(is.numeric, mean, na.rm = TRUE) %>%
 # mutate_if(is.numeric, scale) %>%
  mutate(year = as.numeric(year))

summary(data_wide_years)
head(data_wide_years)

# Visualization
ggplot(data_wide_years, aes(x=year), group = 1) + 
  #geom_line(aes(y = ArbEvne), color = "darkred", size = 1) + 
  geom_line(aes(y = Behandling), color="steelblue", linetype="twodash", size = 1) +
  geom_line(aes(y = Motionsakt), color="darkgreen", linetype="twodash") + 
  geom_line(aes(y = Rygestop), color="darkblue", linetype="twodash") +
  theme_bw() 
# Cluster Analysis
scaled_filt <- dplyr::select(as.data.frame(scaled), ArbEvne, Behandling)
k <- kmeans(scaled_filt[, ], centers = 2, nstart = 25)
k

# Elbow Plot
wssplot <- function(scaled_filt, nc=15, seed=1234){
  wss <- (nrow(scaled_filt)-1)*sum(apply(scaled_filt,2,var))
  for (i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(scaled_filt, centers=i)$withinss)}
  plot(1:nc, wss, type="b", xlab="Number of Clusters",
       ylab="Within groups sum of squares")}

wssplot(scaled_filt, nc=10) 

k2 <- kmeans(scaled_filt[, ], centers = 5, nstart = 25)
k2 #Insight 2 jobs and industries in cluster 1 are potential target groups

# Target group
clust_data <- cbind(data_wide[, 1], k2$cluster) %>%
  filter(k2$cluster == 1)
names(clust_data) <- c("group", "cluster")





