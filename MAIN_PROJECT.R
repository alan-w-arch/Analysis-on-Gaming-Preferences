# Load required libraries
library(readxl)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggpubr)
library(corrplot)

# Read the data
data <- read_csv("C:/Users/shiva/Downloads/mock_gaming_data.csv")  # Replace path accordingly
View(data)

# Rename for convenience
colnames(data) <- make.names(colnames(data))

# Quick preview
str(data)
summary(data)

data <- data %>%
  select(-Timestamp)
data <- data %>%
  arrange(Your.Name)
# Convert numeric columns
data$Your.Age <- as.numeric(data$Your.Age)
data$Hours.Per.Week <- as.numeric(data$How.many.hours.per.week.do.you.spend.gaming.)
data$Hardware.Importance <- as.numeric(data$How.important.is.hardware.performance..CPU..GPU..RAM..for.your.gaming.experience.)

# Convert relevant factors
data$Platform <- as.factor(data$On.which.platform.do.you.play.games.the.most.)
data$Gender <- as.factor(data$Gender)
data$Voice.Chat <- as.factor(data$Do.you.use.voice.chat.or.communication.apps.while.gaming.)
data$Toxicity <- as.factor(data$Have.you.ever.faced.toxicity.or.harassment.while.playing.online.games.)
data$Multiplayer <- as.factor(data$Do.you.participate.in.online.multiplayer.gaming.)

# Optional: clean up levels
data$Platform <- factor(data$Platform, levels = unique(data$Platform))

platform_counts <- data %>%
  group_by(Platform) %>%
  summarise(Count = n())


# Create the line graph
ggplot(platform_counts, aes(x = Platform, y = Count ,group = 1)) +  # group = 1 ensures all points are connected
  geom_line(color = "red") +  # Line graph
  geom_point(color = "#2A9D8F") +  # Points on the line
  geom_text(aes(label = Count), vjust = -0.5) +  # Add labels on points
  labs(title = "Preferred Gaming Platform", x = "Platform", y = "Number of Players") +
  ylim(100,150)


Gender_counts <- data %>%
  group_by(Gender) %>%
  summarise(Count = n())

# Create the gender distribution bar chart
ggplot(Gender_counts, aes(x = Gender, y = Count)) +
  geom_bar(stat = "identity", fill = "#E76F51") +  
  geom_text(aes(label = Count), vjust = -0.5) +  # Add labels above the bars
  labs(title = "Gender Distribution of Respondents", x = "Gender", y = "Count")+
  ylim(0,360)



#Average Gaming Hours per Week by Gender
Gender_hours <- data %>%
  group_by(Gender) %>%
  summarise(Average_Hours = mean(How.many.hours.per.week.do.you.spend.gaming., na.rm = TRUE))

# Create the bar chart
ggplot(Gender_hours, aes(x = Gender, y = Average_Hours,color=Gender)) +
  geom_bar(stat = "identity", fill = "#264653") +  # Use stat = "identity" to use the Average_Hours values directly
  theme_minimal() +
  labs(title = "Average Gaming Hours per Week by Gender", x = "Gender", y = "Average Hours") +
  geom_text(aes(label = round(Average_Hours, 1)), vjust = -0.5)+
  ylim(0,15)


summary_data <- data %>%
  group_by(Platform, Gender) %>%
  summarise(Average_Importance = mean(Hardware.Importance, na.rm = TRUE)) %>%
  ungroup()

# Create the line chart
ggplot(summary_data, aes(x = Platform, y = Average_Importance, group = Gender, color = Gender)) +
  geom_line(size = 1) +  # Add lines
  geom_point(size = 3) +  # Add points for each average
  labs(title = "Average Hardware Importance by Platform and Gender", 
       x = "Platform", 
       y = "Average Importance (1-10 scale)") +
  ylim(0,6)



  


# Summarize the data to get the count of interested people for each gender
summary_data <- data %>%
  group_by(Gender) %>%
  summarise(
    Interested = sum(Would.you.be.interested.in.subscription.based.gaming.services.like.Xbox.Game.Pass.or.PlayStation.Plus. == "TRUE"),
    Not_Interested = sum(Would.you.be.interested.in.subscription.based.gaming.services.like.Xbox.Game.Pass.or.PlayStation.Plus. == "FALSE")
  ) %>%
  pivot_longer(cols = c(Interested, Not_Interested), names_to = "Interest_Status", values_to = "Count")

# Create the combined line chart
ggplot(summary_data, aes(x = Gender, y = Count, group = Interest_Status, color = Interest_Status)) +
  geom_line(  ) +  # Add lines for both interest statuses
  geom_point(  ) +  # Add points for each count
  geom_text(aes(label = Count), vjust = -1.5) + 
  labs(title = "Number of People Interested and Not Interested in Subscription-Based Gaming Services by Gender", 
       x = "Gender", 
       y = "Number of People") +
  ylim(0,250) 

ggplot(data, aes(x = Platform, fill = Multiplayer)) +
  geom_bar(position = "dodge") +
  theme_minimal() +
  labs(title = "Multiplayer Participation by Platform", x = "Platform", y = "Count")



Playerpreffer_counts <- data %>%
  group_by(Do.you.prefer.paid.games..free.to.play.games..or.games.with.in.app.purchases.) %>%
  summarise(Count = n(), .groups = 'drop')  # Count the number of responses

# Create the line graph
ggplot(Playerpreffer_counts, aes(x = Do.you.prefer.paid.games..free.to.play.games..or.games.with.in.app.purchases., y = Count, group = 1)) +
  geom_line(color = "black") +  # Line for counts
  geom_point( color = "red") +  # Points for each count
  geom_text(aes(label = Count), vjust = -0.5) +  # Add count labels above points
  theme_minimal() +
  labs(title = "Prefer Which type of Game paid, free or App purchases Games", 
       x = "Game Type", 
       y = "Count")+
  ylim(50,200)

summary_data <- data %>%
  group_by(What.type.of.games.do.you.prefer., Gender) %>%
  summarise(Count = n(), .groups = 'drop')  # Count the number of players for each game type and gender

# Create the bar plot
ggplot(summary_data, aes(x = reorder(What.type.of.games.do.you.prefer., -Count), y = Count, fill = Gender)) +
  geom_bar(stat = "identity", position = "dodge") +  # Create bars side by side for each gender
  geom_text(aes(label = Count), vjust = -0.5) +  # Add count labels above bars
  labs(title = "Number of Players by Game Preference and Gender", 
       x = "Type of Games Preferred", 
       y = "Number of Players") +
  theme_minimal() +  # Use a minimal theme for better aesthetics
  ylim(0,150)

Gfg_data <- data %>%
  group_by(Toxicity, Gender) %>%
  summarise(Count = n(), .groups = 'drop')  

# Create the bar plot
ggplot(Gfg_data, aes(x = Toxicity, fill = Gender),color=Gender) +
  geom_bar(aes(y = Count), position = "dodge", ,stat = "identity", color = "black") +  # Create side-by-side bars for each gender
  geom_text(aes(y = Count, label = Count), position = position_dodge(width = 0.9), vjust = -0.5) +  # Add count labels above bars
  theme_minimal() +
  labs(title = "Have You Faced Toxicity While Gaming?", 
       x = "Response", 
       y = "Count") + 
  ylim(0,200)

