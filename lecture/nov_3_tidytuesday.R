
library(readr)
library(dplyr)
library(ggplot2)

r <- read.csv("https://raw.githubusercontent.com/jebyrnes/rum_data/main/data/merged_rum_master.csv")

str(r)

r <- r %>% 
  select(br_score, price) %>% 
  filter(price != "NA" )

plot(br_score ~price, data=r)

rum_model <- lm(br_score ~price, data=r)

rum_model <- glm(br_score ~price, data=r, family = "gaussian")

?glm

rum_residials <- residuals(rum_model)
hist(rum_residials)
shapiro.test(rum_residials)
plot(rum_model) #there is a relationship!


rum_plot <- ggplot(r, aes(x=price, y=br_score, color=br_score))+
  geom_point()+
  stat_smooth(method = "glm", fill="red")+
  scale_color_gradient(low="red", high="green")+
  labs(title="A poorly-fit model with outliers")

