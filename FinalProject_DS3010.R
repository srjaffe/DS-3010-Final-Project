library(readxl)
library(ggplot2)
library(scales)
library(dplyr)
library(tidyr)
library(corrplot)
library(stringr)
library(ggdist)


setwd("C:/Users/antho/Downloads/DS3010")

USVideosDS3010 <- read_excel("USVideosDS3010.xlsx", 
                             sheet = "Cleaned")



USVideosDS3010 %>%
  filter(views > 10000000) %>%
  count()

ggplot(USVideosDS3010, aes(views))+
  geom_histogram(bins = 100)+
  scale_x_continuous(labels = comma) +
  theme_minimal()

USVideosDS3010_filtered = USVideosDS3010 %>%
  filter(USVideosDS3010$views < 10000000)

ggplot(USVideosDS3010_filtered, aes(views))+
  geom_histogram(bins = 15)+
  scale_x_continuous(labels = comma) +
  theme_minimal()

viral = USVideosDS3010_filtered %>%
  filter(views > 2500000) %>%
  count()

notviral = USVideosDS3010_filtered %>%
  filter(views <= 2500000) %>%
  count()

viral/(viral + notviral)


cor_matrix = cor(USVideosDS3010_filtered[, c("Count_of_!", "Count_of_?", "Count_Capital_Letters", "Num_tags", 
                       "views", "likes", "dislikes",
                       "comment_count", "like_dislike_ratio", 
                       "view_comment_ratio")],
                 use = "complete.obs")

corrplot(cor_matrix,
         method = "color",    
         tl.cex = 0.8,           
         addCoef.col = "black", 
         number.cex = 0.7) 

n_category = USVideosDS3010_filtered %>%
  group_by(Category) %>%
  summarise(cat_count = n()) 

Viral_cat_counts = USVideosDS3010_filtered %>%
  filter(Viral == "Viral") %>%
  group_by(Category) %>%
  summarise(viral_count = n()) %>%
  left_join(n_category, by = "Category") %>%             
  mutate(viral_per = viral_count / cat_count) %>%       
  arrange(desc(viral_count))



Viral_cat_counts$Category <- paste0(
  str_wrap(Viral_cat_counts$Category, width = 10),
  "\n(n=", Viral_cat_counts$cat_count, ")"
)


ggplot(Viral_cat_counts, aes(x = Category, y = viral_per, fill = Category)) +
  geom_bar(stat = "identity", col = "black") +
  geom_text(aes(label = paste0(round(viral_per * 100, 1), "%"), y = viral_per + 0.01), 
            color = "black") +
  scale_fill_viridis_d(option = "G") +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1)),
    labels = percent_format()
  ) +
  labs(y = "Percent of Viral Videos by Category", x = "") +
  theme_minimal() +
  theme(axis.text = element_text(size = 11),
        axis.title = element_text(size = 13),
        legend.position = "None",
        axis.title.y = element_text(margin = margin(r = 15)))

ggplot(USVideosDS3010_filtered, aes(x = Viral, y = likes, col = Viral))+
  geom_boxplot(size = 1)+
  stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape = 19) + 
  coord_flip()

ggplot(USVideosDS3010_filtered, aes(x = likes, fill = Viral)) +
  geom_histogram(position = "identity", alpha = 0.65, bins = 40) +
  theme_minimal()

mean_likes <- USVideosDS3010_filtered %>%
  group_by(Viral) %>%
  summarise(mean_likes = mean(likes, na.rm = TRUE))

ggplot(USVideosDS3010_filtered, aes(x = Viral, y = likes, col = Viral)) +
  geom_jitter(width = 0.35, height = 0, alpha = 0.6) +
  coord_flip() +
  stat_summary(fun = mean, geom = "point", color = "black", size = 2, shape = 19) + 
  geom_text(
    data = mean_likes,
    aes(x = Viral, y = mean_likes, label = paste0("Mean: ", comma(round(mean_likes)))),
    color = "black",
    vjust = -1
  ) +
  scale_color_manual(values = c("#789DBC", "#FFB26F")) +
  theme_minimal() +
  labs(y = "# of likes", x = "") +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    legend.position = "none",
    axis.title.y = element_text(margin = margin(r = 15))
  )

clickbait_stats = USVideosDS3010_filtered %>%
  group_by(Viral) %>%
  summarise(
    "Exclamation Points" = mean(`Count_of_!`, na.rm = TRUE),
    "Question Marks" = mean(`Count_of_?`, na.rm = TRUE),
    "Capital Letters" = mean(`Count_Capital_Letters`, na.rm = TRUE)) %>%
  pivot_longer(cols = -c("Viral"), names_to = "stat", values_to = "average")

View(clickbait_stats)

ggplot(filter(clickbait_stats, clickbait_stats$stat != "Capital Letters"), 
              aes(x=Viral, y = average, fill = stat))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = round(average,2), y = average + 0.005),
            position = position_dodge(width = 0.9))+
  labs(x="", y = "Average", fill = "Punctuation") +
  geom_vline(xintercept = seq(1.5, length(unique(clickbait_stats$Viral)) - 0.5, by = 1), 
             linetype = "dashed", color = "black", size = 0.5) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(values = c("#659287", "#FFE6A9")) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 11),
    axis.title = element_text(size = 13),
    axis.title.y = element_text(margin = margin(r = 15)),
    legend.text = element_text(size = 11),        
    legend.title = element_text(size = 13),      
    legend.key.size = unit(1, "cm")             
  )

ggplot(USVideosDS3010_filtered, aes(x=Viral, y= Count_Capital_Letters, col = Viral)) +
  geom_boxplot()

