library(dplyr)
library(readxl)
library(ggplot2)
library(tidyverse)
library(colorspace)
library(RColorBrewer)
library(lubridate)
library(dplyr)
library(readxl)
library(tidytext)
library(dplyr)
library(syuzhet)
library(lubridate)
library(ggplot2)
library(scales)
library(readr)
library(class)
library(caret)
library(stringr)
library(tm)
library(textclean)
library(SnowballC)
library(ROSE)
library(reshape2)
library(wordcloud)
library(ggrepel)
library(lexicon)
library(textdata)
library(forcats)

cof <- read.csv("simplified_coffee.csv")


head(cof)

colnames(cof)

is.na(cof)

dim(cof)

str(cof)

duplicated(cof)

unique(cof$roast)
cof$roast[cof$roast == ""] <- "Other"

cof$full_date <- paste0("01 ", cof$review_date)
cof$full_date <- dmy(cof$full_date)
cof$year <- year(cof$full_date)


#custom palettes
display.brewer.all()
pastel1 <- brewer.pal(n = min(9, brewer.pal.info["Pastel1", "maxcolors"]), name = "Pastel1")
pastel2 <- brewer.pal(n = min(8, brewer.pal.info["Pastel2", "maxcolors"]), name = "Pastel2")
brbg <- brewer.pal(n = min(8, brewer.pal.info["Spectral", "maxcolors"]), name = "BrBG")
# Combine the palettes into one
my_palette2 <- colorRampPalette(c(pastel1, brbg, pastel2))
my_palette <- c(pastel1, brbg, pastel2)


unique(cof$origin)
unique(cof$roast)
unique(cof$rating)

#graph1----------------------------------------------------------------------------------------------------


total_roast_counts_1 <- cof %>%
  group_by(origin ) %>%
  summarise(total_roast_counts_1 = n())

ggplot(total_roast_counts_1, aes(x = total_roast_counts_1, y = reorder(origin, total_roast_counts_1), size = total_roast_counts_1, fill = total_roast_counts_1)) +
  geom_point(shape = 21, color = "black") +  
  labs(x = "Uretim", y = "Ulke", title = "Ulkelere Gore Toplam Kahve Uretimi") +
  scale_fill_gradient(low = "white", high =  "#BF812D", name = NULL) +  
  scale_size(name = NULL) +  
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 15),
        legend.text = element_text(size = 12)) 

##graph2---------------------------------------------------------------------------------------------------

total_roast_counts_2 <- cof %>%
  group_by(origin, roast) %>%
  summarise(total_roast_counts_2 = n(), .groups = 'drop')

ggplot(total_roast_counts_2, aes(x = roast, y = reorder(origin, total_roast_counts_2), size = total_roast_counts_2, fill = total_roast_counts_2)) +
  geom_point(shape = 21, color = "black") +  # shape 21 allows both fill and color
  labs(x = "Kavrulma Yontemi", y = "Ulke", title = "Orijin ve Kavrulma Yontemlerine Gore Kahve Uretimi") +
  scale_fill_gradient(low = "white", high = "#8C510A", name =NULL) +
  scale_size(name = NULL)+ # Adjust color scale from white to dark blue
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text ( size = 11),
        axis.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.5, size = 15))

###graph3--------------------------------------------------------------------------------------------------

cof$rating_numeric <- as.numeric(cof$rating)
str(cof$X100g_USD)

# Create scatter plot with smoothed trend line
ggplot(cof, aes(x = rating_numeric, y = X100g_USD)) +
  geom_point() +  # Add scatter plot
  geom_smooth(method = "auto") +  # Add smoothed trend line
  labs(x = "Puan (max 100)", y = "100g/$ Fiyati", title = "Kahve Fiyati & Puan") +
  theme_minimal()


####graph4-------------------------------------------------------------------------------------------------
#total roast counts

roast_counts <- cof %>%
  filter(roast != "") %>%
  mutate(year = year(full_date)) %>%
  group_by(loc_country, roast, origin, year) %>%
  count(loc_country, roast, origin, year)

# Reorder loc_country by total roast counts in descending order
roast_counts <- roast_counts %>%
  mutate(loc_country = factor(loc_country, levels = names(sort(tapply(roast_counts$n, roast_counts$loc_country, sum)))))
#plot
ggplot(roast_counts, aes(x = year, y = loc_country, fill = n)) +
  geom_dotplot(binaxis = 'y', stackdir = 'center', stackratio = 1.5, dotsize = 0.5, fill ="#BF812D") +
  labs(title = "Roaster Sirketlerin Yillara Gore Urettikleri Kahve Miktari  ",
       x = "\n",
       y = "\n") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
        axis.text.y = element_text ( size = 11),
        axis.title = element_text(size = 15),
        plot.title = element_text(hjust = 0.4, size = 13))

#####graph5-----------------------------------------------------------------------------------------------
total_reviews_y <- cof %>%
  filter(roast != " ") %>%
  mutate(year = year(full_date)) %>%
  group_by(year, roast) %>%
  summarise(total_reviews_y = n_distinct(review))

medium_light_data <- total_reviews_y%>%
  filter(roast == "Medium-Light")



ggplot(total_reviews_y, aes(x = year, y = total_reviews_y, color = roast)) +
  geom_point() +  # Add scatter plot
  geom_smooth(method = "loess", se = FALSE) +  # Add smoothed trend line
  labs(x = "Yil", y = "Toplam Yorum", title = "Kavrulma Yontemine ve Yillara Gore Yorum Sayisi") +
  geom_text(x = max(medium_light_data$year), y = max(medium_light_data$total_reviews_y), 
            label = "Medium-Light", color = "black", vjust = -1, hjust = 2, size = 3) +
  theme_minimal()

######graph6 is in "Python" source------------------------------------------------------------------------

#######graph7---------------------------------------------------------------------------------------------

#text mining and making sentiment analyze for text reviews
stop_words <- stopwords("en")
# Tokenization pipeline
freq_measured <- data_frame(text = cof$review) %>% 
  mutate(text = tolower(text)) %>% 
  mutate(text = str_remove_all(text, '[[:punct:]]')) %>% 
  mutate(tokens = str_split(text, "\\s+")) %>%
  unnest(tokens) %>%
  count(tokens) %>%
  filter(!tokens %in% stop_words) %>%
  mutate(freq = n / sum(n)) %>%
  arrange(desc(n))

# View the result
print(freq_measured)

# Select top 10 words
top_10_words <- head(freq_measured, 10)

library(textshape)

# Convert the top 10 words data frame to a format suitable for textplot
textplot_data <- with(freq_measured, data.frame(word = tokens, freq = freq))

top_10_wordsplot <- head(textplot_data,10)

library(wordcloud2) 
# or a vector of colors. vector must be same length than input data
wordcloud2(textplot_data, size=1.6, color=rep_len( c( "#8C510A",  "#DFC27D"), nrow(textplot_data) ) )


########--------------------------------------------------------------------------------------

