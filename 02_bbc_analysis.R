####################################
#TEXT MINING BBC HEADLINES
#Name: Adam Walker
#Date: July 2017
#Purpose: Analyze scraped BBC headline data using tidy text mining approach
####################################

rm(list=ls())
options(scipen=999)

library(tidytext)
library(ggplot2)
library(purrr)
library(stringr)
library(dplyr)
library(lubridate)
library(tidyverse)
library(tidyr)
library(scales)
library(igraph)
library(ggraph)
library(widyr)
library(broom)
library(gridExtra)
library(gtable)

path<-'/Users/walkerag/Documents/bbc/data/'

#Define palette:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

####################################
#DATA PREP
####################################

#Read in raw file
wb_scrape_all<-readRDS(file = paste0(path,"wb_scrape_all.rds"))

#Remove spaces at start/end of headlines
wb_scrape_all$headlines_trim<-str_trim(wb_scrape_all$headlines)

#Remove unneeded fields
wb_scrape_all<-subset(wb_scrape_all,select=-c(headlines,headlines_v2,row_num))

#Remove unnecessary stories
wb_scrape_all<-wb_scrape_all[wb_scrape_all$link_clean!='www.bbc.com/sport/formula1',]
wb_scrape_all<-wb_scrape_all[wb_scrape_all$headlines_trim!='One-minute World News',]
wb_scrape_all<-wb_scrape_all[wb_scrape_all$headlines_trim!='Week in pictures',]

#Remove 's
wb_scrape_all$headlines_trim<-gsub(pattern="'s",replacement ="",wb_scrape_all$headlines_trim)

#Remove punctuation
wb_scrape_all$headlines_trim<-gsub(pattern="'",replacement ="",wb_scrape_all$headlines_trim, fixed = TRUE)
wb_scrape_all$headlines_trim<-gsub(pattern=":",replacement ="",wb_scrape_all$headlines_trim, fixed = TRUE)
wb_scrape_all$headlines_trim<-gsub(pattern="?",replacement ="",wb_scrape_all$headlines_trim, fixed = TRUE)
#View(wb_scrape_all)

#Limit to top headline only
wb_scrape_all.top<-wb_scrape_all[wb_scrape_all$story_order==1,]

#Remove NA values
wb_scrape_all.top<-wb_scrape_all.top[!is.na(wb_scrape_all.top$headlines_trim),]

#Remove dupes
wb_scrape_all.top<-wb_scrape_all.top %>% group_by(link_clean) %>% mutate(rownum=row_number())
wb_scrape_all.top<-wb_scrape_all.top[wb_scrape_all.top$rownum==1,]
#View(wb_scrape_all.top)

#Get timestamp
wb_scrape_all.top$timestamp<-sapply(strsplit(wb_scrape_all.top$links,"/"), "[[", 3)

#Get date
wb_scrape_all.top$Date<-as.Date(substr(wb_scrape_all.top$timestamp,1,8),format = "%Y%m%d")

#Limit to post-2014
wb_scrape_all.top<-wb_scrape_all.top[wb_scrape_all.top$Date>="2014-01-01",]
wb_scrape_all.top<-wb_scrape_all.top[!is.na(wb_scrape_all.top$Date),]

#Check headline counts by day
day_counts<-wb_scrape_all.top %>% group_by(Date) %>% summarise(count=n())
#View(day_counts)
rm(day_counts)

#Check headline lengths
wb_scrape_all.top$headline_char<-nchar(wb_scrape_all.top$headlines_trim)
hist(wb_scrape_all.top$headline_char)
#View(wb_scrape_all.top[wb_scrape_all.top$headline_char<=20,])

#Add month and year fields
wb_scrape_all.top$month<-month(wb_scrape_all.top$Date,label=TRUE,abbr=TRUE)
wb_scrape_all.top$year<-year(wb_scrape_all.top$Date)

rm(wb_scrape_all)

########################################
#STORIES BY REGION
########################################

region_dat<-subset(wb_scrape_all.top,select=c(link_clean,Date,headlines_trim,year))

#Parse out story category using URL
region_dat$region<-sapply(strsplit(region_dat$link_clean,"/"), "[[", 3)
region_dat$region<-gsub("[[:digit:]]+","",region_dat$region)
region_dat$region<-gsub("-","",region_dat$region)
#View(region_dat)

#Replace with sport if a sport link
region_dat[grepl("sport",region_dat$link_clean),"region"]<-"sport"

#Place all UK categories together
region_dat[grepl("^uk",region_dat$region),"region"]<-"uk"

#Place US election stories into US/Canada group
region_dat[region_dat$region=="electionus","region"]<-"worlduscanada"

#Check category counts  
counts<-region_dat %>% group_by(region) %>% summarise(total=n())
#View(counts)
rm(counts)

#Roll <75 story categories into an all other bucket
region_count<-region_dat %>% group_by(region) %>% summarise(region_count=n())
region_count$region_rollup<-ifelse(region_count$region_count<75,"Other",region_count$region)
region_dat<-region_dat %>% inner_join(region_count)

#Calculate region frequency by year, and as perc of total
frequency <- region_dat %>% 
  group_by(year) %>% 
  count(region_rollup, sort = TRUE) %>% 
  left_join(region_dat %>% 
              group_by(year) %>% 
              summarise(year_total = n())) %>%
  left_join(region_dat %>% 
              group_by(region_rollup) %>% 
              summarise(region_total = n())) %>%
  mutate(freq = n/year_total)
head(frequency)

#Give regions clearer names
unique(frequency$region_rollup)
frequency[frequency$region_rollup=="worlduscanada","Region"]<-"US+Canada"
frequency[frequency$region_rollup=="worldeurope","Region"]<-"Europe"
frequency[frequency$region_rollup=="worldmiddleeast","Region"]<-"Middle-East"
frequency[frequency$region_rollup=="worldasia","Region"]<-"Asia"
frequency[frequency$region_rollup=="worldafrica","Region"]<-"Africa"
frequency[frequency$region_rollup=="uk","Region"]<-"UK"
frequency[frequency$region_rollup=="worldlatinamerica","Region"]<-"Latin America"
frequency[frequency$region_rollup=="Other","Region"]<-"Other/No Region"

#Order the factor for better legend clarity
frequency<-data.frame(frequency)
frequency$Region <- factor(frequency$Region, levels=frequency[frequency$year=="2017","Region"])

#Plot the data
ggplot(frequency,aes(year, freq, color=Region)) +
  geom_line(lwd=3.8) + 
  #geom_point(size=3) +
  ggtitle("The U.S. Is Taking A Larger Share Of BBC Headlines") +
  ylab("Percentage of Headlines") +
  xlab("Year") +
  theme(text = element_text(size = 28,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
        ,legend.key.size = unit(2.5,"line")
        ,legend.title = element_text(size=28)
  ) +
  scale_y_continuous(labels=scales::percent
                     ,limits=c(0,0.43),breaks=c(0,0.1,0.2,0.3,0.4)
                     )  + 
  guides(colour = guide_legend(override.aes = list(lwd=4)))


rm(region_dat)
rm(region_count)
rm(frequency)

########################################
#BEGIN TIDY TEXT ANALYSIS
########################################

#Combine some obvious bigrams
wb_scrape_all.top$headlines_trim<-gsub("White House","WhiteHouse",wb_scrape_all.top$headlines_trim)
wb_scrape_all.top$headlines_trim<-gsub("N Korea","NorthKorea",wb_scrape_all.top$headlines_trim)
wb_scrape_all.top$headlines_trim<-gsub("North Korea","NorthKorea",wb_scrape_all.top$headlines_trim)
wb_scrape_all.top$headlines_trim<-gsub("Hong Kong","HongKong",wb_scrape_all.top$headlines_trim)
wb_scrape_all.top$headlines_trim<-gsub("Boko Haram","BokoHaram",wb_scrape_all.top$headlines_trim)

#Put in tidy format
text_df<-wb_scrape_all.top %>%
  unnest_tokens(input=headlines_trim, word,to_lower=FALSE,drop=FALSE)

#Keep only necessary columns
text_df<-subset(text_df,select=c(Date,link_clean,headlines_trim,headline_char,word,month,year))

#Format some words for clarity and to avoid being removed as stop words
text_df[text_df$word=="IS","word"]<-"ISIS"
text_df[text_df$word=="N","word"]<-"North"
text_df[text_df$word=="US","word"]<-"U.S."
text_df[text_df$word=="UN","word"]<-"U.N."
text_df[text_df$word=="May","word"]<-"(Theresa) May"

#Make everything lower case
text_df$word<-tolower(text_df$word)
head(text_df)

#Look at stop word counts, check no useful words will be removed
bbc_stop_words <- text_df %>%
  inner_join(stop_words) %>%
  group_by(word) %>% summarise(count=n()) %>% arrange(desc(count))
#View(bbc_stop_words)
rm(bbc_stop_words)
#Looks good

#Remove stop words
text_df <- text_df %>%
  anti_join(stop_words)

#Ungroup
text_df <- text_df %>% ungroup()

#Plot counts
text_df %>%
  count(word, sort = TRUE) %>%
  filter(n > 30) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip()

#Save formatted data
saveRDS(text_df,file = paste0(path,"text_df.rds"))

#################################
#Plot nice table of top ten words
#################################

counts<-
  text_df %>%
  group_by(word) %>% 
  summarise(count=n()) %>% 
  arrange(desc(count))

colnames(counts)<-c("Word","Count")
g<-tableGrob(counts[1:10,],rows=NULL,theme=ttheme_default(
  base_colour = "darkcyan"
  ,core = list(fg_params=list(fontsize=22)
               ,bg_params=list(fill="white"))
  
  ,colhead = list(bg_params=list(fill="white")
                  ,fg_params=list(fontsize=24))
  
  ,rowhead = list(fg_params=list(fontsize=20)
                  ,bg_params=list(col="black"))))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 2, b = nrow(g), l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t = 1, l = 1, r = ncol(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=1, l = 2, r = ncol(g),b=nrow(g))
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=3, l = 1, r = ncol(g),b=nrow(g)-1)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=4, l = 1, r = ncol(g),b=nrow(g)-2)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=5, l = 1, r = ncol(g),b=nrow(g)-3)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=6, l = 1, r = ncol(g),b=nrow(g)-4)
g <- gtable_add_grob(g,
                     grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)),
                     t=6, l = 1, r = ncol(g),b=nrow(g)-5)
grid.draw(g)

rm(counts)
rm(g)

################################
#BASIC SENTIMENT OVER TIME
################################

text_df.sentiment <- text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(year, index=month, sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)

#Make year a factor
text_df.sentiment$year<-as.factor(text_df.sentiment$year)

#Plot
ggplot(text_df.sentiment, aes(index, sentiment, fill = year)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=cbPalette[2:6]) +
  facet_wrap(~year, ncol = 2, scales = "free_x")
#Weird results recently. Why?

#Turns out Trump is counted as a positive word!
text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment,sort=TRUE)

#Take out Trump this time
text_df.sentiment.notrump <- text_df %>%
  inner_join(get_sentiments("bing")) 
text_df.sentiment.notrump<-text_df.sentiment.notrump[text_df.sentiment.notrump$word!="trump",]
text_df.sentiment.notrump <- text_df.sentiment.notrump %>%
  count(year, index=month,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = positive - negative)
#Make year a factor
text_df.sentiment.notrump$year<-as.factor(text_df.sentiment.notrump$year)

#Plot (non-Trump version)
ggplot(text_df.sentiment.notrump, aes(index, sentiment, fill = year)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=cbPalette[2:6]) +
  facet_wrap(~year, ncol = 2, scales = "free_x")

#Try looking at proportion of positive or negative words
text_df.sentiment.proportion <- text_df %>%
  inner_join(get_sentiments("bing")) 
text_df.sentiment.proportion<-text_df.sentiment.proportion[text_df.sentiment.proportion$word!="trump",]
text_df.sentiment.proportion <- text_df.sentiment.proportion %>%
  count(year, index=month,sentiment) %>%
  spread(sentiment, n, fill = 0) %>%
  mutate(sentiment = negative/(positive+negative))
#Make year a factor
text_df.sentiment.proportion$year<-as.factor(text_df.sentiment.proportion$year)
ggplot(text_df.sentiment.proportion, aes(index, sentiment, fill = year)) +
  geom_col(show.legend = FALSE) +
  scale_fill_manual(values=cbPalette[2:6]) +
  facet_wrap(~year, ncol = 2, scales = "free_x")
#Pretty consistent over time

rm(text_df.sentiment)
rm(text_df.sentiment.notrump)
rm(text_df.sentiment.proportion)

###########################
#WORD COUNTS BY SENTIMENT
###########################

bing_word_counts <- text_df %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#Trump and defeat classed as positive
bing_word_counts<-bing_word_counts[bing_word_counts$word!="trump",]
bing_word_counts<-bing_word_counts[bing_word_counts$word!="defeat",]

bing_word_counts %>%
  group_by(sentiment) %>%
  top_n(20) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(y = "Word Counts",
       x = NULL) +
  coord_flip() +
  ggtitle("Top Positive and Negative Words Using Bing Liu Sentiment Dictionary") +
  theme(text = element_text(size = 28,family="Trebuchet MS")
  )

rm(bing_word_counts)

#######################################
#WORD FREQUENCY OVER TIME
#######################################

text_df.freq<-subset(text_df,select=c(word,month,year,Date))

#Summarize word counts by half
words_by_time <- text_df.freq %>%
  mutate(time_floor = floor_date(Date, unit = "1 half")) %>%
  group_by(time_floor,word) %>% summarise(count=n())

#Fill out missing combos
words_by_time<-words_by_time %>% ungroup() %>% complete(nesting(word), time_floor)
words_by_time[is.na(words_by_time$count),"count"]<-0

#Get word count and time counts, limit to words with at least 50 occurrences
words_by_time <- words_by_time %>%
  group_by(time_floor) %>%
  mutate(time_total = sum(count)) %>%
  group_by(word) %>%
  mutate(word_total = sum(count)) %>%
  ungroup() %>%
  filter(word_total > 50)

tail(words_by_time)

#Remove July 2017 half as very limited data
words_by_time<-words_by_time[words_by_time$time_floor!="2017-07-01",]

#Nest data
nested_data <- words_by_time %>%
  nest(-word) 
#nested_data[[2]][[1]]

#Run models
nested_models <- nested_data %>%
  mutate(models = map(data, ~ glm(cbind(count, time_total) ~ time_floor, ., 
                                  family = "binomial")))

#Get slope values and adjsuted p-values
slopes <- nested_models %>%
  unnest(map(models, tidy)) %>%
  filter(term == "time_floor") %>%
  mutate(adjusted.p.value = p.adjust(p.value))

#Take significant slopes
top_slopes <- slopes %>% 
  filter(adjusted.p.value < 0.01)

top_slopes

words_by_time<-words_by_time %>%
  inner_join(top_slopes, by = c("word"))

#Order the factor for better legend clarity
words_by_time<-data.frame(words_by_time)
levels<-words_by_time[words_by_time$time_floor=="2017-01-01",]
levels<-levels[order(levels$count,decreasing = TRUE),"word"]
words_by_time$Word <- factor(words_by_time$word
                             ,levels=levels)

#Plot the data
ggplot(words_by_time,aes(time_floor, count/time_total, color = Word)) +
  geom_line(lwd=3.8) +
  labs(x = NULL, y = "Word Frequency") +
  ggtitle("Ukraine Falls, Trump Rises In BBC Headlines") +
  scale_y_continuous(labels=scales::percent
                     ,limits=c(0,0.06),breaks=c(0,0.01,0.02,0.03,0.04,0.05,0.06)) +
  theme(text = element_text(size = 28,family="Trebuchet MS")
        ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
        ,axis.title.x=element_text(margin=margin(10,0,0,0))
        ,legend.key.size = unit(2.5,"line")
  ) + 
  guides(colour = guide_legend(override.aes = list(lwd=4)))

rm(words_by_time)
rm(top_slopes)
rm(nested_models)
rm(text_df.freq)
rm(slopes)
rm(nested_data)

#######################################
#NETWORK GRAPH
#######################################

title_word_pairs <- text_df %>% 
  pairwise_count(item=word, link_clean, sort = TRUE, upper = FALSE)

title_word_pairs$Matches<-title_word_pairs$n

#ABSOLUTE COUNT VERSION
title_word_pairs %>%
  filter(Matches >= 8) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = Matches, edge_width = Matches), edge_colour = "cyan4") +
  geom_node_point(size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size=9.5, 
                 point.padding = unit(0.2, "lines"),family="Trebuchet MS") +
  theme(text = element_text(size = 9.5,family="Trebuchet MS")
        ,legend.key.size = unit(2.5,"line")
        ,legend.text = element_text(size=20)
        ,legend.title = element_text(size=20)
        ,panel.border = element_blank()
        ,panel.grid.major = element_blank()
        ,panel.grid = element_blank()
        ,panel.background = element_blank()
        ,axis.text = element_blank()
        ,axis.ticks = element_blank()
        ,axis.title = element_blank()
        ,legend.box.background = element_blank()
        ,legend.key=element_blank()
        ,title = element_text(size=26)
  ) +
  ggtitle("BBC Headline Network Graph")

rm(title_word_pairs)

#CORRELATION VERSION
# keyword_cors <- text_df %>% 
#   group_by(word) %>%
#   filter(n() >= 10) %>%
#   pairwise_cor(word, link_clean, sort = TRUE, upper = FALSE)
# 
# keyword_cors
# 
# set.seed(1234)
# keyword_cors %>%
#   filter(correlation > .3) %>%
#   graph_from_data_frame() %>%
#   ggraph(layout = "fr") +
#   geom_edge_link(aes(edge_alpha = correlation, edge_width = correlation), edge_colour = "royalblue") +
#   geom_node_point(size = 5) +
#   geom_node_text(aes(label = name), repel = TRUE,
#                  point.padding = unit(0.2, "lines")) +
#   theme_void()

#######################################
#TF IDF
#######################################

word_tf_idf <- text_df %>%
  count(year, word) %>%
  bind_tf_idf(word, year, n) %>%
  arrange(desc(tf_idf))

word_tf_idf

#Needs at least X appearances
word_tf_idf<-word_tf_idf[word_tf_idf$n>=10,]

#Check why Ukraine isn't appearing in char
word_tf_idf[word_tf_idf$word=="ukraine",]

#Order factor for plot
word_tf_idf.plot <- word_tf_idf %>%
  arrange(year,tf_idf) %>%
  group_by(year) %>% top_n(n=6) %>% ungroup()
word_tf_idf.plot <- word_tf_idf.plot %>% mutate(ordered = paste0(year, word) %>% 
                                                  forcats::fct_inorder())

#Plot the data
word_tf_idf.plot %>% 
  ggplot(aes(ordered, tf_idf, fill = year)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~year, ncol = 2, scales = "free") +
  coord_flip() +
  scale_x_discrete(labels = setNames(word_tf_idf.plot$word,word_tf_idf.plot$ordered)) +
  ggtitle("Highest tf-idf Scores By Year") +
  theme(
    text = element_text(size = 28,family="Trebuchet MS")
    #,title = element_text(face="bold")
    ,plot.subtitle = element_text(face="bold")
    ,axis.title.y=element_text(margin=margin(0,10,0,0)) 
    ,axis.title.x=element_text(margin=margin(10,0,0,0))
  )

rm(word_tf_idf)
rm(word_tf_idf.plot)

################################################
#ADDITIONAL, UNUSED CODE
#Code for charts not included in blog post
################################################

###########################################
#WORD FREQUENCIES BETWEEN YEARS
###########################################

text_df.plot<-text_df[text_df$year %in% c('2015','2016'),]

#Paste year in front to make things easier
text_df.plot$year<-paste0('year_',text_df.plot$year)

#Get word frequency as proportion of total words in that year
frequency <- text_df.plot %>% 
  group_by(year) %>% 
  count(word, sort = TRUE) %>% 
  left_join(text_df.plot %>% 
              group_by(year) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

head(frequency)

#Get years in separate columns
counts <- frequency %>% 
  group_by(word) %>% summarise(n_all=sum(n))
head(counts)

frequency_arrange <- frequency %>% 
  select(year, word, freq) %>% 
  spread(year, freq) %>%
  arrange(year_2015,year_2016)
head(frequency_arrange)

comb<-frequency_arrange %>% inner_join(counts[counts$n_all>15,])
comb[is.na(comb$year_2015),"year_2015"]<-0.00001
comb[is.na(comb$year_2016),"year_2016"]<-0.00001
head(comb)

ggplot(comb, aes(year_2015,year_2016)) +
  geom_point(alpha = 0.1, size = 2.5) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format(),limits=c(0.001,0.07)) +
  scale_y_log10(labels = percent_format(),limits=c(0.001,0.07)
                #,breaks=c(0.01,0.01,0.05)
  ) +
  geom_abline(color = "red")

rm(comb)
rm(counts)
rm(frequency)
rm(frequency_arrange)
rm(text_df.plot)


#######################################
#N-GRAMS
#######################################

#Put in tidy format, bigrams version
text_df.bigrams<-wb_scrape_all.top %>%
  unnest_tokens(input=headlines_trim, bigram,to_lower=FALSE,drop=FALSE,token="ngrams",n=2)

#Keep only necessary columns
text_df.bigrams<-subset(text_df.bigrams,select=c(Date,link_clean,headlines_trim,headline_char,bigram))

text_df.bigrams %>%
  count(bigram, sort = TRUE)

bigrams_separated <- text_df.bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

#Format some words for clarity and to avoid being removed as stop words
bigrams_separated[bigrams_separated$word1=="IS","word1"]<-"ISIS"
bigrams_separated[bigrams_separated$word1=="N","word1"]<-"North"
bigrams_separated[bigrams_separated$word1=="US","word1"]<-"U.S."
bigrams_separated[bigrams_separated$word2=="IS","word2"]<-"ISIS"
bigrams_separated[bigrams_separated$word2=="N","word2"]<-"North"
bigrams_separated[bigrams_separated$word2=="US","word2"]<-"U.S."

#Make everything lower case
bigrams_separated$word1<-tolower(bigrams_separated$word1)
bigrams_separated$word2<-tolower(bigrams_separated$word2)

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united

#Add month and year
bigrams_united$month<-month(bigrams_united$Date,label=TRUE,abbr=TRUE)
bigrams_united$year<-year(bigrams_united$Date)

bigram_tf_idf <- bigrams_united %>%
  count(year, bigram) %>%
  bind_tf_idf(bigram, year, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf

bigram_tf_idf.plot <- bigram_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram = factor(bigram, levels = rev(unique(bigram))))

#Needs at least 5 appearances
bigram_tf_idf.plot<-bigram_tf_idf.plot[bigram_tf_idf.plot$n>=5,]

bigram_tf_idf.plot %>% 
  top_n(20) %>%
  ggplot(aes(bigram, tf_idf, fill = year)) +
  geom_col() +
  labs(x = NULL, y = "tf-idf") +
  coord_flip()

bigram_tf_idf.plot %>% 
  group_by(year) %>% 
  top_n(8) %>% 
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill = year)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = "tf-idf") +
  facet_wrap(~year, ncol = 2, scales = "free") +
  coord_flip()