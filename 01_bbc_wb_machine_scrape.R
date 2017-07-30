rm(list=ls())
gc()

options(scipen=999)

library(stringr)
library(jsonlite)
library(httr)
library(rvest)
library(V8)
library(tweenr)

path<-'/Users/walkerag/Documents/bbc/data/'

###############################################
#WAYBACK API CALL
#Get the Wayback Machine URLs for bbc.com
###############################################

ts_url<-'http://web.archive.org/cdx/search/cdx?url=bbc.com&output=json'

#API request
try(req <- httr::GET(ts_url, timeout(20)))

#Get data
json <- httr::content(req, as = "text")
api_dat <- fromJSON(json)
head(api_dat)

#Get WB Machine timestamps
time_stamps<-api_dat[-1,2]
head(time_stamps)
tail(time_stamps)

#Reverse order (so recent first)
time_stamps<-rev(time_stamps)

#Save output
saveRDS(time_stamps, file = paste0(path,"time_stamps.rds"))
#time_stamps<-readRDS(file = paste0(path,"time_stamps.rds"))

###############################################
#WAYBACK URL SCRAPE
#Got through each URL, get BBC headline data
###############################################

head(time_stamps,n=50)

#Post-2013 only
time_stamps<-time_stamps[as.numeric(substr(time_stamps,1,8))>=20130000]

#MUST RUN
wb_scrape_all<-NA
for(s in 1:length(time_stamps)){

  Sys.sleep(1)
  
  furl<-paste0('https://web.archive.org/web/',time_stamps[s],'/http://www.bbc.com/')

  print(furl)
  
  if(!is.na(furl)){
  
    print('Valid URL')
    
    #SCRAPE
    try(feed_dat<-read_html(furl),timeout(10),silent=TRUE)

    if(exists('feed_dat')){
      
      #USE
      initial<-html_nodes(feed_dat,"[href*='/']")
      
      #Get links
      links<-html_node(initial,xpath='@href') %>% html_text()
      
      #Get headlines
      headlines<-initial  %>% html_text()
      
      #Get headlines (alt approach)
      headlines_v2<-html_node(initial, xpath="@title")  %>% html_text()
      
      #Combine
      comb<-data.frame(links,headlines,headlines_v2,stringsAsFactors = FALSE)
  
      #Remove NA headlines
      comb<-comb[!is.na(comb$headlines),]
      
      #Needs to have BBC and news or BBC and sport
      comb<-comb[grepl(pattern="bbc.com/news",comb$links) 
                 | grepl(pattern="bbc.com/sport",comb$links)
                 | grepl(pattern="bbc.co.uk/news",comb$links)
                 | grepl(pattern="bbc.co.uk/sport",comb$links)
                 ,]
      
      #Get BBC link only
      comb$link_clean<-substr(comb$links,regexpr('www.bbc', comb$links),nchar(comb$links))
      
      #Make sure article ID present
      comb<-comb[grepl("[[:digit:]]",comb$link_clean),]
      
      #Remove dupes
      comb<-comb %>% group_by(link_clean) %>% mutate(row_num=row_number())
      comb<-comb[comb$row_num==1,]
      
      #Add rownumber as variable
      comb$story_order<-as.numeric(rownames(comb))

      #As a df
      comb<-data.frame(comb)

      if(length(comb$links)>0){
        
        #Add URL
        comb$furl<-furl
          
        #Save with the rest
        wb_scrape_all<-rbind(wb_scrape_all,comb)
        
      }
      
      rm(comb)
      rm(feed_dat)
      
    }
  
    #Save output every ten
    if((s %% 10)==0){
      saveRDS(wb_scrape_all, file = paste0(path,"wb_scrape_all.rds"))
      print(tail(wb_scrape_all))
    }
    
  }
  
}
#Save output
saveRDS(wb_scrape_all, file = paste0(path,"wb_scrape_all.rds"))