---
title: "R Notebook"
output: html_notebook
---


#Load Packages
(Hidden)
```{r warning=FALSE}
library(ggplot2)
library(tictoc)
library(tidyverse)
library(jsonlite)
library(rdrop2)
```

Using Politico Data
Need to pull https://www.politico.com/2020-election/data/general-election-results/metadata/01/potus.meta.json for meta data for state
then pull https://www.politico.com/2020-election/data/general-election-results/live-results/01/potus-counties.json

Overall results countrywide: https://www.politico.com/2020-election/data/general-election-results/live-results/10/potus.json
#Web Scraping
```{r}
#Dropbox token
#token <- drop_auth()
#saveRDS(token, "droptoken.rds")



webscrape_county_results = function() {
  #stateFIPS = c("01","02","04","05","06","08","09","10","11","12","13","15","16","17","18","19","20","21","22","23","24","25","26","27","28","29","30","31","32","33","34","35","36","37","38","39","40","41","42","44","45","46","47","48","49","50","51","53","54","55","56")
  
  county_results = data.frame( FIPS = character(0), dem_votes.results = numeric(0), rep_votes.results = numeric(0), pct_complete = numeric(0), progressReporting = numeric(0), progressTotal = numeric(0),stringsAsFactors = FALSE)
  #state_FIPS_code = 23
  
  for(state_FIPS_code in stateFIPS){
    print(state_FIPS_code)
    
    metadata_url = paste0("https://www.politico.com/2020-election/data/general-election-results/metadata/",state_FIPS_code,"/potus.meta.json")
    metadata = fromJSON(metadata_url)
    
    url = paste0("https://www.politico.com/2020-election/data/general-election-results/live-results/",state_FIPS_code,"/potus-counties.json")
    data = fromJSON(url)
    
    if(!is.data.frame(metadata$candidates)){
      cand_metadata = select(metadata$candidates[[1]], candidateID, party)
      print(paste("multiple:",state_FIPS_code))
    } else {
      cand_metadata = select(metadata$candidates, candidateID, party)  
    }
    
    
    county_data = select(data$races,-candidates)
    
    flat_cand_county_data = data$races %>%
      select(countyFips, candidates) %>%
      unnest(cols=c(candidates))
    
    results = flat_cand_county_data %>%
      left_join(cand_metadata, by="candidateID") %>% 
      select(-candidateID) %>%
      filter(party %in% c("dem","gop")) %>%
      spread(party,vote) %>%
      left_join(select(county_data,countyFips,progressReporting,progressTotal), by="countyFips") %>%
      mutate(pct_complete = progressReporting/progressTotal) %>%
      select(FIPS = countyFips, dem_votes.results = dem, rep_votes.results = gop, pct_complete,  progressReporting, progressTotal)
    
    county_results = rbind(county_results,results)
    Sys.sleep(runif(1)/10)
  }
  toc()
  update_time = Sys.time()
  
  attr(county_results,"update_time") = update_time
  
  saveRDS(county_results,"live_county_results.Rds")
  
  results_historical_name = paste0("historical_data/results_historical_",gsub(":","_",gsub(" ","_",strftime(update_time))),".csv")
  write.csv(x = county_results, file = results_historical_name,)
  
  drop_auth(rdstoken = "droptoken.rds")
  drop_upload(file = "live_county_results.Rds",path = "election_night/")#,mode = "overwrite")
  drop_upload(file = results_historical_name,path = "election_night/historical_data/")
  
  return(county_results)
}

#county_results = webscrape_county_results()
```




