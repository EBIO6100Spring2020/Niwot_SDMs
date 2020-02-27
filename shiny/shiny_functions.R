## make_url
library(stringr)
library(XML)



#this now just makes the keywords we want into a bit we can paste into our curl call.
make_keywords =function(keywords){

  words_bl <- ''
  for (word in keywords){
    if (words_bl != ''){
      words_bl = paste(words_bl,'+',word, sep='')
    }else {
      words_bl = word
    }
  }
  return(words_bl)

}
  
filter_nwt = function(df){
  df = df[grep('nwt', df$packageid),]
  return(df)
}

get_data = function(keywords){
  curl_call <- paste0("curl -X GET https://pasta.lternet.edu/package/search/eml?defType=edismax\\&q=",keywords,"\\&fl=title,packageid,doi,begindate,enddate,coordinates\\&sort=score,desc\\&sort=packageid\\&start=0\\&rows=100")
  test.biz <- xmlParse(system(curl_call,intern=T))
  test.df <- xmlToDataFrame(test.biz)
  test.df = filter_nwt(test.df)
  return(test.df)
}

filter_exactmatch = function(search, vect, df){
  filter_df = as.data.frame(matrix(nrow = 0, ncol = dim(df)[2]))
  colnames(filter_df) = colnames(df)
  for(k in 1:length(search)){
    match = df[grep(search[k], vect)]
    filter_df = rbind(filter_df, match)
  }
  return(fitler_df)
}


## batch_pull
# this function will search for keywords on the Niwot EDI portal. It wraps scrape to search the html.
# You can use a filter (argument: filter = TRUE) to make sure that the words you search are in the title of the data set
# currently there are issues with data sets not having column names. I've added a fix so data isn't the column name but,
# there might be a better solution 

batch_search = function(search = c("Saddle", "Composition"), filter = FALSE) {
  study_ids = get_data(make_keywords(search)) # find the data sets
  if (filter == TRUE){
    study_ids = filter_exactmatch(search, study_ids$title, study_ids)
  }
  
  return(study_ids)
  
}


batch_pull = function(study_ids){
  
  full_url = c()
  final_studies = c()
  
  iter = 1
  for(i in 1:length(study_ids$packageid)){
    
    temp_name = gsub("\\.","/",study_ids$packageid[i])
    u_r = paste("https://pasta.lternet.edu/package/data/eml/",temp_name,sep="")
    pastas <- system(paste("curl",u_r, sep = " "),intern = T)
    
    if(length(pastas)>1){
      for(j in 1:length(pastas)){
        full_url[iter] = paste(u_r,pastas[j],sep="/")
        final_studies[iter] = paste(as.character(study_ids$title[j]),j,sep=".")
        iter = iter+1    
      }
    }else{ 
      full_url[iter] = paste(u_r,pastas,sep="/")
      #set up the eventual file we want, named after the package ID
      final_studies[iter] = as.character(study_ids$title[i])
      #update that iterator pls
      iter = iter+1
    }
  }
  
  ## Start the data loading
  data_list = list() # Storage list
  data_index = 1 # Index incase there are multiple data sets or we filter one out of the loop
  
  ## Loop through, I think I could add  this to the earlier loop but I'm not sure yet. TBD look at Thursday
  for(i in 1:length(full_url)){
    
    
    tmp_csv = read.csv(full_url[i], stringsAsFactors = TRUE) # Read in csv for some QA/Qc
    cn = colnames(tmp_csv) # get column name to check if they are there at all
    
    if (substr(cn,0, 1)[1] == "X" | any(grepl(cn[1], tmp_csv[,1]))){ # check for X meaning numeric in any of column names. (This is likely if there is no column names)
      warning(paste("Data set: ", "[",final_studies[i], "]", "has unkown column names")) # warn ya
      missing_data = rep(NA, length(cn)) # place holder df to be the new first row
      
      missing_data <- as.data.frame(matrix(missing_data, nrow = 1, ncol = length(missing_data)), byrow = TRUE)
      colnames(tmp_csv) <- colnames(missing_data)
      tmp_csv = rbind(missing_data, tmp_csv)
      
      for(c in 1:length(cn)){
        if(str_detect(cn[c], "[[:digit:]]")){
          tmp_value = gsub("[[:alpha:]]","", cn[c])
          suppressWarnings(tmp_csv[1,c] <- as.numeric(tmp_value))
        } 
        else {
          suppressWarnings(tmp_csv[1, c] <- as.character(cn[c]))
        }
      }
    }

    
    file_info = system(paste0("curl --head ",full_url[i]), intern = T)
    
    if(any(grepl(".csv", file_info[grep("filename", file_info)]))){
      data_list[[data_index]] = read.csv(full_url[i], stringsAsFactors = FALSE)
      names(data_list)[data_index] = final_studies[i]
      data_index = data_index + 1 
    }
    
  }
  
  return(data_list)
  
}

timeline = function(begin_vect, end_vect, label_vect){
  
  begin_vect = as.Date(begin_vect)
  end_vect = as.Date(end_vect)
  print(class(begin_vect))
  print(min(begin_vect))
  
  
  plot(1:length(begin_vect) ~ begin_vect, 
       bty = "n",
       yaxt = "n",
       pch = 19, 
       cex = 1.1, 
       ylim = c(0, length(begin_vect) + 1),
       ylab = "",
       xlim = c(min(begin_vect), max(end_vect) + 6000))
  points(1:length(begin_vect) ~ end_vect, pch = 19, cex = 1.1)

  for(i in 1:length(begin_vect)){
    rect(xleft = begin_vect[i], xright = end_vect[i], ybottom = i - 0.5, ytop = i + 0.5, col = colors()[i])
    text(x = end_vect[i] + 1, y = i, labels = label_vect[i], pos = 4, cex = 1)

  }
}




