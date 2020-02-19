library(rvest)
library(stringr)

make_url = function(keywords){
  words_block = ''
  for (word in keywords){
    if (words_block != ''){
      words_block = paste(words_block,'+',word, sep='')
    }
    else {
      words_block = word
    }
  }
  
  str=paste("http://portal.lternet.edu:80/nis/simpleSearch?start=0&rows=1500&defType=edismax&q=%22",words_block,"%22&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false", sep='')
  return(str)
}

scrape = function(keywords=c("niwot", "saddle")){
  html = make_url(keywords) %>% read_html(.)
  lines = html %>% html_nodes("a[href]") %>% grep('knb-lter-nwt', ., value=T) 
  n_studies = length(lines)/2
  print(paste(n_studies, "Studies Found"))
  studies = data.frame(matrix(nrow=length(lines)/2, ncol=2))
  colnames(studies) = c("paper_title", "paper_id")
  
  entry_ct = 1 
  line_ct = 1
  for (line in lines){
    text = line %>% read_html(.) %>% html_nodes("a") %>% html_text(., trim=T)
    if (entry_ct %% 2 == 0){
      studies[line_ct,2] = text
      line_ct = line_ct + 1
    }
    else{
      studies[line_ct,1] = text
    }
    entry_ct = entry_ct + 1
  }
  return(studies)
}



batch_pull = function(search = c("Niwot", "Saddle"), filter = TRUE){
  
  study_ids = scrape(search)
  
  tmp_ids <- as.data.frame(matrix(nrow = 0, ncol = dim(study_ids)[1]))
  colnames(tmp_ids) <- colnames(study_ids)
  
  if(filter == TRUE){
    for(k in 1:length(search)){
      study_ids_match_tmp <- study_ids[grep("Niwot", study_ids$paper_title),]
      
      
      
      
      tmp_ids <- rbind(tmp_ids, study_ids_match_tmp)
      
    }
    
    study_ids <- tmp_ids
    
  }
  
  print(study_ids)
  
 
  infile1 <- "Pasta.Ids.txt"
  full_url <- c()
  final_studies <- c()
  
   
  iter <- 1
  for(i in 1:length(study_ids$paper_id)){
    #minor issue here is that our package IDs use like knb-lter-nwt.120.1 but the directory structure on the server 
    #obviously uses nwt/120/1 so we just replace the . with a /
    temp_name <- gsub("\\.","/",study_ids$paper_id[i])
    
    #paste the id onto the url, still now lacking our pasta id
    u_r <- paste("https://pasta.lternet.edu/package/data/eml/",temp_name,sep="")
    
    #download the info at that url, which (hopefully) is our pasta ID. This saves it to a file. I have yet to
    #figure out how to download it into R's working memory. If you figure out how to do that, lemme know.
    print(download.file(u_r,infile1,method="curl"))
    write("\n", infile1, append = T)
    
    
    #read that pasta ID in
    dumb_id <- read.table(infile1)
    
    #some data packages have multiple data files associated with a given ID. As a result, when you get the PASTA ID,
    #it will return more than one. Here we check to make sure we don't have more than one. If we do, we loop through
    #them, creating a URL for each, naming a file, and continuing to update the iterator we use to index each
    #object.
    if(length(dumb_id$V1)>1){
      for(j in 1:length(dumb_id$V1)){
        full_url[iter] <- paste(u_r,dumb_id$V1[j],sep="/")
        final_studies[iter] <- paste(study_ids$paper_title,j,sep=".")
        #add CSV to that final file name
        iter <- iter+1
      }
      
      
    }else{ #if we don't have more than one PASTA for a given package, just create the URL and file name
      #create the full URL with the pasta ID separated by a slash
      full_url[iter] <- paste(u_r,dumb_id$V1,sep="/")
      #set up the eventual file we want, named after the package ID
      
      
      
      #add CSV to that final file name
      #we need two calls here because creating the directory path uses /, whereas adding the .csv won't use any
      #separator.
      
      print(study_ids$paper_title[i])
      
      final_studies[iter] <- study_ids$paper_title[i]
      #update that iterator pls
      iter <- iter+1
    }
  }
  
  
  data_list <- list()
  data_index <- 1
  
  for(i in 1:length(full_url)){
    
    print(read.csv(full_url[i], stringsAsFactors = FALSE)[1])
    
    # if('date' %in% check.names){
    #   print(full_url[i])
    #   data_list[[data_index]] <- read.csv(full_url[i])
    #   names(data_list)[data_index] <- final_studies[i]
    #   data_index <- data_index + 1 
    # }
    
  }

  return(data_list)
  
}


data_list <- batch_pull(c("Soil", "Texture"))

#########################################



