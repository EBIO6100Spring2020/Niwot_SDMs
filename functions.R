## make_url
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
  print(paste("querying url: ", str))
  return(str)
}

## scrape
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


## batch_pull
# this function will search for keywords on the Niwot EDI portal. It wraps scrape to search the html.
# You can use a filter (argument: filter = TRUE) to make sure that the words you search are in the title of the data set
# currently there are issues with data sets not having column names. I've added a fix so data isn't the column name but,
# there might be a better solution 

batch_pull = function(search = c("Niwot", "Saddle"), filter = FALSE, save = FALSE){
  
  study_ids = scrape(keywords=search) # find the data sets
  
  tmp_ids = as.data.frame(matrix(nrow = 0, ncol = dim(study_ids)[2])) # store the names
  colnames(tmp_ids) = colnames(study_ids) # change column names
  
  if(filter == TRUE){ ## here filter out any data sets that don't  have search words in the name
    for(k in 1:length(search)){
      study_ids_match_tmp = study_ids[grep(search[k], study_ids$paper_title),]
      tmp_ids = rbind(tmp_ids, study_ids_match_tmp)
    }
    study_ids = tmp_ids
  }
  
  infile1 = "pasta_ids.txt" # initialize storage file
  full_url = c()
  final_studies = c()


  iter = 1
  for(i in 1:length(study_ids$paper_id)){
    temp_name = gsub("\\.","/",study_ids$paper_id[i])

    #paste the id onto the url, still now lacking our pasta id
    u_r = paste("https://pasta.lternet.edu/package/data/eml/",temp_name,sep="")

    download.file(u_r,infile1,method="curl")
    write("\n", infile1, append = T)
    
    dumb_id = read.table(infile1)

    if(length(dumb_id$V1)>1){
      for(j in 1:length(dumb_id$V1)){
        full_url[iter] = paste(u_r,dumb_id$V1[j],sep="/")
        final_studies[iter] = paste(study_ids$paper_title[j],j,sep=".")
        iter = iter+1
      }
    }else{ 
      full_url[iter] = paste(u_r,dumb_id$V1,sep="/")
      #set up the eventual file we want, named after the package ID
      final_studies[iter] = study_ids$paper_title[i]
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
      } else {
        suppressWarnings(tmp_csv[1, c] <- as.character(cn[c]))
        
      }
    }
  } 

  if(ncol(tmp_csv) > 1){
    data_list[[data_index]] = tmp_csv
    names(data_list)[data_index] = final_studies[i]
    data_index = data_index + 1
    }
  }
  
  if (save == TRUE){
    fn = paste(paste(search, collapse = "_"), "raw" ,Sys.Date(), sep = "_")
    fp = paste("data/", fn, ".Rdata",sep = "")
    print(fp)
    save(data_list, fn, file = fp)
  }
  
  file.remove("pasta_ids.txt")
  return(data_list)
  
}

## summarize_data()
summarize_data = function(data_list, plot = FALSE){
  for(i in 1:length(data_list)){

    summary_list = list(name = names(data_list[i]), Columns = dim(data_list[[i]])[2], Rows = dim(data_list[[i]])[1])
    print(summary_list$name)
    print(summary_list$Columns)
    print(summary_list$Rows)
    print("---------------------------------------------")
    
    if (plot == TRUE){
      plot(Filter(is.numeric, data_list[[i]]))
    }

  }
  
}
