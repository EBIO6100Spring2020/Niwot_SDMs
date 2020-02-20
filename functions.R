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
  print(paste("querying url: ", str))
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
  
  study_ids = scrape(keywords=search)
  
  tmp_ids = as.data.frame(matrix(nrow = 0, ncol = dim(study_ids)[2]))
  colnames(tmp_ids) = colnames(study_ids)
  
  if(filter == TRUE){
    for(k in 1:length(search)){
      study_ids_match_tmp = study_ids[grep(search[k], study_ids$paper_title),]
      tmp_ids = rbind(tmp_ids, study_ids_match_tmp)
    }
    study_ids = tmp_ids
  }
  print(study_ids)
  
 
  infile1 = "Pasta.Ids.txt"
  full_url = c()
  final_studies = c()


  iter = 1
  for(i in 1:length(study_ids$paper_id)){
    temp_name = gsub("\\.","/",study_ids$paper_id[i])

    #paste the id onto the url, still now lacking our pasta id
    u_r = paste("https://pasta.lternet.edu/package/data/eml/",temp_name,sep="")

    download.file(u_r,infile1,method="curl")
    print(read.table(infile1))
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
    
      print(study_ids$paper_title[i])

      final_studies[iter] = study_ids$paper_title[i]
      #update that iterator pls
      iter = iter+1
    }
  }


  data_list = list()
  data_index = 1

  for(i in 1:length(full_url)){

    
  tmp_csv = read.csv(full_url[i])
  if(ncol(tmp_csv) > 1){
    data_list[[data_index]] = tmp_csv
    names(data_list)[data_index] = final_studies[i]
    data_index = data_index + 1
  }
  
  
  
  
  }
  
  file.remove("Pasta.Ids.txt")
  return(data_list)

}


data_list = batch_pull(c("Soil", "Texture"))

#########################################

#batch_pull()

