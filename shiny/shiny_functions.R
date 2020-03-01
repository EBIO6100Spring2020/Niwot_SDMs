library(stringr)
library(XML)
library(ggplot2)
library(DT)
library(plotly)
library(maps)


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
  curl_call <- paste0("curl -X GET https://pasta.lternet.edu/package/search/eml?defType=edismax\\&q=",keywords,"\\&fl=title,packageid,begindate,enddate,coordinates\\&sort=score,desc\\&sort=packageid\\&start=0\\&rows=100")
  
  test.biz <- xmlParse(system(curl_call,intern=T))
  
  
  if(length(xpathSApply(test.biz, "//title", xmlValue))){
    test.df <- xmlToDataFrame(test.biz)
    test.df = filter_nwt(test.df)
    
  } else {
    test.df = as.data.frame(matrix(nrow = 0, ncol = 5))
    colnames(test.df) = c("title", "packageid", "begindate", "enddate", "spatialcoverage")
  }
  
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

get_colors = function(n){
  color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)]
  colors_touse = sample(color, n)
  return(colors_touse)
}

timeline = function(begin_vect, end_vect, label_vect, colors_touse){
  

  begin_vect = as.Date(begin_vect)
  end_vect = as.Date(end_vect)
  
  
  in_order = order(begin_vect)

  begin_vect = begin_vect[in_order]
  end_vect = end_vect[in_order]
  label_vect = label_vect[in_order]
  
  not_na = is.na(begin_vect)
  begin_vect = begin_vect[!not_na]
  end_vect = end_vect[!not_na]
  label_vect = label_vect[!not_na]
  
  
  plot(1:length(begin_vect) ~ begin_vect, 
       bty = "n",
       yaxt = "n",
       pch = 19, 
       cex = 1.1, 
       ylim = c(0, length(begin_vect) + 1),
       ylab = "",
       xlim = c(min(begin_vect), max(end_vect) + 6000),
       xlab = "Date",
       cex.axis = 1.2,
       cex.lab = 1.4)
  
  abline(v = as.Date(c("1960-01-01", "1980-01-01", "2000-01-01", "2020-01-01", "1970-01-01", "1990-01-01", "2010-01-01", "2030-01-01")), col = rgb(0.1, 0.1, 0.1, 0.3), lwd = 2)
  abline(v = as.Date(c("1965-01-01", "1985-01-01", "2005-01-01", "2025-01-01", "1975-01-01", "1995-01-01", "2015-01-01")), col = rgb(0.1, 0.1, 0.1, 0.2), lwd = 1, lty = 2)
  points(1:length(begin_vect) ~ end_vect, pch = 19, cex = 1.1)
  
  for(i in 1:length(begin_vect)){
    rect(xleft = begin_vect[i], xright = end_vect[i], ybottom = i - 0.5, ytop = i + 0.5, col = alpha(colors_touse[i], 0.5))
    text(x = end_vect[i] + 1, y = i, labels = label_vect[i], pos = 4, cex = 1)

  }
  
  
  
}

map_emall = function(df){
  geo_list <- as.character(df$spatialCoverage)
  bb.frame = as.data.frame(matrix(nrow = 0, ncol = 5))
  colnames(bb.frame) = c("Easting", "Westing", "Northing", "Southing", "title")
  
  for(i in 1:length(geo_list)){
    split_coords = unlist(strsplit(geo_list[i], split = " "))
    split_coords = sort(split_coords)
    if (length(split_coords) == 4){
      bb.frame[nrow(bb.frame) + 1, "Easting"] = as.numeric(split_coords[1])
      bb.frame[nrow(bb.frame), "Westing"] = as.numeric(split_coords[2])
      bb.frame[nrow(bb.frame), "Northing"] = as.numeric(split_coords[4])
      bb.frame[nrow(bb.frame), "Southing"] = as.numeric(split_coords[3])
      bb.frame[nrow(bb.frame), "title"] = as.character(df$title[i])
      
    } else {
      #print(split_coords)
    }
    
  }
  
  return(bb.frame)
  
}

clip_titles = function(title_vect){
  clipped_titles = NULL
  for (i in title_vect) {
    clipped_titles[i] = gsub(",.*", "", i) 
  }
    names(clipped_titles) = NULL
    return(clipped_titles)
}


make_bbox = function(df){
  final_bbox = as.data.frame(matrix(nrow = 0, ncol = 4))
  colnames(final_bbox) = c('Easting', 'Northing', 'color', 'title')
  
  
    
  for(i in 1:nrow(df)){
    
    bbox = as.data.frame(matrix(nrow = 5, ncol = 4))
    colnames(bbox) = c('lon', 'lat', 'color', 'title')
    bbox[1,1] = c(df[i,1])
    bbox[2,1] = c(df[i,2])
    bbox[4,1] = c(df[i,1])
    bbox[3,1] = c(df[i,2])
    bbox[5,1] = c(df[i,1])
    
    bbox[1,2] = c(df[i,3])
    bbox[2,2] = c(df[i,3])
    bbox[4,2] = c(df[i,4])
    bbox[3,2] = c(df[i,4])
    bbox[5,2] = c(df[i,3])
    
    bbox$color = as.factor(i)
    bbox$title = df$title[i]
    
    
    final_bbox = rbind(final_bbox, bbox)
  }
  

 
    return(final_bbox) 
}



#######################


make_map = function(y){
  
  dat = map_data('county')
  dat = dat %>% filter(region == 'colorado')
  dat = dat %>% filter(subregion %in% 'boulder')
  colors = viridis::plasma(nrow(y), alpha = 0.5)
  
  y2 = make_bbox(y)
  y2 = y2 %>% group_by(color)
  
  
  fig <- plot_ly(y2,
                 mode = 'lines',
                 text = ~title,
                 fill = 'toself',
                 type = 'scattermapbox',
                 hoverinfo = 'text',
                 lon =  ~lon,
                 lat =  ~lat,
                 color = ~color,
                 colors = colors,
                 opacity = 0.2)

 
  fig <- fig %>% layout(
          mapbox = list(
            data = y2,
            style = "carto-positron",
            center = list(lat = ~mean(lat, na.rm = T), lon = ~mean(lon, na.rm = T)),
            zoom = 11),
          showlegend = FALSE)  
      
      
  

  
  return(fig)
  
}

timeline2 = function(df){
  
  print(df$begindate)
  df$title = clip_titles(df$title)
  
  ax <- list(
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  
  colors = viridis::plasma(nrow(df), alpha = 0.5)
  
  df = df[df$begindate != "", ]
  
  df = df[order(df$begindate),]
  names = colnames(df)
  df = cbind(df, seq(1:nrow(df)))
  colnames(df) = c(names, 'index')
  
  fig = plot_ly(data = df,
          type = 'scatter',
          x = ~begindate,
          y = ~index,
          text = ~paste(title, "\n", "Start:",begindate, "\n", "End:", enddate),
          hoverinfo = 'text',
          showlegend = FALSE,
          size = I(75),
          color = I("black")
          )
  
  fig = fig %>% add_markers(x = ~as.Date(enddate),
                            y = ~index,
                            showlegend = FALSE,
                            size = I(75),
                            hoverinfo = 'none',
                              color = I("black"))
  
  shapes = list()
  for(i in 1:nrow(df)){
    
    shapes[[i]] = list(type = "rect",
                              fillcolor = colors[i], line = list(color = "black"), opacity = 0.9,
                              x0 = df[i, 'begindate'], x1 = df[i, 'enddate'], xref = "x",
                              y0 = df[i, 'index'] - 0.5, y1 = df[i, 'index'] + 0.5, yref = "y")
    
    names(shapes[i]) = paste("shape", i, sep = "")
                        
  }
  
  fig = fig %>% layout(yaxis = ax,
                       xaxis = list(title = "Date"),
                       shapes = shapes)
  
  fig
  
}

x = batch_search(c("dfasdfasdf"))
