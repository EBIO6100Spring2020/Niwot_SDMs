## Shiny Functions ##
#-------------------#

# These are functions to run teh shiny niwot app. IT helps dowload and view data. 
# Check for ways to help by searching for the "-improve" flag. 

## Load pacakges 
library(stringr)
library(XML)
library(DT)
library(plotly)
library(maps)

## make_keywords()
# this function makes the keyewords you are searching into the proper format
# ie. "word1+word2+etc..."
# just takes argument keywords
make_keywords =function(keywords){
  words_bl = '' # make an empty object to store the word block
  for (word in keywords){ # start loop through keyword vector
    if (words_bl != ''){ #add word one at a time to word block
      words_bl = paste(words_bl,'+',word, sep='') # 
    }else { # the first one
      words_bl = word 
    }
  }
  return(words_bl) # spit it out
}

## filter_nwt()
# this function removes any data that isn't from the niwot ridge lter 
# -improve
# I think maybe make this optional at some point, but I'm not sure if 
# I should do that in this function or a higher level one (batch_search())
filter_nwt = function(df){
  df = df[grep('nwt', df$packageid),] # just make sure nwt is in the packageid
  return(df) # give it to me
}

## get_data()
# here this does the actual retrieval of the data list. 
# Uses the xml package to parse a system curl, then make a data frame 
# from the xml that is retrieved
get_data = function(keywords){
  curl_call <- paste0("curl -X GET https://pasta.lternet.edu/package/search/eml?defType=edismax\\&q=", # paste the keywords into the system command
                      keywords,
                      "\\&fl=title,packageid,begindate,enddate,coordinates,cite\\&sort=score,desc\\&sort=packageid\\&start=0\\&rows=100")
  test.biz <- xmlParse(system(curl_call,intern=T)) # curl that bad boy and get some XML
  if(length(xpathSApply(test.biz, "//title", xmlValue))){ #search for title nodes in the data frame to make sure some exist.
    test.df <- xmlToDataFrame(test.biz) # XML to data frame (its in the name)
    test.df = filter_nwt(test.df) # make sure that they are alllll nwt. 
    } else { # no titles? no problem. Empty data frame
      test.df = as.data.frame(matrix(nrow = 0, ncol = 5)) # boom. empty data frame.  
      colnames(test.df) = c("title", "packageid", "begindate", "enddate", "spatialcoverage") # boom. names.
      }
    return(test.df) # boom. spit it back out. 
  }

## filter_exactmatch() 
# this function is just used if you want to get only data that has the exact search words in it
# used inside conditional in batch_search()
filter_exactmatch = function(search, vect, df){
  filter_df = as.data.frame(matrix(nrow = 0, ncol = dim(df)[2])) # make a data frame to put matches in 
  colnames(filter_df) = colnames(df) # name the columns the same as input df
  for(k in 1:length(search)){ # loop through keywords 
    match = df[grep(search[k], vect)] # check title for keyword[i]
    filter_df = rbind(filter_df, match) # add it to the data frame if it matches
  }
  return(fitler_df) # 100% filtered data frame, brought straight to you. 
}


## batch_seach()
# this function will search for keywords on the Niwot EDI portal.
# You can use a filter (argument: filter = TRUE) to make sure that the words you search are in the title of the data set
# -improve 
# I want to filter out non-csv's here so that it isn't confusing between the view - download 
batch_search = function(search = c("Saddle", "Composition"), filter = FALSE) { 
  study_ids = get_data(make_keywords(search)) # find the data sets
  if (filter == TRUE){ # if you want exact matches use that function here 
    study_ids = filter_exactmatch(search, study_ids$title, study_ids)
  } 
  return(study_ids) # this is the list of data - just info. no actual data yet. 
}

## batch_pull()
# this uses the info from batch_search to actually download the data. This is a slow step so 
# I've sort of modulated it into making sure you only download stuff after you have looked at it. 
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
  # plot.new()
  # print(length(full_url))
  # plot.window(xlim = c(0,length(full_url)), ylim = c(0,1))
  
  
  for(i in 1:length(full_url)){
    #rect(xleft = i - 0.95, xright = i - 0.05, ybottom = 0, ytop = 1, fill)
    tmp_csv = read.csv(full_url[i], stringsAsFactors = TRUE) # Read in csv for some QA/Qc
    cn = colnames(tmp_csv) # get column name to check if they are there at all
    
    # if (substr(cn,0, 1)[1] == "X" | any(grepl(cn[1], tmp_csv[,1]))){ # check for X meaning numeric in any of column names. (This is likely if there is no column names)
    #   warning(paste("Data set: ", "[",final_studies[i], "]", "has unkown column names")) # warn ya
    #   missing_data = rep(NA, length(cn)) # place holder df to be the new first row
    #   
    #   missing_data <- as.data.frame(matrix(missing_data, nrow = 1, ncol = length(missing_data)), byrow = TRUE)
    #   colnames(tmp_csv) <- colnames(missing_data)
    #   tmp_csv = rbind(missing_data, tmp_csv)
    #   
    #   for(c in 1:length(cn)){
    #     if(str_detect(cn[c], "[[:digit:]]")){
    #       tmp_value = gsub("[[:alpha:]]","", cn[c])
    #       suppressWarnings(tmp_csv[1,c] <- as.numeric(tmp_value))
    #     } 
    #     else {
    #       suppressWarnings(tmp_csv[1, c] <- as.character(cn[c]))
    #     }
    #   }
    # }

    
    file_info = system(paste0("curl --head ",full_url[i]), intern = T)
    
    if(any(grepl(".csv", file_info[grep("filename", file_info)]))){
      data_list[[data_index]] = read.csv(full_url[i], stringsAsFactors = FALSE)
      names(data_list)[data_index] = final_studies[i]
      data_index = data_index + 1 
    }
    
  }
  
  return(data_list)
  
}

## get_colors()
# this function is for the old, non plotly graphs to get a list of okay colors
# I need to change this to pull the virdis plasma colors or whatever we end up wanting.
# -improve
# switch to be useful w/ plotly
get_colors = function(n){
  color = grDevices::colors()[grep('gr(a|e)y', grDevices::colors(), invert = T)] # get no gra/ey
  colors_touse = sample(color, n) # get number of colors 
  return(colors_touse)
}

## timeline()
# this function is pre plotly, it will sort date vectors and plot them with their title.
# It is pretty cool but the plotly is better. Probabbly remove this at some point. 
timeline = function(begin_vect, end_vect, label_vect, colors_touse){

  begin_vect = as.Date(begin_vect) # make sure it is date
  end_vect = as.Date(end_vect) # make sure it is date
  
  in_order = order(begin_vect) # get the order indices from the first vector
  begin_vect = begin_vect[in_order] # sort all vectors by those indices ^
  end_vect = end_vect[in_order] 
  label_vect = label_vect[in_order]
  
  not_na = is.na(begin_vect) # get rid of nas 
  begin_vect = begin_vect[!not_na] 
  end_vect = end_vect[!not_na]
  label_vect = label_vect[!not_na]


  plot(1:length(begin_vect) ~ begin_vect, # this plots points at each of the start dates
       bty = "n", # plot formatting 
       yaxt = "n",
       pch = 19,
       cex = 1.1,
       ylim = c(0, length(begin_vect) + 1),
       ylab = "",
       xlim = c(min(begin_vect), max(end_vect) + 6000),
       xlab = "Date",
       cex.axis = 1.2,
       cex.lab = 1.4)

  abline(v = as.Date(c("1960-01-01", "1980-01-01",  # add gridlines 
                       "2000-01-01", "2020-01-01", 
                       "1970-01-01", "1990-01-01", 
                       "2010-01-01", "2030-01-01")), 
        col = rgb(0.1, 0.1, 0.1, 0.3), lwd = 2)
  abline(v = as.Date(c("1965-01-01", "1985-01-01", # add more gridlines 
                       "2005-01-01", "2025-01-01", 
                       "1975-01-01", "1995-01-01", 
                       "2015-01-01")), 
         col = rgb(0.1, 0.1, 0.1, 0.2), lwd = 1, lty = 2)
  
  points(1:length(begin_vect) ~ end_vect, pch = 19, cex = 1.1) # end date points 

  for(i in 1:length(begin_vect)){ # plot pretty rectangels and titles for each one. 
    rect(xleft = begin_vect[i], xright = end_vect[i], ybottom = i - 0.5, ytop = i + 0.5, col = alpha(colors_touse[i], 0.5))
    text(x = end_vect[i] + 1, y = i, labels = label_vect[i], pos = 4, cex = 1)
  }
}

## map_emall()
# this filters the spaital coverage field to only return sensible bounding boxes.
# PEOPLE PUT DATA IN WRONG AND THEN YOU CANT STRSPLIT UGH
# example of stupid : 40.3242-105.453232 (WHY NO SPACE???)
# -improve
# make bounding boxes from list of plot points 
map_emall = function(df){
  geo_list <- as.character(df$spatialCoverage) # convet the factor to char
  bb.frame = as.data.frame(matrix(nrow = 0, ncol = 5)) # make empty storage frame
  colnames(bb.frame) = c("Easting", "Westing", "Northing", "Southing", "title") # make sensible column names 
  
  for(i in 1:length(geo_list)){ # loop through all "coordinates" haha
    split_coords = unlist(strsplit(geo_list[i], split = " ")) # split them
    split_coords = sort(split_coords) # sort them
    if (length(split_coords) == 4){ # if there are coords (representing bbox) then keep em 
      bb.frame[nrow(bb.frame) + 1, "Easting"] = as.numeric(split_coords[1])
      bb.frame[nrow(bb.frame), "Westing"] = as.numeric(split_coords[2])
      bb.frame[nrow(bb.frame), "Northing"] = as.numeric(split_coords[4])
      bb.frame[nrow(bb.frame), "Southing"] = as.numeric(split_coords[3])
      bb.frame[nrow(bb.frame), "title"] = as.character(df$title[i])
      
    } else { # deal with this later. I'm not sure what to do about shit coordinates yet. 
      #print(split_coords)
    }
    
  }
  
  return(bb.frame)
  
}

## clip_titles()
# this will make titles nicer for dispalying on graphs. Right now it just clips 
# off the year since that feels redundant on the timeline. 
# -improve
# add more prettieness to titles (unclear what that means right now)
clip_titles = function(title_vect){
  clipped_titles = NULL
  for (i in title_vect) {
    clipped_titles[i] = gsub(",.*", "", i) 
  }
    names(clipped_titles) = NULL
    return(clipped_titles)
}

## make_bbox()
# It makes a nicer data frame for plotting rectangles in plotly
# its moslty needed because idk how plotly works 
make_bbox = function(df){
  final_bbox = as.data.frame(matrix(nrow = 0, ncol = 4)) # holder df
  colnames(final_bbox) = c('Easting', 'Northing', 'color', 'title') #name them! 
  
  for(i in 1:nrow(df)){ # loop through the row and give back an expanded df with each corner
    
    bbox = as.data.frame(matrix(nrow = 5, ncol = 4))
    colnames(bbox) = c('lon', 'lat', 'color', 'title')
    bbox[1,1] = c(df[i,1]) # corner 1x
    bbox[2,1] = c(df[i,2]) # corner 2x
    bbox[4,1] = c(df[i,1]) # corner 3x
    bbox[3,1] = c(df[i,2]) # corner 4x
    bbox[5,1] = c(df[i,1]) # corner 1x
    
    bbox[1,2] = c(df[i,3]) # corner 1y
    bbox[2,2] = c(df[i,3]) # corner 2y
    bbox[4,2] = c(df[i,4]) # corner 3y
    bbox[3,2] = c(df[i,4]) # corner 4y
    bbox[5,2] = c(df[i,3]) # corner 5y 
    
    bbox$color = as.factor(i) # give it a color/factor this was for weird plotly colors
    bbox$title = df$title[i] # store the title for easiest display
    
    final_bbox = rbind(final_bbox, bbox) # nice bounding box with colr and title attached
  }
    return(final_bbox) 
}

## make_mape()
# this function needs the data frame output by map_emall
# with the coordiantes split to East West North and South
# It makes a plotly map with a geographic background
# could make prettier. 
# -improve
# Make it so timeline colors match map colors. 
make_map = function(y){  
  
  ## This step isnt needed anymore but if we ever want a map polygon of CO. idk
  dat = map_data('county') # get county level map data. 
  dat = dat %>% filter(region == 'colorado') # remove everything but CO
  dat = dat %>% filter(subregion %in% 'boulder')
  colors = viridis::plasma(nrow(y), alpha = 0.5) # generate colors 
  
  y2 = make_bbox(y) # convet to bounding boxes. 
  y2 = y2 %>% group_by(color) # I'm not sure if this does anything but, I think its important. Group by pseudo group indicator
  
  fig <- plot_ly(y2,                    # this plots all the rectangles! yay
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

 
  fig <- fig %>% layout( # make it prettier/center the map on CO (data points)
          mapbox = list(
            data = y2,
            style = "carto-positron",
            center = list(lat = ~mean(lat, na.rm = T), lon = ~mean(lon, na.rm = T)),
            zoom = 11),
          showlegend = FALSE)  
      
  return(fig)
  
}

## timeline2 
# plotly version of making a timeline. 
# this looks good. Hover over points to see the title
timeline2 = function(df){
  
  df$title = clip_titles(df$title) # make titles slightly nicer 
  
  ax <- list( # define style for axis later
    title = "",
    zeroline = FALSE,
    showline = FALSE,
    showticklabels = FALSE,
    showgrid = FALSE
  )
  
  
  colors = viridis::plasma(nrow(df), alpha = 0.5) # get colors 
  
  df = df[df$begindate != "", ] # remove any with missing dates. 
  
  df = df[order(df$begindate),] # sort by start date 
  names = colnames(df) # get names to rename columns later. 
  df = cbind(df, seq(1:nrow(df))) # add index which will be the ~y coordinate
  colnames(df) = c(names, 'index') # rename with old names + index
  
  fig = plot_ly(data = df, # start the plot, this just does the begindate points
          type = 'scatter',
          x = ~begindate,
          y = ~index,
          text = ~paste(title, "\n", "Start:",begindate, "\n", "End:", enddate),
          hoverinfo = 'text',
          showlegend = FALSE,
          size = I(75),
          color = I("black")
          )
  
  fig = fig %>% add_markers(x = ~as.Date(enddate), # add enddate points 
                            y = ~index,
                            showlegend = FALSE,
                            size = I(75),
                            hoverinfo = 'none',
                              color = I("black"))
  
  shapes = list() # empty list for the bars/rectangles 
  for(i in 1:nrow(df)){ # make all the rectangles! they are stored in a list and then accessed in layout()
                          
    shapes[[i]] = list(type = "rect",
                              fillcolor = colors[i], line = list(color = "black"), opacity = 0.9,
                              x0 = df[i, 'begindate'], x1 = df[i, 'enddate'], xref = "x",
                              y0 = df[i, 'index'] - 0.5, y1 = df[i, 'index'] + 0.5, yref = "y")
    
    names(shapes[i]) = paste("shape", i, sep = "")
                        
  }
  
  fig = fig %>% layout(yaxis = ax, # add the rectangles 
                       xaxis = list(title = "Date"),
                       shapes = shapes)
  
  return(fig)
  
}

## hist_plot()
# This is just a wrapper for plotly histogram so I don't have to put it in
# the server.R file looks good though. Sets som standard colors 
hist_plot = function(hist_vect, title, color){
  
  if (color == 1){    # define color for input 1 or 2 
    cc = rgb(0.1, 0.4, 0.1, 0.3)
  } else if (color == 2){
    cc = rgb(0.1, 0.1, 0.4, 0.3)
  }
  
  fig = plot_ly(alpha = 0.6, showlegend = FALSE) # initalize plotly
  fig = fig %>% add_histogram(x = ~hist_vect,    # add the histogram
                              marker = list(color = cc), 
                              alpha = 0.5)
  fig = fig %>% layout(xaxis = list(title = ~title), showlegend = FALSE) # change axis and remove lgend
  return(fig)
}

## scatter_plot()
# much like hist_plot() this just packages the plotly scatter into one line
# for server.R makes a scatter plot of 2 selected variables 
scatter_plot = function(vect1, vect2, titlex, titley){
  fig = plot_ly(alpha = 0.6)
  fig = fig %>% add_markers(x = ~vect1, y = ~vect2, size = 5, marker = list(color = rgb(0.1, 0.4, 0.4, 0.3)))
  fig = fig %>% layout(xaxis = list(title = ~titlex),
                       yaxis = list(title = ~titley))
  return(fig)
}

