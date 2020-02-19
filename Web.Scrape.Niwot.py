#! /usr/bin/env python3

import requests
from bs4 import BeautifulSoup
import urllib.request
import time, datetime, os

#So here's the first hurdle. To get a URL to scrape in the first place, I had to go into the Advanced
#Search on the LTER Network page (not the other page that Sarah had sent) and select the Niwot
#site. You could probably search Saddle or Niwot Sensor Network or something like that too.
#Anyways, point here is, the actual URL for the query is given on the page itself not in the URL
#at the top of the browser. So, we have to figure out how to get multiple pages of this if we want to
#scrape all the Niwot packageid's.

#url="http://portal.lternet.edu:80/nis/simpleSearch?defType=edismax&q=*:*&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fq=scope:(knb-lter-nwt)&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false"

#syntax for a search is, &q=%22keyword+keyword+keyword+etc%22&fq etc etc

#Here you can put whatever search terms you would use in the actual search box on the website. Keep them
#as separate strings, and then you can join them later. At this point I haven't implemented a way
#to serach like "Niwot Ridge" and "sensor" as two separte terms. It's either one long term ("Niwot Ridge Sensor")
#or three seaparate terms ("Niwot" "Ridge" and "Sensor")

search_file = open(r"Search.Words.txt", "r")



keywords = search_file.readlines()
print(keywords)

#keywords=["Niwot Ridge", "Soil Moisture", "Sensor Network"]




#here we make a query URL that we'll scrape for all the html content there. The "join" option should be
#either True or False, and determines whether you search, for example, "Niwot Ridge" or "Niwot" and "Ridge"
#as separate terms.

def make_url(words, join):
    if join == True:
        new_words = "+".join(words)
        full_url="".join(["http://portal.lternet.edu:80/nis/simpleSearch?start=0&rows=1500&defType=edismax&q=%22",new_words,"%22&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false"])
        return full_url
    if join == False:
        new_words="+".join(words)
        full_url="".join(["http://portal.lternet.edu:80/nis/simpleSearch?start=0&rows=1500&defType=edismax&q=",new_words,"&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false"])
        return full_url


#Where we make the url using whatever keywords we want, either joining or not joining them
url=make_url(keywords,False)

print(url)

#scrape our html
response=requests.get(url)

#This next line should return [200] if the request is successful.

#here we parse all the text we scraped into the BeautfiulSoup format. You can use
#other parsers too for this, but since our page is all html, we specify that.
soup=BeautifulSoup(response.text, 'html.parser')

#print(soup)
#now i want to loop through all the tags that have a link to a data object and
#pull the text. This will give me both the name of the study and the packageid on
#the following line. So two lines per entry here.

#NOTE: This will only catch niwot data using the nwt identifier. If you want others,
#change that here. I tried making this more general, using just knb-lter or just knb,
#but it kept breaking for some reason. Couldn't figure it out so I just went back
#to this.
raw_links = soup.select('a[href*=knb-lter-nwt]')
raw_links.extend(soup.select('a[href*=msb]'))
titles = []
ids = []


#print(range(len(raw_links)))

for l in range(len(raw_links)):
    #print(l)
    #print(raw_links[l])
    #print("\n")
    if raw_links[l].string is None:
        print("NoneType")
    else:
        if l % 2 == 0:
            raw_title = raw_links[l].string
            cleaned_title = raw_title.replace('\t','')
            cleaned_title = cleaned_title.replace('\n',' ')
            cleaned_title = cleaned_title.replace('-','to')
            cleaned_title = cleaned_title.replace(' ', '_')
            cleaned_title = cleaned_title.replace(',','')
            cleaned_title = cleaned_title.replace('.','')
            titles.append(cleaned_title)
        else:
            ids.append(raw_links[l].string)

#print(titles)



output_file = 'study_ids.csv'
if not os.path.exists(output_file):
    with open(output_file, 'w') as f:
        f.write('paper_id,query_time,paper_title\n')

# write study ids, names, and query time to csv
query_time = datetime.datetime.now()
with open(output_file, 'a') as f:
    for i in range(len(titles)):
        paper_id = ids[i]
        paper_title = titles[i]
        line = "%s,%s,%s\n" % (paper_id,query_time,paper_title)
        f.write(line)
