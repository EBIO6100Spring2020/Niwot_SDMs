import requests
from bs4 import BeautifulSoup
import urllib.request
import time

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

keywords=["Sensor","Network"]

#here we make a query URL that we'll scrape for all the html content there. The "join" option should be
#either True or False, and determines whether you search, for example, "Niwot Ridge" or "Niwot" and "Ridge"
#as separate terms.

def make_url(words, join):
    if join == True:
        new_words = "+".join(words)
        full_url="".join(["http://portal.lternet.edu:80/nis/simpleSearch?start=0&rows=400&defType=edismax&q=%22",new_words,"%22&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false"])
        return full_url
    if join == False:
        new_words="+".join(words)
        full_url="".join(["http://portal.lternet.edu:80/nis/simpleSearch?start=0&rows=400&defType=edismax&q=",new_words,"&fq=-scope:ecotrends&fq=-scope:lter-landsat*&fl=id,packageid,title,author,organization,pubdate,coordinates&debug=false"])
        return full_url


#Where we make the url using whatever keywords we want, either joining or not joining them
url=make_url(keywords,True)

#print(url)

#scrape our html
response=requests.get(url)

#This next line should return [200] if the request is successful.
response

#here we parse all the text we scraped into the BeautfiulSoup format. You can use
#other parsers too for this, but since our page is all html, we specify that.
soup=BeautifulSoup(response.text, 'html.parser')

#now i want to loop through all the tags that have a link to a data object and
#pull the text. This will give me both the name of the study and the packageid on
#the following line. So two lines per entry here.

#NOTE: This will only catch niwot data using the nwt identifier. If you want others,
#change that here. I tried making this more general, using just knb-lter or just knb,
#but it kept breaking for some reason. Couldn't figure it out so I just went back
#to this.
newlist=[]
for link in soup.select('a[href*=knb-lter-nwt]'):
    newlist.append(link.string)

#Some of the niwot data packages have a second identifier starting with msb. Not sure why,
#But this will catch it.
for link in soup.select('a[href*=msb]'):
    newlist.append(link.string)

#print(newlist)
#So what this returns is a list where we have a study name then the ID. It's all one list
#so there's no nice structure to it at the moment. It wouldn't be too tough though to just grep
#for anything without a space I'd guess?
#print(newlist)

#or just go full python and do list comprehension like so

tags = ["nwt","msb"]
#note the any function here which lets us search for a match in any of the tags included
#in the tags list above.

#I don't want to explain list comprehension because its very weird and has nonsensical syntax
#but feel free to google it to understand what this is doing.
papers_please = [name for name in newlist if any(z in name for z in tags)]

Study_name = [name for name in newlist if " " in name]

#write IDs and study names to a text file.
with open('packageids.txt', 'w') as filehandle:
    for listitem in papers_please:
        filehandle.write('%s\n' % listitem)

with open('study_names.txt', 'w') as filehandle:
    for listitem in Study_name:
        filehandle.write('%s\n' % listitem)

#print(papers_please)
