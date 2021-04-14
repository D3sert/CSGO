# To add a new cell, type '# %%'
# To add a new markdown cell, type '# %% [markdown]'
# %%
from selenium import webdriver
from bs4 import BeautifulSoup
import pandas as pd
import time
import requests
driver = webdriver.Chrome(executable_path='C:\chromedriver\chromedriver.exe')


# %%
##HELPERFUNCTIONS

def get_matchLinks(page):
    matchesLinks = []
    ## https://www.hltv.org/results?offset=0
    driver.get('https://www.hltv.org/results?offset='+ str(page) +'&stars=1')
    content = driver.page_source
    soup = BeautifulSoup(content)

    ## FIND MATCHES
    for div in soup.findAll('div', attrs={'class': 'results'}):
        for a in div.findAll('a', attrs={'class': 'a-reset'}):
            link = a['href']
            if (link[:8] == "/matches"):
                matchesLinks.append(link)
    return matchesLinks

def get_page_data(link):
    url = 'https://www.hltv.org/' + str(link)
    driver.get(url)
    content = driver.page_source
    soup = BeautifulSoup(content)    
    return soup

def get_game_links(soup):
    statlinks = []
    for div in soup.findAll('div', attrs={'class': 'results-center-stats'}):
        for a in div.findAll('a'):
            statlinks.append(a['href'])
    return statlinks

def get_game_stats(link):
    result = get_page_data(link)
    result = result.findAll('table', {'class':'stats-table'})
    result = pd.read_html(str(result))
    return result


# %%
##GET SOUP STORE SOUP
matchesLinks = get_matchLinks(0)
soup = get_page_data(matchesLinks[0])


# %%
##BASIC INFO

##GET DATE from https://www.hltv.org/matches/2347834***
names = ["Date","Tournament","Team1","Result1","Team2","Result2","Map","MapID","StatLinks","Players"]
data = []
for div in soup.findAll('div', attrs={'class': 'match-page'}):
    pageDate = div.find('div', attrs={'class': 'date'})
    pageTournament = div.find('div', attrs={'class': 'event text-ellipsis'})
    data.append(pageDate.text)
    data.append(pageTournament.text)

##GET FINALRESULT1 from https://www.hltv.org/matches/2347834***
for div in soup.findAll('div', attrs={'class': 'team1-gradient'}):
    pageTeam1 = div.find('div', attrs={'class': 'teamName'})
    pageResult1 = div.find('div', attrs={'class': ['won', 'lost', 'tie']})
    data.append(pageTeam1.text)
    data.append(pageResult1.text)

##GET FINALRESULT2 from https://www.hltv.org/matches/2347834***
for div in soup.findAll('div', attrs={'class': 'team2-gradient'}):
    pageTeam2 = div.find('div', attrs={'class': 'teamName'})
    pageResult2 = div.find('div', attrs={'class': ['won', 'lost', 'tie']})
    data.append(pageTeam2.text)
    data.append(pageResult2.text)

maps = []
mapsID = []
players = []

##GET MAPS AND MAPSID from https://www.hltv.org/matches/2347834***
for div in soup.findAll('div', attrs={'class': 'stats-menu-link'}):
    map1 = div.findAll('div', attrs= {'class': 'dynamic-map-name-full'})
    for div in map1:
        maps.append(div.text)
        mapsID.append(div['id'])
data.append(maps)
data.append(mapsID)

##GET DETAILED STATS LINKS
data.append(get_game_links(soup))

##GET PLAYER INFO
for div in soup.findAll('div', attrs={'class': 'lineups'}):
    for table in div.findAll('td',attrs={'class': 'player player-image'}):
        for a in table.findAll('a'):
            players.append(a['href'])
data.append(players)
    
result = [[names],[data]]
for i in range(0,len(names)):
    print(names[i] + " : " + str(data[i]))


# %%
## GAME STAT INFO



results = get_game_stats(data[8][0])
for i in range(0,len(results)):
    print(results[i])


