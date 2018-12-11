# -*- coding: utf-8 -*-
"""
Created on Sat Dec  8 15:32:07 2018

@author: khasim_power
"""
import requests
from bs4 import BeautifulSoup
from datetime import datetime, timedelta


INITIAL_WEEK = '2017-01-02'


week=datetime.strptime(INITIAL_WEEK, '%Y-%m-%d')
base_chart_url = 'http://www.billboard.com/charts/rap-albums/'
response = requests.get('https://www.billboard.com/charts/rap-albums/2017-01-02')
soup = BeautifulSoup(response.text, 'html.parser')
chart_row_title_divs = soup.find_all('div',{'class':'chart-list-item'})
albums = set()
for div in chart_row_title_divs:
    album_name = div.find_all('span',{'class':'chart-list-item__title-text'})[0].text.strip()
#    
    artist_name_as = div.find_all('a')
    if artist_name_as:
        artist_name = artist_name_as[0].text.strip()
    else:
        artist_name = div.find_all('h3',{'class':'chart-list-item__artist'})[0].text.strip()
    album = (album_name, artist_name)
    albums.add(album)
