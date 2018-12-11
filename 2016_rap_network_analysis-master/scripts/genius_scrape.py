# -*- coding: utf-8 -*-
"""
Created on Sat Dec  8 19:39:56 2018

@author: khasim_power
"""

import requests
from bs4 import BeautifulSoup

response=requests.get('https://genius.com/albums/Lil-uzi-vert/Luv-is-rage-2')
soup = BeautifulSoup(response.text, 'html.parser')
chart_row_title_divs = soup.find_all('div',{'class':'white_container u-x_large_top_margin'})

for blob in chart_row_title_divs:
    boxes=blob.find_all('div',{'class':'metadata_unit metadata_unit--table_row'})
    box=boxes.find_all('span',{'class':'metadata_unit-info'})
    print(box)
    print("\n")