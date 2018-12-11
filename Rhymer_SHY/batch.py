# -*- coding: utf-8 -*-

import os
import subprocess

path = r"C:\Users\HSANG3\Desktop\Rhymer\lyricsGenerator"
path = r"C:\Users\HSANG3\Desktop\Rhymer\lyrics_en"

def IPA(dir=path, artist=None, album=None, print_stats=False, language='en-us', lookback=15):

    if artist is not None:
        artists = [artist]
    else:
        artists = os.listdir(dir)
    for artist in artists:
        artist_dir = dir+"\\"+artist
        print ("Analyzing artist: %s" % artist)
        songs = os.listdir(artist_dir)
            # Only the .txt files
        songs = [song for song in songs if len(song)>=4 and song[-4:]=='.txt']
        for song in songs:
            song_dir = artist_dir+"\\"+song
            # create .ipa file
            cmd = u"espeak -xqf %s --phonout=%s.ipa" %(song_dir, song_dir)
            subprocess.run(cmd,shell=True)
            
def main():
    IPA(dir=path, language='en-us', lookback=15)

if __name__ == '__main__':
    main()

