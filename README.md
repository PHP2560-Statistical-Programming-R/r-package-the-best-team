
# Plan 1: Games Collection Package
This package we would like to design is to turn the boring R platform into a gaming platform. 
This package named as Brown64in1, from the old school nintendo video game compliation. (64in1 was taken by others)
The package consist several small games for entertainment, including mine sweeper, gomoku, sodoku and so on. 
Some of the games in the package are fetched from others design. Others might need some work to get finished by ourselves. 
And we will create a list in order to show all the games included. 

# Plan 2: Music Analysis Package
This package we would like to design is to explore musical data amd do some sound analysis with R. 
Use the method we learned in the text mining project to visualize the musical data.

##Data sources
[McGill Billboard dataset](http://ddmal.music.mcgill.ca/research/billboard)
[Million Song Dataset](https://labrosa.ee.columbia.edu/millionsong/)
[Billboard Chart Archive](http://www.song-database.com/charts.php)

##Audio Analysis
1. --Ed Sheeran once said, 'I can play almost all the pop songs, with just five chords'.
We want to analyze the factors that can influence the popularity of a song. Similar to text, there are some kind of patterns in songs. For example, when we say a song is catching, it may represent the chord pattern of "G-Am-C". In order to do this, we first want to recognize the pattern, which is, the pattern that is repeated the most in a specific song; Secondly, we want to extract the pattern; Thirdly, we want to match the pattern in other songs. An important thing is, every song begins with its own key, so the pattern of "G-Am-C" may be the same as "A-Bm-D" in this context, so the pattern should not carry the exact frequency, but a serires of frequency that has the same "shape". We want to write three functions that can respectively detect, extract and match patterns in songs.

2. --'People get high because the notes are high'.
Some people enjoy listening to songs where there are high notes, while some people enjoy listening to "soft" songs. Or say, people in different era exhibited different preference. We want to show the popularity of songs with different range of notes. To do this, we first may want to classify the songs according to gender (condering that female voice is naturally higher that male voice); Secondly, we want to define a song as "high", "low" or "median"; Thirdly, we want to see their popularity, or their percent in a fixed time interval, to evaluate the change of people's taste. 

3. This might be a difficult one. In this task, we want to write a function that contains the audio information of a song, and return the genre of this song--"pop, rock, jazz, R&B, country, rap", and the evaluation is purely based on the rhythm of the song. This task may be based on task, or we can run a linear model to categorize the song(see what kinds of linear model the song fits the most).

(We do not want the package to be a project; but if permitted, and in order to apply what we learn this semester to this group work, we may want to do some text mining in the next part)

##Text mining
The procedure was pretty similar as before.

##Difficulty
We are quite not sure if we can do this project. The package we find dealing with audio is "tuneR", and none of us are familiar with this package and we are not sure how much it can help us with building this package.

