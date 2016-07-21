# Masters Project
## The Idea
My masters project: Analysis of mobile vs non-mobile internet traffic using Wikipedia page traffic dumps

Based on Wikipedia page traffic, I calculate the ratio of mobile traffic relative to total traffic
(in terms of visits and the amount of data transmitted). The purpose is to analyse patterns within time periods
and accross time.

For within time periods, I can analyse how relative mobile internet usage varies depending on the hour of the day
or the day of the week. `PreliminaryExploration.ipynb` provides some evidence of this using 2015 data with English
and German language articles as an example.

Across time, I am interested how relative mobile usage changed over the years in response to the introduction of new mobile
devices. For this, I modify the S-curve model first proposed by Griliches (1957). Results to follow.

## Data Gathering
The data underlying this project are Wikipedia page traffic dumps which can be found
[here](https://dumps.wikimedia.org/other/pagecounts-raw/ "Data Description"). The data is recorded hourly for each page/article
and language. Inside `DataScripts` are the tools I used to download and aggregate the data to hourly data of page traffic by
language and type of the page. Specifically, `main_cli.py` is a script made to used in a terminal/command line which prompts for
the time frame of interest and then proceeds to download the data.

## Analysis
To follow.
