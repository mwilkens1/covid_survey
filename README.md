# Eurofound: Living, Working and Covid-19 survey data visualisation

1. [Project Motivation](#motivation)
2. [Data](#data)
3. [File Descriptions](#files)
4. [Structure](#structure)
4. [Results](#results)

## Project Motivation<a name="motivation"></a>
[Eurofound](https://www.eurofound.europa.eu/) launched a 10-minute e-survey to capture the most immediate changes and their impact of Covid-19 on people's quality of life and work. This repository contains the code for the data visualisation of the data on the Eurofound website.  

## Data<a name="data"></a>
The data used are the results of the Eurofound survey. Currently the data are not publicly available. The data was collected with [soSci](https://www.soscisurvey.de/) and for this app the data is directly retrieved from their API. 

## File Descriptions <a name="files"></a>
This repository contains the following files:

* app.R: main R Shiny app including ui and server. Run this script to start the app. It loads several helper functions listed below
* create_reference_list.R: creates a list object that contains all variable names, descriptions and other attributes. This is used in the webapp to choose labels and texts.
* label_and_recode.R: script for recoding and labelling the raw data from the API
* make_data.R: script for calculating the data for the maps and plots. Its output is also the data that can be downloaded with the 'download data' button.
* make_description.R: script for creating the text under the plot. It generates text describing what is shown in the plot and which categories have been excluded if any.
* make_map.R: script for creating the map using Leaflet and the shapefile in the 'data' folder.
* make_plot.R: script for making plotly plot. 
* merge_waves.R: script for merging the datasets of the two waves of the survey into one datafile. 
* weighting_by_country.R: script for weighting the data.
* (/wave1 and /wave2) cleaning.R: script for cleaning the data
* (/wave1 and /wave2) extract.R: script for pulling the data from the API
* (/wave1 and /wave2) get_data_from_API.R: helper function for pulling the data from the API.

The data folder is not included in this Github repository because the data is not (yet) public.

## Structure<a name="structure"></a>
To create the data:

1. Run extract.R for both wave 1 and wave 2. This creates raw datafiles for wave 1 and 2.
2. Run 'merge_waves.R'. This combines the dataset by adding rows. It also weighs and cleans the data and creates the list of descriptions.
3. Run app.R to show the app.

## Results<a name="results"></a>
The web visualiation can be found on the [Eurofound website](https://www.eurofound.europa.eu/data/covid-19). It is deployed on shinyapps.io. The benchmark that is built in to the app is only available to respondents to the survey. 