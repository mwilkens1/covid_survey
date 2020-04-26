
# Eurofound: Living, Working and Covid-19 survey data visualisation

1. [Project Motivation](#motivation)
2. [Data](#data)
3. [File Descriptions](#files)
4. [Results](#results)

## Project Motivation<a name="motivation"></a>
[Eurofound](https://www.eurofound.europa.eu/) launched a 10-minute e-survey to capture the most immediate changes and their impact of Covid-19 on people's quality of life and work. This repository contains the code for three R Shiny apps:

* Data visualisation of the data on the Eurofound website (/app_web)
* Benchmark offered to respondents in which they can compare their own answers to the average (/app_benchmark)
* Dashboard for tracking the response to the survey (/app_response)

## Data<a name="data"></a>
The data used are the results of the Eurofound survey. Currently the data are not publicly available. The data was collected with [soSci](https://www.soscisurvey.de/) and for this app the data is directly retrieved from their API. 

The app on the Eurofound website as well as the benchmark are based on static datafile, while the dashboard pulls the data from the API to get realtime updates.

## File Descriptions <a name="files"></a>

The apps are in three seperate subfolders: app_web, app_benchmark and app_response. In the base directory the following additional files can be found:

* import.R: main ETL script which imports the data from the API, labels the variables, creates a list with all the characteristics and labels of each variable for use in the apps and stores the datafiles. Note that the script requires an API key which is not included in this repository.
* cleaning_simple.R: script for cleaning the data which is imported into import.R
* weighting_by_country.R script for weighting the data. Imported into import.R
* weighting_data folder: includes population statistics for weighting
* shapefiles folder: this has shapefiles for creating the leaflet map. These are from [Eurostat](https://ec.europa.eu/eurostat/web/gisco/geodata)
 
### Data visualisation on Eurofound website (/app_web)
The folder /app_web contains the files for creating the data visualisation on the Eurofound website. 

* data folder: has a shapefile for creating the maps and a file called 'varinfo.Rda'. The latter is a list with characteristics for each variable and is used in the app as a reference. Note that the main data file is not included in Github.
* app.R: main R Shiny app including ui and server. Run this script to start the app. It loads several helper functions listed below
* make_data.R: script for calculating the data for the maps and plots. Its output is also the data that can be downloaded with the 'download data' button.
* make_map.R: script for creating the map using Leaflet and the shapefile in the 'data' folder.
* make_plot.R: script for making plotly plot. 
* make_description.R: script for creating the text under the plot. It generates text describing what is shown in the plot and which categories have been excluded if any.

### Benchmark (/app_benchmark)
The folder /app_benchmark contains the files for creating the benchmark that respondents receive. 

* app.R: R shiny app for running the benchmark. It queries the API to retrieve the respondents data so that it can be shown next to the data for the entire survey. 
* get_respondent_data.R: The respondent ID is passed on with a URL parameter and this script queries the API with that ID.
* data/varinfo.Rda: a list with characteristics for each variable and is used in the app as a reference.

### Dashboard
The folder /app_benchmark contains the files for creating the dashboard for tracking the response of the survey. 

* app.R: app for running the dashboard
* import.R: this pulls the data from the API when the app is started.
* cleaning_simple.R: this labels repsonses as clean, i.e. when the respondent has filled out enough questions and a few mor rules. This is used in the dashboard to track the full and clean response. 

## Results<a name="results"></a>

The web visualiation can be found on the Eurofound website (link to follow). It is deployed on shinyapps.io. The benchmark is only available to respondents to the survey. The dashboard is not publicly available. 