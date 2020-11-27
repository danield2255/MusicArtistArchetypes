# MusicArtistArchetypes
## Description
This was my senior project advised on by Professor Kelly Bodwin at Cal Poly SLO in Winter 2019. The project aimed to group albums by popular music artists using unsupervised clustering. Once the clusters were made, we could infer on what factors the model was separating groups by and then see if any of those groups aligned with traditional music genres. The html report in "MarkdownKnittedOutputs" gives detailed results. 

All data scraping was done using Python in Jupyter Notebooks, and all analysis was done using R. 

## File and Folder Descriptions 
- MarkdownKnittedOutputs: Contains HTML output of the reports (versions with and without appendix)
- Scrapers: Contains the notebooks used to scrape data used for the analysis
  - BillboardScraping(Charlie).ipynb: File largely authored by a previous student of the advisor, I added onto it to add more cleaning functionality, turn some items to functions, and get writing credits
  - Cleaning(Charlie).ipynb: File largely authored by a previous student of the advisor, I added onto it to add more cleaning functionality
  - OtherScrapingFrost.ipynb: Data scraper which gets data from the Grammys, Spotify, RIAA, and ultimate-guitar.com. Not all of this data is used in this project, as it was purposed for a tangent project. 
  - SpotifyClientLinker.ipynb: API calling notebook to get data from spotify. You will need personal API credentials to use for yourself. 
  - chromedriver: The selenium required executable to do automated web naviagtion
- AlbumClustering.Rmd: Main analysis file used to knit the output reports
- AlbumDataGetter.Rmd: Data cleaning and subsetting file
- WhatMakesUpAnAlbumFinal.pdf: Schema of fields which were included in the analysis


## Potential Next Steps
As far as next steps for this project, one could enrich the data on artists based on how their albums were clustered in the current itteration of this project, then they could run PCA and K-means clustering on the artists themselves to see how this information would influence a model to group artists. 
