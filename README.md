## Overview
An HTML quick read of population statistics with language and content suitable for schools.
## Files and folders
| File | Purpose  |
| --- | ---  |
| 'index.html' | The main page  |
| 'nisra-styles.css'  | Follows NISRA style guide for headings, fonts, colours, spacing etc.  |
| 'src/r/data.R' | Read in data portal tables, summarise and write to json  |
| 'data.json' | Stores data created in R ready to be used in HTML  |
## Getting started
### Install VS Code
Download and install from the ITAssist Store.
### Clone the Github repository
Open VS Code:
 * Copy the URL `https://github.com/NISRA-Tech-Lab/quick-read-population/tree/main`
 * `View > Command Palette` <br>
 * `Git: Clone` <br>
 * Paste the copied github URL
 
 ### Install Live Server extension
 In VS Code, go to Extensions > search for Live Server > install.
 ### View the HTML
 * Open `index.html` in VS Code.
 * Click 'go live` (bottom right corner of VS Code)
 * The page will open in your browser, and will refresh automatically when you save any code changes.
 ### Making changes
 Text changes can be made directly in index.html.<br>
 To read in any new tables, add to the list of datasets at the top of `data.R`.<br>
 Create any new variables in `data.R` and add to the list of variables to export to data.json at the end of this file.
 ### Updates
 When tables are updated on the data portal, you will need to upload an updated version of the HTML.<br> 
 When you open the project in VS Code, it will pull the latest version of the tables from the portal.<br>
 Check the figures, make any necessary text updates and then upload the new version of index to the server.
