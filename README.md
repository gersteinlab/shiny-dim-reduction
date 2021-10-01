# shiny-dim-reduction

##### Table of Contents  
[Overview](#overview)  
[Installing R](#installing-r)  
[Running App Code](#running-app-code)  
[Installing RStudio](#installing-rstudio)  
[Installing Rtools](#installing-rtools)  
[Installing Anaconda](#installing-anaconda)  

<a name="overview"/>  
## Overview

This workflow applies dimensionality reduction methods (PCA, VAE, UMAP, PHATE, upSetR, tSNE, hdbscan) to tabular data and generates R Shiny apps for interactive, precomputed visualization of the results. 

Developed by Justin Chang under the mentorship of Joel Rozowsky and Mark Gerstein at the Gerstein Lab.

There are three ways to use this workflow:

* <b>Portable App:</b> If you received a portable Windows application generated by this workflow, no installations are necessary. To run the application, unzip the directory and run "run.bat".
* <b>App Code:</b> If you possess source code for an application generated by this workflow, read the "Installing R" and "Running App Code" sections of this document.
* <b>Reduction Workflow</b>: If you intend to perform dimensionality reduction with this workflow, please read the rest of this document before proceeding.

<a name="installing-r"/>  
## Installing R

<b>If you intend to use this tool's data analysis workflow, we recommend R >= 4.0.0.</b>  

You can download an installer for R from "https://cran.r-project.org".  
For reproducibility, the following settings were used in development:  

* Run the installer as an administrator.  
* Keep the default installation location.  
* Select the 64-bit user installation.  
* Select customized startup, MDI, plain text help, no start menu folder.  
* Keep the defaults for additional tasks.  

<a name="running-app-code"/>  
## Running App Code

, navigate to the appropriate directory (should contain app folder) and run the following commands:

* "install.packages("shiny")", only needed if first time
* "library("shiny")", only needed if first time
* "runApp("app.R")", follow installation instructions if first time

<a name="installing-rstudio"/>  
## Installing RStudio

<b>If you intend to use this tool's data analysis workflow, we recommend RStudio >= 1.3.0.</b>
This tool is intended for RStudio 1.3.1056, but cross-version compatability was a focus during development.  

You can download an installer for RStudio from "https://rstudio.com/products/rstudio/download/".  
For reproducibility, the following settings were used in development:  

* Run the installer as an administrator. Keep the default installation location.  
* Do not create start menu shortcuts. Do not allow automated crash reporting.  

<a name="installing-rtools"/>  
## Installing Rtools

Rtools is only necessary if you intend to use this tool's data analysis workflow.  
This tool is intended for Rtools40, but cross-version compatability was a focus during development.  
You can download an installer for Rtools40 from "https://cran.r-project.org/bin/windows/Rtools/".  
For reproducibility, the following settings were used in development:  

* Run the installer as an administrator. Keep the default installation location.  
* Save version history to registry and don't create start menu icons.  

<a name="installing-anaconda"/>  
## Installing Anaconda

If you intend to use this tool's data analysis workflow with your own datasets, the Anaconda requirement below must be satisfied:  

* If you do not have Anaconda, a Python package manager, please ensure RStudio is closed and install it from "anaconda.com" in a PATH without spaces, such as "C:/Anaconda".  
* If you have Anaconda, please ensure that no existing environments are named "r-reticulate". To do so, run "conda env remove --name r-reticulate". If the r-reticulate folder persists, delete it manually.  

Then set up r-reticulate in the Anaconda Command Prompt:  

* "conda create --name r-reticulate"  
* "conda activate r-reticulate"  
* "conda install keras"  
* "conda install matplotlib numba pandas scikit-learn"  
* "pip install umap-learn"  
* "pip install phate"

# Updating Path Variables

These steps are only necessary if you intend to use this tool's data analysis workflow.  
Note that TILDE needs to be replaced with the appropriate character.  
Go to your Windows environment variables ("env" in search).  
Set HOME, R_LIBS_USER under System Variables.  
Use the following R code to add Rtools to the PATH and specify the root directory of your projects (replace '~/Justin-Tool' with your intended directory):  

* "writeLines('PATH="\${RTOOLS40_HOME}\\usr\\bin;\${PATH}"', con = "TILDE/.Renviron")"
* "cat('SHINY_DIM_REDUCTION_ROOT="TILDE/Justin-Tool"\n', append=TRUE, file="TILDE/.Renviron")"
Restart RStudio and run the following snippets of R code to test functionality:
* "Sys.which("make")"  
* "Sys.getenv("SHINY_DIM_REDUCTION_ROOT")"  

The actual '.Renviron' file can be modified in a text editor to alter these settings.  

# Package Installation

In your specified directory (SHINY_DIM_REDUCTION_ROOT), create a folder called "shiny-dim-reduction" and place all source code in "shiny-dim-reduction".  
To finish installation, open and execute the code in "installer.R".  
The following warning(s) can be safely ignored:  
"Your CPU supports instructions that this TensorFlow binary was not compiled to use ..." 

# AWS S3 Integration

This tool offers two options for storing the precomputed data that the application uses. 

* The first option is local storage. This is sufficient for portable executables, but
will not be hostable online via Shiny and requires the source code version to be accompanied by a dataset. 
* The second option is AWS.S3 storage, which resolves the issues above. Although AWS offers a free plan, the onus is on the user to ensure that their AWS usage does not exceed their budget.

To pursue AWS integration, the user must first create an "s3_master.R" file as shown in "s3_master_template.R" with the credentials of an AWS IAM account that has full permissions in Amazon S3. This master account will upload data and its credentials should not be distributed with the generated app.

Generated apps should each be distributed with a set of AWS keys that link to an account with limited permissions. These permissions generally ought to include list / get / put, but the overall budget and permissions are in the hands of the developer. Please see example_aws_json.txt for an example policy.

# Generating Portable Executables

To begin, install R-Portable and GoogleChromePortable into your dist folder.
Add this line to R-Portable/App/R-Portable/etc/Rprofile.site: 
".First = function(){.libPaths(.Library)}"
  
Open up R-Portable as an admin. Check the library location is portable with .libPaths(). Run the app once to install all necessary packages.

Create a file called runShinyApp.R with the following lines:

* message('library paths:\n', paste('... ', .libPaths(), sep='', collapse='\n'))
* options(browser='C:/Program Files (x86)/Google/Chrome/Application/chrome.exe')
* shiny::runApp('app', launch.browser=T)

Create a file called run.bat with the following lines:

* SET ROPTS=--no-save --no-environ --no-init-file --no-restore --no-Rconsole
* R-Portable\App\R-Portable\bin\Rscript.exe %ROPTS% runShinyApp.R 1> ShinyApp.log 2>&1

Final product:

* runShinyApp.R
* run.bat
* app (folder)
* reference (folder)
* R-Portable (folder)
* GoogleChromePortable (folder)
