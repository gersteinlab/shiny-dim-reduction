# OBJECTIVE

The purpose of this tool is to perform dimensionality reduction and visualize the results.  
This project contains a standalone app for visualizing preprocessed data and a data analysis workflow for generating new data to visualize.  
Developed at the Gerstein Lab from 2019-2020 by Justin Chang.  

# INSTALLING R

This tool is intended for R 4.0.3, but cross-version compatability was a focus during development.  
You can download an installer for R from "https://cran.r-project.org".  
For reproducibility, the following settings were used in development:  
* Run the installer as an administrator.  
* Keep the default installation location.  
* Select the 64-bit user installation.  
* Select customized startup, MDI, plain text help, no start menu folder.  
* Keep the defaults for additional tasks.  

# INSTALLING RSTUDIO

RStudio is only necessary if you intend to use this tool's data analysis workflow.  
This tool is intended for RStudio 1.3.1056, but cross-version compatability was a focus during development.  
You can download an installer for RStudio from "https://rstudio.com/products/rstudio/download/".  
For reproducibility, the following settings were used in development:  
* Run the installer as an administrator. Keep the default installation location.  
* Do not create start menu shortcuts. Do not allow automated crash reporting.  

# INSTALLING RTOOLS40

Rtools is only necessary if you intend to use this tool's data analysis workflow.  
This tool is intended for Rtools40, but cross-version compatability was a focus during development.  
You can download an installer for Rtools40 from "https://cran.r-project.org/bin/windows/Rtools/".  
For reproducibility, the following settings were used in development:  
* Run the installer as an administrator. Keep the default installation location.  
* Save version history to registry and don't create start menu icons.  

# UPDATING PATH VARIABLES

These steps are only necessary if you intend to use this tool's data analysis workflow.  
Go to your Windows environment variables ("env" in search).  
Set HOME, R_LIBS_USER under System Variables.  
Use the following R code to add Rtools to the PATH and specify the root directory of your projects (replace '~/Justin-Tool' with your intended directory):  
* "writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")"
* "cat('SHINY_DIM_REDUCTION_ROOT="~/Justin-Tool"\n', append=TRUE, file="~/.Renviron")"
Restart RStudio and run the following snippets of R code to test functionality:
* "Sys.which("make")"  
* "Sys.getenv("SHINY_DIM_REDUCTION_ROOT")"  
The actual '.Renviron' file can be modified in a text editor to alter these settings.  

# PACKAGE INSTALLATION

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

You can then open RStudio.  

In your specified directory (SHINY_DIM_REDUCTION_ROOT), create a folder called "shiny-dim-reduction" and place all source code in "shiny-dim-reduction".  
To finish installation, open and execute the code in "installer.R".  
The following warning(s) can be safely ignored:  
"Your CPU supports instructions that this TensorFlow binary was not compiled to use ..."  