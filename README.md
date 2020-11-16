# INSTALLING R

This tool is intended for R 4.0.2, but compatability and cross-version robustness was a focus during development.  
Download the installer for R 4.0.2 from: "https://cran.r-project.org/bin/windows/base/".  
Run the installer as an administrator.  
Keep the default installation location.  
Select the 64-bit user installation.  
Select customized startup, MDI, plain text help, no start menu folder.  
Keep the defaults for additional tasks.  
Wait for the installation to complete.  

# INSTALLING RSTUDIO

RStudio is only necessary if you intend to use this tool's processing workflow with your own datasets.  
Download the installer for RStudio 1.3.1056 from: "https://rstudio.com/products/rstudio/download/"  
Run the installer as an administrator. Keep the default installation location.  
Do not create start menu shortcuts. Do not allow automated crash reporting.  
Wait for the installation to complete.  

# INSTALLING RTOOLS40

Rtools is only necessary if you intend to use this tool's processing workflow with your own datasets.  
Download the installer for Rtools40 from: "https://cran.r-project.org/bin/windows/Rtools/"  
Run the installer as an administrator. Keep the default installation location.  
Save version history to registry and don't create start menu icons.  
Use the following line of code to add Rtools to the PATH:  
"writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")"  
Restart RStudio and test with: 
"Sys.which("make"); install.packages("cpp11", type = "source");"

# UPDATING PATH VARIABLES

Go to your Windows environment variables ("env" in search).  
Set HOME, R_LIBS_USER under System Variables.  
Make sure R_LIBS_USER is completely empty.  

# PACKAGE INSTALLATION

Before installing, please determine whether you intend to use this tool's processing workflow with your own datasets.  
If so, the Anaconda requirement below must be satisfied:  

If you do not have Anaconda, a Python package manager, please ensure RStudio is closed and install it from "anaconda.com".  
Make sure you install in a PATH without spaces, such as "C:/Anaconda".  
Miniconda does not suffice.  
If you have Anaconda, please ensure that no existing environments are named "r-reticulate".  
To do so, run "conda env remove --name r-reticulate". If the r-reticulate folder persists, delete it manually.  

Then set up r-reticulate:  
"conda create --name r-reticulate"  
"conda activate r-reticulate"  
"conda install keras"  
"conda install matplotlib numba pandas scikit-learn"  
"pip install umap-learn"  
"pip install phate"  
You can then open RStudio.  

Open and execute the code in "installer.R".  
If they appear, the following warnings can be safely ignored:  
"Your CPU supports instructions that this TensorFlow binary  was not compiled to use ..."  
