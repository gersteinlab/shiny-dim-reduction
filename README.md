<!---
---
title: "Shiny Dimensionality Reduction"
pagetitle: Shiny Dimensionality Reduction
---
-->

# Shiny Dimensionality Reduction

<i>Justin Chang, Joel Rozowsky, Mark Gerstein</i>

##### Table of Contents  
[Overview](#overview)  
[Installing R](#installing-r)  
[Running App Code](#running-app-code)  
[Installing RStudio](#installing-rstudio)  
[Installing Rtools](#installing-rtools)  
[Installing Anaconda](#installing-anaconda)  
[Performing Reduction](#performing-reduction)  
[AWS Integration](#aws-integration)  
[Running Tests](#running-tests)
[Acknowledgements](#acknowledgements) 

<a name="overview"/>

## Overview

This shiny dimensionality reduction (SDR) project applies dimensionality reduction methods to tabular data through workflows and generates R Shiny apps for interactive, precomputed visualization of the results. Supported analyses include:

* Principal Component Analysis (PCA)
* Variational Autoencoders (VAE)
* Uniform Manifold Approximation and Projection (UMAP)
* Potential of Heat diffusion for Affinity-based Transition Embedding (PHATE)
* t-Distributed Stochastic Neighbor Embedding (tSNE)
* Set Thresholding and Intersection with UpSetR

There are several ways to use this project, each with varying system requirements:

* <b>Online App:</b> If you received a URL, no downloads are necessary - visit the provided link with any browser.
* <b>Local App: </b> If you received a zipped file representing a Store (the stored data for an SDR application), you will need a way to unzip files (we recommend 7Zip) and R >= 4.0.0 (see [Installing R](#installing-r)). To run the app, begin by unzipping the Store and obtaining this repository locally by downloading / unzipping, using "git clone", or using RStudio's "Create Project (Version Control)" functionality. Then run the local application code as described in [Running App Code](#running-app-code).
* <b>Pipeline:</b> If you intend to perform dimensionality reduction with this project, please read the rest of this document before proceeding.  

<a name="installing-r"/>

## Installing R

To run app source code or generate dimensionality reduction workflows, you must have R >= 4.0.0 installed.

You can download an installer for R from https://cran.r-project.org.  
For reproducibility, the following settings were used in development:  

* Run the installer as an administrator.  
* Keep the default installation location.  
* Select the 64-bit user installation.  
* Select customized startup, MDI, plain text help, no start menu folder.  
* Keep the defaults for additional tasks.

You can customize your home / library locations with the environmental variables "HOME", "R_LIBS_USER":

* <b>Windows:</b> Go to your environment variables ("env" in search) under "System Variables".
* <b>MacOS / Linux:</b> Edit your environment variables through your shell's ".bashrc" file.

<a name="running-app-code"/>

## Running App Code

In R, set the working directory to the "app" folder in the repo and run the following code to install necessary packages for the app:

```R
source("install.R")
```

The above code does not need to be run on subsequent app launches. To launch the app, set the working directory to the "app" folder in the repo and run the following code to launch the app:

```
shiny::runApp()
```

<a name="installing-rstudio"/>

## Installing RStudio

To generate dimensionality reduction workflows, we recommend RStudio >= 1.3.0.

You can download an installer for RStudio from https://rstudio.com/products/rstudio/download.  
For reproducibility, the following settings were used in development:  

* Run the installer as an administrator. Keep the default installation location.  
* Do not create start menu shortcuts. Do not allow automated crash reporting.  

<a name="installing-rtools"/>

## Installing Rtools

To generate dimensionality reduction workflows, we recommend Rtools >= Rtools40.

You can download an installer for Rtools40 from https://cran.r-project.org/bin/windows/Rtools.  
For reproducibility, the following settings were used in development:  

* Run the installer as an administrator. Keep the default installation location.  
* Save version history to registry and don't create start menu icons.  

To add Rtools to your PATH, add the following code to your .Renviron file:

```
PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"
```

Restart R and run the following code to test functionality:

```
Sys.which("make")
```

<a name="installing-anaconda"/>

## Installing Anaconda

To generate dimensionality reduction workflows, a specialized environment in Anaconda (a Python package manager) is necessary. To install Anaconda, download an installer from https://anaconda.com, ensure RStudio is closed, and perform installation in a PATH without spaces, such as "C:/Anaconda".

Once Anaconda is installed, ensure that no existing environments are named "r-reticulate". You can do so through the following commands:

```
conda info --envs
conda env remove --name r-reticulate
```

Then set up r-reticulate in the Anaconda Command Prompt:  

```
conda create --name r-reticulate
conda activate r-reticulate
conda install tensorflow
pip install phate
```

<a name="performing-reduction"/>

## Performing Reduction

For each application that you intend to create, you should have a single corresponding dimensionality reduction workflow. To set up these workflows, please perform the following steps in RStudio:

* Navigate to "File" -> "New Project..." -> "Version Control" -> "Git"
* Set the URL to https://github.com/gersteinlab/shiny-dim-reduction.git
* Name the Project Directory and select the parent directory.
* Press "Create Project" and wait for the project to open.
* To install necessary packages, open "install.R", and source the file. 

During installation, the following warning(s) can be safely ignored:  
```
Your CPU supports instructions that this TensorFlow binary was not compiled to use ...
```

Once installation completes, you will need to designate a folder for storing your workflows, which we will call the workflows folder. The desired folder should be initially empty. To set the empty folder as your workflows folder, you will need to open "workflows.R" and source the file. You will be prompted to enter a directory path and should enter the path of your empty folder. After doing so, we strongly recommend against modifying the folder through methods not listed below. You will subsequently be given an interactive prompt to change your workflows folder, open an existing workflow, or create a new workflow. If you move the workflows folder to a new location, you will need to change your workflows folder in the prompt so that it can be found by the project.

<a name="aws-integration"/>

## AWS Integration

To create applications for visualizing workflow results, select a supported storage method:

* <b>Local Storage:</b> Store all generated data in a folder on a file system, usually named "reference". This is useful for portable executables, but may not be hostable through Shiny if the folder size is too large. 
* <b>AWS.S3:</b> Store all generated data in a bucket on AWS.S3. This substantially decreases app bundle size, but requires more setup. Although AWS offers a free plan, the onus is on the user to ensure that their AWS usage does not exceed their budget.

To upload data to AWS.S3, create an AWS.IAM account with full AWS.S3 permissions. The user should use the functions in "storage.R" to save a master key for that account in the project folder. This master account will upload data and its credentials should not be distributed with the generated app.

Generated apps should each be distributed with an AWS key (id, secret, S3 bucket) that link to an account with limited permissions. These permissions generally ought to include list / get / put, but the overall budget and permissions are in the hands of the developer. Please see example_aws_json.txt for an example policy.

<a name="running-tests"/>

## Running Tests

The tests folder contains scripts for testing various components of the project. Please feel free to share ideas for increasing test coverage.

We suggest running the tests in the following order:

1. test_install.R
2. test_plotting.R
3. test_authentication.R
4. test_find_replace.R
5. test_text_work.R
6. test_ui_functions.R
7. test_storage.R
8. test_preprocess.R
9. test_make_requests.R
10. test_workflows.R
11. test_sca_nor_fun.R
12. test_validation.R
13. test_red_methods.R
14. test_red_requests.R
15. converter.R

<a name="acknowledgments"/>

## Acknowledgments

We thank the following additional contributors:

* Suchen Zheng (Yale)
* Ran Meng (Yale)
* Emily LaPlante (Baylor)
* David Chen (Baylor)
* Roger Alexander (PNDRI)
* Matthew Roth (Baylor)
* Aleks Milosavljevic (Baylor)
* Abhinav Godavarthi (Yale)
* Ana Berthel (Yale)
* Max Sun (Yale)
* Smita Krishnaswamy (Yale)
* Rob Kitchen (Harvard)

