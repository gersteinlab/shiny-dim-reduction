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
[App Instructions](#app-instructions)  
[Installing RStudio](#installing-rstudio)  
[Installing Rtools](#installing-rtools)  
[Installing Anaconda](#installing-anaconda)  
[Performing Reduction](#performing-reduction)  
[AWS Integration](#aws-integration)  
[Running Tests](#running-tests)  
[Contributors](#contributors) 

<a name="overview"/>

## Overview

This shiny dimensionality reduction (SDR) project performs dimensionality reduction on tabular data and generates R Shiny apps for interactive, precomputed visualization of the results. Supported analyses include:

* Principal Component Analysis (PCA)
* Variational Autoencoders (VAE)
* Uniform Manifold Approximation and Projection (UMAP)
* Potential of Heat diffusion for Affinity-based Transition Embedding (PHATE)
* t-Distributed Stochastic Neighbor Embedding (tSNE)
* Set Thresholding and Intersection with UpSetR

There are several ways to use this project, each with varying system requirements:

* <b>Online App:</b> If you received a URL, no downloads are necessary - visit the provided link with any browser.
* <b>Local App: </b> If you received a zipped file representing a Store (the stored data for an SDR application), you will need a way to unzip files (e.g. 7Zip) and R >= 4.0.0 (see [Installing R](#installing-r)). Begin by unzipping the Store and obtaining this repository locally by downloading / unzipping, using "git clone", or using RStudio's "Create Project (Version Control)" functionality. Then follow the instructions in [Running App Code](#running-app-code).
* <b>Pipeline:</b> If you intend to run a dimensionality reduction pipeline on tabular data, please read the rest of this document before proceeding.  

<a name="installing-r"/>

## Installing R

To run app source code or perform dimensionality reduction, you must have R >= 4.0.0 installed.

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

In the received Store folder, you will see a file called "app_data.rds". Copy it into the "app" folder of this repository. Then run the application code. In R, set the working directory to the "app" folder in the repo and run the following code to install necessary packages for the app:

```R
source("install.R")
```

The above code does not need to be run on subsequent app launches. To launch the app, set the working directory to the "app" folder in the repo and run the following code to launch the app:

```R
shiny::runApp()
```

<a name="app-instructions"/>

## App Instructions

In Shiny, values are reactive and observe their dependencies. If one of their
dependencies is invalidated - meaning one of the inputs has changed - then they
recalculate their values. Once a reactive value is recalculated, the functions
that depend on it will be invalidated, causing their own downstream recalculation.
This approach allows the app to be highly responsive, without constantly performing
upstream calculations. Users can therefore freely change parameters as they go.
Invisible outputs are also not activated, which saves drawing time.

If you want to save plotly output as an image, use the camera icon. To save all other output, right click the image and use 'Save image as ...'

<b>Controls Glossary</b>

* <u>Start Plotting:</u> Links inputs to the reactive system, which causes plots to update instantaneously.
* <u>Stop Plotting:</u> Unlinks inputs to the reactive system, which freezes plotting.
* <u>Request:</u> Submit a custom analysis request.
* <u>Refresh:</u> Check the application's store for new user requests.
* <u>Bookmark:</u> Creates a URL that replicates this session.
* <u>Numeric Data:</u> Downloads the numeric data used to produce the current plot.
* <u>Metadata:</u> Downloads the metadata used to produce the current plot.
* <u>Notes:</u> See documentation, including a brief description of each category.

<b>Parameters Glossary</b>

* <u>Category:</u> A category is a set of samples with conserved features. Since categories do not necessarily share common metadata characteristics, distinct inputs exist for each category in row subsets, column subsets, colors, shapes, labels, filters, selections, and thresholds.
* <u>Sample Subset:</u> A sample subset is a row subset for a category.
* <u>Feature Subset:</u> A feature subset is a column subset for a category.
* <u>Scaling:</u> Logarithmic data underwent a transformation of f(x) = log2(x+1). Linear data was not transformed by this function.
* <u>Normalization:</u> Global methods normalize over the whole matrix. Local methods normalize over each feature. Normalization generally benefits neural networks, so it is suggested for VAE. On the other hand, normalization can be detrimental to PCA, as it removes the relative magnitudes of all features.
* <u>Method of Dimensionality Reduction:</u>
  * PCA: Principal Component Analysis.
  * VAE: Variational Auto-Encoder.
  * UMAP: Uniform Manifold Approximation and Projection.
  * PHATE: Potential of Heat diffusion for Affinity-based Transition Embedding.
  * Sets: Uses a user-calculated threshold to analyze characteristic intersections.
* <u>Method of Visualization:</u>
  * Explore: Plots combinations of principal components.
  * Summarize: Generates a filter-free summary, with best-fit lines in ggplot2 and plotly3.
  * tSNE: Flattens all components into a t-distributed Stochastic Neighbor Embedding.
* <u>Perplexity:</u> Perplexity is a hyperparameter used in dimensionality reduction methods that create clusters. It is analogous to the expected number of neighbors for any data point.
* <u>Threshold:</u> Let a sample's range be normalized to (0,1) after the scaling transformation. Then a sample will be considered present in a categorical characteristic if it is expressed above this threshold. The bounds of this slider were selected to ensure a broad range of expression patterns without overflowing memory.

<b>Filters Glossary</b>

* <u>Fraction of Samples:</u> Suppose S samples belong to a characteristic. Suppose a feature is present at the threshold level in G of those samples. Then this slider determines the acceptable values of G/S to be displayed.
* <u>Number of Characteristics:</u> This slider determines the number of possible sets that must contain a feature for the feature to be displayed. A feature is included if it is present in an appropriate number of samples.
* <u>Maximum Features:</u> The maximum number of features displayed by set-based approaches.
* <u>Color By, Shape By, Label By:</u>
What category should points on the graph be colored / shaped / labeled by?
(Note: depends on the category selected.)
* <u>Current Filter, Filter By:</u>
'Current Filter' lists all available categories for which filters can be applied.
Samples satisfying the intersection of all filters will be plotted. Filters consist
of including / excluding factors of a selected metadata characteristic.
(Note: depends on the category selected.)

<b>Settings Glossary</b>

* <u>Settings Menu:</u>
If 'Embed Title' is checked, then the title of the plot will be included within the
plot graphic. Otherwise, it will be displayed as actual text. If 'Embed Legend' is
checked, then the legend of the plot will be included within the plot graphic.
Otherwise, the legend will be displayed as an external table. If 'Boost Graphics' is
checked, certain plots will be drawn with more expensive methods. If 'Separate Colors' is
unchecked, then colors / shapes / labels will all be bound to the current color.
If 'Uninverted Colors' is unchecked, then color scales will be reversed.
* <u>Color Palette:</u>
Plots support 12 color scales. The custom color scales are
'Custom' and 'Grayscale'. The color scales from R are
'Rainbow', 'Heat', 'Terrain', 'Topography', and 'CM'. The color scales from Viridis are
'Viridis', 'Magma', 'Plasma', 'Inferno', and 'Cividis'. Cividis
'enables nearly-identical visual-data interpretation' for color-deficient vision,
'is perceptually uniform in hue and brightness, and increases in brightness linearly'.
* <u>Graph Height:</u>
The height of the plotting graphic. If a data table's UI does not appear responsive, try
increasing this value.
* <u>Notification Time:</u>
The time it takes for notifications to fade away. Set to a nonpositive value to hide
all notifications.
* <u>Number of Columns:</u>
The number of bars in the UpSetR histogram plot.
* <u>Displayed Components:</u>
Displayed Component 1 denotes the component, after dimensionality reduction,
that will be usually shown on the x-axis. Displayed Component 2 denotes the component,
after dimensionality reduction, that will usually be shown on the y-axis. For plotly3,
Displayed Component 3 denotes the component, after dimensionality reduction,
that will be usually shown on the z-axis. Components can equal each other.
* <u>Console Output:</u> A tool used to see the precise inputs being passed to the plotting system.

<a name="installing-rstudio"/>

## Installing RStudio

To perform dimensionality reduction, we recommend RStudio >= 1.3.0.

You can download an installer for RStudio from https://rstudio.com/products/rstudio/download.  
For reproducibility, the following settings were used in development:  

* Run the installer as an administrator. Keep the default installation location.  
* Do not create start menu shortcuts. Do not allow automated crash reporting.  

<a name="installing-rtools"/>

## Installing Rtools

To perform dimensionality reduction, we recommend Rtools >= Rtools40.

You can download an installer for Rtools40 from https://cran.r-project.org/bin/windows/Rtools.  
For reproducibility, the following settings were used in development:  

* Run the installer as an administrator. Keep the default installation location.  
* Save version history to registry and don't create start menu icons.  

To add Rtools to your PATH, add the following code to your .Renviron file:

```bash
PATH="${RTOOLS40_HOME}\usr\bin;${PATH}"
```

Restart R and run the following code to test functionality:

```R
Sys.which("make")
```

<a name="installing-anaconda"/>

## Installing Anaconda

To perform dimensionality reduction, a specialized environment in Anaconda (a Python package manager) is necessary. To install Anaconda, download an installer from https://anaconda.com, ensure RStudio is closed, and perform installation in a PATH without spaces, such as "C:/Anaconda".

Once Anaconda is installed, ensure that no existing environments are named "r-reticulate". You can do so through the following commands:

```bash
conda info --envs
conda env remove --name r-reticulate
```

Then set up r-reticulate in the Anaconda Command Prompt:  

```bash
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
15. test_converter.R

<a name="contributors"/>

## Contributors

We thank the following contributors:

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

Additionally, the following R packages were used:

* <u>Rtsne:</u> Jesse H. Krijthe (2015). Rtsne: T-Distributed Stochastic Neighbor
Embedding using a Barnes-Hut Implementation, URL:
<a href=\"https://github.com/jkrijthe/Rtsne\" target=\"_blank\">
https://github.com/jkrijthe/Rtsne</a>
*  <u>Keras:</u> Keras, (2018), GitHub repository,
https://github.com/charlespwd/project-title
<a href=\"https://github.com/keras-team/keras\" target=\"_blank\">
https://github.com/keras-team/keras</a>
*  <u>UMAP:</u> McInnes et al., (2018). UMAP: Uniform Manifold Approximation and
Projection. Journal of Open Source Software, 3(29), 861,
<a href=\"https://doi.org/10.21105/joss.00861\" target=\"_blank\">
https://doi.org/10.21105/joss.00861</a>
* <u>PHATE:</u> Moon, K.R., van Dijk, D., Wang, Z. et al.
Visualizing structure and transitions in high-dimensional biological data.
Nat Biotechnol 37, 1482-1492 (2019).
<a href=\"https://doi.org/10.1038/s41587-019-0336-3\" target=\"_blank\">
https://doi.org/10.1038/s41587-019-0336-3</a>
* <u>UpSetR:</u> Jake R Conway, Alexander Lex, Nils Gehlenborg UpSetR: An R Package
for the Visualization of Intersecting Sets and their Properties doi:
<a href=\"https://doi.org/10.1093/bioinformatics/btx364\" target=\"_blank\">
https://doi.org/10.1093/bioinformatics/btx364</a>
* <u>heatmaply:</u> Galili, Tal, O'Callaghan, Alan, Sidi, Jonathan, Sievert,
Carson (2017). \"heatmaply: an R package for creating interactive cluster heatmaps
for online publishing.\" Bioinformatics. doi:
<a href=\"http://dx.doi.org/10.1093/bioinformatics/btx657\" target=\"_blank\">
http://dx.doi.org/10.1093/bioinformatics/btx657</a>
* <u>Cividis:</u> Nuñez, Jamie R., Christopher R. Anderton, and Ryan S. Renslow.
\"Optimizing colormaps with consideration for color vision deficiency to enable
accurate interpretation of scientific data.\" PloS one 13.7 (2018): e0199239.

Further Reading:

* <u>Optimizing tSNE:</u> Wattenberg, et al., \"How to Use t-SNE Effectively\",
Distill, 2016. <a href=\"http://doi.org/10.23915/distill.00002\" target=\"_blank\">
http://doi.org/10.23915/distill.00002</a>
* <u>James Diao's ERCC Plotting Tool:</u>
<a href=\"https://github.com/jamesdiao/ERCC-Plotting-Tool\" target=\"_blank\">
https://github.com/jamesdiao/ERCC-Plotting-Tool</a>

