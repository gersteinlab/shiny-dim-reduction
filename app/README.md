citations2 <- "<br><br>
<b>R Packages</b>
<br>
<u>Rtsne:</u> Jesse H. Krijthe (2015). Rtsne: T-Distributed Stochastic Neighbor
Embedding using a Barnes-Hut Implementation, URL:
<a href=\"https://github.com/jkrijthe/Rtsne\" target=\"_blank\">
https://github.com/jkrijthe/Rtsne</a>
<br>
<u>Keras:</u> Keras, (2018), GitHub repository,
https://github.com/charlespwd/project-title
<a href=\"https://github.com/keras-team/keras\" target=\"_blank\">
https://github.com/keras-team/keras</a>
<br>
<u>UMAP:</u> McInnes et al., (2018). UMAP: Uniform Manifold Approximation and
Projection. Journal of Open Source Software, 3(29), 861,
<a href=\"https://doi.org/10.21105/joss.00861\" target=\"_blank\">
https://doi.org/10.21105/joss.00861</a>
<br>
<u>PHATE:</u> Moon, K.R., van Dijk, D., Wang, Z. et al.
Visualizing structure and transitions in high-dimensional biological data.
Nat Biotechnol 37, 1482-1492 (2019).
<a href=\"https://doi.org/10.1038/s41587-019-0336-3\" target=\"_blank\">
https://doi.org/10.1038/s41587-019-0336-3</a>
<br>
<u>UpSetR:</u> Jake R Conway, Alexander Lex, Nils Gehlenborg UpSetR: An R Package
for the Visualization of Intersecting Sets and their Properties doi:
<a href=\"https://doi.org/10.1093/bioinformatics/btx364\" target=\"_blank\">
https://doi.org/10.1093/bioinformatics/btx364</a>
<br>
<u>heatmaply:</u> Galili, Tal, O'Callaghan, Alan, Sidi, Jonathan, Sievert,
Carson (2017). \"heatmaply: an R package for creating interactive cluster heatmaps
for online publishing.\" Bioinformatics. doi:
<a href=\"http://dx.doi.org/10.1093/bioinformatics/btx657\" target=\"_blank\">
http://dx.doi.org/10.1093/bioinformatics/btx657</a>
<br>
<u>Cividis:</u> Nu"

citations3 <- intToUtf8(0x00F1) # Spanish tilde

citations4 <- "ez, Jamie R., Christopher R. Anderton, and Ryan S. Renslow.
\"Optimizing colormaps with consideration for color vision deficiency to enable
accurate interpretation of scientific data.\" PloS one 13.7 (2018): e0199239.
<br><br>
<b>Further Reading</b>
<br>
<u>Optimizing tSNE:</u> Wattenberg, et al., \"How to Use t-SNE Effectively\",
Distill, 2016. <a href=\"http://doi.org/10.23915/distill.00002\" target=\"_blank\">
http://doi.org/10.23915/distill.00002</a>
<br>
<u>James Diao's ERCC Plotting Tool:</u>
<a href=\"https://github.com/jamesdiao/ERCC-Plotting-Tool\" target=\"_blank\">
https://github.com/jamesdiao/ERCC-Plotting-Tool</a>
"

instructions <- "
Welcome! Please feel free to explore this dimensionality reduction tool.
Developed by Justin Chang at the Gerstein Lab from 2019-2021,
under the mentorship of Joel Rozowsky.
<br><br>
In Shiny, values are reactive and observe their dependencies. If one of their
dependencies is invalidated - meaning one of the inputs has changed - then they
recalculate their values. Once a reactive value is recalculated, the functions
that depend on it will be invalidated, causing their own downstream recalculation.
This approach allows the app to be highly responsive, without constantly performing
upstream calculations. Users can therefore freely change parameters as they go.
Invisible outputs are also not activated, which saves drawing time.
<br><br>
If you want to save plotly output as an image, use the camera icon.
To save all other output, right click the image and 'Save image as ...'
<br><br>
<b>Plotting Glossary</b>
<ul>
<li><u>Start Plotting:</u>
Links inputs to the reactive system, which causes plots to update instantaneously.
</li>
<li><u>Stop Plotting:</u>
Unlinks inputs to the reactive system, which freezes plotting.
</li>
<li><u>Bookmark:</u>
Creates a URL that replicates this session.
</li>
<li><u>Numeric Data:</u>
Downloads the numeric data used to produce the current plot.
</li>
<li><u>Metadata:</u>
Downloads the metadata used to produce the current plot.
</li>
<li><u>Randomize:</u>
Randomizes the data selection parameters.
</li>
</ul>
<b>Data Selection Glossary</b>
<ul>
<li><u>Category:</u>
A category is a set of samples with conserved features. Since categories do not
necessarily share common metadata characteristics, distinct inputs exist for each
category in subsetting, coloring, shaping, labeling, filtering, selecting, thresholding.
</li>
<li><u>Feature Subset:</u>
A feature subset is a subset of features for a category.
</li>
<li><u>Method of Dimensionality Reduction:</u>
PCA: Principal Component Analysis.
VAE: Variational Auto-Encoder.
UMAP: Uniform Manifold Approximation and Projection.
PHATE: Potential of Heat diffusion for Affinity-based Transition Embedding.
Sets: Uses a user-calculated threshold to analyze characteristic intersections.
</li>
<li><u>Method of Visualization:</u>
Explore: Plots combinations of principal components.
Summarize: Generates a filter-free summary, with best-fit lines in ggplot2 and plotly3.
tSNE: Flattens all components into a t-distributed Stochastic Neighbor Embedding.
</li>
<li><u>Scale:</u>
Logarithmic data underwent a transformation of f(x) = log2(x+1).
</li>
<li><u>Normalization:</u>
Global methods normalize over the whole matrix. Local methods normalize over each
feature. Normalization generally benefits neural networks, so it is suggested for VAE.
On the other hand, normalization is usually detrimental
to PCA, as it removes the relative magnitudes of all features.
</li>
<li><u>Percentage of Features Used:</u>
Let x be the fraction of features used.
Let t be the total number of features.
Let c be the number of components in the final reduction.
Then the first {c + x * (t - c)} features, in decreasing order by variance,
will be used to form the model.
</li>
<li><u>Perplexity:</u>
Perplexity is a hyperparameter used in dimensionality reduction methods that create
clusters. It is analogous to the expected number of neighbors for any data point.
</li>
<li><u>Threshold:</u>
Let a sample's range be normalized to (0,1) after the scaling transformation.
Then a sample will be considered present in a categorical characteristic if it is
expressed above this threshold. The bounds of this slider were selected to ensure a
broad range of expression patterns without overflowing memory.
</li>
<li><u>Fraction of Samples:</u>
Suppose S samples belong to a characteristic.
Suppose a feature is present at the threshold level in G of those samples.
Then this slider determines the acceptable values of G/S to be displayed.
</li>
<li><u>Number of Characteristics:</u>
This slider determines the number of possible sets that must contain a feature
for the feature to be displayed. A feature is included if it is present
in an appropriate number of samples.
</li>
<li><u>Maximum Features:</u>
The number of features displayed by set-based approaches.
</li>
</ul>
<b>Settings Glossary</b>
<ul>
<li><u>Settings Menu:</u>
If 'Embed Title' is checked, then the title of the plot will be included within the
plot graphic. Otherwise, it will be displayed as actual text. If 'Embed Legend' is
checked, then the legend of the plot will be included within the plot graphic.
Otherwise, the legend will be displayed as an external table. If 'Boost Graphics' is
checked, certain plots will be drawn with more expensive methods. If 'Separate Colors' is
unchecked, then colors / shapes / labels will all be bound to the current color.
If 'Uninverted Colors' is unchecked, then color scales will be reversed.
</li>
<li><u>Color Palette:</u>
Plots support 12 color scales. The custom color scales are
'Custom' and 'Grayscale'. The color scales from R are
'Rainbow', 'Heat', 'Terrain', 'Topography', and 'CM'. The color scales from Viridis are
'Viridis', 'Magma', 'Plasma', 'Inferno', and 'Cividis'. Cividis
'enables nearly-identical visual-data interpretation' for color-deficient vision,
'is perceptually uniform in hue and brightness, and increases in brightness linearly'.
</li>
<li><u>Graph Height:</u>
The height of the plotting graphic. If a data table's UI does not appear responsive, try
increasing this value.
</li>
<li><u>Notification Time:</u>
The time it takes for notifications to fade away. Set to a nonpositive value to hide
all notifications.
</li>
<li><u>Number of Columns:</u>
The number of bars in the UpSetR histogram plot.
</li>
<li><u>Displayed Components:</u>
Displayed Component 1 denotes the component, after dimensionality reduction,
that will be usually shown on the x-axis. Displayed Component 2 denotes the component,
after dimensionality reduction, that will usually be shown on the y-axis. For plotly3,
Displayed Component 3 denotes the component, after dimensionality reduction,
that will be usually shown on the z-axis. Components can equal each other.
</li>
<li><u>Console Output:</u>
A tool used to see the precise inputs being passed to the plotting system.
</li>
</ul>
<b>Filters Glossary</b>
<ul>
<li><u>Color By, Shape By, Label By:</u>
What category should points on the graph be colored / shaped by?
(Note: depends on the category selected.)
</li>
<li><u>Current Filter, Filter By:</u>
'Current Filter' lists all available categories for which filters can be applied.
Samples satisfying the intersection of all filters will be plotted. Filters consist
of including / excluding factors of a selected metadata characteristic.
(Note: depends on the category selected.)
</li>
</ul>"