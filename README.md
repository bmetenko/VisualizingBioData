# VisualizingBioData
## General R code for bioinformatic visualization of acquired gene hybridization data.
Data values and gene names randomized and scrambled in the interest of professionalism.

## Uses xlsx files with specifically formatted data tables. 
Please see Data and R folder for specifics. 

Briefly, the CorrHeatPCA function uses an excel sheet that contains a column of genes, i.e. "Probe Name", and multiple columns of expression count data with annotation for condition and replicate. This sheet is of log2 transformed median centered data.
The VolcanoMake function takes log2 tranformed data that has been grouped into the appropriate comparisons, with log2FC (fold change), Pvalue, and Neglog10p columns. These groupings are addressed by and split by sheet name.

  ![3D PCA](https://cdn.rawgit.com/bmetenko/VisualizingBioData/master/ExampleCodeOutput/PCA3D_Example.png "3D PCA")

Other examples I could include can be found in the ExampleCodeOutput Folder and subfolders. They are tiff files, so did not natively show up on the "preview changes" operation on this file, but I can add a function argument that can be specified for image format if there is demand for it, i.e. imageOutputType = png, jpg, or gif.
