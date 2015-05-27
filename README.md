MSGFplus
======
![](http://bioconductor.org/shields/years-in-bioc/MSGFplus.svg) ![](http://bioconductor.org/shields/downloads/MSGFplus.svg) Release: ![](http://bioconductor.org/shields/build/release/bioc/MSGFplus.svg) Devel: ![](http://bioconductor.org/shields/build/devel/bioc/MSGFplus.svg)

This package aims to bridge R and the peptide database search tool MS-GF+. The 
main class of the package is `msgfPar` which handles parsing of parameters to 
MS-GF+. The `msgfPar` class has a `runMSGF` method that starts a peptide search
for a given set of MS data files and possibly reimports the results using mzID.  

Besides this basic functionality it also supports reading in parameters from 
mzIdentML files created by MS-GF+ in order to replicate a search setup as well 
as a very simple gWidgets based GUI to fill out a `msgfPar` object.

MSGFplus is intended as a pure R wrapper. For a more engaging user experience
have a look at [MSGFgui](https://github.com/thomasp85/MSGFgui) which provides a
visual interface on top of this package using shiny.

Installation
------
MSGFgui and it's sister package MSGFplus is intented for inclusion within the next Bioconductor release. Until then, try it out by installing as follows:

```R
source("http://bioconductor.org/biocLite.R")
biocLite('mzR')
biocLite('mzID')
install.packages('shiny')
install.packages('devtools')
install_github('MSGFplus', 'thomasp85')
```

Credit
------
Sangtae Kim is the developer behind the MS-GF+ algoritm, without which this package would be rather shallow. Furthermore he has provided fast and helpful feedback during the development process.

References
------
1. [MSGFgui (R based GUI for MSGFplus)](https://github.com/thomasp85/MSGFgui)
2. [MS-GF+ (Original Java program)](http://proteomics.ucsd.edu/Software/MSGFPlus.html)  
3. [mzID (R package)](https://github.com/thomasp85/mzID)
