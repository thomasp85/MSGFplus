MSGFplus
======

This package aims to bridge R and the peptide database search tool MS-GF+. The 
main class of the package is `msgfPar` which handles parsing of parameters to 
MS-GF+. The `msgfPar` class has a `runMSGF` method that starts a peptide search
for a given set of MS data files and possibly reimports the results using mzID.  

Besides this basic functionality it also supports reading in parameters from 
mzIdentML files created by MS-GF+ in order to replicate a search setup.

MSGFplus is intended as a pure R wrapper. For a more engaging user experience
have a look at [MSGFgui](https://github.com/thomasp85/MSGFgui) which provides a
visual interface on top of this package using shiny.


Installation
------

MSGFgui was in Bioconductor until version [3.14](https://bioconductor.org/packages/3.14/bioc/html/MSGFplus.html). 
It as been removed but is still available on Github, although not actively maintained anymore.

```R
remotes::install_github('thomasp85/MSGFplus')
```

Credit
------
Sangtae Kim is the developer behind the MS-GF+ algoritm, without which this package would be rather shallow. Furthermore he has provided fast and helpful feedback during the development process.

References
------
1. [MSGFgui (R based GUI for MSGFplus)](https://github.com/thomasp85/MSGFgui)
2. [MS-GF+ (Original Java program)](http://proteomics.ucsd.edu/Software/MSGFPlus.html)  
3. [mzID (R package)](https://github.com/thomasp85/mzID)
