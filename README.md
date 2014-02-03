MSGFplus
================================
This package aims to bridge R and the peptide database search tool MS-GF+. The 
main class of the package is `msgfPar` which handles parsing of parameters to 
MS-GF+. The `msgfPar` class has a `runMSGF` method that starts a peptide search
for a given set of MS data files and possibly reimports the results using mzID.  

Besides this basic functionality it also supports reading in parameters from 
mzIdentML files created by MS-GF+ in order to replicate a search setup as well 
as a very simple gWidgets based GUI to fill out a `msgfPar` object.

References
---------------------------------
[MS-GF+](http://proteomics.ucsd.edu/Software/MSGFPlus.html)  
[mzID](https://github.com/thomasp85/mzID)