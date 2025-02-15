---
output:
  word_document: default
  html_document: default
---
### **Module:** ***Niche Overlap***

**BACKGROUND**

Quantifying niche differences between species has been a topic of interest for ecologists for decades (e.g. Colwell and Futuyma 1971), originally with a focus on the consumption of depletable resources (i.e. related to the Eltonian niche). As an extension, quantifying niche differences along environmental gradients (i.e. related to the the Grinellian and Huntchinsonian niches) using species occurrences became common in the late 1990s with the development of models of species’ niches/distributions (Guisan and Zimmermann 2000). Subsequently, Warren et al. 2008 developed a method based on niche/distribution models (and associated randomization tests) to quantify niche differences using pixel-by-pixel comparisons of predictions in geographic space. One potential issue with this approach is that many pixels predicted as suitable by the model may not be occupied; although the models assume that species are at equilibrium with their environment, many factors (e.g. biotic interactions, dispersal limitations) might prevent these pixels to be occupied. The method is thus prone to overestimate niche overlap. To avoid this potential bias, Broennimann et al. (2012) developed a similar quantification and associated randomization tests in environmental space, quantifying niche overlap in a defined environmental space using smoothed occurrence densities. This approach is implemented in this module.

**IMPLEMENTATION**

The niche overlap quantification is based on the occurrence and background densities in the available environmental space estimated in Module: *Occurrence Density Grid*. The metric **overlap D** (Schoener 1968) is calculated with the function ecospat.niche.overlap from the R package ecospat and provides a global index of niche overlap ranging from 0 to 1. A value of 1 is given when the ratio of density of occurrence to density of available environmental conditions (i.e., density of background) is exactly the same for both species in all pixels of environmental space. D is thus highly dependent on the delimitation of the background extent. To avoid results with problematic interpretations, it is important that the study extent which defines the environmental space encompasses a region area that has been accessible to the species over evolutionary time and does not include areas beyond important dispersal boundaries, etc. (Barve et al. 2011).  

To be independent of the study extent, niche overlap indices can be based on binary characterizations of niches (i.e. pixels represent presence/absence of the species). Note that in this case, however, the information about occurrence density is lost (i.e., any part of the niche is considered to have the same “quality”). Binary overlap (**BinOv**, **BinOv<sub>1⊃2</sub>** and **BinOv<sub>2⊂1</sub>**) values are derived from the function ecospat.niche.dyn.index, originally developed to study niche changes in biological invasions (Guisan et al. 2014). **BinOv** measures the proportion of the overlap between the two species’ niches.  **BinOv<sub>2⊂1</sub>** measures the proportion of the niche of species 1 that overlaps with the niche of species 2. The plot on the left shows these indices in environmental space: the environmental conditions covered only by the niche of species 1 (blue), the environmental conditions covered only by the niche of species 2 (red), and the environmental conditions covered by both species, or the niche overlap (purple). The histogram on the right shows the results of a **niche similarity test**. This test is analogous to the test presented by Warren et al. 2008, except that it corresponds to environmental rather than geographical space. Both of the observed species niches are randomly shifted around the background extent and these simulated “null” niche overlaps are calculated (gray). If the observed overlap (red) is higher than 95% of the simulated overlaps (p-value < 0.05), we can consider the two species to be more similar than random.  

**REFERENCES**

Colwell, R.K., & Futuyma, D.J. (1971).  On the Measurement of Niche Breadth and Overlap. *Ecology*, 52(4), 567-576. <a href="https://doi.org/10.2307/1934144" target="_blank">https://doi.org/10.2307/1934144</a>

Guisan, A., & Zimmermann, N.E. (2000) Predictive habitat distribution models in ecology. *Ecological Modelling*, 135(2-3), 147-186. <a href="https://doi.org/10.1016/S0304-3800(00)00354-9" target="_blank">https://doi.org/10.1016/S0304-3800(00)00354-9</a> 

Warren, D.L., Glor, R.E., & Turelli, M. (2008). Environmental niche equivalency versus conservatism: quantitative approaches to niche evolution. *Evolution*, 62(11), 2868–2883. <a href="https://doi.org/10.1111/j.1558-5646.2008.00482.x" target="_blank">https://doi.org/10.1111/j.1558-5646.2008.00482.x</a> 

Broennimann, O., Fitzpatrick, M.C., Pearman, P.B., Petitpierre, B., Pellissier, L., Yoccoz, N.G., Thuiller, W., Fortin, M.J., Randin, C., Zimmermann, N.E., Graham, C.H., & Guisan, A. (2012). Measuring ecological niche overlap from occurrence and spatial environmental data. *Global Ecology and Biogeography*, 21(4), 481-497. <a href="https://doi.org/10.1111/j.1466-8238.2011.00698.x" target="_blank">https://doi.org/10.1111/j.1466-8238.2011.00698.x</a> 

Broennimann, O., Di Cola V., & Guisan, A. (2016). ecospat: Spatial Ecology Miscellaneous Methods. R package version 2.1.1. <a href="https://CRAN.R-project.org/package=ecospat" target="_blank">https://CRAN.R-project.org/package=ecospat</a>

Schoener, T.W. (1968). Anolis lizards of Bimini: resource partitioning in a complex fauna. *Ecology*, 49(4), 704-726. <a href="https://doi.org/10.2307/1935534" target="_blank">https://doi.org/10.2307/1935534</a> 

Barve, N., Barve, V., Jiménez-Valverde, A., Lira-Noriega, A., Maher, S.P., Peterson, A.T., Soberón, J., & Villalobos, F. (2011). The crucial role of the accessible area in ecological niche modeling and species distribution modeling. *Ecological Modelling*, 222(11), 1810–1819. <a href="https://doi.org/10.1016/j.ecolmodel.2011.02.011" target="_blank">https://doi.org/10.1016/j.ecolmodel.2011.02.011</a> 

Guisan, A., Petitpierre, B., Broennimann, O., Daehler, C., & Kueffer, C. (2014). Unifying niche shift studies: Insights from biological invasions. *Trends in Ecology & Evolution*, 29(5), 260–269. <a href="https://doi.org/10.1016/j.tree.2014.02.009" target="_blank">https://doi.org/10.1016/j.tree.2014.02.009</a>
