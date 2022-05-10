### **Module:**

**BACKGROUND**

Remove spatial outliers from species occurrence records.

**IMPLEMENTATION**

This module relies on functions in `occOutliers`, which identifies outlying species records using statistical tests. 

The user can choose from four statistical tests to detect spatial anomalies in the species occurrence data: the Grubbs, Dixon, and Rosner, and interquartile range (IQR) tests. The **Grubbs** test detects whether the most extreme value is a significant outlier from the rest of the sample. This may be a good choice if you are unsure whether your data contains an outlier. Note this test assumes your data is normally distributed. The **Dixon** test measures the difference between an outlier and its nearest neighbor to the range of the sample. This test is designed to overcome the masking effect that multiple potential outliers can cause, so may be used if you know your data includes outliers. This test is most useful for small sample sizes (usually n < 25). The **Rosner** test is designed to detect 2-10 outliers in a larger sample (n > 25). The test assumes the data are normally distributed after the outliers are removed. This test requires that the suspected number of anomalies is identified prior. Finally, the **IQR** is the range between the first and third quartiles (Q1 and Q3). In this test, the data points which fall below Q1 - 1.5 IQR or above Q3 + 1.5 IQR are defined as outliers.


**REFERENCES**

Cory Merow (2022). occOutliers: Identify Outlying Species Occurrence Records. R package
version 0.1.0.

Kasunic, M., McCurley, J., Goldenson, D., & Zubrow, D. (2011). An Investigation of Techniques for Detecting Data Anomalies in Earned Value Management Data: Defense Technical Information Center. https://doi.org/10.21236/ADA591417

Lukasz Komsta (2011). outliers: Tests for outliers. R package version 0.14.
https://CRAN.R-project.org/package=outliers

Millard SP (2013). _EnvStats: An R Package for Environmental Statistics_. Springer, New York.
ISBN 978-1-4614-8455-4, <URL: https://www.springer.com>.
