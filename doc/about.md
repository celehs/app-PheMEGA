
## PheMEGA: Phenotype Multi-ethnicity Genetic Architecture

### Overview

This tool utilizes summary results from a comprehensive genome-wide phenome-wide association study of over 635,000 Veterans enrolled in MVP. It provides a visualization of the association network between SNPs and multiple phenotypes, multiple related phenotypes and SNPs and allows users to compare results across the populations to highlight key results on heterogeneity. The current version includes visualizations of single trait GWAS findings, single SNP PheWAS findings, as well as comparisons of these findings across the populations highlighting SNP-trait pairs with significant differences between a minority population and European population. Users may search by SNPs or traits to select results of interest.


### Using the app

#### Steps:

1. Enter a SNP, Phenotype name, Phecode number, lab, survey question, or vital signs. <br>**Note:** Please use exact search terms and variables, for example: "Phecode:250" or "250"; "BRACA1"; "SMKAG".
2. Click to select rows in the search results.
3. Select the populations (select multiple to compare results across the populations).
4. Select the candidate nodes you would like to screen the results.
5. `Submit` your selection to the server and show the network.

Optional Steps:

1. Press the `Unselect` button to clear your selection.
2. To adjust the threshold p-value. Default 5 x 10<sup>-8</sup>

#### Main Page:

Click the tabs at the top of the main page to view the plots of the significant signals, shared signals and heterogeneous signals for your selection. You can also view the details in table format by going to the table tab.

**Significant signals**: Scatter plot. 

- X-axis: rsID(traits as input) / traits(SNP as input)
- Y-axis: -log10(P-value)
- Hover info: 
  - y value; 
  - x value: string(id); 
  - p value; 
  - beta/or: Beta Coefficient(quantitative	traits) / Odds Ratio(binary traits)
  - hetero p-value(vs EUR); 
  - Maf
- Legends: the populations with different colors.
- Settings (when the connected traits/SNPs are more than 500):
  - Maximum number of points to show: Default 500.


**Shared signals**: Shared Significant signals. 

**Heterogeneous signals**: bar plot.

- X-axis: rsID(traits as input) / traits(SNP as input)
- Y-axis: Beta Coefficient(quantitative	traits) / Odds Ratio(binary traits)
- Hover info: 
  - y value; 
  - x value: string(id); 
  - p value; 
  - beta/or; 
  - hetero p-value(vs EUR); 
  - Maf
- Legends: the populations with different colors.
- Settings:
  - Filter by FDR adjusted p-value of heterogeneity test: Default 0.05
  - Min MAF: Default 0
  - To compare with EUR: Default AFR.
  - Select chromosomes to show: Default all.

**Table**: 

- Settings:
  - Sort the table by: Variable; P value Adjusted P Heterogeneity (to EUR)
  - Decreasing: Default OFF.



### References

<hr>
