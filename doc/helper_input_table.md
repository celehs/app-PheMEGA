Gene for SNP:

1. In case where an rsid has multiple genes mapped, we concat them together. (Eg: PABPC4-AS1|PABPC4)

2. When a gene mapping is absent, we choose the upstream or downstream gene based on which one is closest. 
  - If the closest one is upstream we add + in front of the gene name (Eg: +IFNGR1)
  - If the closer one is downstream we add - in front it (Eg: -IFNGR1)

