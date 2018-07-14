Replication files
================

Overview
--------

This repository provides replication files to construct regionalized and country-level educational selectivity measures.   

Content
-------

1: The file MASTER_repl_files.R can be run to generate all auxiliary files needed to construct selectivity profiles. Note that you need the underlying individual level data. Consult the file "data_sources.csv" for details and links to the individual level data.

2: The folder "scripts" contains the R-scripts for each country. Note that some countries have multiple scripts because data was available for multiple years.


Structure of replication files
------------------

<<<<<<< HEAD
Each file is structured in the same way: a header containing information on the country and the source of this specific individual level data. Note that education and age are categorized in such a way that they are compatible with the Barro-Lee dataset (http://www.barrolee.com/). In other words, age is categorized into 5-year intervals starting from age 15 up to age 64 and educational attainment is measured using four categories (less than secondary, lower secondary, upper secondary, more than secondary).

The first chunck of code generates the regionalized educational distributions. Note that we did not change the regional identifier in the original data so please consult the corresponding documentation of the respecitive individual level data for more information.

The second chunck of code generates the country-level educational distributions.

Optionally, there may be additional material at the end of each script detailing how to aggregate regions to less fine-grained categorizations used by some datasets.


Additional resources
----------------------



