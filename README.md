## Community (re)establishment of governance and management reduces hunting pressure

<br>

This repository contains code and data to reproduce the results (including figures) for the forthcoming paper, currently submitted for peer review: *Community (re)establishment of governance and management reduces hunting pressure*

Shiny participatory apps in English and French, and forthcoming apps and updates are at: [https://www.nadagabon.org](https://www.nadagabon.org.) 
To run the code, download the entire repository and run scripts within the R project. The four scripts are as follows:

* **mgmt_prep.R**: this explores and prepares the input data for fitting the models of offtake in relation to community hunting management.
* **mgmt_fit.R**: this fits, checks, and plots the above models. Note it creates a suite of .rds files in *./outputs/mgmt_fits* that total ~200 MB in size, and then reads these fits back in to check and plot them. It creates Figure 1 in  *./outputs/figs*
* **gov.R**: this explores, analyzes and plots hunters' perceived quality of their community governance . It creates Figure 3 and S1 in *./outputs/figs*
* **f4.tex**: this is the TeX code that creates Figure 4.

To restore the project library locally onto your machine before running scripts (in case of issues using different package versions), begin by running the following command in the console: `renv::restore()`

Note: Figure 2 is not reproduced here, as it is a anonymous map whose creation requires spatial data that is not anonymous.









