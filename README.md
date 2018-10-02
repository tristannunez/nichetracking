# nichetracking

This github repository includes the folders and code used in the manuscript "Species traits mediate niche tracking in variable climates."

The folder "downloaded_data_in" contains a folder called "PRISM" and a csv file called holdridge_thresholds.csv. You will need to download the following .zip files from the PRISM website, and unzip them into the PRISM folder in "downloade_data_in":
PRISM_ppt_stable_4kmM2_189501_198012_bil
PRISM_tmean_stable_4kmM2_189501_198012_bil

The R code is located in the R_scripts folder. 

Start by running the code in the file _1_load_climate_data.R, and run each subsequent script until _15_numb_stability.R. You don't need to run _0_function_niche_tracking.R or _0_tools_libraries.R as they are sourced by the other scripts. 
