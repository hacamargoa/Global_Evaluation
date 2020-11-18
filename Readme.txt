Readme 

Global evaluation Program

Program to Estimate Corn and Wheat irrigated and rainfed harvested 0.5 x 0.5 gridded areas since 1960 to 2010 using SPAM and HYDE databases. Then masking LPJGUESS output based on thos areas and aggregating by country. Global National and grid level statistics are performed to evaluate model outputs.

Option 1
Calculating Areas
-First Run the SpamAreasInput.R to enter and rasterizing the SPAM models for 2000, 2005 and 2010, 
-Then CropsAreaEstim.R and CropsAreaEstimIrr.R to estimate areas from 1960 to 2010.
-Finally Areas are forced to match to FAO by country statistics using Area_correction.R

Option 2
-Calling .nc areas at the beggining of AreaMasking.R


-Continue to AreaMasking.R to mask LPJ-GUESS outputs and aggregate globally and by country
-Run BC1_1graph.R 
-Regandcorr.R
-Reg&corrGR.R
-Spinup
-Constant-inputs.R