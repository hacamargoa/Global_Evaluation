Readme 

Global evaluation Program

Program to Estimate Corn and Wheat irrigated and rainfed harvested 0.5 x 0.5 gridded areas since 1960 to 2010 using SPAM and HYDE databases. Then masking LPJGUESS output based on thos areas and aggrgating by country. Global National and grid level statistics are performed to evaluate model outputs.

Option 1

-First Run the SpamAreasInput.R to enter and rasterizing the SPAM models for 2000, 2005 and 2010, 
-Then Wheat, Maize, IrrigMaize and IrrigWheatAreaEstim.R to estimate areas from 1960 to 2010. 
-Now AreaMasking.R to mask LPJ-GUESS outputs and aggregate by country 
-Then mergingFAO.R to join JPG aggragated data to FAO national statistics
-Ray data input.R 
-Regandcorr.R
-Reg&corrGR.R

Option 2
Open the .nc files for area estimation of SPAM  and LPJ areas and FAO outputs in csv and start on step 5 of the list above. 
