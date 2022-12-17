# Dryhazards
This repository contains the scripts to reproduce the content of the dry hazard paper: 
Sutanto, S. J., Vitolo, C., Di Napoli, C., D’Andrea, M., and Van Lanen, H. A. J.: Heatwaves, drought, and wildfires: exploring compound and cascading events of dry hazards at the pan-European scale, Environment International, 134, https://doi.org/10.1016/j.envint.2019.105276, 2019

Datasets: 
Sutanto, Samuel, Vitolo, Claudia, & Di Napoli, Claudia. (2022). Data used in the paper: Heatwaves, droughts, and fires: Exploring compound and cascading dry T hazards at the pan-European scale [Data set]. Zenodo. https://doi.org/10.5281/zenodo.7451196

1. Dry_hazards_Newmodel.pro: script to calculate the single hazard hotspots and compound hazard hotspots. The number and duration of cascading events are also calculated. The datasets are available.
2. Dry_hazards_Newmodel_7days.pro: same script as Dry_hazards_Newmodel.pro but combine two events if there are 7 days without hazard in between into one event. The datasets are available.
3. New_Dryhazards_June19.pro: the latest script to calculate and plot compound and cascading dry hazards during the reviewing process. The datasets are available.
4. New_Lisflood_Soilmoisture_Ovima.pro: original script to calculate soil moisture drought for dry hazard paper.
5. New-WB_Threshold_Soilmoisture.pro: original script to calculate the drought threshold using the daily variable threshold method (VTM). The saved threshold is used for drought calculation (New_Lisflood_Soilmoisture_Ovima.pro). 
6. Sequent_Cascading_June19.pro: script to calculate the sequent of cascading event for dry hazards (the results are presented in table). The datasets are available.
7. paper_dry_hazards.R: script in R studio to identify the dry hazard hotspots
