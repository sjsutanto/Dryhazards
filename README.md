# Dryhazards
scripts used in the dryhazard paper

scripts:
Dry_hazards_Newmodel.pro: script to calculate the single hazard hotspots and compound hazard hotspots. The number and duration of cascading events are also calculated.
Dry_hazards_Newmodel_7days.pro: same script as Dry_hazards_Newmodel.pro but combine two events if there are 7 days without hazard in between into one event. 
New_Dryhazards_June19.pro: the latest script to calculate and plot compound and cascading dry hazards during the reviewing process. 
New_Lisflood_Soilmoisture_Ovima.pro: script to calculate soil moisture drought for dry hazard paper.
New-WB_Threshold_Soilmoisture.pro: script to calculate the drought threshold using the daily variable threshold method (VTM). The saved threshold is used for drought calculation (New_Lisflood_Soilmoisture_Ovima.pro). 
Sequent_Cascading_June19.pro: script to calculate the sequent of cascading event for dry hazards (the results are presented in table).
