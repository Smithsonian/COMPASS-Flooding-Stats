2019 - 2025 water level data collected on OTT Compact Bubbler System (CBS) and cs475a2 Radar Gauge  instrumentation at the MarineGEO Upper Chesapeake Bay observatory (USA-MDA), Smithsonian Environmental Research Center, Edgewater, MD, USA. Location: 38.886928, -76.540172

Radar Guauge data collection did not begin until 2023 

The authors thank the National Estuarine Research Reserve System (NERRS) System-Wide Monitoring Program (SWMP) data group for their collaboration and support. The flag and code system used in this dataset is a modified version of NERRS SWMP quality assurance and quality control protocols with changes made to fit unique site conditions and data collection methods. Numeric flags are applied to the data algorithmically to note observations that are in/out of range or missing. Character codes are applied by MarineGEO technicians and add context for groups of observations - for example, noting data that looks suspicious or extreme weather events.

DESCRIPTION OF FILES INCLUDED IN THE DATA PUBLICATION

data_year_waterlevel_USA-MDA.csv: Data collected using CS475A instruments. Quality control codes are included, and column names are appended with "_code". For example, the column "barometric_pressure_mbar_avg" contains average barometric pressure measurements, and the column "barometric_pressure_mbar_avg_code" contains any codes associated with a given observation's barometric pressure (MBAR) average measurement. Missing values within a "_code" column represents that no code was applied for that observation. Missing "code" columns for a given parameter represents parameters that have not had any codes applied to the whole timeseries. 

dictionary_waterlevel_USA-MDA.csv: This file includes both the unit and description of all paramters, as well as the definition of any quality control codes used in the data file.

codes_year_waterlevel_USA-MDA.csv: this files contains the NERRS-defined codes that have been assigned to certain rows of this dataset durring QA/QC checks. 