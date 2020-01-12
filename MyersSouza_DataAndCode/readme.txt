Social Comparison Nudges Without Monetary Incentives: 
       Evidence from Home Energy Reports
	   
Journal of Environmental Economics and Management, January 2020

Erica Myers and Mateus Souza

University of Illinois at Urbana-Champaign
Department of Agricultural and Consumer Economics
_______________________________________________________________________

This folder contains replication code and data for:
"Social Comparison Nudges Without Monetary Incentives: Evidence from Home Energy Reports"

Here we provide some basic descriptions of the code and data. Further details can be found in the published paper.
Please submit any inquiries to the authors via email.

_____________________________
Code:
main.do > code used for most analyses in the paper, including appendices
Power_Calcs_Cluster.do > code used for power calculations performed prior to RCTs described in the paper
thermostat_energy.do > code used to establish a relationship between thermostat settings and energy usage (steam and chilled water) for the specific context of the paper

Collectively, those codes produce all results (plus additional analyses) presented in the paper.
Tables and figures should be saved in in the subfolder "Results"

_____________________________
Data:
data_anonym.dta > data used for main analyses in the paper
- data sources are described in the paper

powercalc_data.dta > data used for power calculations and for establishing relationsship between thermostat settings and energy usage

pre_treat_survey.dta > pre-treatment survey responses
post_treat_survey.dta > post-treatment survey responses

room_randomization.dta > auxiliary file containing randomization assignment from main Fall trial

power_graphs.dta > auxiliary file used to generate graphs to illustrate results from power calculations
- obtained after some basic post-processing of the Excel sheet produced by Power_Calcs_Cluster.do





