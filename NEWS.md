

Future TODO:
- Add save/load project files (based on RDS formats)

v0.9.0 (Release date: 2024-08-05)
=============================================
 - Pre-step LR threshold (LR=1) is now turned off by default 
	- Option was added to settings.
	- Modified LRthresh argument in calculateExhaustive.
 - The exhaustive calculation can now adjust for number of sub-hypotheses 
	- Option was added to settings (adjustNumSubHyps
 	- Added adjustNumSubHyps argument in calculateExhaustive.

 - Included whether used or not in "Download Settings" file
 
 - The user must now choose between LRex and GLR
 - Added option to calculate Shaving Method as an output
 
 - Fixed the print out of "Peeling out reference" issue in case of more than one

 - dataTableOutput replaced with DT::DTOutput
 
v0.8.1 (Release date: 2024-11-04)
=============================================
 - Added the Generalized LR (GLR) in addition to the exhaustive and conventional:
	- Based on the maximum of any of the likelihoods of each corresponding hypothesis subsets.
	- The table returned was extended to also include GLR.

v0.8.0 (Release date: 2024-05-15)
=============================================
 - calculateExhaustive was updated to restrict which references that are considered further:
	- The argument LRthresh was introduced (default restriction is LR>1 which the GUI is forced to use)


v0.7.0 (Release date: 2023-10-08)
=============================================
 - Fixed issue with dye-specific settings not changing.
 