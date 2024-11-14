
Issues:
- AT=50 cannot be changed (introduced with dye-specific settings). Fixed in v0.7.

Future TODO:
- Add save/load project files (based on RDS formats)


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
 