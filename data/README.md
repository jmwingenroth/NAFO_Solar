

# uspvdb_v1_0_20231108.geojson
## [1.0.0] - 2023-11-08
- First public release of the USPVDB.

### Added
- Dedicated home, data, partner, and help landing pages.
- Added end-user web application capabilities including:
	- Dynamic filtering and solar facility statistic computation.
	- Data-driven styling.
	- Dynamic project information table.
	- Enhanced project search (with auto-complete).
	- Temporal animation controls.
	- Enhanced navigation (pitch and bearing controls).
	- Enhanced geocoder (with auto-complete).
	- Enhanced geolocation controls.
	- Extended solar facility pop-up information.

# Renewable Energy All Sectors Net Summer Capacity Solar.csv
- https://www.eia.gov/outlooks/aeo/data/browser/#/?id=16-AEO2023&region=0-0&cases=ref2023~highmaclowZTC~lowmachighZTC&start=2021&end=2050&f=A&linechart=~~~ref2023-d020623a.53-16-AEO2023~highmaclowZTC-d020623a.53-16-AEO2023~lowmachighZTC-d020623a.53-16-AEO2023&ctype=linechart&sourcekey=0
- 14:53:04 GMT-0400 (Eastern Daylight Time)
- Data source: U.S. Energy Information Administration

# Solar_Future_Study_DOE_NREL.csv
- Solar Futures Study by DOE & NREL (2021)
- https://www.energy.gov/eere/solar/solar-futures-study

# SEIA data

- Data were copied manually from state-level projections on the SEIA website (e.g., [North Carolina Solar](https://www.seia.org/state-solar-policy/north-carolina-solar))
- Scaling from total capacity to utility-scale capacity was challenging because no numeric data were provided
	- States were grouped into three categories based on visual analysis of column charts
		- Almost no utility scale solar (0.1)
		- Some utility scale solar (0.5)
		- Almost all utility scale solar (0.9)
	- Projections for total solar in 2024 and 2029 were then multiplied by the corresponding scaling factor to estimate utility-scale solar
	- This method closely matched the combination of utility-scale and community solar (which were treated interchangably) from the [SEIA website](https://www.seia.org/solar-industry-research-data)
