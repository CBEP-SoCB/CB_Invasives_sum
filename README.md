# CB_Invasives
Repository for data in prevalence of marine invasive species from MIMIC 
volunteer surveys and periodic Rapid Assessment Surveys.

<img
    src="https://www.cascobayestuary.org/wp-content/uploads/2014/04/logo_sm.jpg"
    style="position:absolute;top:10px;right:50px;" />

# Statement of Purpose
CBEP is committed to the ideal of open science.  Our State of the Bay data
archives ensure the science underlying the 2020/2021 State of Casco Bay report
is documented and reproducible by others. The purpose of these archives is to
release  data and data analysis code whenever possible to allow others to
review, critique, learn from, and build upon CBEP science.

# Archive Structure
CBEP 2020/2021 State of the Bay data analysis summaries contain a selection of 
data,  data analysis code, and visualization code as used to produce 
results shared via our most recent State of Casco Bay report. Usually, these
archives are organized into two or three folders, including the following:

- `Data`  folder.  Contains data in simplified or derived form as used in our
data  analysis.  Associated metadata is contained in related Markdown documents,
usually `DATA_SOURCES.md` and `DATA_NOTES.md`.

- Analysis.  Contains one or more R Notebooks proceeding through the principal
data analysis steps that underpin SoCB reporting. To simplify the archives,
much preliminary analysis, and many analysis "dead ends" have been omitted. 

- Graphics.  Contains R Notebooks stepping through development of graphics, and
also copies of resulting graphics, usually in \*.png and \*.pdf formats.  These
graphics may differ from graphics as they appear in final State of the Bay
graphical layouts. Again, most draft versions of graphics have been omitted for 
clarity.

# Summary of Data Sources
Data is derived from two complementary New England wide long-term marine 
invasive species monitoring programs coordinated by the Massachusetts Office of 
Coastal Zone Management.

MIMIC (Marine Invader Monitoring and Information Collaborative) is a regional 
citizen science program collecting data on presence and prevalence of selected
invasive marine species.  Scientists at the Wells Estuarine Research Reserve 
have been managing the MIMIC network in Maine for more than a decade.  The 
program works with volunteers at (currently) 38 monitoring sites across New
England, including several in Casco Bay. Data is collected monthly during the
summer months.  MIMIC volunteers are trained to observe and identify on the 
order of twenty invasive species that can be identified readily in the field 
without microscopes or other specialized equipment.

The "New England Rapid Assessment Survey" brings in taxonomic specialists to
conduct intensive marine community surveys at monitoring sites across New
England. Surveys are conducted every three to five years. Experts conduct a few
hours of observation and sample collection, followed by late nights in the
laboratory trying to identify everything growing at the sample location, from
snails and marine worms to fish and macroalgae (including invasives).

For more information on the MIMIC program in Maine, visit the website of the
[Wells Estuarine Research Reserve](https://www.wellsreserve.org/blog/mimic-marine-invader-monitoring-and-information-collaborative).
For more information on the RAS program, look to the 
[Massachusetts Office of Coastal Zone Management](https://www.mass.gov/service-details/rapid-assessment-surveys-of-marine-invasive-species)
