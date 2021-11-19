
# Data Sources
The file "MIMIC_MASTER_DATA_Wells_0225210.xls" received from Jeremy Miller of 
Wells Reserve in an e-mail to Curtis C. Bohlen, dated February 25, 2021.

The file "Maine_2020_MIMIC Sites_WellsNERR.kmz" contains geographic data 
showing locations of monitoring locations inMaine, including the Casco Bay
monitoring Sites.

We extracted the Placemarks from the KML file in Python, and produced a
CSV file, "Maine_2020_MIMIC Sites_WellsNERR.csv". It contains the same 
geospatial data as teh KMZ, but in a more convenient tabular form.



# Notes
1.    Because this
      is a volunteer based effort, records are sometimes incomplete.
      With "PERFECT" attendance (or monitoring) each site would produce 
      6 monitoring events or site visits per year. Sometimes May and 
      even June can be pretty "bare" (few species are present and easy to
      spot) so if only one or two species were present, it makes for
      "scant" data.  "Not present" is also real data, but the way the data
      organized and presented, MIMIC only recordds presence, not absence.

      Data completeness among the island sitees is highest for Peaks island, 
      with Chebeague coming in a close second.  Long Island's data is less complete.

      The mainland sites (SMCC Dock, Spring Point, and Siegel's
      reef) all have pretty good records, with SMCC dock going back to
      2009.  Spring. Point marina was a recent addition, with well 
      trained observers.

2.    Abundance is handled by grouping into "Abundant, Common, Few, and Rare". 
      The attached explains what each category "means".

3.    Some islands  have multiple sites AND site types, so be careful in 
      analysis not to confound them.

