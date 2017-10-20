# traffic_safety_camera_assessment
Code needed to reproduce traffic safety camera analysis

Here are the basic instructions needed to use this code:

1. Begin running the first R script, `0_clean_crashes.R`.
2. When you reach lines 79 and 80, you will write to disk a table of all crashes missing geographic coordinates. Then you will need to use the ESRI World Geocoder or a similar tool to geocode those entries. The remainder of `0_clean_crashes.R` reads the geocoded table back into R and appends these entries to those that already have suitable geographic coordinates.
3. Run `1_clean_cameras.R`, which removes duplicates from the contractor's camera data (i.e. entries with identical geographic coordinates).
3. Run `2_geoprocessing.R`, which actually calls the Python script stored in the `python` directory. This Python file uses ArcGIS geoprocessing tools to join the crashes and cameras to their appropriate road segments.
4. Run `3_aggregate_crashes.R`, which summarizes the crashes by road segment and time period.
5. Run `4_inference_euclidean.R`, which performs the analysis.
