##Data Pipeline for CPS Project##

This folder includes scripts for querying, merging, and saving the data we used for most of our analyses.

###Student Data###

`create_students_table.R` gathers data for student-level modelling. First, it runs the query `student_table_with_ninth_snapshot_vw.sql` to get the students who attended CPS in either 8th or 9th grades in the last few years. There is one record per student, per school year, and it combines which school the students went to in 8th and/or 9th grade. 

Then it runs `student_ISAT_scores_vw.sql` and `student_attendance_vw.sql` to get the students' ISAT scores and historical attendance, respectively. This is done separately, to increase speed when combined with the original query. These data sets are all merged together. Finally, the data is saved one year at a time.

Finally, we run the script `geocode_addresses.R` to get the latitude and longitude for each student's home address.

###School Data###

`create_schools_table.R` pulls all their features from the database. After this, it geocodes the schools, gets their census block information, and saves the results as `schools_static_features.csv`.

This script also runs the query in `school_aggregate_features_vw.sql`, which pulls yearly summaries of the students who attend each of the schools. It captures the percentage of students receiving free or reduced priced lunches, the percentage of ESL students, the percentage of male students, the percentage of homeless students, the average student's GPA, and the students' attendance rate.

The results are saved in `schools_aggregate_features_YEAR.csv`, with YEAR replaced by the appropriate year.

The code in `create_features_table.R` creates the dataframe of school features that change every year. This file combines information from the school report cards downloaded from the [Chicago data portal](https://data.cityofchicago.org/), mobility and race data from [CPS's website](http://cps.edu/SchoolData/Pages/SchoolData.aspx), and [AUSL data](http://auslchicago.org/). Since the inputs are school progress reports, race files and ausl files, they are all excel sheets. Part of the work is done by hand and part of the work is automated. In the future, all files will change but will be publicly available data.

###Note###

All the SQL queries pull data from views created for us by CPS. This is done to keep CPS's data stucture private.


