Ruby scrape
Paste STAR download files into the Data directory.
Adjust date tested format to yyyy-mm-dd

SR intervention levels- don't pull from STA because ID column exports as PS Charger Number instead of PS Student Number!

SQL 2 student exports
Run the two SQL queries and save the result as a csv titled 'students.csv' and 'students all.csv'.

R-->, Run "update star workbook", convert Students!G to date, fill down students formulas from top, 
paste values into the Tier Summary from file exported, manually set dates on headers

Run the two SQL queries and save the result as a csv titled 'students.csv' and 'students all.csv' into the Data directory.

### Running STAR files for data day

1. Run the file 'export enrollments.r' in masterdash and copy the resulting file ('enrollments data.csv') into the Data folder of this directory.

2. Run the file 'STAR report for Data day US2.r'. This will export both the student level model info and the teacher roll-ups as csvs.