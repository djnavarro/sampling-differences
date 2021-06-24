
# Data

This folder contains the cleaned data sets. It includes `.csv` file per 
experiment. These files are generated automatically, using the script in the
[preprocessing](./preprocessing) folder. They should not be edited by hand, 
nor should any of the analysis files alter these data files.

## Data dictionaries

TBA

## Style guide

To the extent that this is possible, the format of a data file should adhere 
to the idea of "tidy data". Each row should describe a single observation 
(i.e., each response from the same person should appear in its own row), and
each column should correspond to a single variable.

Variable names should use "snake_case", should not include white space, etc.
Generally speaking, the variable names should be the kind of thing produced by
`janitor::clean_names()` and should be easy to understand. 

Values stored should be informative (e.g., store gender as text "male", "female",
etc, rather than numeric dummy codes 1, 2, 3). If something about the data is 
not obvious from inspection, there should be explanatory "notes"
file, called `notes_urntask.md`. Unlike the CSV file, the notes file can 
(and probably should) be created manually.
