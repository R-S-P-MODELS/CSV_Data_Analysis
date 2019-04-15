# CSV_Data_Analysis

This is a shinyApp for data exploration

The user may give a data in tabular form, it can be CSV but the program may accept another formats that can be read by the data.table package

After pressing the button complete dataset the user may draw graphs of the file variables, explore their correlations and their dependencies

A model to find the best curve to fit your numerical data is also provided where the program tells the user what is the set of equations that best models the data in ordered format by different statitical metrics

A PCA vizualization is provided as well, along with a way to explore your missing data.

to run this code the user should use

shiny::runGitHub(repo="CSV_DATA_Analysis",username="R-S-P-MODELS")
