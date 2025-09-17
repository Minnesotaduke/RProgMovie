---
editor_options: 
  markdown: 
    wrap: 72
---

**Overview**

------------------------------------------------------------------------

This Project is a Movie/Film Box Office Analysis and Predictor tool
given movie metadata such as Budget, run-time, notable actors, genre,
MPA ratings, and more. Using R to produce replicatable and scalable
Linear and random forest modest for total worldwide revenue of a film
predicting.

-   Note Proejct is done entirely in R, Github shows "ROFF" due to
    issues with handling x.x Version names for RDF files.

**Skills Demonstrated**

------------------------------------------------------------------------

This Portfolio project demonstrates the following skills

-   Data Science: Data Analysis thorugh GGPlot graphs upwards of 20
    total graphs analyzed. Cleaned Data. Feature engineered Data.

-    Machine Learning Engineering: Constructed Predictive Models,
    Training and test data splitting, RMSE and metric evaluation and
    analysis

-   Software Engineering: Clean Documented and Reproducible code,
    created UI user friendly movie input for anaylsis

**How it Works**

------------------------------------------------------------------------

Data Collection

Initial Dataset containing metadata such as TMDB ID, Genre, all box
office data such as revenue and budget, run-time, and release date
information were sourced from
<https://www.kaggle.com/datasets/asaniczka/tmdb-movies-dataset-2023-930k-movies/data>

Second Dataset that was merged with the former which had meta data such
as IMDB Meta Score, Director, Notable actors, and MPA rating was sourced
from
<https://www.kaggle.com/datasets/ggtejas/tmdb-imdb-merged-movies-dataset>
and
<https://www.kaggle.com/datasets/ashutoshdevpura/imdb-top-10000-movies-updated-august-2023>

Inflation data used to created inflation multiplier and all Adjusted
columns was sourced at: https://data.bls.gov/timeseries/CUUR0000SA0

Feature engineering

-   Text Preprocessing to include: Standardizatiom, punctual removal,
    space removal, and major word extraction (present in genre) for ease
    of use.

-   Log base 10 many numerical values for ease of modeling.

-   Adjusted all numerical and monetary values for rudimentary
    comparison between years of film.

-   Profit status created with ROI Base.

-   Est total cost added with scaling equations to best estimate total
    advertising and other total cost per film.

-   Studio Revenue calculated with scaling formulas and equations to
    best estimate total take home for a film.

-   ROI created as the difference between studio take home and total
    cost to have an easy to identify movie success matrix.

-   Star Director Boolean, Star Count, and Famous Production Boolean all
    source from an extensive list of notable actors, directors, and
    production companies to see if a film has a famous team before it.

**Predictive Model**

1.  User Inputs Requested Information about their movie such as budget,
    run time, and genre.

2.  System inputs all user movie data into a temporary data frame.

3.  The Predictive Random Forest Models compares the movie to over 6900
    total analyzed movies and outputs a predicted total worldwide
    Revenue in both Exponential and Log base.
