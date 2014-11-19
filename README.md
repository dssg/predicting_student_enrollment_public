##Predicting School-Level Enrollment for Chicago Public Schools##
<img src="http://dssg.io/img/partners/cps.png" width="300" align="right">

Statistical models and analysis of student enrollment to better allocate school budgets in Chicago. 
This project is a 2014 Data Science for Social Good project in partnership with Chicago Public Schools.

For a more comprehensive overview, please consult our [blog post](http://dssg.io/2014/07/23/cps-enrollment-prediction.html).

###Project Description###
Lack of accurate enrollment forecasting (at the individual school level) causes millions of dollars to be re-allocated every year causing administration and teaching disruption.

In the Chicago Public Schools system, each school's budget depends on how many students enroll there. Unfortunately, it can be difficult to predict how many students will enroll in a given year, and inaccurate predictions can result in last-minute money shuffles, staff changes, and wasted educational time. Because budget allocations are made in the spring, long before students start in September, early and accurate predictions of student enrollment are crucial.

###The Solution###
To answer these questions, we approached the problem in the following ways:
  * Extensive Exploratory Data Analysis
  * Linear Regression (School Level)
  * Conditional Logit Model (Student Level)
  * Decision Tree (Predict Misprojection)

###Project Layout###
  * Data_Pipeline: process raw data into structured data frame
  * Data_Analysis: data visualization and regression analysis
  * Tool_Box: main tool box for CPS usage to make prediction and diagnose prediction for the future

###Installation###

#### Git
To download the code we used for the project, you will need to clone the repository using Git. [Git](http://git-scm.com/) is used version control system we used to orginize our code. We hosted the code on [Github](http://github.com/). You can download Git [here](http://git-scm.com/downloads). Information on getting started with Git is [here](http://git-scm.com/book/en/Getting-Started-Git-Basics). Additionally, you will need to create a Github account.

Once you have installed Git, you will need to navigate in command line to the folder in which you want to download the code. Then you will need to clone the respository with the following commands. 

```
git clone https://github.com/dssg/predicting_student_enrollment.git
cd predicting_student_enrollment/
```

#### R

We did most of our analyses in R. R is available for free via the [R Project website](http://www.r-project.org) or, with a slightly friendlier user interface, as [RStudio](http://www.rstudio.com). See [these resources](https://github.com/dssg/nfp#using-r) for a more detailed overview of R.

Much of R's functionality comes not from the base installation but from additional packages.  Details about how to install packages are available [here](http://www.r-bloggers.com/installing-r-packages).  Our code makes use of the following packages:

* ggplot2
* plyr
* survival
* clogitL1
* randomForest

### Team
![CPS Team](http://dssg.io/img/posts/cps-team.png)

  * [Vanessa Ko](https://github.com/vanessako), Political Science, McGill University
  * [Andrew Landgraf](https://github.com/andland), Statistics, The Ohio State University
  * [Tracy Schifeling](https://github.com/tracyschifeling), Statistics, Duke University
  * [Zhou Ye](https://github.com/ZhouYeJava), Computer Science, Johns Hopkins University
  * [Joe Walsh](https://github.com/jtwalsh0), Mentor

### License
Copyright (C) 2014 Data Science for Social Good Fellowship at the University of Chicago

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS," WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
