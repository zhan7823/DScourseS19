# ECON 5253: Data Science for Economists (Spring 2019) #

[![Join the chat at https://gitter.im/DScourseS19/community](https://badges.gitter.im/DScourseS19/community.svg)](https://gitter.im/DScourseS19/community?utm_source=badge&utm_medium=badge&utm_campaign=pr-badge&utm_content=badge)

|  | [Tyler Ransom](http://tyleransom.github.io) |
|--------------|--------------------------------------------------------------|
| Email | [ransom@ou.edu](mailto:ransom@ou.edu) |
| Office | 322 CCD1 |
| Office Hours | M 9:30-10:30am, Th 12-1pm |
| GitHub | [tyleransom](https://github.com/tyleransom) |

* **Meeting day/time**: T,Th 1:30-2:45pm, CCD1, Room 174
* Office hours also available by appointment
* This course takes inspiration and extensively borrows materials from similar courses taught by [Jason DeBacker](https://github.com/jdebacker/CompEcon_Fall17) (U of South Carolina) and [Rick Evans](https://github.com/rickecon/OGcourse_F17) (U of Chicago). Thanks to them for providing a framework for using GitHub as a class collaboration tool and for insights into teaching programming skills.

## Course description ##

Data science is a rapidly developing field that combines the recent Big Data revolution with ever-developing statistical algorithms to inform business and policy decisions. Nearly every company you've heard of uses data science to optimize its services: Netflix uses it to recommend new programs to its viewers, Amazon uses it to determine how much it should charge for its Prime services. This class will provide students with an overview of the data science workflow, from collecting raw data to drawing a set of insights from which a decision maker can make informed decisions. Along the way we will broadly cover a variety of advances in data collection, data storage, visualization, machine learning and econometrics topics, as well as teaching and reinforcing good programming practices. The primary goal of this course is to provide you, the student, with a set of skills that will allow you to compete for a data science job.

## Course Objectives and Learning Outcomes ##

By the end of the course, students should be able to do the following:

1. Explain the data science workflow from start to finish
2. Be able to collect data from online sources via APIs or scraping
3. Describe similarities and differences between econometrics and machine learning
4. Explain what data science is, and how Big Data differs from other types of data
5. Demonstrate good programming practices by writing code that can allow for easy collaboration with others
6. Understand the differences between prediction and causality, and the cases in which each is useful

In this course students, through lecture and application, will learn about:
* Good programming practices, including how to write code collaboratively with others
* Software to increase research productivity including:
    * LaTeX/Markdown
    * git
* Software to collect & clean data, and estimate statistical models:
    * R
    * Julia
    * Python
* Software to manage big data sets:
    * SQL
    * RDDs (Resilient Distributed Datasets) --- Spark, Hadoop
* How to access and utilize cluster computing resources
    * SSH (Secure Shell)
    * SFTP (Secure File Transfer Protocol)
    * SLURM (Simple Linux Utility for Resource Management)
* Methods to gather and handle data including:
    * Costs and benefits of different data structures
    * Using APIs
    * Web scraping
* Best practices for cleaning and visualizing data
* Computational methods to:
    * Optimize and find roots of functions
    * Perform Monte Carlo simulations
    * Run computations in parallel using multiple processors (time permitting)
* Basics for modeling different types of data
* Machine learning basics:
    * Supervised vs. unsupervised learning
    * The five "tribes" of machine learning: how they are interconnected, and how they differ
    * Machine learning vs. econometrics: prediction vs. causality
    * Evaluating model performance
* Using economic models to inform policy decisions
    * Computing structural models


## Grades ##

Grades will be based on the categories listed below with the corresponding weights.

Component                    |   Percent  |
-----------------------------|------------|
Class Participation          |    10%     |
Problem Sets                 |    35%     |
Exam & Quizzes               |    20%     |
Final project                |    35%     |
**Total points**             |  **100%**  |

Final grades will be assigned according to the standard cutoffs (90%+ for an A, 80%-89.99% for a B, etc.).

* **Participation:**
    * An important part of learning is face-to-face interaction. Thus, some of your grade will depend on attendance and active participation in class meetings.
* **Problem sets:** will be assigned approximately weekly throughout the semester.
    * You must write and submit your own computer code, although I encourage you to collaborate with your fellow students. I **DO NOT** want to see a bunch of copies of identical code. I **DO** want to see each of you learning how to code these problems so that you could do it on your own.
    * Problem set solutions, both written and code portions, will be turned in via a pull request from your private [GitHub.com](https://github.com/) repository which is a fork of the class master repository on my account. (You will need to set up a GitHub account if you do not already have one.)
    * Written solutions must be submitted as PDF documents or Jupyter Notebooks.
    * Problem sets will be due on the day listed in the Daily Course Schedule section of this syllabus (see below) unless otherwise specified. Late problem sets will not receive any credit. Partially completed problem sets will receive partial credit.

* **Exam & Quizzes:**
    * We may periodically have in-class quizzes as low-stakes ways to get feedback
    * There will be a written final exam, but no midterm

* **Final Project:**
    * Collect data on and analyze a research question of your choosing, using methods taught in this course
    * Write up a ~10 page (12pt font, double spaced, excluding References, Figures, and Tables) summary of your findings, including discussion about what prior studies of the same topic have found, as well as citations to prior studies
    * Turn in the written summary report and a GitHub repository containing all materials required to reproduce the results
    * Summary report should be written in LaTeX or RMarkdown and turned in as a PDF (source code for the summary report should also be included in your GitHub repository)
    * An example of what the final product should look like is [here](https://github.com/tyleransom/DScourseS19/blob/master/FinalProject/ExampleProject.pdf), with LaTeX source code [here](https://github.com/tyleransom/DScourseS19/blob/master/FinalProject/ExampleProject.tex) and BibTeX source code [here](https://github.com/tyleransom/DScourseS19/blob/master/FinalProject/References.bib).
    * A detailed rubric for the final project is [here](https://github.com/tyleransom/DScourseS19/blob/master/FinalProject/README.md)

## Communication ##

* I will always be available via email, and in person during office hours.
* Additionally, I have set up a Gitter community (see the badge at the top of this document) where I am hoping you can chat with each other about programming or other questions you have regarding the course. I will also be a participant in that community.


## Daily Course Schedule ##
(Will be continuously updated throughout the semester)

| Date   | Day | Topic                                                         | Due                                                                                                                            |
|--------|-----|-------------------------------------------------------------- |--------------------------------------------------------------------------------------------------------------------------------|
| Jan 15 | T   | What is data science / big data / why is it important? ([Slides](https://github.com/tyleransom/DScourseS19/blob/master/CourseIntro/WhatIsDataScience.pdf))        |                                                                                                                                |
| Jan 17 | Th  | Git, GitHub, computing environment, and Coding best practices ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/Productivity/README.md)) | Read Gentzkow & Shapiro's [handbook](https://web.stanford.edu/~gentzkow/research/CodeAndData.pdf); Ch. 1 of *The Master Algorithm*; register for GitHub account |
| Jan 22 | T   | Linux command line, SSH, accessing OSCER ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/Productivity/README.md))                      | [PS 1](https://github.com/tyleransom/DScourseS19/blob/master/Productivity/PS1.pdf)                                             |
| Jan 24 | Th  | Overview of Data Scientists' tools ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/DatabaseMgmt/README.md)) |                                                                                                                                |
| Jan 29 | T   | Using data: data types, storage ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/DatabaseMgmt/DataTypes.md)) | [PS 2](https://github.com/tyleransom/DScourseS19/blob/master/Productivity/PS2.pdf) |
| Jan 31 | Th  | Big Data: SQL ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/DatabaseMgmt/SQLoverview.md)) & RDDs ([link](https://spark.apache.org/docs/0.9.1/scala-programming-guide.html)); running jobs on the OSCER cluster |                                                                                                                                |
| Feb 5  | T   | Sampling & storing Big Data ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/DatabaseMgmt/HadoopSpark.md)) | [PS 3](https://github.com/tyleransom/DScourseS19/blob/master/DatabaseMgmt/PS3.pdf) |
| Feb 7  | Th  | Web scraping/APIs to gather data ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/WebData/README.md)) |  |
| Feb 12 | T   |  Web scraping/APIs to gather data ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/WebData/README.md)) | [PS 4](https://github.com/tyleransom/DScourseS19/blob/master/DatabaseMgmt/PS4.pdf) |
| Feb 14 | Th  | (Maybe) No class: career fair; otherwise Intro to Julia ([Julia notes](https://github.com/jmxpearson/duke-julia-ssri/blob/master/intro_slides.ipynb))                           |                                                                                                                                |
| Feb 19 | T   | "Snow" day (class canceled) |  |
| Feb 21 | Th  | Getting to know your data: descriptive statistics, cleaning, tips, tricks, transformations, visualization ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/Visualization/README.md); [HTML slides](https://github.com/tyleransom/DScourseS19/blob/master/Visualization/slides.html)) | [PS 5](https://github.com/tyleransom/DScourseS19/blob/master/WebData/PS5.pdf) |
| Feb 26 | T   | Modeling continuous and discrete variables ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/ModelingOptimization/README.md)) [HTML slides](https://github.com/tyleransom/DScourseS19/blob/master/ModelingOptimization/slides.html)); [Simple R script](https://github.com/tyleransom/DScourseS19/blob/master/ModelingOptimization/modelingBasics.R) |   |
| Feb 28 | Th  | Linear Algebra Introduction / Review ([Handout](https://minireference.com/static/tutorials/linear_algebra_in_4_pages.pdf)) |   |
| Mar 5  | T   | Introduction to optimization ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/ModelingOptimization/OptimizationIntro.pdf)) | [PS 6](https://github.com/tyleransom/DScourseS19/blob/master/Visualization/PS6.pdf) |
| Mar 7  | Th  | Writing and optimizing functions in R, Python, and Julia ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/ModelingOptimization/OptimizationCodingBasics.md))   |    |
| Mar 12 | T   | Writing and optimizing functions in R, Python, and Julia  ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/ModelingOptimization/OptimizationCodingBasics.md))  | [PS 7](https://github.com/tyleransom/DScourseS19/blob/master/ModelingOptimization/PS7.pdf) |
| Mar 14 | Th  | Debugging strategies and simulations ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/ModelingOptimization/SimulationNotes.md))                           |                                                                                                                                |
| Mar 19 | T   | No class (Spring break)                                       |                                                                                                                                |
| Mar 21 | Th  | No class (Spring break)                                       |                                                                                                                                |
| Mar 26 | T   | Intro to Machine Learning ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/MachineLearning/README.md#machine-learning)) | [PS 8](https://github.com/tyleransom/DScourseS19/blob/master/ModelingOptimization/PS8.pdf) |
| Mar 28 | Th  | Supervised ML: Regularization, measuring model fit, tuning with cross-validation, the elastic net model ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/MachineLearning/README.md#bias-variance-tradeoff)) |                                                                                                                                |
| Apr 2  | T   | Supervised ML: The 5 Tribes of Machine Learning ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/MachineLearning/README.md#the-5-tribes-of-machine-learning)) | [PS 9](https://github.com/tyleransom/DScourseS19/blob/master/MachineLearning/PS9.pdf) |
| Apr 4  | Th  | Unsupervised ML: Clustering ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/MachineLearning/UnsupervisedLearning.md)) |                                                                                                                                |
| Apr 9  | T   | Unsupervised ML: Dimensionality reduction and reinforcement learning ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/MachineLearning/UnsupervisedLearning.md)) | [PS 10](https://github.com/tyleransom/DScourseS19/blob/master/MachineLearning/PS10.pdf) 
| Apr 11 | Th  | Machine learning vs. econometrics ([Notes](https://github.com/tyleransom/DScourseS19/blob/master/Structural/README.md)) | |
| Apr 16 | T   | Structural modeling: static discrete choice ([Slides](https://github.com/tyleransom/DScourseS19/blob/master/Structural/discreteChoiceSlides.pdf)) | [PS 11](https://github.com/tyleransom/DScourseS19/blob/master/Structural/PS11.pdf) 
| Apr 18 | Th  | Structural modeling: dynamic discrete choice ([Slides](https://github.com/tyleransom/DScourseS19/blob/master/Structural/discreteChoiceSlides.pdf)) | || Apr 23 | T   | Structural modeling: dynamic discrete choice ([Slides](https://github.com/tyleransom/DScourseS19/blob/master/Structural/discreteChoiceSlides.pdf)) | |
| Apr 25 | Th  | Final Project presentations ([Rubric](https://github.com/tyleransom/DScourseS19/blob/master/FinalProject/README.md)) | |
| Apr 30 | T   | Final Project presentations  ([Rubric](https://github.com/tyleransom/DScourseS19/blob/master/FinalProject/README.md)) | |
| May 2  | Th  | Final Project presentations   ([Rubric](https://github.com/tyleransom/DScourseS19/blob/master/FinalProject/README.md)) | [PS 12](https://github.com/tyleransom/DScourseS19/blob/master/Structural/PS12.pdf) **(optional)**  |
| May 9  | Th  | Final Exam (in class, 1:30-3:30pm)                            | Final project due ([Scoresheet](https://github.com/tyleransom/DScourseS19/blob/master/FinalProject/Scoresheet.pdf)) |


## Helpful Links ##

* [QuantEcon](https://quantecon.org)
* [Notes on Machine Learning & Artificial Intelligence](https://chrisalbon.com) by Chris Albon
* [R data wrangling cheatsheet](https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf)
* [R tidyverse](https://www.tidyverse.org)
* [Julia vs. Python for Data Science](https://www.infoworld.com/article/3241107/python/julia-vs-python-julia-language-rises-for-data-science.html)
* [Machine Learning "Mind Map"](https://github.com/dformoso/machine-learning-mindmap/blob/master/Machine%20Learning.pdf)
* [JP Morgan massive overview of Big Data & Machine Learning](http://www.valuesimplex.com/articles/JPM.pdf)
* [Why it's becoming increasingly more difficult to learn to program](https://developers.slashdot.org/story/18/02/17/0947212/learning-to-program-is-getting-harder)

## Books ##

* The Master Algorithm ([Amazon link](https://www.amazon.com/Master-Algorithm-Ultimate-Learning-Machine-ebook/dp/B012271YB2))
* Julia for Data Science ([Amazon link](https://www.amazon.com/Julia-Data-Science-Zacharias-Voulgaris/dp/1634621301))
* R for Data Science ([Free PDF](http://r4ds.had.co.nz/))
* Data Science at the Command Line ([Free eBook](https://www.datascienceatthecommandline.com/))

## University Policies ## 

### Religious Observance ###

It is the policy of the University to excuse the absences of students that result from religious observances and to reschedule examinations and additional required classwork that may fall on religious holidays, without penalty.

### Reasonable Accommodation Policy ###

If a student requires an accommodation based on disability, the student should meet with me in my office during the first week of the semester. Student responsibility primarily rests with informing faculty at the beginning of the semester and in providing authorized documentation through designated administrative channels. The Disability Resource Center is located in the University Community Center at 730 College Avenue (405-325-3852).

### Academic Integrity: ###

I do not tolerate academic misconduct, [and neither does the University of Oklahoma](http://integrity.ou.edu/files/nine_things_you_should_know.pdf). I will not hesitate to fail students who do not fully comply with the University's academic misconduct policy. If you find yourself contemplating cheating, plagiarism, or other forms of academic misconduct, please come see me first. Help is available if you are struggling. I want everyone in the class to try their best and to do their own work. Please be advised that I reserve the right to utilize anti-plagiarism resources such as TurnItIn when grading assignments.

### Title IX Resources and Reporting Requirement ###

For any concerns regarding gender-based discrimination, sexual harassment, sexual assault, dating/domestic violence, or stalking, the University offers a variety of resources. To learn more or to report an incident, please contact the Sexual Misconduct Office at (405) 325-2215 (8 to 5, M-F) or [smo@ou.edu](mailto:smo@ou.edu). Incidents can also be reported confidentially to OU Advocates at (405) 615-0013 (phones are answered 24 hours a day, 7 days a week). Also, please be advised that a professor/GA/TA is required to report instances of sexual harassment, sexual assault, or discrimination to the Sexual Misconduct Office. Inquiries regarding non-discrimination policies may be directed to: Bobby J. Mason, University Equal Opportunity Officer and Title IX Coordinator at (405) 325-3546 or [bjm@ou.edu](mailto:bjm@ou.edu). For more information, visit http://www.ou.edu/eoo.html.

### Adjustments for Pregnancy/Childbirth Related Issues ###

Should you need modifications or adjustments to your course requirements because of documented pregnancy-related or childbirth-related issues, please contact your professor or the Disability Resource Center at (405) 325-3852 as soon as possible. Also, see http://www.ou.edu/eoo/faqs/pregnancy-faqs.html for answers to commonly asked questions.


### Reasonable Accommodations for Students with Disabilities ###

If a student requires an accommodation based on disability, the student should meet with me in my office during the first week of the semester. Student responsibility primarily rests with informing faculty at the beginning of the semester and in providing authorized documentation through designated administrative channels. The Disability Resource Center is located in Goddard Hall (405-325-3852).
