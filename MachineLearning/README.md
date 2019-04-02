# Machine Learning

The next unit of this class will cover machine learning. But first, let's discuss the difference between Machine Learning and Artificial Intelligence (AI).

## What is machine learning? What is AI?

What is machine learning?

* **ML:** Allowing computers to learn for themselves without being explicitly programmed
    * **USPS:** Computers read handwritten addresses and sort mail accordingly
    * **Google:** AlphaGo, AlphaZero (computers that are world-class chess, go players)
    * **Apple/Amazon/Microsoft:** Siri, Alexa, Cortana voice assistants can understand human speech
    * **Facebook:** automatically finds faces in a photo and tags them to the corresponding user
* In each of the above examples, the machine is "learning" to do something only humans had previously been able to do
* Put differently, the machine was not programmed to read numbers or recognize voices -- it was given a bunch of examples of numbers and human voices and came up with a way to predict what's a number or a voice and what isn't

What is AI?

* **AI:** Constructing machines (robots, computers) to think and act like human beings

Thus, machine learning is a (large) subset of AI

## Machine learning's role in data science

* In data science, machine learning closely resembles statistics.
* Why? Because a lot of data science is about finding "insights" or "policies" in your data that can be used to increase profits or improve quality of life in some other dimension.
* Finding "insights" is about finding correlative and causal relationships among variables
* Statistics is the science that has the vast majority of these tools

## Machine learning vs. econometrics

* **Machine Learning** is all about maximizing and out-of-sample prediction
* **Econometrics** is all about finding "consistent" parameter estimates (e.g. making sure that the estimated parameters of the model will converge to their "true" values given a large enough sample size)
* **Machine Learning** is all about finding y&#770;
* **Econometrics** is all about finding &beta;&#770;

## The fundamental objective of Machine Learning
The fundamental objective is to maximize out-of-sample fit

* But how is this possible given that -- by definition -- we don't see what's not in our sample?
* The solution is to choose functions that predict well in-sample, but penalize them from being too complex.
* **Regularization** is the tool by which in-sample fit is penalized. That is, regularization prevents overly complex functions from being chosen by the algorithm
* **Overfitting** is when we put too much emphasis on in-sample fit, leading us to make poor out-of-sample fit

## Elements of Machine Learning
The following are the basic elements of machine learning in data science:

1. Loss function (this is how one measures how well a particular algorithm predicts in- or out-of-sample)
2. Algorithm (a way to generate prediction rules in-sample that can generate to out-of-sample)
3. Training data (the sample on which the algorithm estimates)
4. Validation data (the sample on which algorithm tuning occurs)
5. Test data (the "out-of-sample" data which is used to measure predictive power on unseen cases)

The algorithm typically comes with **tuning parameters** which are ways to regularize the in-sample fit

**Cross-validation** is how tuning parameters are tuned.

## Example
* Suppose you want to predict housing prices (this is the "hello world" of machine learning)
* You have a large number of relevant variables
* What would you do?
    * You would want to have a model that can detect non-linear relationships (like the USPS handwriting reader)
    * You would also want to have a model that you can tractably estimate
    * And a model that will predict well out-of-sample

### One option: a separate dummy variable for every house
* In this scneario, you run `lm(log(price) ~ as.factor(houseID))`
* What you get is a separate price prediction for every single house
* But what to do when given a new house that's not in the sample?
    * Which house in the sample is the one you should use for prediction?
* The resulting prediction will have horrible out-of-sample fit, even though it has perfect in-sample fit
* This is a classic case of **overfitting**
* We say that this prediciton has **high variance** (i.e. the algorithm thinks random noise is something that is important to the model)

### Another option: house price is a linear function of square footage
* In this scenario, you simple run `lm(log(price) ~ sqft)`
* When given a new hosue with a given square footage, you will only look at the square footage to predict the price of the new house
* This algorithm will result in **underfitting** because the functional form and features it uses for prediction is too simplistic
* We say that this prediction has **high bias** (i.e. the algorithm does not think enough variation is important to the model)

## Bias-variance tradeoff
The **bias-variance tradeoff** refers to the fact that we need to find a model that is complex enough to generalize to new datasets, but is simple enough that it doesn't "hallucinate" random noise as being important

The way to optimally trade off bias and variance is via **regularization**

The following graphic from p. 194 of Hastie, Tsibshirani, and Friedman's *Elements of Statistical Learning* illustrates this tradeoff:

![Source: p. 194 of *Elements of Statistical Learning*](../Graphics/biasVarianceHTFp194.png "Bias-Variance Tradeoff")

## Regularization methods

1. Regression models (OLS or logistic)
    * L0 (Subset selection): penalize objective function by sum of the parameters &ne;0
    * L1 (LASSO): penalize objective function by sum of absolute value of parameters
    * L2 (Ridge): penalize objective function by sum of square of parameters 
    * Elastic net: penalize by a weighted sum of L1 and L2
2. Tree models
    * Depth, number of leaves, minimal leaf size, other criteria
3. Random forests
    * Number of trees, complexity of each tree, other criteria
4. Nearest neighbors
    * Number of neighbors
5. Neural networks
    * Number of layers, number of neurons per layer, connectivity between neurons (i.e. via L1/L2 regularization)
6. Support Vector Machine
    * L1/L2 regularization
7. Naive bayes
    * Naturally geared towards not overfitting, but can be regularized with iterative variable selection algorithms (similar to stepwise/stagewise regression)

## Visualization of different predictors
The following graphic shows a visualization of different classification algorithms, accross two features (call them X and Y). Note the stark differences in the prediction regions.

![Source: Sebastian Raschka on twitter](../Graphics/DYTAagSVAAACVc7.jpg "Comparison of different machine learning classification algorithms")

Source: [this tweet](https://twitter.com/rasbt/status/974115063308091392?s=12)

## Combined predictors
Often you can get a better prediction by combining multiple sets of prediction. We call these **combined predictors**. 

You can think of the combined predictor as a "committee of experts." Each "expert" votes on the outcome and votes are aggregated in such a way that the resulting prediction ends up being better than any individual component prediction.

3 types of combined predictors (cf. Mullainathan & Spiess, 2017):

1. Bagging (unweighted average of predictors from bootstrap draws)
2. Boosting (linear combination of predictions of a residual)
3. Ensemble (weighted combination of different predictors)

Combined predictors are regularized in the number of draws, number of iterations, or structure of the weights.

## Visualization of combined predictors
The following graphic shows a similar visualization as above, but now incorporates an ensemble prediction region. This provides some solid intuition for why ensemble predictors usually perform better than the predictions from any one algorithm.

![Source: Sebastian Raschka on twitter](../Graphics/ensemble_decision_regions_2d.png "Comparison of different machine learning classification algorithms and an ensemble prediction")

Source: [The `mlxtend` GitHub repository](https://github.com/rasbt/mlxtend)

## Measuring prediction accuracy
How do we measure prediction accuracy? It depends on if our target variable is continuous or categorical

* **If continuous:** Use *Mean Squared Error (MSE)* which is equal to the sum of the squared differences between y and y&#770; divided by the sample size. Sometimes the *Root Mean Squared Error (RMSE)* is used, which is the square root of this quantity. You may also see the *Mean Absolute Error (MAE)* which is the sum of the absolute value of the differences between y and y&#770; divided by the sample size.
* **If binary:** Use the *confusion matrix* which compares how often y and y&#770; agree with each other (i.e. for what fraction of cases y&#770; = 0 when y = 0)

Example confusion matrix

|    | y&#770; |  |
|--------|-----|-------|
| y | 0  | 1 |
| 0 | True negative   | False positive |
| 1 | False negative  |  True positive |

## Using the confusion matrix
The three most commonly used quantities that are computed from the confusion matrix are:

1. **sensitivity (aka recall):** what fraction of y = 1 have y&#770; = 1 ? (What is the true positive rate?)
2. **specificity:** what fraction of y = 0 have y&#770; = 0 ? (What is the true negative rate?)
3. **precision:** what fraction of y&#770; = 1 have y = 1 ? (What is the rate at which positive predictions are true?)

The goal is to trade off Type I and Type II errors in classification. The **F1 score** is the most common way to quantify this tradeoff.

* F1 score = 2/(1/recall  +  1/precision); a number in [0,1] with 1 being best
* It is the harmonic mean of recall and precision

There are a bunch of other quantities that one could compute from the confusion matrix, but we won't cover any of those.

## Why use the confusion matrix?
When assessing the predictive accuracy of a classification problem, we want to make sure that we can't "game" the accuracy measure by always predicting "negative" (or always predicting "positive"). This is critical for cases like classifying emails as "spam" or "ham" because of the relative paucity of "spam" messages.

In other words, if spam messages are only 1% of all messages seen, we don't want to be able to always predict "ham" and have that be a better measure of accuracy than actually trying to pin down the 1% of spam messages.

The F1 measure attempts to quantify the tradeoff between Type I and Type II errors (false negatives and false positives) that would be rampant if we were to always predict "ham" in the email example.

## Cross validation
How do we decide what level of complexity our algorithm should be, especially when we can't see out-of-sample?

The answer is we choose values of the **tuning parameters** that maximize out-of-sample prediction

for example:

* the &lambda; that comes in front of L1, L2, and elastic net regularization
* the maximum depth of the tree or the min. number of observations within each leaf
* etc.

## Splitting the sample
To peform cross-validation, one needs to split the sample. There are differing opinions on this:

Camp A ("Holdout")

1. Training data (~70%)
2. Test ("holdout") data (~30%)

Camp B ("Cross-validation")

1. Training data (~60%)
2. Validation data (~20%)
3. Test data (~20%)

Sample is split randomly, **or** randomly according to how it was generated (e.g. if it's panel data, sample *units*, not observations)

It is ideal to follow the "Cross-validation" camp, but in cases where you don't have many observations (training examples), you may have to go the "Holdout" route.

## k-fold cross-validation
Due to randomness in our data, it may be better to do the cross validation multiple times. To do so, we take the 80% training-plus-validation sample and randomly divide it into the 60/20 components k number of times. Typically k is between 3 and 10. (See graphic below)

![Source: Brant Deppa, Winona State Univ.](../Graphics/k-foldCV.png "Illustration of k-fold cross validation")

In the extreme, one can also do *nested* k-fold cross-validation, wherein the test set is shuffled some number of times and the k-fold CV is repeated each time. So we would end up with "3k" fold CV, for example. For simplicity, we'll only use simple k-fold CV in this course.

## How to do all of this in R 
There are a couple of nice packages in R that will do k-fold cross validation for you, and will work with a number of commonly used prediction algorithms

1. `caret`
2. `mlr`

`caret` likely still has a strong following, but `mlr` is up-and-coming and has a better user interface. We'll be using `mlr` for the rest of this unit of the course

In Python, `scikit-learn` is the workhorse machine learning frontend.

## How to use `mlr`
You need to tell `mlr` the following information before it will run:

1. Training data (which will end up being split k ways when doing cross-validation)
2. Testing data
3. The task at hand (e.g. regression or classification)
4. The validation scheme (e.g. 3-fold, 5-fold, or 10-fold CV)
5. The method used to tune the parameters of the algorithm (e.g. &lambda; for LASSO)
6. The prediction algorithm (e.g. "decision tree")
7. The parameters of the prediction algorithm that need to be cross-validated (e.g. tree depth, &lambda;, etc.)

For a complete list of prediction algorithms supported by `mlr`, see [here](http://mlr-org.github.io/mlr-tutorial/devel/html/integrated_learners/index.html)

## Simple example
Let's start by doing linear regression on a well-known dataset: the Boston housing data from UC Irivne:

```r
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# From UC Irvine's website (http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.names)
#    1. CRIM      per capita crime rate by town
#    2. ZN        proportion of residential land zoned for lots over 
#                 25,000 sq.ft.
#    3. INDUS     proportion of non-retail business acres per town
#    4. CHAS      Charles River dummy variable (= 1 if tract bounds 
#                 river; 0 otherwise)
#    5. NOX       nitric oxides concentration (parts per 10 million)
#    6. RM        average number of rooms per dwelling
#    7. AGE       proportion of owner-occupied units built prior to 1940
#    8. DIS       weighted distances to five Boston employment centres
#    9. RAD       index of accessibility to radial highways
#    10. TAX      full-value property-tax rate per $10,000
#    11. PTRATIO  pupil-teacher ratio by town
#    12. B        1000(Bk - 0.63)^2 where Bk is the proportion of blacks 
#                 by town
#    13. LSTAT    % lower status of the population
#    14. MEDV     Median value of owner-occupied homes in $1000's
```

## OLS

```r
library(mlr)
library(tidyverse)
library(magrittr)

# Add some other features
housing %<>% mutate(lmedv = log(medv),
                    medv = NULL,
                    dis2 = dis^2,
                    crimNOX = crim*nox)

# Break up the data:
n <- nrow(housing)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
housing.train <- housing[train,]
housing.test  <- housing[test, ]

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")
print(theTask)

# tell mlr what prediction algorithm we'll be using (OLS)
predAlg <- makeLearner("regr.lm")

# Set resampling strategy (here let's do 6-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 6 folds
print(sampleResults$aggr)

# Train the model (i.e. estimate OLS)
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set
prediction <- predict(finalModel, newdata = housing.test)

# Print out-of-sample RMSE
get.rmse <- function(y1,y2){
   return(sqrt(mean((y1-y2)^2)))
}
print(get.rmse(prediction$data$truth,prediction$data$response))

# Trained parameter estimates
getLearnerModel(finalModel)$coefficients

# OLS parameter estimates
summary(lm(lmedv ~ crim + zn + indus + chas + 
                   nox + rm + age + dis + rad + 
                   tax + ptratio + b + lstat + 
                   dis2 + crimNOX, data=housing.train))
```

Now, there's not much to do here, since there is no regularization in OLS. But you can see that there is quite a bit of variation in the RMSE based on which sample you use. Also, the trained parameter estimates match what OLS gives us.

## LASSO
Now let's repeat but instead use the LASSO estimator from the `glmnet` package

Since `mlr` is awesome, we only need to load the package, reset the `makeLearner` function, and add the tuning over the &lambda; parameter

```r
library(glmnet)
# Tell it a new prediction algorithm
predAlg <- makeLearner("regr.glmnet")

# Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

# Take 50 random guesses at lambda within the interval I specified above
tuneMethod <- makeTuneControlRandom(maxit = 50L)

# Do the tuning
tunedModel <- tuneParams(learner = predAlg,
                        task = theTask,
                        resampling = resampleStrat,
                        measures = rmse,       # RMSE performance measure, this can be changed to one or many
                        par.set = modelParams,
                        control = tuneMethod,
                        show.info = TRUE)
```

## LASSO (continued)
The workhorse of the cross-validation is done by calling the `tuneParams` function. Once we have that, we need only to train the alogrithm with the optimal parameters and then use them for prediction

```r
# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

print(head(prediction$data))
print(get.rmse(prediction$data$truth,prediction$data$response))

# Trained parameter estimates
getLearnerModel(finalModel)$beta
```

## Ridge regression
We could do ridge regression on the same data by instead setting `alpha` to be 0 (instead of 1). In this case, all we would need to do is adjust the `makeParamSet()` function, re-call the tuning function, re-compute the in-sample fit of the tuned model, and re-compute the out-of-sample fit of the optimally tuned model:

```r
# Search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))

# Do the tuning again
tunedModel <- tuneParams(learner = predAlg,
                        task = theTask,
                        resampling = resampleStrat,
                        measures = rmse,       # RMSE performance measure, this can be changed to one or many
                        par.set = modelParams,
                        control = tuneMethod,
                        show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction <- predict(finalModel, newdata = housing.test)

# Print RMSE
print(get.rmse(prediction$data$truth,prediction$data$response))

# Trained parameter estimates
getLearnerModel(finalModel)$beta
```
This showcases the nicest feature of the `mlr` package: we are still using the same algorithm, data, and cross-validation scheme, so we don't need to tell it those parameters again. We do, however, need to tell it to tune to the new parameter space (in this case, ridge instead of LASSO).

# The 5 Tribes of Machine Learning
Let's revisit the 5 Tribes of Machine Learning, according to Pedro Domingos. These "tribes" are philosophical identities associated with commonly used regression and classification algorithms in machine learning. 

| Tribe Name     | World view / Goal                                  | Master Algorithm(s)                       | R package(s)     |
|----------------|----------------------------------------------------|-------------------------------------------|------------------|
| Symbolists     | Inverse deduction (commonly called "induction")    | Tree models                               | `rpart`          |
| Connectionists | Re-engineering the brain                           | Neural networks                           | `nnet`           |
| Evolutionaries | Genomic evolution                                  | Genetic algorithms                        | `GA`             |
| Bayesians      | Probabilistic inference (via Bayes' Rule)          | Naive Bayes                               | `e1071`          |
| Analogizers    | Matching (i.e. analogizing) different bits of data | Nearest neighbor, Support Vector Machine  | `kknn`, `e1071`  |

Let's go through each of these algorithms, how they are used, and how they are regularized. We'll also use `mlr` to do a cross-validated classification example for each

## Tree models
Tree models seek to partition the data in such a way that observations are grouped according to the outcome (Y). These "groups" are called leaves. The goal of the model is twofold:

1. Maximize *similarity* of outcome within leaves (i.e. minimize RMSE, maximize F1 score)
2. Maximize *dissimilarity* of outcome across leaves

![Source: J.R. Quinlan, *Induction of Decision Trees*](../Graphics/Decision_Tennis.jpg "Tennis")


### Pros and Cons of Tree models
- Pros:
    * Fully nonparametric
    * Easy to interpret decision rules
    * Built-in searching of interaction effects among features (X's)
    * Handles categorical features easily
- Cons:
    * Easy to overfit (i.e. make the tree too complex)


### Regularizing Decision Trees
Since decision trees are nonparametric, we can't use L1 or L2 regularization. Instead, we regularize according to the following criteria

1. Depth
2. Number of nodes or leaves
3. Minimal leaf size
4. Information gain at splits (i.e. how "greedy" the algorithm is)

## Neural networks
Neural networks model the brain. They work by forming "connections" across "neurons" which work together to decide whether to "fire." A given neuron only fires if it has enough "activation energy." Why is this the case? Because that's how the human brain works, and the human brain is the triumph of biology.

The human brain is composed of many neurons (brain cells). Your body receives a stimulus from one of the 5 senses, which travels to the brain. Once arriving at the brain, it travels through a network of neurons until it reaches the appropriate part of the brain (e.g. the part governing thought or motor skills, or speech, etc.). Because of something known as neuroplasticity, the brain learns over time how to react to any given pattern of incoming signals.

Neural networks are set up to mimic the brain's structure, although they are vastly simplified. These networks contain the following components:

* Input layer / features
    - You can think of these as external stimuli to your own brain (e.g. sound, vision, etc.)
* Network structure
    - You must decide a priori how many layers of neurons you'll include, and how many neurons per layer
* Activation function
    - This determines the circumstances under which each neuron will "fire"
    - Main activiation functions are:
        * Sigmoid (logistic function)
        * Tanh (hyperbolic tangent function)
        * ReLU (kinked line)
        * Linear
        * Step function
* Output layer / outcome
    - The outcome comes at the end and is the biological equivalent of a response to the stimulus

Each of these components can drastically affect performance of the neural network. Thus, cross-validation and regularization are critical when working with neural networks. To see how this works, check out [Google's Neural Network Playground](https://www.google.com/url?q=https%3A%2F%2Fplayground.tensorflow.org%2F%23activation%3Dtanh%26regularization%3DL1%26batchSize%3D22%26dataset%3Dspiral%26regDataset%3Dreg-gauss%26learningRate%3D0.03%26regularizationRate%3D0.001%26noise%3D5%26networkShape%3D7%2C6%26seed%3D0.41460%26showTestData%3Dfalse%26discretize%3Dtrue%26percTrainData%3D70%26x%3Dtrue%26y%3Dtrue%26xTimesY%3Dtrue%26xSquared%3Dtrue%26ySquared%3Dtrue%26cosX%3Dfalse%26sinX%3Dtrue%26cosY%3Dfalse%26sinY%3Dtrue%26collectStats%3Dfalse%26problem%3Dclassification%26initZero%3Dfalse%26hideText%3Dfalse&sa=D&sntz=1&usg=AFQjCNHj4f1ROekPPItg1SLlk2FMnI6FVw).

![Source: Milijkovic et al. (2011) "Using Artificial Neural Networks to Predict Professional Movements of Graduates" Croatian Journal of Education, 13(3): 117-141](../Graphics/neuralNetwork.png "Examle Neural Network Structure")

### Pros and Cons of Neural Networks
- Pros:
    * Built in flexibility to come up with arbitrary nonlinear functions for prediction
    * Strong performance, particularly in image recognition
    * Takes a short time to compute on the test data
- Cons:
    * Can take a very long time to train
    * Can take a lot of computational resources to train
    * Very easy to overfit

### Regularizing Neural Networks
Regularization of neural networks comes in three forms:

1. L1/L2 regularization on connections among the layers
2. Restricting the number of neurons in a given layer
3. Restricting the number of hidden layers

### What is "Deep Learning"? 
Deep learning refers to learning that is done using neural networks with more than one hidden layer. That's it. Don't be fooled by buzzwords!

### Neural Networks and Stochastic Gradient Descent
Due to their computational burden, SGD was invented to aid in the computation of neural networks.

### Neural Networks and linear / logistic regression
It turns out that linear regression and logistic regression each are special cases of the neural network. Thus, these two well-known algorithms can be thought of as "connectionist" algorithms.

## Genetic Programming
Genetic programming seeks to approximate an arbitrary nonlinear function by simulating genetic mutation. In each "generation" the mutation with the best "fitness" is then chosen as the next generation's parents.

We won't go into the details of this approach, since it is not immediately applicable to data analysis tasks. 

If you're curious about these types of algorithms, you can check out a nice primer [here](https://cran.r-project.org/web/packages/GA/vignettes/GA.html).

## Naive Bayes
Bayes' Rule is the idea that one should update one's degree of belief in a given hypothesis once she receives new evidence. If the evidence is consistent with the hypothesis, the probability of belief goes up. If not, the probability goes down. Example from *The Master Algorithm* (p. 144): "If you test positive for AIDS, your probability of having it goes up."

The "updated" probability is called the **posterior probability** while the original probability is called the **prior probability**.

Mathematically, Bayes' Rule states that, for any two events A and B: P(A | B) = P(A) \* P(B | A) / P(B).

Maybe a better way to put it is: P(hypothesis | data) = P(hypothesis) \* P(data | hypothesis) / P(data).

So what does that mean? Let's think about it with the AIDS example. We want to know the likelihood of our having AIDS given that the test returned a positive result. Let's replace "A" with "HIV" and "B" with "positive" in the above formula:

P(HIV | positive) = P(HIV) \* P(positive | HIV) / P(positive)

suppose P(HIV) = 0.003 in the population, that P(positive) [regardless of whether or not you actuall have HIV] is 0.01, and that, if you actually have HIV, the test would be positive with probability 0.99. Plugging this into the above formula, we get

P(HIV | positive) = 0.003 \* 0.99 / 0.01 = 0.297

Before getting the test, we believed we had HIV with probability 0.003 (the population rate of HIV). After getting the positive test, we update our prior to be 0.297. Why not all the way up to 0.99? Because the test could yield false positives, so we don't want to be too hasty in our belief updating.

If P(positive) were equal to 0.003, then we'd be really worried because that would mean that the false positive rate of the test was much lower. If that were the case, our posterior probability would skyrocket to 0.99 from 0.297. 

### Naive Bayes and Bayes Rule
How does the above formula relate to Naive Bayes as a classification algorithm? It turns out that we can compute an observation's probability of being a certain class (P(Y = y), y=0 or y=1) based on the data that we have.

We want to know P(Y | X) which is our "updated" hypothesis or posterior probability. Let's say we want to classify people as college graduates or not. Our prior would be the rate of college graduates in our data set (e.g. 30%). We observe their X's (e.g. family income, parent's education, high school class rank, etc.). We want to look at a new observation's X's and make a reasonable guess as to whether they are a college graduate.

The formula we would use to update this is: 

P(college graduate | family income in 3rd quintile) = P(college graduate) \* P(family income in 3rd quintile | college graduate) / P(family income in 3rd quintile)

= 0.30 \* 0.50 / 0.20 = 0.75

What if we wanted to look at a case that had family income in 3rd quintile *and* whose father graduated from college? Here's where the "naivete" comes in handy: We assume that parental education is independent of family income (this is dumb, I know, but bear with me), which greatly simplifies the following formula:

P(college graduate | family income in 3rd quintile AND father college graduate) = P(college graduate) \* P(family income in 3rd quintile | college graduate) \* P(father college graduate | college graduate)

In practice, we typically ignore the denominator [P(data)] because it doesn't change with the outcome.

For a primer on Naive Bayes, consult [here](http://www.saedsayad.com/naive_bayesian.htm).

### Regularization in Bayesian algorithms
Bayesian algorithms actually don't use regularization in the same way as the other algorithms. Instead, the prior belief acts as a way to regularize to prevent overfitting.

### Pros and Cons of Naive Bayes
- Pros:
    * Easy to compute in training phase
    * Works well in high dimensions
- Cons:
    * Relies on the **naive** assumption that X variables are mutually indpendent
    * Can be computationally expensive to compute in the testing phase

Naive Bayes is most famous for working well with spam classification and other text classification objectives (e.g. document classification).

## k Nearest Neighbors (kNN) and Support Vector Machine (SVM)
kNN and SVM are the two workhorse algorithms of the analogizers. They seek to find commonality between different instances of data.

### kNN
kNN is a nonparametric algorithm that finds a specific observations nearest neighbor where "nearest neighbor" is defined in terms of the X's for each observation. Specifically, "nearest" is defined by some distance metric (e.g. Euclidean distance). k is the number of neighbors over which the prediction is calculated. 

#### Example
Suppose we are trying to classify a voter as being Republican or Democrat. We might look at their age, where they live, and how much education they have. To do the classification, we would follow these steps:

1. Look at a given observation's five (if k=5) "nearest" observations (in terms of Euclidean distance)
2. Compute what fraction of those neighbors are Republican
3. The quantity in step 2 would be our predicted probability of the new observation's likelihood of being Republican.

For a nice illustration of the bias-variance tradeoff in kNN, see [here](http://scott.fortmann-roe.com/docs/BiasVariance.html)

Here is a [complete guide to kNN in Python and R](https://kevinzakka.github.io/2016/07/13/k-nearest-neighbor/).

#### Regularization in kNN
The regularization parameter that needs to be cross-validated in kNN models is the number of neighbors, k. If k is too small (e.g. 1 or 2) it will result in overfitting (high variance). If k is too large, it will result in underfitting (high bias).

#### Pros and Cons of kNN
- Pros:
    * Nonparametric
    * Easy to compute
    * Easy to interpret
    * Computationally light to compute in the training phase
- Cons:
    * Doesn't work well in large dimensions
    * Computationally expensive to compute in the testing phase

### SVM
SVM can be thought of as a generalization of kNN. It works much better in high-dimensional spaces. The goal of SVM is to find the largest separating hyperplane between the positive and negative cases. It was originally set up to be a classification algorithm, but it can also be used for regression.

Originally, SVMs were limited to hyperplanes, but due to a discovery known as the **kernel trick**, they can be used to draw arbitrarily nonlinear boundaries.

We won't get into the specifics of SVM, but [here](https://www.quantstart.com/articles/Support-Vector-Machines-A-Guide-for-Beginners) is a nice primer for newbies.

#### Regularization in SVMs
The regularization parameters that needs to be cross-validated in SVM models are C, which governs the size of the margin between positive and negative examples, and other parameters that govern the complexity of the shape of the kernel function.

#### Pros and Cons of SVM
- Pros:
    * Effective in high-dimensional settings
    * Computationally efficient for testing
    * Allows for non-linear decision boundaries
- Cons:
    * Not well suited for "Big K" data (i.e. when the number of columns of X is larger than the number of rows)
    * It doesn't produce a probabilistic out-of-sample prediction

## Using the Five Tribes' algorithms in `mlr`
| Tribe Name     | Master Algorithm(s)     | R package | Regularization parameters | `mlr` algorithm name |
|----------------|-------------------------|-----------|---------------------------|----------------------|
| Symbolists     | Tree models             | `rpart`   | `minsplit` (integer), `minbucket` (integer), `cp` (numeric, typically very small) | `classif.rpart` |
| Connectionists | Neural networks         | `nnet`    | `size` (number of neurons in hidden layer; integer), `decay` (lambda; numeric) | `classif.nnet` |
| Evolutionaries | Genetic algorithms      | `GA`      | N/A | N/A |
| Bayesians      | Naive Bayes             | `e1071`   | N/A | N/A |
| Analogizers    | Nearest neighbor        | `kknn`    | `k` (integer) | `classif.kknn` |
|                | Support Vector Machine  | `e1071`   | `cost` (numeric ranging from 2^-10 to 2^10); `gamma` (same as `cost`) | `classif.svm` |

## Recommender systems
One commonly used application for prediction algorithms is recommender systems. For example, Netflix or YouTube want to "recommend" to you what to watch next, based on what you've shown interest in previously. The target variable here is "how much the user will like offering W" and the features are things like "list of shows the user has watched previously" or "list of shows user showed interest in previously" (e.g. by reading the overview of a video).

There are three main types of ways to form a recommendation system:

1. Collaborative filtering
    * Create recommendations based on what users who are similar to you are consuming
    * e.g. if we both like rock music, but you listen to a slightly different set of bands than I do, then the best recommendation for me are the bands that you listen to, but I don't
    * This approach relies on having a large number of users
    * It assumes that people who have a lot in common will continue to have a lot in common
    * kNN is the fundamental way to pick out the new recommendations using this approach
2. Content-based filtering
    * Create recommendations based on quantifiable characteristics about what you are consuming
    * e.g. come up with a way to describe each product's characteristics (e.g. a song by Rush vs. a song by Metallica, etc.)
    * Recommend new products to users whose average description matches the new products
    * This approach relies on coming up with a way to quantify each product's characteristics
    * It will only recommend new products that are closely related to the user's existing portfolio
    * Bayesian classifiers and neural networks are popular algorithms for offering new recommendations using this approach
3. Hybrid recommender systems
    * Because collaborative and content-based filtering each have advantages and disadvantages, it often is the case that better recommendations can be had by combining the two approaches

### How to build a recommender system in R
The simplest R package to do this is called `recommenderlab` and a step-by-step example is [here](https://www.r-bloggers.com/recommender-systems-101-a-step-by-step-practical-example-in-r/). Another walkthrough using the same library to build moving recommendations is [here](http://rstudio-pubs-static.s3.amazonaws.com/150913_3ccebcc146d84e5e98c40fc548322929.html).

As far as I can tell, `mlr` does not provide an interface to the `recommenderlab` package.

# Helpful links
* [Mullainathan & Spiess (2017)](https://www.aeaweb.org/articles?id=10.1257/jep.31.2.87&within%5Btitle%5D=on&within%5Babstract%5D=on&within%5Bauthor%5D=on&journal=3&q=mullainathan&from=j)
* [Kaggle notebook by Pondering Panda, showing how to use `mlr`](https://www.kaggle.com/xanderhorn/train-r-ml-models-efficiently-with-mlr)
* [Complete list of `mlr` algorithms](http://mlr-org.github.io/mlr-tutorial/devel/html/integrated_learners/index.html)
* [Quick start `mlr` tutorial](https://mlr-org.github.io/mlr-tutorial/release/html/index.html)
* Laurent Gatto's [*An Introduction to Machine Learning with R*](https://lgatto.github.io/IntroMachineLearningWithR/) online textbook
* [Five Tribes blog post](https://medium.com/@jrodthoughts/the-five-tribes-of-machine-learning-c74d702e88da)
* [Blog post](https://medium.com/the-theory-of-everything/understanding-activation-functions-in-neural-networks-9491262884e0): "Understanding Activation Functions in Neural Networks"
