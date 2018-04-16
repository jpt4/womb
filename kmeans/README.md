# website-classifier

A suite of classifiers mapping websites to actionable categories.

Possible methods:
Naive Bayesian with maximum a posteriori probability decision rule.
k-means clustering.

Feature sources:
word frequency

First approach:
1. 2-category, 2-dimensional k-means clustering. Bank vs. gambling, % "bank" word and % "gambling" words
2. 2-category, n-dimensional k-means clustering. Bank vs. gambling, % all words.
3. m-category, n-dimensional k-means clustering. All categories, % all words.
?. Visualization.

Second approach:
1. 2-category naive Bayesian inference.
2. m-category naive Bayesian inference.

