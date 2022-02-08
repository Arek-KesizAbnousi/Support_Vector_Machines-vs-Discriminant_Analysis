# Classification----DiscriminantAnalysis-SupportVectorMachines

In this Classification Project, We will code some Discriminant Analysis Methods and compare them to
Support Vector Machines (SVMs).   
 - We will create a simulated accuracy matrix with a dimension of 100 x 10. Judging by the Accuracy matrix and comparing each of the ten columns, we could say
We will conclude whether the ten methods differ much and which classification method is the best.

 # Classification Methods 
 We perform classification using the below methods.
 - LDA (Linear Discriminant Analysis).
 - QDA (Quadratic Discriminant Analysis).
 - RDA (Regularized Discriminant Analysis).
 - Naïve Bayes.

 Additionally, we will use the training data to fit two SVMs (Support Vector Machines),

   - One linear SVM, Support Vector Machines ( Kernel)
   - One polynomial SVM (Kernel).
    We will use the e1071 package in R. This package was the first implementation of SVM in R. Offers quick and easy implementation of SVMs.
     
 # Data  
    - Sonar Dataset. To generate in R Code we use : library(mlbench) and data(Sonar)

 # Code implementation.
We implement and calculate an accuracy matrix with a dimension of 100 x 10. We achieve this  by breaking the data set into two parts (158 observations
for training data, 50 for test data). In The 100x9 Accuracy Matrix, 
Row-wise is the accuracy values of 100 different simulations, 
and column-wise given are the accuracies for different methods (LDA, QDA, RDA (with alpha = 0, 0.1, 0.5 & 0.9),
Naive Bayes, Linear SVM & origin passing Polynomial SVM with degree = 3.

We will split the data into a training set of size 158 and a testing set of size 50. Then by using the
training data, we will summarize the Accuracy in a matrix. In the resulting table, we will record each of the four methods ( for the 100 repetitions ).


 # Conlusion
 Finally, judging by the Resulting Accuracy Table and comparing each of the values in the ten columns, we can say
that the ten methods do not differ much and look pretty similar.
