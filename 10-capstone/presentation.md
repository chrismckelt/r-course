---
output:
  revealjs::revealjs_presentation:
    theme: night
    highlight: pygments
    center: true
---

##Introduction
This report outlines the methodology for building a word prediction application using Natural Language Processing techniques as part of the the <a href="https://www.coursera.org/specializations/jhu-data-science">Coursera Data Science Specialization</a>.

The <a href="http://chrismckelt.github.io/r-course/10-capstone/milestone/milestone-report.html">milestone report</a> outlines the initial approach to building a series of <a href="https://en.wikipedia.org/wiki/N-gram">ngram models</a> from a range of text documents.

##Prediction models
<ul>
<li>A cleaned corpus sample was used to create 5 bag of word 'ngram' models</li>
<li>The below process was used to search for words from an input sentence</li>
</ul>
![image](https://user-images.githubusercontent.com/662868/33523738-e134d69a-d848-11e7-9868-ed3d2992e81b.png)
</p>

##Algorithm used to make the prediction
<p>
A <a href="http://www.aclweb.org/anthology/D07-1090.pdf">Stupid Backoff</a> smoothing strategy
was used to calculate a 'score' for each word follows:

    if the rows ngram model was 5
      score = matched 5 gram Count / input 4 gram Count
    else if the rows ngram model was 4
      score = 0.4 * matched 4 gram Count / input 3 gram Count
    else if the rows ngram model was 3
      score = 0.4 * 0.4 * matched 3 gram Count / input 2 gram Count
    else if the rows ngram model was 2
      score = 0.4 * 0.4 * 0.4 * matched 2 gram Count / input 1 gram Count

|   ngram | predicted   | score   |    
|---|---|---|---|
|  4 | you  |  0.2 |
|  3 | you  |  0.1 |
|  2 | you  |  0.05 |

The total score for the predicted word 'you' is (0.2 + 0.1 + 0.05) = 0.35
The final scoring is aggregated and summed for each word.  The top 3 words are selected according to the highest score.
</p>

##Evaluation
The prediction model was evaluated using the Benchmark.R tool (see references for source).

Initial predicts were quite high but also quite slow.  The decision to only use 1-3 ngram models sped up the search time by half but also dropped the accuracy by 10%. 

![image](https://user-images.githubusercontent.com/662868/33319651-68536484-d47a-11e7-9fdb-7e5f029235c6.png)

##Instructions

To use the application navigate to the following URL
 
 <a href="https://chrismckelt.shinyapps.io/datascience-capstone/">https://chrismckelt.shinyapps.io/datascience-capstone/ </a>

To use the application start typing in text.

![image](https://user-images.githubusercontent.com/662868/33580697-22e67850-d989-11e7-857d-046f26af52c1.png)

<small>When no results are found the 3 most common words from the English language ('the', 'be', 'to') are returned as a response.</small>

Click on the green side menu for visual display options.