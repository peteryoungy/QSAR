### Overview

Final project of EECS 6690 Statistical learning @ Columbia University.

QSAR (Quantitative Structure-Activity Relationships) is used to predict the biodegradability of chemicals. QSAR biodegradation data set was built to develop QSAR models for studying the relationship between chemical structure and biodegradability of molecules.

### File Structure

```
 ๐ฆQSAR
 โฃ ๐data
 โ โ ๐biodeg.csv        
 โฃ ๐materials
 โ โฃ ๐pics                         # pics
 โ โ ๐ presentation_v2.pptx        # slides
 โ โ ๐ presentation_v2.pdf         # slides pdf
 โ โ ๐ final_paper_v1.pdf          # Final Report
 โฃ ๐R code
 โ โฃ ๐tree.Rmd                     # Decision Tree Model using rpart
 โ โฃ ๐read_data.Rmd                # Our implementation
 โ โฃ ๐plot.Rmd                     # Result visualization
 โ โ ๐6690_proj_algorithm.R        # Reproduce paper method, Adaboost, NN and consensus model
 โ โ ๐6690_proj_algorithm.Rmd      # Rmd version of 6690_proj_algorithm.R
 โฃ ๐.gitignore
 โฃ ๐QSAR.Rproj
 โ ๐README.md
```

Presentation slides: [pptx](./materials/presentation_v2.pptx)    [pdf](./materials/presentation_v2.pdf)

Final report: [paper](./materials/final_paper_v1.pdf)

Data Set: [Data Set](https://archive.ics.uci.edu/ml/datasets/QSAR+biodegradation)

Result pics: [pictures](./materials/pics)

### Data Set Description

- **Number of Instances: 1055** 
  - 356 molecules are ready biodegradable (RB) and 699 are not ready biodegradable (NRB)

- **Number of Attributes: 41** 
  - selected using many classification modeling methods combined with genetic algorithms
- **Correlationships**

<img src="./materials/pics/correlations.png" width=70% height=70%>

### Reproduce

- KNN
- PLSDA
- SVM

### Implementation

- LDA
- Naive Bayes
- Decision Tree
- Bagging
- RandomForest
- Adaboost
- Neural Network
- Consensus Model

### Conclusion

#### โ  Individual model

<img src="./materials/pics/individual_model.png" width=70% height=70%>

#### โก Consensus Model

<img src="./materials/pics/consensus_model.png" >

### Citation

Mansouri, K., Ringsted, T., Ballabio, D., Todeschini, R., Consonni, V. (2013). Quantitative Structure - Activity Relationship models for ready biodegradability of chemicals. Journal of Chemical Information and Modeling, 53, 867-878

### Our Team

- [Lei Lyu](https://github.com/ll-l-77)

- [Yi Chen](https://github.com/YiChennee)
- [Wenxiang Zhou](https://github.com/ShionWayne)
- [Yang Yu](https://github.com/peteryoungy)
