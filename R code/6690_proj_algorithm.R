qsar_raw<-read.csv2("C:/Users/asus/Desktop/EECSE6690/Project/biodeg.csv",header = FALSE)
names(qsar_raw) <- c('SpMax_L',
                     'J_Dz_e',
                     'nHM',
                     'F01_N_N',
                     'F04_C_N',
                     'NssssC',
                     'nCb',
                     'C',
                     'nCp',
                     'nO',
                     'F03_C_N',
                     'SdssC',
                     'HyWiB_m',
                     'LOC',
                     'SM6_L',
                     'F03C_O',
                     'Me',
                     'Mi',
                     'nN_N',
                     'nArNO2',
                     'nCRX3',
                     'SpPosA_B_p',
                     'nCIR',
                     'B01_C_Br',
                     'B03_C_Cl',
                     'N_073',
                     'SpMax_A',
                     'Psi_i_1d',
                     'B04_C_Br',
                     'SdO',
                     'TI2_L',
                     'nCrt',
                     'C_026',
                     'F02_C_N',
                     'nHDon',
                     'SpMax_B_m',
                     'Psi_i_A',
                     'nN',
                     'SM6_B_m',
                     'nArCOOR',
                     'nX',
                     'class');
qsar_raw$SpMax_L<-as.numeric(qsar_raw$SpMax_L)
qsar_raw$J_Dz_e<-as.numeric(qsar_raw$J_Dz_e)
qsar_raw$C<-as.numeric(qsar_raw$C)
qsar_raw$SdssC<-as.numeric(qsar_raw$SdssC)
qsar_raw$HyWiB_m<-as.numeric(qsar_raw$HyWiB_m)
qsar_raw$LOC<-as.numeric(qsar_raw$LOC)
qsar_raw$SM6_L<-as.numeric(qsar_raw$SM6_L)
qsar_raw$Me<-as.numeric(qsar_raw$Me)
qsar_raw$Mi<-as.numeric(qsar_raw$Mi)
qsar_raw$SpPosA_B_p<-as.numeric(qsar_raw$SpPosA_B_p)
qsar_raw$SpMax_A<-as.numeric(qsar_raw$SpMax_A)
qsar_raw$Psi_i_1d<-as.numeric(qsar_raw$Psi_i_1d)
qsar_raw$SdO<-as.numeric(qsar_raw$SdO)
qsar_raw$TI2_L<-as.numeric(qsar_raw$TI2_L)
qsar_raw$SpMax_B_m<-as.numeric(qsar_raw$SpMax_B_m)
qsar_raw$Psi_i_A<-as.numeric(qsar_raw$Psi_i_A)
qsar_raw$SM6_B_m<-as.numeric(qsar_raw$SM6_B_m)
for (i in 1:1055) {
  if (qsar_raw$class[i] == "RB") {
    qsar_raw$class[i]<-"0"
  }
  else {
    qsar_raw$class[i]<-"1"
  }
}
qsar_raw$class<-as.integer(qsar_raw$class)
#test_index<-sample(1055, 128)
qsar_train<-qsar_raw[(1:837), ]
qsar_test<-qsar_raw[-(1:837), ]

library(tibble)
library(cvms)
cm_plot<-function(cm_table) {
  cvm<-as_tibble(cm_table)
  plot_confusion_matrix(cvm, target_col = "Truth", prediction_col = "Prediction",
                        counts_col = "n")
}


## 1. SVM_Improved_Version

library(e1071)
# First use all descriptors to fit in svm
set.seed(6690)
costs_svm<-seq(0.2, 10, length = 50)
valid_acc_svm<-seq(0, 0, length = 50)
# Use C-V to choose the best slack budget
for (epoch in 1:3) {
  for (i in 1:50) {
    qsar.svm.cv<-svm(class~., data = qsar_train, kernel = "radial", 
                     type = "C-classification", cost = costs_svm[i], cross = 7)
    valid_acc_svm[i]<-valid_acc_svm[i] + qsar.svm.cv$tot.accuracy
  }
}
svm_cost<-costs_svm[which.max(valid_acc_svm)]
qsar.svm<-svm(class~., data = qsar_train, kernel = "radial", 
              type = "C-classification", cost = svm_cost)
# Confusion Matrix for 7-fold CV
qsar.svm.cv<-svm(class~., data = qsar_train, kernel = "radial", 
                 type = "C-classification", cost = svm_cost, cross = 7)
svm_cv_table<-table(qsar.svm.cv$fitted, qsar_train$class, dnn = c("Prediction", "Truth"))
# CM for testing
svm_test_predict<-predict(qsar.svm, qsar_test)
svm_test_table<-table(svm_test_predict, qsar_test$class, dnn = c("Prediction", "Truth"))

# CM for fitted
svm_train_predict<-predict(qsar.svm, qsar_train)
svm_train_table<-table(svm_train_predict, qsar_train$class, dnn = c("Prediction", "Truth"))

svm.pred<-svm_test_predict



## 2. SVM_Paper_Version

# Then use specific descriptors chosen by the paper to fit in svm
set.seed(6690)
costs_re_svm<-seq(0.2, 10, length = 50)
valid_acc_re_svm<-seq(0, 0, length = 50)
for (epoch in 1:3) {
  for (i in 1:50) {
    qsar.re.svm.cv<-svm(class~C_026 + F02_C_N + nArNO2 + nCrt + nHDon + 
                          nN + nN_N + NssssC + nX + Psi_i_A + SM6_B_m + 
                          SpMax_B_m + SpMax_L, data = qsar_train, 
                        kernel = "radial", type = "C-classification",
                        cost = costs_re_svm[i], cross = 5)
    valid_acc_re_svm[i]<-qsar.re.svm.cv$tot.accuracy
  }
}
re_svm_cost<-costs_re_svm[which.max(valid_acc_re_svm)]
qsar.re.svm<-svm(class~C_026 + F02_C_N + nArNO2+nCrt + nHDon + 
                   nN + nN_N + NssssC + nX + Psi_i_A + SM6_B_m + 
                   SpMax_B_m + SpMax_L, data = qsar_train, 
                 kernel = "radial", type = "C-classification", 
                 cost = re_svm_cost)

# CM for 5-fold CV 
qsar.re.svm.cv<-svm(class~C_026 + F02_C_N + nArNO2+nCrt + nHDon + 
                      nN + nN_N + NssssC + nX + Psi_i_A + SM6_B_m + 
                      SpMax_B_m + SpMax_L, data = qsar_train, 
                    kernel = "radial", type = "C-classification",
                    cost = re_svm_cost, cross = 5)
resvm_cv_table<-table(qsar.re.svm.cv$fitted, qsar_train$class, dnn = c("Prediction", "Truth"))

# CM for testing
re_svm_test_predict<-predict(qsar.re.svm, qsar_test)
resvm_test_table<-table(re_svm_test_predict, qsar_test$class, dnn = c("Prediction", "Truth"))

# CM for fitted
re_svm_train_predict<-predict(qsar.re.svm, qsar_train)
resvm_train_table<-table(re_svm_train_predict, qsar_train$class, dnn = c("Prediction", "Truth"))

resvm.pred<-re_svm_test_predict

## 3. KNN_Paper_Version
library(kknn)
set.seed(6690)
k_val<-c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21)
dist_val<-c(0.1, 0.5, 1, 2, 3, 4, 5, 7, 10, 15)
cv_totoal_correct<-matrix(seq(0, 0, length = 100), nrow =10)
for (epoch in 1:5) {
  cv_index<-sample(837, 837)
  for (i in 1:10) {
    for (j in 1:10) {
      for (k in 1:5) {
        cv_test_index<-cv_index[(1+round(167.4*(k-1))):round(167.4*k)]
        qsar.re.knn.cv<-kknn(class~C + F01_N_N + F03_C_N + F04_C_N + 
                               J_Dz_e + nCb + nCp + nHM + nO + 
                               NssssC + SdssC + SpMax_L, 
                             train = qsar_train[-cv_test_index,], 
                             test = qsar_train[cv_test_index, ],
                             k = k_val[i], distance = dist_val[j])
        cv_predict<-round(qsar.re.knn.cv$fitted.values)
        num_correct<-sum(cv_predict == qsar_train[cv_test_index, ]$class)
        cv_totoal_correct[i, j]<-cv_totoal_correct[i, j] + num_correct
      }
    }
  }
}
opt_re_knn_par<-which.max(cv_totoal_correct)
k_re_kknn<-k_val[opt_re_knn_par%%10]
dist_re_knn<-dist_val[(opt_re_knn_par-1)%/%10 + 1]
qsar.re.knn<-kknn(class~C + F01_N_N + F03_C_N + F04_C_N + 
                    J_Dz_e + nCb + nCp + nHM + nO + 
                    NssssC + SdssC + SpMax_L, 
                  train = qsar_train, test = qsar_test,
                  k = k_re_kknn, distance = dist_re_knn)
# CM for testing
re_kknn_test_predict<-round(qsar.re.knn$fitted.values)
rekknn_test_table<-table(re_kknn_test_predict, qsar_test$class, dnn = c("Prediction", "Truth"))

# CM for training
qsar.re.knn.train<-kknn(class~C + F01_N_N + F03_C_N + F04_C_N + 
                          J_Dz_e + nCb + nCp + nHM + nO + 
                          NssssC + SdssC + SpMax_L, 
                        train = qsar_train, test = qsar_train,
                        k = k_re_kknn, distance = dist_re_knn)
re_kknn_train_predict<-round(qsar.re.knn.train$fitted.values)
rekknn_train_table<-table(re_kknn_train_predict, qsar_train$class, dnn = c("Prediction", "Truth"))

rekknn.pred<-re_kknn_test_predict

## 4. PLSDA_Paper_Version
library(mixOmics)
set.seed(6690)
plsda_n_val = c(2, 3, 4, 5, 6, 7, 8, 9)
re_plsda_index<-c(1, 5, 8, 10, 13, 14, 15, 16, 17, 18, 19, 20,
                  21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
train_x_plsda<-as.matrix(qsar_train[re_plsda_index])
test_x_plsda<-as.matrix(qsar_test[re_plsda_index])
cv_totoal_correct2<-seq(0, 0, length = 8)
for (epoch in 1:10) {
  cv_index<-sample(837, 837)
  for (i in 1:8) {
    for (k in 1:5) {
      cv_test_index<-cv_index[(1+round(167.4*(k-1))):round(167.4*k)]
      qsar.re.plsda.cv<-plsda(train_x_plsda[-cv_test_index,], 
                              qsar_train[-cv_test_index,]$class, 
                              ncomp = plsda_n_val[i])
      cv_predict<-predict(qsar.re.plsda.cv, train_x_plsda[cv_test_index,])$class$max.dist[, plsda_n_val[i]]
      num_correct<-sum(cv_predict == qsar_train[cv_test_index, ]$class)
      cv_totoal_correct2[i]<-cv_totoal_correct2[i] + num_correct
    }
  }
}
plsda_n<-plsda_n_val[which.max(cv_totoal_correct2)]
qsar.re.plsda<-plsda(train_x_plsda, qsar_train$class, ncomp = plsda_n)
# CM for testing
re_plsda_class<-predict(qsar.re.plsda, test_x_plsda)$class$max.dist[, plsda_n]
replsda_test_table<-table(re_plsda_class, qsar_test$class, dnn = c("Prediction", "Truth"))

# CM for training
re_plsda_class_train<-predict(qsar.re.plsda, train_x_plsda)$class$max.dist[, plsda_n]
replsda_train_table<-table(re_plsda_class_train, qsar_train$class, dnn = c("Prediction", "Truth"))


plsda_loadings<-qsar.re.plsda$loadings.star[[1]]
plot(plsda_loadings[,1], -plsda_loadings[,2])
text(plsda_loadings[,1], -plsda_loadings[,2], labels = rownames(plsda_loadings))

replsda.pred<-re_plsda_class

## 5. Adaboost-Breiman
library(adabag)
qsar_train_adaboost<-qsar_train
qsar_train_adaboost$class<-as.factor(qsar_train_adaboost$class)
qsar_train_adaboost$nHM<-as.numeric(qsar_train_adaboost$nHM)
qsar_train_adaboost$F01_N_N<-as.numeric(qsar_train_adaboost$F01_N_N)
qsar_train_adaboost$F04_C_N<-as.numeric(qsar_train_adaboost$F04_C_N)
qsar_train_adaboost$NssssC<-as.numeric(qsar_train_adaboost$NssssC)
qsar_train_adaboost$nCb<-as.numeric(qsar_train_adaboost$nCb)
qsar_train_adaboost$nCp<-as.numeric(qsar_train_adaboost$nCp)
qsar_train_adaboost$nO<-as.numeric(qsar_train_adaboost$nO)
qsar_train_adaboost$F03_C_N<-as.numeric(qsar_train_adaboost$F03_C_N)
qsar_train_adaboost$F03C_O<-as.numeric(qsar_train_adaboost$F03C_O)
qsar_train_adaboost$nN_N<-as.numeric(qsar_train_adaboost$nN_N)
qsar_train_adaboost$nArNO2<-as.numeric(qsar_train_adaboost$nArNO2)
qsar_train_adaboost$nCRX3<-as.numeric(qsar_train_adaboost$nCRX3)
qsar_train_adaboost$nCIR<-as.numeric(qsar_train_adaboost$nCIR)
qsar_train_adaboost$B01_C_Br<-as.numeric(qsar_train_adaboost$B01_C_Br)
qsar_train_adaboost$B03_C_Cl<-as.numeric(qsar_train_adaboost$B03_C_Cl)
qsar_train_adaboost$N_073<-as.numeric(qsar_train_adaboost$N_073)
qsar_train_adaboost$B04_C_Br<-as.numeric(qsar_train_adaboost$B04_C_Br)
qsar_train_adaboost$nCrt<-as.numeric(qsar_train_adaboost$nCrt)
qsar_train_adaboost$C_026<-as.numeric(qsar_train_adaboost$C_026)
qsar_train_adaboost$F02_C_N<-as.numeric(qsar_train_adaboost$F02_C_N)
qsar_train_adaboost$nHDon<-as.numeric(qsar_train_adaboost$nHDon)
qsar_train_adaboost$nN<-as.numeric(qsar_train_adaboost$nN)
qsar_train_adaboost$nArCOOR<-as.numeric(qsar_train_adaboost$nArCOOR)
qsar_train_adaboost$nX<-as.numeric(qsar_train_adaboost$nX)
set.seed(6690)
trees_adaboost<-seq(50, 200, length = 7)
bootstrap<-c(TRUE, FALSE)
adaboost_cv_error<-matrix(seq(0, 0, length = 14), nrow =7)
for (epoch in 1:3) {
  for (i in 1:7) {
    for (j in 1:2) {
      qsar.adaboost.cv<-boosting.cv(class~., data = qsar_train_adaboost, 
                                    boos = bootstrap[j], v = 5, 
                                    mfinal = trees_adaboost[i])
      adaboost_cv_error[i, j]<-adaboost_cv_error[i, j] + qsar.adaboost.cv$error
    }
  }
}
opt_adaboost_par<-which.min(adaboost_cv_error)
# trees = 175, boos = TRUE
set.seed(6690)
# bootstrap_adaboost<-bootstrap[(opt_adaboost_par - 1)%/%7 + 1]
# numtree_adaboost<-trees_adaboost[opt_adaboost_par%%7 + (7 - opt_adaboost_par%%7)%/%7 ]
bootstrap_adaboost<-TRUE
numtree_adaboost<-175
qsar.adaboost<-boosting(class~., data = qsar_train_adaboost,
                        boos = bootstrap_adaboost, 
                        mfinal = numtree_adaboost)

# CM for testing
adaboost_test_predict<-predict(qsar.adaboost, qsar_test)$class
ada1_test_table<-table(adaboost_test_predict, qsar_test$class, dnn = c("Prediction", "Truth"))
ada1.pred<-adaboost_test_predict

## 6. Adaboost-Freund
set.seed(6690)
trees_adaboost2<-seq(25, 175, length = 7)
bootstrap2<-c(TRUE, FALSE)
adaboost_cv_error2<-matrix(seq(0, 0, length = 14), nrow =7)
for (epoch in 1:3) {
  for (i in 1:7) {
    for (j in 1:2) {
      qsar.adaboost.cv2<-boosting.cv(class~., data = qsar_train_adaboost, 
                                     boos = bootstrap2[j], v = 5, 
                                     mfinal = trees_adaboost2[i],
                                     coeflearn = "Freund")
      adaboost_cv_error2[i, j]<-adaboost_cv_error2[i, j] + qsar.adaboost.cv2$error
    }
  }
}
opt_adaboost_par2<-which.min(adaboost_cv_error2)
# False, 50
set.seed(100)
# bootstrap_adaboost2<-bootstrap2[(opt_adaboost_par2 - 1)%/%7 + 2]
# numtree_adaboost2<-trees_adaboost2[opt_adaboost_par2%%67 + (7 - opt_adaboost_par2%%7)%/%7]
bootstrap_adaboost2<-FALSE
numtree_adaboost2<-50
qsar.adaboost2<-boosting(class~., data = qsar_train_adaboost,
                         boos = bootstrap_adaboost2, 
                         mfinal = numtree_adaboost2,
                         coeflearn = "Freund")

# CM for testing
adaboost_test_predict2<-predict(qsar.adaboost2, qsar_test)$class
ada2_test_table<-table(adaboost_test_predict2, qsar_test$class, dnn = c("Prediction", "Truth"))
ada2.pred<-adaboost_test_predict2

## 7. Neural Network
set.seed(6690)
library(keras)
qsar_train_x<-as.matrix(qsar_train[1:41])
qsar_train_y<-as.matrix(qsar_train[42])
qsar_test_x<-as.matrix(qsar_test[1:41])
qsar_test_y<-as.matrix(qsar_test[42])
qsar_train_y<-to_categorical(qsar_train_y, 2)
qsar_test_y<-to_categorical(qsar_test_y, 2)
num_units = c(100, 200, 300, 400, 500, 600)
cv_totoal_acc<-seq(0, 0, length = 6)
for (epoch in 1:6) {
  print(epoch)
  cv_index<-sample(837, 837)
  for (i in 1:6) {
    for (k in 1:5) {
      cv_test_index<-cv_index[(1+round(167.4*(k-1))):round(167.4*k)]
      qsar.nn.cv<-keras_model_sequential()
      qsar.nn.cv %>%
        layer_dense(units = num_units[i], activation = "relu") %>%
        layer_dropout(0.4) %>%
        layer_dense(units = 2, activation = "softmax")
      qsar.nn.cv %>% compile(loss = "categorical_crossentropy", 
                             optimizer = "adam", metrics = "accuracy")
      history.cv<-qsar.nn.cv %>% fit(qsar_train_x, qsar_train_y, epochs = 60,
                                     batch_size = 100, verbose = 0, validation_split = 0.2)
      cv.test<-history.cv$metrics$val_accuracy[60]
      cv_totoal_acc[i]<-cv_totoal_acc[i] + cv.test
    }
  }
}
num_unit<-num_units[which.max(cv_totoal_acc)]
# num_unit = 400
qsar.nn<-keras_model_sequential()
qsar.nn %>%
  layer_dense(units = num_unit, activation = "relu") %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 2, activation = "softmax")
qsar.nn %>% compile(loss = "categorical_crossentropy", 
                    optimizer = "adam", metrics = "accuracy")
nn.train.history<-qsar.nn %>% fit(qsar_train_x, qsar_train_y, epochs = 100,
                                  batch_size = 100, validation_split = 0.2)
plot(nn.train.history)
test_acc_nn<-qsar.nn %>% evaluate(qsar_test_x, qsar_test_y, verbose = 0)
test_acc_nn[2]
nn.pred<-qsar.nn %>% predict_classes(qsar_test_x)
nn_test_table<-table(nn.pred, qsar_test$class, dnn = c("Prediction", "Truth"))

## 8. Deep Neural Network
set.seed(6690)
num_units1 = c(200, 400)
num_units2 = c(25, 50, 100)
cv_totoal_acc2<-seq(0, 0, length = 6)
for (epoch in 1:6) {
  print(epoch)
  cv_index<-sample(837, 837)
  for (i in 1:6) {
    for (k in 1:5) {
      cv_test_index<-cv_index[(1+round(167.4*(k-1))):round(167.4*k)]
      j<-((i-1) %/% 3) + 1
      qsar.dnn.cv<-keras_model_sequential()
      qsar.dnn.cv %>%
        layer_dense(units = num_units1[j], activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dropout(0.4) %>%
        layer_dense(units = num_units2[i - 3*(j-1)], activation = "relu") %>%
        layer_dropout(0.4) %>%
        layer_dense(units = 2, activation = "softmax")
      qsar.dnn.cv %>% compile(loss = "categorical_crossentropy", 
                              optimizer = "adam", metrics = "accuracy")
      history.cv<-qsar.dnn.cv %>% fit(qsar_train_x, qsar_train_y, epochs = 60,
                                      batch_size = 100, verbose = 0, validation_split = 0.2)
      cv.test<-history.cv$metrics$val_accuracy[60]
      cv_totoal_acc2[i]<-cv_totoal_acc2[i] + cv.test
    }
  }
}
dnn_index<-which.max(cv_totoal_acc2)
unit1_dnn_index<-((dnn_index-1) %/% 3) + 1
unit2_dnn_index<-dnn_index - 3*(unit1_dnn_index-1)
num_unit1<-num_units1[unit1_dnn_index]
num_unit2<-num_units2[unit2_dnn_index]
# num_unit1 = 400; num_unit2 = 50
qsar.dnn<-keras_model_sequential()
qsar.dnn %>%
  layer_dense(units = num_unit1, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(0.4) %>%
  layer_dense(units = num_unit2, activation = "relu") %>%
  layer_dropout(0.4) %>%
  layer_dense(units = 2, activation = "softmax")
qsar.dnn %>% compile(loss = "categorical_crossentropy", 
                     optimizer = "adam", metrics = "accuracy")
dnn.train.history<-qsar.dnn %>% fit(qsar_train_x, qsar_train_y, epochs = 60,
                                    batch_size = 100, validation_split = 0.05)
plot(dnn.train.history)
test_acc_dnn<-qsar.dnn %>% evaluate(qsar_test_x, qsar_test_y, verbose = 0)
test_acc_dnn[2]
dnn.pred<-qsar.dnn %>% predict_classes(qsar_test_x)
dnn_test_table<-table(dnn.pred, qsar_test$class, dnn = c("Prediction", "Truth"))

## PCA
library(factoextra)
library(FactoMineR)
re_svm_index<-c(1, 6, 19, 20, 32, 33, 34, 35, 36, 37, 38, 39, 41)
re_knn_index<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
qsar.svm.pca<-PCA(qsar_raw[re_svm_index], ncp = 10)
fviz_pca_ind(qsar.svm.pca, axes = c(1, 4), label = "none",
             habillage = as.factor(qsar_raw$class))
fviz_pca_var(qsar.svm.pca, axes = c(1, 4), repel = TRUE, 
             fill.var = "red",
             col.var = "black", palette = "npg")
qsar.knn.pca<-PCA(qsar_raw[re_knn_index], ncp = 10)
fviz_pca_ind(qsar.knn.pca, axes = c(1, 4), label = "none",
             habillage = as.factor(qsar_raw$class))
fviz_pca_var(qsar.knn.pca, axes = c(1, 4), repel = TRUE, 
             fill.var = "red",
             col.var = "black", palette = "npg")
qsar.pca<-PCA(qsar_raw, ncp = 10)
fviz_pca_ind(qsar.pca, axes = c(1, 4), label = "none",
             habillage = as.factor(qsar_raw$class))
fviz_pca_var(qsar.pca, axes = c(1, 4), repel = TRUE, 
             fill.var = "red",
             col.var = "black", palette = "npg")

# 9. LDA
# create lda model
library(MASS)
lda.fit <- lda(class ~ ., data = qsar_train)
lda.predict <- predict(lda.fit, qsar_test)
lda.class <- lda.predict$class
lda_test_tabel<-table(lda.class, qsar_test$class)
mean(lda.class == qsar_test$class)
lda.pred<-lda.class


# 10. Naive Bayes
library(e1071)
nb.fit <- naiveBayes(class ~.,data = qsar_train)
nb.class <- predict(nb.fit, qsar_test)
naive_test_table<-table(nb.class, qsar_test$class)
mean(nb.class == qsar_test$class)
naive.pred<-nb.class

# 11. Consensus
library(caret)
all_pred<-data.frame(svm.pred, resvm.pred, rekknn.pred, replsda.pred,
                     ada1.pred, ada2.pred, nn.pred, dnn.pred, naive.pred,
                     rf.pred, bag.pred)
cm.svm<-confusionMatrix(svm_test_table)
cm.resvm<-confusionMatrix(resvm_test_table)
cm.reknn<-confusionMatrix(rekknn_test_table)
cm.replsda<-confusionMatrix(replsda_test_table)
cm.ada1<-confusionMatrix(ada1_test_table)
cm.ada2<-confusionMatrix(ada2_test_table)
cm.nn<-confusionMatrix(nn_test_table)
cm.dnn<-confusionMatrix(dnn_test_table)

# C1
consensus1<-as.numeric(all_pred$rekknn.pred) + as.numeric(all_pred$replsda.pred) + as.numeric(all_pred$resvm.pred) - 1
consensus1<-as.numeric(consensus1 > 1)
cm.c1<-confusionMatrix(table(consensus1, qsar_test$class, dnn = c("Prediction", "Truth")))
cm.c1

# C2
consensus2<-as.numeric(all_pred$nn.pred) + as.numeric(all_pred$rekknn.pred) + as.numeric(all_pred$svm.pred) - 1
consensus2<-as.numeric(consensus2 > 1)
cm.c2<-confusionMatrix(table(consensus2, qsar_test$class, dnn = c("Prediction", "Truth")))
cm.c2

# C3
consensus3<-as.numeric(all_pred$nn.pred) + as.numeric(all_pred$rekknn.pred) + as.numeric(all_pred$dnn.pred)
consensus3<-as.numeric(consensus3 > 1)
cm.c3<-confusionMatrix(table(consensus3, qsar_test$class, dnn = c("Prediction", "Truth")))
cm.c3

# C4
consensus4<-as.numeric(all_pred$nn.pred) + as.numeric(all_pred$rekknn.pred) + as.numeric(all_pred$ada1.pred)
consensus4<-as.numeric(consensus4 > 1)
cm.c4<-confusionMatrix(table(consensus4, qsar_test$class, dnn = c("Prediction", "Truth")))
cm.c4

# C5
consensus5<-as.numeric(all_pred$nn.pred) + as.numeric(all_pred$rekknn.pred) + as.numeric(all_pred$rf.pred) - 1
consensus5<-as.numeric(consensus5 > 1)
cm.c5<-confusionMatrix(table(consensus5, qsar_test$class, dnn = c("Prediction", "Truth")))
cm.c5

# C6
consensus6<-as.numeric(all_pred$nn.pred) + as.numeric(all_pred$rekknn.pred) + as.numeric(all_pred$bag.pred) - 1
consensus6<-as.numeric(consensus6 > 1)
cm.c6<-confusionMatrix(table(consensus6, qsar_test$class, dnn = c("Prediction", "Truth")))
cm.c6

# C7
consensus7<-as.numeric(all_pred$nn.pred) + as.numeric(all_pred$rekknn.pred) + as.numeric(all_pred$ada2.pred)
consensus7<-as.numeric(consensus7 > 1)
cm.c7<-confusionMatrix(table(consensus7, qsar_test$class, dnn = c("Prediction", "Truth")))
cm.c7

# C8
consensus8<-as.numeric(all_pred$nn.pred) + as.numeric(all_pred$rekknn.pred) + as.numeric(all_pred$ada1.pred) + 
  + as.numeric(all_pred$ada2.pred) + as.numeric(all_pred$nn.pred)
consensus8<-as.numeric(consensus8 > 2)
cm.c8<-confusionMatrix(table(consensus8, qsar_test$class, dnn = c("Prediction", "Truth")))
cm.c8


# C?
consensus<-consensus4 + consensus7 + consensus8
consensus<-as.numeric(consensus > 1)
cm.c<-confusionMatrix(table(consensus, qsar_test$class, dnn = c("Prediction", "Truth")))
cm.c


# 12. Plot Confusion Matrix
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(130, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(155, 425, 240, 370, col='#3F97D0')
  text(195, 435, 'RB', cex=2)
  rect(255, 425, 340, 370, col='#F7AD50')
  text(295, 435, 'NRB', cex=2)
  text(125, 370, 'Predicted', cex=1.8, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.8, font=2)
  rect(155, 300, 240, 365, col='#F7AD50')
  rect(255, 300, 340, 365, col='#3F97D0')
  text(140, 400, 'RB', cex=2, srt=90)
  text(140, 335, 'NRB', cex=2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=2.5, font=2, col='white')
  text(195, 335, res[2], cex=2.5, font=2, col='white')
  text(295, 400, res[3], cex=2.5, font=2, col='white')
  text(295, 335, res[4], cex=2.5, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=2)
  text(30, 85, names(cm$byClass[2]), cex=2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=2)
  text(50, 85, names(cm$byClass[5]), cex=2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=2)
  text(70, 85, names(cm$byClass[6]), cex=2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=2)
  text(90, 85, names(cm$byClass[7]), cex=2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=2, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=2)
  text(70, 35, names(cm$overall[2]), cex=2, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=2)
} 
draw_confusion_matrix(cm.ada1)
draw_confusion_matrix(cm.ada2)
draw_confusion_matrix(cm.nn)
draw_confusion_matrix(cm.dnn)
draw_confusion_matrix(cm.bag)
draw_confusion_matrix(cm.rf)

# 13. Bagging
library(randomForest)
set.seed(6690)
num_trees<-c(50, 100, 150, 200, 300, 400)
bagging_cv_acc<-seq(0, 0, length = 6)
for (epoch in 1:6) {
  cv_index<-sample(837, 837)
  for (i in 1:6) {
    for (k in 1:5) {
      cv_test_index<-cv_index[(1+round(167.4*(k-1))):round(167.4*k)]
      qsar.bag.cv<-randomForest(as.factor(class) ~.,data = qsar_train[-cv_test_index, ], 
                                ntree = num_trees[i], mtry = 41, importance = TRUE)
      bag.cv.pred <- predict(qsar.bag.cv, newdata = qsar_train[cv_test_index, ])
      cv.acc<-mean(bag.cv.pred == qsar_train[cv_test_index, ]$class)
      bagging_cv_acc[i]<-bagging_cv_acc[i] + cv.acc
    }
  }
}
num_tree = num_trees[which.max(bagging_cv_acc)]
qsar.bagging <- randomForest(as.factor(class) ~.,data = qsar_train, 
                             ntree = num_tree, mtry = 41, importance = TRUE)
bag.pred <- predict(qsar.bagging, newdata = qsar_test)
cm.bag<-confusionMatrix(table(bag.pred, qsar_test$class, dnn = c("Prediction", "Truth")))
mean(bag.pred == qsar_test$class)

# 13. Random Forest
set.seed(6690)
num_trees_rf<-c(100, 150, 200, 300, 400, 500)
rf_cv_acc<-seq(0, 0, length = 6)
for (epoch in 1:6) {
  cv_index<-sample(837, 837)
  for (i in 1:6) {
    for (k in 1:5) {
      cv_test_index<-cv_index[(1+round(167.4*(k-1))):round(167.4*k)]
      qsar.rf.cv<-randomForest(as.factor(class) ~.,data = qsar_train[-cv_test_index, ], 
                               ntree = num_trees_rf[i], importance = TRUE)
      rf.cv.pred <- predict(qsar.rf.cv, newdata = qsar_train[cv_test_index, ])
      cv.acc<-mean(rf.cv.pred == qsar_train[cv_test_index, ]$class)
      rf_cv_acc[i]<-rf_cv_acc[i] + cv.acc
    }
  }
}
num_tree_rf = num_trees_rf[which.max(rf_cv_acc)]
qsar.rf <- randomForest(as.factor(class) ~.,data = qsar_train, 
                        ntree = num_tree_rf, importance = TRUE)
rf.pred <- predict(qsar.rf, newdata = qsar_test)
cm.rf<-confusionMatrix(table(rf.pred, qsar_test$class, dnn = c("Prediction", "Truth")))
mean(rf.pred == qsar_test$class)


plot(trees_adaboost2, adaboost_cv_error2[, 1]/3, main = "Cross Validation for AdaBoost",
     xlab = "ntree", ylab = "cross-validation error", type = "o", ylim = c(0.12, 0.145),
     pch = 16, col = "red")
lines(trees_adaboost2, adaboost_cv_error2[, 2]/3, type = "o", pch = 16, col = "blue")
legend("topright", c("Boostrap", "No Boostrap"), pch = c(16, 16), col = c('red', 'blue'))
