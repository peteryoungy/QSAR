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
table(qsar_train$class, qsar.svm.cv$fitted, dnn = c("Truth", "Prediction"))

# CM for testing
svm_test_predict<-predict(qsar.svm, qsar_test)
table(qsar_test$class, svm_test_predict, dnn = c("Truth", "Prediction"))

# CM for fitted
svm_train_predict<-predict(qsar.svm, qsar_train)
table(qsar_train$class, svm_train_predict, dnn = c("Truth", "Prediction"))





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
table(qsar_train$class, qsar.re.svm.cv$fitted, dnn = c("Truth", "Prediction"))

# CM for testing
re_svm_test_predict<-predict(qsar.re.svm, qsar_test)
table(qsar_test$class, re_svm_test_predict, dnn = c("Truth", "Prediction"))

# CM for fitted
re_svm_train_predict<-predict(qsar.re.svm, qsar_train)
table(qsar_train$class, re_svm_train_predict, dnn = c("Truth", "Prediction"))


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
re_kknn_test_predict<-round(qsar.re.knn$fitted.values)
table(qsar_test$class, re_kknn_test_predict, dnn = c("Truth", "Prediction"))
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
re_plsda_class<-predict(qsar.re.plsda, test_x_plsda)$class$max.dist[, plsda_n]
table(qsar_test$class, re_plsda_class, dnn = c("Truth", "Prediction"))
plsda_loadings<-qsar.re.plsda$loadings.star[[1]]
plot(plsda_loadings[,1], -plsda_loadings[,2])
text(plsda_loadings[,1], -plsda_loadings[,2], labels = rownames(plsda_loadings))


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
trees_adaboost<-seq(50, 250, length = 5)
bootstrap<-c(TRUE, FALSE)
adaboost_cv_error<-matrix(seq(0, 0, length = 10), nrow =5)
for (epoch in 1:3) {
  for (i in 1:5) {
    for (j in 1:2) {
      qsar.adaboost.cv<-boosting.cv(class~., data = qsar_train_adaboost, 
                                 boos = bootstrap[j], v = 5, 
                                 mfinal = trees_adaboost[i])
      adaboost_cv_error[i, j]<-adaboost_cv_error[i, j] + qsar.adaboost.cv$error
    }
  }
}
opt_adaboost_par<-which.min(adaboost_cv_error)
numtree_adaboost<-trees_adaboost[opt_adaboost_par%%5]
bootstrap_adaboost<-bootstrap[(opt_adaboost_par-1)%/%5 + 1]
qsar.adaboost<-boosting(class~., data = qsar_train_adaboost,
                        boos = bootstrap_adaboost, 
                        mfinal = numtree_adaboost)
adaboost_test_predict<-predict(qsar.adaboost, qsar_test)$class
table(qsar_test$class, adaboost_test_predict, dnn = c("Truth", "Prediction"))

## 6. Adaboost-Freund
set.seed(6690)
trees_adaboost2<-seq(50, 250, length = 5)
bootstrap2<-c(TRUE, FALSE)
adaboost_cv_error2<-matrix(seq(0, 0, length = 10), nrow =5)
for (epoch in 1:3) {
  for (i in 1:5) {
    for (j in 1:2) {
      qsar.adaboost.cv2<-boosting.cv(class~., data = qsar_train_adaboost, 
                                    boos = bootstrap[j], v = 5, 
                                    mfinal = trees_adaboost[i],
                                    coeflearn = "Freund")
      adaboost_cv_error2[i, j]<-adaboost_cv_error[i, j] + qsar.adaboost.cv$error
    }
  }
}
opt_adaboost_par2<-which.min(adaboost_cv_error)
numtree_adaboost2<-trees_adaboost[opt_adaboost_par%%5]
bootstrap_adaboost2<-bootstrap[(opt_adaboost_par-1)%/%5 + 1]
qsar.adaboost2<-boosting(class~., data = qsar_train_adaboost,
                        boos = bootstrap_adaboost, 
                        mfinal = numtree_adaboost)
adaboost_test_predict2<-predict(qsar.adaboost, qsar_test)$class
table(qsar_test$class, adaboost_test_predict2, dnn = c("Truth", "Prediction"))


## 7. Deep Nueral Network
set.seed(6690)
library(keras)
qsar_train_x<-as.matrix(qsar_train[1:41])
qsar_train_y<-as.matrix(qsar_train[42])
qsar_test_x<-as.matrix(qsar_test[1:41])
qsar_test_y<-as.matrix(qsar_test[42])
qsar_train_y<-to_categorical(qsar_train_y, 2)
qsar_test_y<-to_categorical(qsar_test_y, 2)
dropout_rates = c(0, 0.1, 0.2, 0.3, 0.4, 0.5)
cv_totoal_acc<-seq(0, 0, length = 6)
for (epoch in 1:6) {
  print(epoch)
  cv_index<-sample(837, 837)
  for (i in 1:6) {
    for (k in 1:5) {
      cv_test_index<-cv_index[(1+round(167.4*(k-1))):round(167.4*k)]
      qsar.nn.cv<-keras_model_sequential()
      qsar.nn.cv %>%
        layer_dense(units = 400, activation = "relu") %>%
        layer_batch_normalization() %>%
        layer_dropout(dropout_rates[i]) %>%
        layer_dense(units = 200, activation = "relu") %>%
        layer_dropout(dropout_rates[i]) %>%
        layer_dense(units = 2, activation = "softmax")
      qsar.nn.cv %>% compile(loss = "categorical_crossentropy", 
                          optimizer = "adam", metrics = "accuracy")
      qsar.nn.cv %>% fit(qsar_train_x, qsar_train_y, epochs = 60,
                                        batch_size = 100, verbose = 0)
      cv.test<-qsar.nn.cv %>% evaluate(qsar_test_x, qsar_test_y, verbose = 0)
      cv_totoal_acc[i]<-cv_totoal_acc[i] + cv.test[2]
    }
  }
}
dropout_rate = dropout_rates[which.max(cv_totoal_acc)]
qsar.nn<-keras_model_sequential()
qsar.nn %>%
  layer_dense(units = 400, activation = "relu") %>%
  layer_batch_normalization() %>%
  layer_dropout(droupout_rate) %>%
  layer_dense(units = 200, activation = "relu") %>%
  layer_dropout(dropout_rate) %>%
  layer_dense(units = 2, activation = "softmax")
qsar.nn %>% compile(loss = "categorical_crossentropy", 
                    optimizer = "adam", metrics = "accuracy")
nn.train.history<-qsar.nn %>% fit(qsar_train_x, qsar_train_y, epochs = 60,
                              batch_size = 100)
plot(nn.train.history)
test_acc_nn<-qsar.nn %>% evaluate(qsar_test_x, qsar_test_y, verbose = 0)
test_acc_nn[2]


## 8. PCA
library(factoextra)
library(FactoMineR)
re_svm_index<-c(1, 6, 19, 20, 32, 33, 34, 35, 36, 37, 38, 39, 41)
re_knn_index<-c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
qsar.svm.pca<-PCA(qsar_raw[re_svm_index], ncp = 10)
fviz_pca_ind(qsar.svm.pca, axes = c(1, 4), label = "none",
             habillage = as.factor(qsar_raw$class))
fviz_pca_var(qsar.svm.pca, axes = c(1, 4), repel = TRUE, 
             geom.var = "text", fill.var = "red",
             col.var = "black", palette = "npg")
qsar.knn.pca<-PCA(qsar_raw[re_knn_index], ncp = 10)
fviz_pca_ind(qsar.knn.pca, axes = c(1, 4), label = "none",
             habillage = as.factor(qsar_raw$class))
fviz_pca_var(qsar.knn.pca, axes = c(1, 4), repel = TRUE, 
             geom.var = "text", fill.var = "red",
             col.var = "black", palette = "npg")
