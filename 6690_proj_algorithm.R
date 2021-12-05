qsar_raw<-read.csv2("C:/Users/asus/Desktop/EECSE6690/Project/biodeg.csv",header = FALSE);
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
    qsar.re.svm.cv<-svm(class~C_026 + F02_C_N + nArNO2+nCrt + nHDon + 
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


## KNN_Paper_Version
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

## PLSDA_Paper_Version
library(mixOmics)
set.seed(6690)
re_plsda_index<-c(1, 5, 8, 10, 13, 14, 15, 16, 17, 18, 19, 20,
                  21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31)
train_x_plsda<-as.matrix(qsar_train[re_plsda_index])
test_x_plsda<-as.matrix(qsar_test[re_plsda_index])
qsar.re.plsda<-plsda(train_x_plsda, qsar_train$class, ncomp = 2)
predict(qsar.re.plsda, test_x_plsda)$class
