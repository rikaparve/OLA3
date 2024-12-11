# Opvarmning

testdf <- data.frame(
  ques=c("Sp1", "Sp2", "Sp3","Sp4"),
  X2021=as.numeric(c(-70, 22, -88, 3)),
  X2022=as.numeric(c(22, 65, -48, 35)),
  X2023=as.numeric(c(20, 25, 30,8))
)


collist <- NULL
col_names <- NULL
collist <- list()
col_names <- list()
n_rows <- 4


### From Mo
set.seed(1234)

ques <- c("Sp1", "Sp2", "Sp3", "Sp4")


X2021 <- sample(-100:100, 4, replace = TRUE)
X2022 <- sample(-100:100, 4, replace = TRUE)
X2023 <- sample(-100:100, 4, replace = TRUE)


testdf <- data.frame(
  ques = ques,
  2021 = X2021,
  2022 = X2022,
  2023 = X2023
)

collist <- list()

testdf_spg <- unique(testdf$ques)

for (i in 1:length(testdf_spg)) {
  combos <- combn(1:4, i, simplify = FALSE)
  collist <- append(collist, combos)
}

test_kombi <- do.call(rbind, collist)

colnames(test_kombi) <- paste("Question", 1:length(testdf_spg))

test_kombi <- as.data.frame(test_kombi, stringsAsFactors = FALSE)



##### Opvarmning fra Nov 1st
data_list <- data.frame(matrix(data=NA, nrow = 10, ncol=10))
for (i in 1:10) {
  for (j in 1:10) {
  result=i*j
  data_list[i, j] <- result
  }
}
