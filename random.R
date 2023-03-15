genNum <- function (fixedNum, n) {
  # fixedNum <- as.numeric(readline('nhập số: '))
  if (round(fixedNum*max(n)) != fixedNum*max(n)) {
    stop('Thông tin nhập không đúng')
  }
  
  if (is.na(fixedNum)) {
    stop('Thông tin nhập không đúng')
  }
  
  if (fixedNum > 10) {
    stop('Thông tin nhập không đúng')
  }
  
  firstNum <- sample(seq(0, 4*n[1], 1), 1)
  secondNum <- sample(seq(0, 1*n[2], 1), 1)
  thirdNum <- sample(seq(0, 1*n[3], 1), 1)
  fourthNum <- sample(seq(0, 4*n[4], 1), 1)
  
  total <- 
    firstNum*max(n)/n[1] + secondNum*max(n)/n[2] + thirdNum*max(n)/n[3] + fourthNum*max(n)/n[4]
  
  dif <-  total - fixedNum * max(n)
  
  while (dif != 0) {
    if (dif > 0) {
      ranNum <- sample(1:4, 1)
      if (ranNum == 1) {
        if (firstNum == 0) next
        firstNum <- firstNum - 1
        dif <- dif - max(n)/n[1]
      } else if (ranNum == 2) {
        if (secondNum == 0) next
        secondNum <- secondNum - 1
        dif <- dif - max(n)/n[2]
      } else if (ranNum == 3) {
        if (thirdNum == 0) next
        thirdNum <- thirdNum - 1
        dif <- dif - max(n)/n[3]
      } else if (ranNum == 4) {
        if (fourthNum == 0) next
        fourthNum <- fourthNum - 1
        dif <- dif - max(n)/n[4]
      }
    } else if (dif < 0) {
      ranNum <- sample(1:4, 1)
      if (ranNum == 1) {
        if (firstNum == 4*n[1]) next
        firstNum <- firstNum + 1
        dif <- dif + max(n)/n[1]
      } else if (ranNum == 2) {
        if (secondNum == n[2]) next
        secondNum <- secondNum + 1
        dif <- dif + max(n)/n[2]
      } else if (ranNum == 3) {
        if (thirdNum == n[3]) next
        thirdNum <- thirdNum + 1
        dif <- dif + max(n)/n[3]
      } else if (ranNum == 4) {
        if (fourthNum == 4*n[4]) next
        fourthNum <- fourthNum + 1
        dif <- dif + max(n)/n[4]
      }
    }
  }
  
  return(c(
    'firstNum' = firstNum/n[1],
    'secondNum' = secondNum/n[2],
    'thirdNum' = thirdNum/n[3],
    'fourthNum' = fourthNum/n[4]
  ))
  
  # print(sum(c(
  #   'firstNum' = firstNum/n[1],
  #   'secondNum' = secondNum/n[2],
  #   'thirdNum' = thirdNum/n[3],
  #   'fourthNum' = fourthNum/n[4]
  # )))
}

library(readxl)

dat <- read_excel(file.choose())

for (i in 1:nrow(dat)) {
  num <- genNum(dat[i,2], c(5, 5, 5, 10))
  dat[i, 'Điểm 1'] <- num[1]
  dat[i, 'Điểm 2'] <- num[2]
  dat[i, 'Điểm 3'] <- num[3]
  dat[i, 'Điểm 4'] <- num[4]
}

dat[, 'tổng'] <- dat$`Điểm 1` +dat$`Điểm 2` +dat$`Điểm 3` + dat$`Điểm 4`

writexl::write_xlsx(dat, 'Điểm tp_1.xlsx')
