digitsum <- function(x) sum(floor(x / 10^(0:(nchar(x) - 1))) %% 10)

HN=function(x){
  flag1=!grepl("\\D", x)
  flag2=nchar(x)==10
  if ((flag1 & flag2 )==FALSE)
  {print ('error')}
  else
  {
    dig=NULL
    for (i in 1:10){
      dig[i]=x%/%10^(10-i)
      x=x%%(10^(10-i))
    }
    dig1=dig[1]*2
    dig3=dig[3]*2
    dig5=dig[5]*2
    dig7=dig[7]*2
    dig9=dig[9]*2
    dig11=digitsum(dig1)
    dig33=digitsum(dig3)
    dig55=digitsum(dig5)
    dig77=digitsum(dig7)
    dig99=digitsum(dig9)
    sum1=dig11+dig33+dig55+dig77+dig99+dig[2]+dig[4]+dig[6]+dig[8]
    cal_num=10-sum1%%10
    
    if (cal_num==dig[10]) {
      print ("valid")
    } else {
      print ("NOT valid")
    }
  }
}