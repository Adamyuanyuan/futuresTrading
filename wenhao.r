##############################################################################
# @Autuor: Adam.Wang 13210240112@fudan.edu.cn
# @Date: 2015.3.1
#
# 这是一个（适用于黄金白银的）期货交易系统，想法来自于文豪：详情看 ：测试.docx
# 
# input: 1. 交易数据文件 第一列：时间 第二列： 残差 
# output: 好多好多数据，文豪给我的，不想写了,代码里清清楚楚 
# 使用方法：eg: modelOfTransaction("ptag1h11.txt")
##############################################################################

##############################################################################
# @Autuor: Adam.Wang 13210240112@fudan.edu.cn
# @Date: 2015.3.16
# 
# 1. 解决了最后一次必须平仓的bug
# 2. 解决了最后一次必须不能建仓的bug
# 3. 可以将结果直接导入txt
# 4. 解决了数据为0的时候的bug
# @Date: 2015.3.22
# 5. 解决了最后时刻只考虑止损的情况，因为也有可能是会有收益
##############################################################################

modelOfdata <- function(data){

    jianCangThreshold <- 1.8; #建仓阈值参数等
    zhiSunThreshold <- 2.0;
    pingCangThreshold <- 0.2

    standardDev <-  sd(data[,2]); # 标准差
    jianCang <- jianCangThreshold * standardDev; # 建仓阈值
    zhiSun <- zhiSunThreshold * standardDev; # 止损水平
    pingCang <- pingCangThreshold * standardDev; # 平仓阈值
    rowLength <- nrow(data);
    isGuanzhu <- FALSE;
    isZhiSun <- FALSE;
    isJianCang <- FALSE;
    isPingCang <- FALSE;

    ###################################################
    #定义一个数组，有三列，默认为0，第几行代表第几次交易
    #1：建仓的时间点，2：建仓时的残差，
    #3：平仓时的时间点，4：平仓时的残差
    #5：持有期(3-1)  ，6：收益 (|2-4|)
    #7：止损时的时间点 ，8：发生止损时的残差 
    #9：止损期(7-1)， 10：亏损(-|2-8|)
    ####################################################
    arrayTime <- array(0,dim=c(20,10)); 
    # 为了解决如果矩阵为0行情况下的bug，在此决定让矩阵前两行为0，这不是最优美的方法，但是简单实用
    j <- 2;
    for(i in seq(1:rowLength)){
        if(!isGuanzhu){
            if(data[i,2] >= jianCang){
                cat("开始关注：", i,"\n");
                isGuanzhu <- TRUE;
            }
            
        }else{
            if(!isJianCang){

                # 如果是最后一次数据，则无论如何皆不建仓
                if((data[i,2] <= jianCang) && (i != rowLength)){
                    j <- j+1;
                    isJianCang <- TRUE;
                    cat("开始第",j-2,"次建仓:","时间：",i,"残差：",data[i,2],"\n");
                    arrayTime[j,1] <- i; #建仓时间点保存
                    arrayTime[j,2] <- data[i,2]; #建仓残差保存
                }

            }else{
                # 如果是最后一次数据，如果此时已经建仓，并且当前的残差大于建仓时的残差，则无论如何皆平仓
                if((data[i,2] <= pingCang) || ((i == rowLength) && (data[i,2] >= array[j,4]))){
                    isGuanzhu <- FALSE;
                    isJianCang <- FALSE;
                    arrayTime[j,3] <- i; #平仓时间点
                    arrayTime[j,4] <- data[i,2]; #平仓残差
                    arrayTime[j,5] <- arrayTime[j,3] - arrayTime[j,1]; # 持有期
                    arrayTime[j,6] <- abs(arrayTime[j,2] - arrayTime[j,4]); # 收益
                    cat("开始第",j-2,"次平仓:","时间：",i,"残差：",data[i,2],"\n")
                }else{

                    # 如果是最后一次数据，如果此时已经建仓，并且当前的残差小于建仓时的残差，则无论如何皆止损
                    if((data[i,2] >= zhiSun) || ((i == rowLength) && (data[i,2] <= array[j,4]))){
                        isGuanzhu <- FALSE;
                        isJianCang <- FALSE;
                        arrayTime[j,7] <- i; # 止损时间点
                        arrayTime[j,8] <- data[i,2]; #止损残差
                        arrayTime[j,9] <- arrayTime[j,7] - arrayTime[j,1]; #止损期
                        arrayTime[j,10] <- -abs(arrayTime[j,2] - arrayTime[j,8]);
                        cat("开始第",j-2,"次止损:","时间：",i,"残差：",data[i,2],"\n")
                    }
                }

            }
            

        }

    }
    cat("交易次数：", j-2,"\n");

    return(arrayTime[1:j,]);

}


modelOfTransaction <- function(fileName){
    file <- read.table(fileName);

    arrayTime1 <- modelOfdata(file);
    # cat("正残差处理结果：\n");
    # print(arrayTime1);    

    reversedData <- file; #将数据取反之后再处理一遍
    reversedData[,2] <- -file[,2];
    
    arrayTime2 <- modelOfdata(reversedData);
    # cat("负残差处理结果：\n");
    # print(arrayTime2);

    arrayTime2[,c(2,4,8)] <- -arrayTime2[,c(2,4,8)];

    arrayTimeTotal <- rbind(arrayTime1,arrayTime2);

    
    print("融合后的结果："); 
    print(arrayTimeTotal);

    transNum <- nrow(arrayTimeTotal) - 4;
    pingCangNum <- length(arrayTimeTotal[,3][arrayTimeTotal[,3]>0]);#成功交易的数量
    zhisunNum <- length(arrayTimeTotal[,7][arrayTimeTotal[,7]>0]); # pingCangNum + zhisunNum <= transNum
    averageTransTime <- sum(arrayTimeTotal[,5])/pingCangNum;
    averageZhiSunTime <- sum(arrayTimeTotal[,9])/zhisunNum;
    totalRevenue <- sum(arrayTimeTotal[,6]);
    totalLoss <- sum(arrayTimeTotal[,10]); #在此为负数
    grossProfit <- totalRevenue + totalLoss;
    

    cat("————————————————最终结果：————————————————\n");
    cat("交易次数：",transNum,"成功交易次数：",pingCangNum,"平均成功交易时间：",averageTransTime,"总收益：",totalRevenue,"\n");
    cat("止损交易次数：",zhisunNum,"平均止损交易时间：",averageZhiSunTime,"总亏损",totalLoss,"\n");
    cat("总利润：",grossProfit,"\n");
    cat(totalRevenue,totalLoss,grossProfit);
    cat("\n——————————————————— O(∩_∩)O ——————————————————\n");

    # result 输出到文件

    resultFileName <- file("futuresTradingResult.txt","a"); #append file
    cat(paste(fileName,"\t totalRevenue_totalLoss_grossProfit: \t",sep=" "),file=resultFileName);
    cat(totalRevenue,totalLoss,grossProfit,file=resultFileName);
    cat("\n",file=resultFileName);

    close(resultFileName);  
}
