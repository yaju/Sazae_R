#サザエさんのじゃんけん R言語によるデータ分析入門

###################################################
#■サザエさんのじゃんけん学から過去データを取得する
library(RCurl)
txtfile <- iconv(getURL("http://www.asahi-net.or.jp/~tk7m-ari/sazae_ichiran.html", .encoding="Shift-JIS"), "Shift-JIS","UTF-8")
txtvec <- strsplit(txtfile,'\n')[[1]]
flg <- FALSE
i <- 1
while(i > 0) {
  tag <- substr(txtvec[i],1,1)
  if(tag == '<' || tag == ''){ i <- i + 1; next }
  str <- strsplit(txtvec[i],'\t')[[1]]
  tmp <- regexpr('\\d+',str[1])
  seq <- as.integer(substr(str[1],tmp,tmp+attr(tmp,'match.length')-1))
  if(seq == 7){ str[2] <- '91.11.30'}
  dt <- as.Date(str[2],format='%y.%m.%d')
  tmp2 <- regexpr('<|（',paste(str[3],'<'))
  str2 <- substr(str[3],0,tmp2-1)
  idx <- 9
  kind <- '-'
  if(str2 == 'グー') { kind <- 'G'; idx=1 }
  if(str2 == 'チョキ'){ kind <- 'C'; idx=2 } 
  if(str2 == 'パー'){ kind <- 'P'; ; idx=3 } 
  if(flg == FALSE){
    tbl <- data.frame(seq,dt,kind,idx)
    flg <- TRUE
  }
  else{
    tbl <- rbind(tbl,data.frame(seq,dt,kind,idx))  
  }
  #print(seq)  
  i <- i + 1
  if(seq == 1) break
}

###################################################
#過去データの手の総数を取得
sumtbl <- sqldf("SELECT kind,COUNT(kind) cnt,idx FROM tbl GROUP BY kind ORDER BY idx")

###################################################
#手の総数の棒グラフ作成
library(ggplot2)
p <- ggplot(sumtbl, aes(x=reorder(kind, idx), y=cnt))
p + geom_bar(stat="identity", aes(fill=reorder(kind, idx))) + xlab("手") + ylab("件数") + labs(fill = "手")

###################################################
#年別種類別の棒グラフ作成　2004～2013
year = function(data){ data$dt <- format(data$dt, "%Y"); data }
ytbl <- sqldf("SELECT * FROM tbl", method = function(x)
  data.frame(year(tbl)))
yeartbl <- sqldf("SELECT kind,dt,COUNT(kind) cnt,idx FROM ytbl WHERE kind<>'-' AND dt BETWEEN 2004 AND 2013 GROUP BY kind,dt ORDER BY dt,kind")
p <- ggplot(yeartbl, aes(x=reorder(kind, idx), y=cnt))
p + geom_bar(stat="identity", aes(fill=reorder(kind, idx))) + facet_grid(. ~ dt)+ xlab("手") + ylab("件数") + labs(fill = "手")


###################################################
#■じゃんけんの癖の分析　

#癖の分析用の10年間データ(休み除く)　
rm(ytblptn)
ytblptn <- sqldf("SELECT seq,kind,dt,idx FROM ytbl WHERE kind<>'-' AND dt BETWEEN 2004 AND 2013 ORDER BY seq")

###################################################
#癖の分析 前手との関連性　関数作成
createPattentbl = function(data){
  dfptn <- data.frame(matrix(0,nrow=10,ncol=13))
  colnames(dfptn) <- c('year','win','lose','draw','GP','CG','PC','GC','CP','PG','GG','CC','PP')
  
  i <- 0;
  y <- 0;
  oldYear <- 0;
  row <- nrow(data);
  while(1) {
    i <- i + 1;
    if(i == row + 1) break;
    
    if(i == 1){ prev <- data$kind[i]; next; }
    if(oldYear != data$dt[i]){ 
      y <- y + 1;
      oldYear <- data$dt[i];
      dfptn[y,'year'] <- oldYear;
    }
    
    cur <- data$kind[i];
    ptn <- paste(prev,cur,sep="");
    
    dfptn[y,ptn] <- dfptn[y,ptn] + 1;
    if(ptn == 'GC' || ptn == 'CP'|| ptn == 'PG'){ dfptn[y,'lose'] <- dfptn[y,'lose'] + 1; } 
    if(ptn == 'GP' || ptn == 'CG'|| ptn == 'PC'){ dfptn[y,'win'] <- dfptn[y,'win'] + 1; } 
    if(prev == cur){ dfptn[y,'draw'] <- dfptn[y,'draw'] + 1; }
    prev <- cur;
  }
  
  dfptn; #戻り値
}

###################################################
#癖の分析 前手との関連性　円グラフ作成
ptndata <- createPattentbl(ytblptn);
#2パターンの合計
sumptn <- colSums(ptndata[,-1])

#円グラフ 総合
allptn <- data.frame(kind=c("win", "lose","draw"), val=c(sumptn["win"],sumptn["lose"],sumptn["draw"]))
p <- ggplot(data.frame(allptn ), aes(x="", y = val, fill = kind)) 
p + geom_bar(width = 1, stat = "identity" ) + coord_polar("y")+ xlab("") + ylab("総合") + labs(fill = "種類") 
#円グラフ 勝手
winptn <- data.frame(kind=c("GP", "CG","PC"), val=c(sumptn["GP"],sumptn["CG"],sumptn["PC"]))
p <- ggplot(data.frame(winptn ), aes(x="", y = val, fill = kind)) 
p + geom_bar(width = 1, stat = "identity" ) + coord_polar("y")+ xlab("") + ylab("勝手") + labs(fill = "種類") 
#円グラフ 負手
loseptn <- data.frame(kind=c("GC", "CP","PG"), val=c(sumptn["GC"],sumptn["CP"],sumptn["PG"]))
p <- ggplot(data.frame(loseptn ), aes(x="", y = val, fill = kind)) 
p + geom_bar(width = 1, stat = "identity" ) + coord_polar("y")+ xlab("") + ylab("負手") + labs(fill = "種類") 
#円グラフ 同手
drawptn <- data.frame(kind=c("GG", "CC","PP"), val=c(sumptn["GG"],sumptn["CC"],sumptn["PP"]))
p <- ggplot(data.frame(drawptn ), aes(x="", y = val, fill = kind)) 
p + geom_bar(width = 1, stat = "identity" ) + coord_polar("y")+ xlab("") + ylab("同手") + labs(fill = "種類") 

###################################################
#癖の分析 三手の組み合わせ　関数作成
createPatten2tbl = function(data){
  dfptn <- data.frame(matrix(0,nrow=10, ncol=28))
  colnames(dfptn) <- c('year','GGG','GGC','GGP','GCG','GCC','GCP','GPG','GPC','GPP',
                       'CGG','CGC','CGP','CCG','CCC','CCP','CPG','CPC','CPP',
                       'PGG','PGC','PGP','PCG','PCC','PCP','PPG','PPC','PPP')
  i <- 0;
  y <- 0;
  oldYear <- 0;
  row <- nrow(data);
  while(1) {
    #次の後
    i <- i + 1;
    if(i == row - 2) break;
    if(oldYear != data$dt[i]){ 
      y <- y + 1;
      oldYear <- data$dt[i];
      dfptn[y,'year'] <- oldYear;
    }
    ptn <- paste(data$kind[i],data$kind[i+1],data$kind[i+2],sep="");
    dfptn[y,ptn] <- dfptn[y,ptn] + 1;
  }
  
  dfptn; #戻り値
}

###################################################
#癖の分析 三手の組み合わせ
ptndata2 <- createPatten2tbl(ytblptn);
#3パターンの合計
sumptn2 <- colSums(ptndata2[,-1])

#合計の多い順に並べてみる
sumptndf <- data.frame(kind=names(sumptn2),cnt=sumptn2) 
sumptndf <- sqldf("SELECT * FROM sumptndf ORDER BY cnt desc")
#トップ10
top10 <- t(as.matrix(head(sumptndf,10)))

#同手が続いた場合
sumptndf2 <- sqldf("SELECT * FROM sumptndf WHERE kind IN('GGG','GGC','GGP','CCG','CCC','CCP','PPG','PPC','PPP') ORDER BY cnt desc")
sameptn <- as.data.frame(t(as.matrix(sumptndf2)))
colnames(sameptn) <- sumptndf2$kind
sameptn <- sameptn[-1,]

#円グラフ　同手後の手
xy <- sqldf("SELECT GGG+CCG+PPG AS G,GGC+CCC+PPC AS C,GGP+CCP+PPP AS P FROM sameptn")
drawxy <- data.frame(kind=c("G", "C","P"),val=c(xy$G[1],xy$C[1],xy$P[1]))
p <- ggplot(data.frame(drawxy), aes(x="", y = val, fill = kind)) 
p + geom_bar(width = 1, stat = "identity" ) + coord_polar("y")+ xlab("") + ylab("同手後の手") + labs(fill = "種類") 

###################################################
#癖の分析 同手の間隔と順序  関数作成
createDrawtbl = function(data){
  i <- 0;
  oldYear <- 0;
  oldkind <- '';
  flg <- FALSE;
  sameflg <- FALSE;
  row <- nrow(data);
  first <- FALSE; 
  while(1) {
    i <- i + 1;
    if(i == row - 1) break;
    
    if(oldYear != data$dt[i]){ 
      oldYear <- data$dt[i];
      oldkind <- data$kind[i];
      sameflg <- FALSE;
      first <- TRUE;
      cnt <- -99;
    }
    
    if(first == FALSE && oldkind == data$kind[i]){
      if(sameflg == FALSE){
        j <- i - 1;
        sameflg <- TRUE;
      }
    }
    else{
      if(sameflg){
        seq <- data$seq[j];
        dt <- data$dt[j];
        kind <- data$kind[j];
        print(seq);
        if(cnt < 0) cnt <- -1;
        if(flg == FALSE){
          tmptbl <- data.frame(seq,dt,kind,cnt);
          flg = TRUE;
        }  
        else{
          tmptbl <- rbind(tmptbl,data.frame(seq,dt,kind,cnt));
        }
        sameflg <- FALSE;
        cnt <- 0;
      }
      else{
        cnt <- cnt + 1;
      }
      oldkind <- data$kind[i];
    }
    first <- FALSE;
  }
  
  tmptbl; #戻り値
}  

#癖の分析 同手の間隔と順序
drawtbl <- createDrawtbl(ytblptn)
sumptndf3 <- sqldf("SELECT cnt,COUNT(cnt) AS spccnt FROM drawtbl WHERE cnt >= 0 GROUP BY cnt")
spccnttbl <- t(as.matrix(sumptndf3))

#同手の中での２パターン組み合わせ
sameptndf <- createPattentbl(drawtbl);
#2パターンの合計
samesumptn <- t(as.matrix(colSums(sameptndf[,-1])))

#10年分の手の出現率
sumtbl2 <- sqldf("SELECT kind,COUNT(kind) cnt,idx FROM ytblptn GROUP BY kind ORDER BY idx")
drawtbl2 <- sqldf("SELECT kind,COUNT(kind) cnt FROM drawtbl GROUP BY kind")

###################################################
#■人間乱数の分析

###################################################
#コンピュータ乱数生成　関数作成
createRandtbl = function(data){
  i <- 0;
  oldYear <- 0;
  flg <- FALSE;
  hand <- c('G','C','P');
  row <- nrow(data);
  while(1) {
    i <- i + 1;
    if(i == row + 1) break;
    
    if(oldYear != data$dt[i]){ 
      y <- 1;
      oldYear <- data$dt[i];
      rnd <- floor(runif(52, 0, 14))
    }
    
    seq <- data$seq[i];
    dt <- data$dt[i];
    idx <- rnd[y] %% 3 + 1;
    kind <- hand[idx];
    
    if(flg == FALSE){
      tmptbl <- data.frame(seq,dt,kind,idx);
      flg = TRUE;
    }  
    else{
      tmptbl <- rbind(tmptbl,data.frame(seq,dt,kind,idx));
    }
    
    y <- y + 1;
  }
  
  tmptbl; #戻り値
}

#コンピュータ乱数生成 10年データ分
randtbl <- createRandtbl(ytblptn);
#乱数手の総数
rndsumtbl <- sqldf("SELECT kind,COUNT(kind) cnt,idx FROM randtbl GROUP BY kind ORDER BY idx")
#乱数手の同手の総数
rnddrawtbl <- createDrawtbl(randtbl);
rnddrawtbl2 <- sqldf("SELECT kind,COUNT(kind) cnt FROM rnddrawtbl GROUP BY kind")
#癖の分析 同手の間隔と順序
rndsumptndf <- sqldf("SELECT cnt,COUNT(cnt) AS spccnt FROM rnddrawtbl WHERE cnt >= 0 GROUP BY cnt")
rndspccnttbl <- t(as.matrix(rndsumptndf))

###################################################
#癖の分析 前手との関連性　円グラフ作成
rndptn <- createPattentbl(randtbl);
rndsumptn <- colSums(rndptn[,-1])
allptn <- data.frame(kind=c("win", "lose","draw"), val=c(rndsumptn["win"],rndsumptn["lose"],rndsumptn["draw"]))
p <- ggplot(data.frame(allptn ), aes(x="", y = val, fill = kind)) 
p + geom_bar(width = 1, stat = "identity" ) + coord_polar("y")+ xlab("") + ylab("総合") + labs(fill = "種類")

###################################################
#癖の分析 三手の組み合わせ
rndptn2 <- createPatten2tbl(randtbl);
#合計の多い順に並べてみる
rndsumptn2 <- colSums(rndptn2[,-1])
rndsumptndf <- data.frame(kind=names(rndsumptn2),cnt=rndsumptn2) 
rndsumptndf <- sqldf("SELECT * FROM rndsumptndf ORDER BY cnt desc")
rndttop10 <- t(as.matrix(head(rndsumptndf,10)))

#同手が続いた場合
rndsumptndf2 <- sqldf("SELECT * FROM rndsumptndf WHERE kind IN('GGG','GGC','GGP','CCG','CCC','CCP','PPG','PPC','PPP') ORDER BY cnt desc")
rndsameptn <- as.data.frame(t(as.matrix(rndsumptndf2)))
colnames(rndsameptn) <- rndsumptndf2$kind
rndsameptn <- rndsameptn[-1,]


###################################################
#■次の手の予測と勝敗結果

###################################################
#次手の勝手の予測 関数作成
#チョキが多いので、グー ＞ チョキ ＞ パーの優先順位とする
#前回と違う手を出すので、上記の優先順位で勝手を選ぶ
#二手前と一手前が違う手なら、残りの手を出すので勝手を選ぶ
#三手の中に同手がある場合、 残りの手を出すので勝手を選ぶ
#二手前と一手前が同じ手なら、勝手を出すので負手を選ぶ
getGuess = function(fstkind,sndkind,thrkind){
  
  guess <- 'G';
  
  if(fstkind == 'G') guess <- 'C';
  if(fstkind == 'C') guess <- 'G';
  if(fstkind == 'P') guess <- 'G';
  
  #2手前が在る場合
  if(sndkind != ''){
    if(sndkind != fstkind){
      #違う組み合わせ　残りの手が出ると予想するので残りの手の勝手にする
      ptn <- paste(fstkind,sndkind,sep="");
      if(ptn == 'GC' || ptn == 'CG') guess <- 'C'; #Pの予想でCにする
      if(ptn == 'CP' || ptn == 'PC') guess <- 'P'; #Gの予想でPにする
      if(ptn == 'PG' || ptn == 'GP') guess <- 'G'; #Cの予想でGにする
    }
    else{
      #同一なら勝手と予想するので負手にする
      if(fstkind == 'G') guess <- 'C';　#Pの予想でCにする
      if(fstkind == 'C') guess <- 'P';　#Gの予想でPにする
      if(fstkind == 'P') guess <- 'G';　#Cの予想でGにする
    }
    
    #3手前が在る場合
    if(thrkind != ''){
      #違う組み合わせ　残りの手が出ると予想するので残りの手の勝手にする
      ptn <- paste(fstkind,sndkind,sep="");
      if(ptn == 'GC' || ptn == 'CG') guess <- 'C'; #Pが出るのでCにす
      ptn <- paste(fstkind,sndkind,thrkind,sep="");
      if(ptn == 'GCG' || ptn == 'CGC') guess <- 'C';　#Pの予想でCにする
      if(ptn == 'CPC' || ptn == 'PCP') guess <- 'P';　#Gの予想でPにする
      if(ptn == 'PGP' || ptn == 'GPG') guess <- 'G';　#Cの予想でGにする
      if(ptn == 'GGC' || ptn == 'CCG' || ptn == 'GCC' || ptn == 'CGG') guess <- 'C'; #Pの予想でCにする
      if(ptn == 'CCP' || ptn == 'PPC' || ptn == 'PCC' || ptn == 'CPP') guess <- 'P'; #Gの予想でPにする
      if(ptn == 'PPG' || ptn == 'GGP' || ptn == 'GPP' || ptn == 'PGG') guess <- 'G'; #Cの予想でGにする
    }
  }
  
  guess; #戻り値
}

#勝敗 関数作成
getFight = function(kind,guess){
  
  ptn <- paste(kind,guess,sep="");
  if(ptn == 'GP' || ptn == 'CG' || ptn == 'PC'){
    result <- 'win'
  }
  else{
    if(kind == guess) result <- 'draw' else result <- 'lose'
  }

  result; #戻り値
}  

#10年分の過去データとの勝敗
i <- 0;
oldYear <- 0;
flg <- FALSE;
row <- nrow(ytblptn);
while(1) {
  i <- i + 1;
  if(i == row - 1) break;
  
  if(oldYear != ytblptn$dt[i]){ 
    oldYear <- ytblptn$dt[i];
    first <- TRUE;
    fstkind <- '';  
    sndkind <- ''
    thrkind <- ''
  }
  
  seq <- ytblptn$seq[i];
  dt <- ytblptn$dt[i];
  kind <- ytblptn$kind[i];

  #次の手の勝手を取得
  guess <- getGuess(fstkind,sndkind,thrkind);
  fight <- getFight(kind,guess);
  
  if(flg == FALSE){
    fighttbl <- data.frame(seq,dt,kind,guess,fight);
    fstkind <- kind;
    flg = TRUE;
  }  
  else{
    thrkind <- sndkind; 
    sndkind <- fstkind;
    fighttbl <- rbind(fighttbl,data.frame(seq,dt,kind,guess,fight));
    fstkind <- kind;
  }

  first <- FALSE;
}

#年別の勝率計算
fightcnt <- sqldf("SELECT dt,fight,COUNT(fight) AS cnt FROM fighttbl GROUP BY dt,fight ORDER BY dt")

i <- 0;
oldYear <- 0;
flg <- FALSE;
row <- nrow(fightcnt);
while(1) {
  i <- i + 1;
  if(i == row - 1) break;
  
  if(oldYear != fightcnt$dt[i]){ 
    oldYear <- fightcnt$dt[i];
    year <- oldYear;
    draw <- fightcnt$cnt[i];
    lose <- fightcnt$cnt[i+1];
    win <- fightcnt$cnt[i+2];
    rate <- round(win / (win + lose),2);
  
    if(flg == FALSE){
      ratetbl <- data.frame(year,win,lose,draw,rate);
      flg = TRUE;
    }  
    else{
      ratetbl<- rbind(ratetbl,data.frame(year,win,lose,draw,rate));
    }
  }
}




