library(timeSeries)
library(quantmod)
rm(list = ls())
getSymbols("BTC-USD", from = '2015-01-01',
           warnings = FALSE,
           auto.assign = TRUE)
plot(`BTC-USD`)

retornos = dailyReturn(`BTC-USD`)
plot(
  as.timeSeries(retornos),
  at = "chic",
  minor.ticks="year",
  mar.multi = c(0.2, 5.1, 0.2, 1.1), oma.multi = c(4, 0, 4, 0),
  col = .colorwheelPalette(1), cex.lab = 0.8, cex.axis = 0.8)

library(ggdist)
library(tidyquant)
library(tidyverse)

retornos %>%
  density()%>%
  plot()

Fx = retornos %>%
  ecdf()

quantile(retornos,seq(0.9,1,length.out=20))

#Ok..quiero retornos diarios del mas del 5%
sum(retornos>(5/100))

#como estan armados los retornos?
head(`BTC-USD`)
head(retornos)

286.18/274.189

#definir Señales de entrada
signal = rep(0,length(retornos))
signal[retornos>(5/100)]<-1
plot(signal,type="l")

#Hay señales de compra q estan muy juntas...
#cual es la minima distancia q tiene q haber
#para q valga la pena
plot(cumsum(signal))
l = sum(retornos[which(signal==1)])
idx = which(signal==1)
hist(diff(idx))
#prrobar con 3 dia de dist
ganancia_con_hold = function(dias){
  extender = idx[c(diff(idx)<dias,FALSE)]
  extender = lapply(extender, function(x){
    seq(x-dias,x)
  })
  extender = unique(do.call(c,extender))
  signal   = sort(unique(c(idx,extender)))
  sum(retornos[signal])
}

ganancia = function(entrar_con=5/100,dias_hold=1){
  signal = rep(0,length(retornos))
  signal[retornos>entrar_con]<-1
  idx = which(signal==1)
  extender = idx[c(diff(idx)<dias_hold,FALSE)]
  extender = lapply(extender, function(x){
    seq(x-dias_hold,x)
  })
  extender = unique(do.call(c,extender))
  out   = sort(abs(unique(c(idx,extender))))
  sum(retornos[out]) - (2/100 * length(out))
}
ganancia(0/100,1)

max(retornos)
df = expand.grid(entrar=seq(0,15,length.out=100)/100,dias=1:4)
df$ganancia = 0
head(df)
for (i in seq_along(df$ganancia)) {
  df$ganancia[i] = ganancia(df$entrar[i],df$dias[i])
}
max(df$ganancia)

df%>%
  ggplot(aes(x=dias,y=entrar,z=ganancia,color=ganancia)) + 
  geom_contour_filled(show.legend = T)

#Hay q entrar con 2.5% y holdear 1 dia como max

#tratamos de maximizar la estrategia con GA

retornos = dailyReturn(`BTC-USD`)
signal   = as.numeric(as.vector(retornos)>0)


f = function(signal){
  cost     = sum(abs(diff(signal)),na.rm = T) * 1/100
  profit   = sum(retornos[signal==1])
  return(profit-cost)
}

library(GA)
#fit1 = ga(type = "binary",nBits = length(retornos),fitness = f,
#   maxiter = 1000,suggestions = as.numeric(retornos>0))

#sum(fit1@solution)
sugestion = "0000111010000010011111111100000110001100001110010111000001111101111111000110011110011110001111100000000011000111100010011110001100000111100111000111000011111111111111100000000111111001110011111000000010001111100000000000111000000011000011110000001111101100000110001111001111111110011111111101111111111110111100100001000110000000111111001111111110011011000011101111110000010011000011111001101100001111001111111111111110011111100000111111011111011110011100011111100001111111111111111101111000111110111110000111111111111111111000111101101100011001001110100010001111110000011001110000111110000000011111110000011100111111100011100000000000000111111111111111111000111001111100110111111000011111101100000111111111111111111111111110011100111110011000111111011110011111111111111100000111111111110111111011001011111000111000111100111100100000000111111000111111111111111111011001111011100011011111110111101001101111100011000111110100010000111101000011011001111110111110100000111111111110001110000000100100001011100111001111111111000011100011001111111101100001111011101011111011111111001110111000000101100001011111000101100001110001001100000100111100101111011001001101111100000100011011110000000000011000110111111001111111010110011110000001100001110000000011111101110000001000111001110001111111110000011111111111100111000000000010110011100011111111001110000001111111001111000011000000111110001111000000000000000011111110011000000000100010111010010001100000011111011101010001100011110000101111000000000000110000011100001111111110100111001111100001111111000001111111111111111011101111111100100011111001111101110001000111110001110011100111111110111111110100011001111001001011110000000001111111011001000111110001001000111110000000110000000000000000011100001110000000001110001110000111111001100000110000000111110000011110001100001000100001110000111110011111001100000001110111000111110110000010011100000001001100000010101111001100000111111110100010001100011111111111111110000111011110011000111101011001111000011000111000001111000011111110000111111111110111101111111101111110000000000111110010110111111110000000111111000011111111111111111111101110011110100111001111110010001110110110000011111111001011111111111011110000110001000101110110011111111101001111111100000001010001111111001100000000111111110110011111100000000000011001100011110010110000010001110001111100000100011100000011101101001110000111000001100111111111100001111110001000011111101010001111111000011011001000110000011111111011110111111110000100110001000111000011000011001000111110001110001"
sugestion = strsplit(sugestion,split = "",fixed = T)
sugestion = as.numeric(sugestion[[1]])
#si cambian la longitud de los retornos
pad = length(retornos)-length(sugestion)
sugestion = c(sugestion,rep(0,pad))
fit2 = ga(type = "binary",nBits = length(retornos),fitness = f,
          maxiter = 100000,run=100,suggestions = sugestion)

sum(fit2@solution)
sum(retornos[fit2@solution==1])
signal = t(fit2@solution)
print(paste(signal,collapse = ""))
#0000111010000010011111111100000110001100001110010111000001111101111111000110011110011110001111100000000011000111100010011110001100000111100111000011000011111111111111100000000111111001110011111000000010001111100000000000111000000011000011110000001111101100000110001111001111111110011111111101111111111110111100100001000110000000111111001111111110011011000011101111110000010011000011111001101100001111001111111111111110011111100000111111011111011110011100011111100001111111111111111101111000111110111110000111111111111111111000111101101100011001001110100010001111110000011001110000111110000000011111110000011100111111100011100000000000000111111111111111111000111001111100110111111000011111101100000111111111111111111111111110011100111110011000111111011110011111111111111100000111111111110111111011001011111000111000111100111100100000000111111000111111111111111111011001111011100011011111110111101001101111100011000111110100010000111101000011011001111110111110100000111111111110001110000000100100001011100111001111111111000011100011001111111101100001111011101011111011111111001110111000000101100001011111000101100001110001001100000100111100101111011001001101111100000100011011110000000000011000110111111001111111010110011110000001100001110000000011111101110000001000111001110001111111110000011111111111100111000000000010110011100011111111001110000001111111001111000011000000111110001111000000001000000011111110011000000100100010111010010001100000011111011101010001100011110000101111000000000000110000011100001111111110100111001111100001111111000001111111111111111011101111111100100011111001111101110001000111110001110011100111111110111111110100011001111001001011110000100001111111011001000111110001001000111110000000110000000000000000011100001110000000001110001110000111111001100000110000000111110000011110001100001000100001110000111110011111001100000001110111000111110110000010011100000001001100000010101111001100000111111110100010001100011111111111111110000111011110011000111101011001111000011000111000001111000011111110000111111111110111101111111101111110000000000111110010110111111110000000111111000011111111111111111111101110011110100111001111110010001110110110000011111111001011111111111011110000110001000101110110011111111101001111111100000001010001111111001100000000111111110110011111100000000000011001100011110010110000010001110001111100000100011100000011101101001110000111000001100111111111100001111110001000011111101010001111111000011011001000110000011111111011110111111110000100110001000111000011000011001000111110001110001
dim(`BTC-USD`)
cbind(retornos,signal)
df = `BTC-USD` %>%
  as.data.frame()%>%
  rownames_to_column()%>%
  left_join(retornos%>%
              cbind(signal)%>%
              as.data.frame()%>%
              rownames_to_column(),
            by="rowname")


#Simular Estrategia Perfecta

capital     = 100
stock       = 0
last_signal = 0
df$action   = "HOLD"
for(i in seq_along(df$rowname)){
  if(is.na(df$signal[i+1])){
    next;
  }
  if(df$signal[i+1]!=last_signal){
    #hay un cambio...
    if(last_signal==0){
      #hay q comprar
      stock   = (capital * 0.99)/df$`BTC-USD.Adjusted`[i]
      capital = 0 
      df$action[i] = "BUY"
    }else{
      #hay q vender
      capital = stock * df$`BTC-USD.Adjusted`[i] * 0.99
      stock   = 0
      df$action[i] = "SELL"
    }
    last_signal = df$signal[i+1]
  }
}

#usar growth??
#growth(df$`BTC-USD.Close`[!is.na(df$`BTC-USD.Close`)],
#       as.numeric(df$action=="BUY")[!is.na(df$`BTC-USD.Close`)])

head(df,n=20)

library(TTR)
#funciones = (ls("package:TTR"))
#write.csv(funciones)

dataset = `BTC-USD`
dataset = dataset[!is.na(dataset$`BTC-USD.High`)]

n = length(dataset$`BTC-USD.Open`)
valids = floor(seq(1,10.99,length.out=n))
table(valids)
fe = dataset[valids==1,]
dim(fe)


armar_fe = function(fe){
  fe_addicional = list()
  O = "BTC-USD.Open"
  H = "BTC-USD.High"
  L = "BTC-USD.Low"
  C = "BTC-USD.Close"
  V = "BTC-USD.Volume"
  fe_addicional[["ADX"]]    = TTR::ADX(fe[,c(H,L,C)])
  fe_addicional[["ALMA"]]   = TTR::ALMA(fe[,C])
  fe_addicional[["arooon"]] = TTR::aroon(fe[,c(H,L)])
  fe_addicional[["ATR"]]     = TTR::ATR(fe[,c(H,L,C)])
  fe_addicional[["BBands"]]=TTR::BBands(fe[,c(H,L,C)])
  fe_addicional[["CCI"]]=TTR::CCI(fe[,c(H,L,C)])
  fe_addicional[["chaikinAD"]]=TTR::chaikinAD(fe[,c(H,L,C)],fe[,V])
  fe_addicional[["chaikinVolatility"]]=TTR::chaikinVolatility(fe[,c(H,L)])
  fe_addicional[["CLV"]]=TTR::CLV(fe[,c(H,L,C)])
  fe_addicional[["CMF"]]=TTR::CMF(fe[,c(H,L,C)],fe[,V])
  fe_addicional[["CMO"]]=TTR::CMO(fe[,C])
  fe_addicional[["CTI"]]=TTR::CTI(fe[,C])
  fe_addicional[["DEMA"]]=TTR::DEMA(fe[,C])
  fe_addicional[["DonchianChannel"]]=TTR::DonchianChannel(fe[,c(H,L)])
  fe_addicional[["DPOp"]]=TTR::DPO(fe[,C])
  fe_addicional[["DPOv"]]=TTR::DPO(fe[,V])
  #fe_addicional[["DVI"]]=TTR::DVI(fe[,C])
  fe_addicional[["EMA"]]=TTR::EMA(fe[,C])
  fe_addicional[["EMV"]]=TTR::EMV(fe[,c(H,L)],fe[,V])
  fe_addicional[["EVWMA"]]=TTR::EVWMA(fe[,C],fe[,V])
  fe_addicional[["GMMA"]]=TTR::GMMA(fe[,C])
  fe_addicional[["HMA"]]=TTR::HMA(fe[,C])
  fe_addicional[["KST"]]=TTR::KST(fe[,C])
  #fe_addicional[["lags"]]=TTR::lags(fe)
  fe_addicional[["MACD"]]=TTR::MACD(fe[,C])
  fe_addicional[["MFI"]]=TTR::MFI(fe[,c(H,L,C)],fe[,V])
  fe_addicional[["momentum"]]=TTR::momentum(fe[,C])
  #fe_addicional[["naCheck"]]=TTR::naCheck(fe)
  fe_addicional[["OBV"]]=TTR::OBV(fe[,C],fe[,V])
  fe_addicional[["PBands"]]=TTR::PBands(fe[,C])
  fe_addicional[["ROC"]]=TTR::ROC(fe[,C])
  fe_addicional[["RSI"]]=TTR::RSI(fe[,C])
  
  fe_addicional[["SAR"]]=TTR::SAR(fe[,c(H,L)])
  fe_addicional[["SMA"]]=TTR::SMA(fe[,c(C)])
  fe_addicional[["SMI"]]=TTR::SMI(fe[,c(C)])
  #fe_addicional[["SNR"]]=TTR::SNR(fe[,c(H,L,C)])
  #fe_addicional[["stoch"]]=TTR::stoch(fe)
  #fe_addicional[["stockSymbols"]]=TTR::stockSymbols(fe)
  fe_addicional[["TDI"]]=TTR::TDI(fe[,C])
  fe_addicional[["TRIX"]]=TTR::TRIX(fe[,C])
  fe_addicional[["ultimateOscillator"]]=TTR::ultimateOscillator(fe[,c(H,L,C)])
  fe_addicional[["VHF"]]=TTR::VHF(fe[,c(H,L,C)])
  #fe_addicional[["VMA"]]=TTR::VMA(fe[,C])
  fe_addicional[["volatility"]]=TTR::volatility(fe[,c(O,H,L,C)])
  fe_addicional[["VWAP"]]=TTR::VWAP(fe[,C],fe[,V])
  fe_addicional[["VWMA"]]=TTR::VWMA(fe[,C],fe[,V])
  #fe_addicional[["wilderSum"]]=TTR::wilderSum(fe)
  fe_addicional[["williamsAD"]]=TTR::williamsAD(fe[,c(H,L,C)])
  fe_addicional[["WMA"]]=TTR::WMA(fe[,C])
  fe_addicional[["WPR"]]=TTR::WPR(fe[,c(H,L,C)])
  fe_addicional[["ZigZag"]]=TTR::ZigZag(fe[,c(H,L)])
  fe_addicional[["ZLEMA"]]=TTR::ZLEMA(fe[,c(C)])
  #lapply(fe_addicional,dim)
  fe_addicional = do.call(cbind,fe_addicional)
  return(fe_addicional)
}

dim(fe)
train = armar_fe(dataset[valids%in%c(1)])
test  = armar_fe(dataset[valids%in%c(1,2)])

train %<>%
  as.data.frame() %>%
  rownames_to_column() %>%
  left_join(df %>% select(rowname,action))

test %<>%
  as.data.frame() %>%
  rownames_to_column() %>%
  left_join(df %>% select(rowname,action))

#Listo xa LGBM
library(lightgbm)
train$rowname = NULL
test$rowname = NULL
train_action = as.numeric(factor(train$action))-1
test_action = as.numeric(factor(test$action))-1
train$action = NULL
test$action = NULL

dtrain <- lgb.Dataset(as.matrix(train), label = train_action)
dtest  <- lgb.Dataset(as.matrix(test) , label = test_action)
validset <- list(train = dtrain, test = dtest)



model <- lgb.cv(
  data = dtrain
  , params = list(
    objective = "multiclass"
    , valids = valids
    , num_class = 3L
    , eval = "multi_error"
    , nfold = 5L
  )
)
for(b in model$boosters){
  #str(b)
  print(lgb.importance(b[[1]]))
}


bst = model$boosters[[2]]$booster

# information can be extracted from lgb.Dataset using get_field()
label <- get_field(dtest, "label")
pred <- predict(bst, as.matrix(test),reshape = T)
for (i in 1:3) {
  rpred = ROCR::prediction(pred[,i],label==(i-1))
  rperf = ROCR::performance(rpred,"tpr","fpr")
  auc   = ROCR::performance(rpred,"auc")
  print(auc@y.values)
  plot(rperf)
}

head(pred)

boxplot(pred[,1]~label==0)

test2  = armar_fe(dataset[valids%in%c(2,3,4,5,6,8,9,10)])

test2 %<>%
  as.data.frame() %>%
  rownames_to_column() %>%
  left_join(df %>% select(rowname,action))

test2$rowname = NULL
test2_action = as.numeric(factor(test2$action))-1
test2$action = NULL

pred <- predict(bst, as.matrix(test2),reshape = T)
for (i in 1:3) {
  rpred = ROCR::prediction(pred[,i],test2_action==(i-1))
  rperf = ROCR::performance(rpred,"tpr","fpr")
  auc   = ROCR::performance(rpred,"auc")
  print(auc@y.values)
  plot(rperf)
}

tail(pred)
#entrar 2021-12-13 con 48620

#err <- as.numeric(sum(as.integer(pred > 0.5) != label)) / length(label)
#print(paste("test-error=", err))