ZAJECIA 5 - DZREWA DECYZYJNE, DRZEWA REGRESJI, OCENA MODELI

1. wczytywanie danych
dane <- read.csv2('https://raw.githubusercontent.com/kaftanowicz/sgh_ird_2017/bae80011e646b910bab577b87019edc2aad67379/data/winequality-white.csv',  
stringsAsFactors = FALSE, dec = '.')

2. Eksploracja danych
head(dane)
str(dane)
summary(dane)
hist(dane$quality)

DRZEWA REGRESYJNE

1. Inicjalizacja ziarna
set.seed(1)

2. Podział na zbiór treningowy i uczący się
train_proportion <- 0.7
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane[!train_index,]

3. Regresja liniowa
lin_m <- lm(quality ~ ., data = train)

4. Drzewo regresji liniowej
d.regr <- rpart(quality ~., data = train, cp = 0.01)
#plot(d.regr, margin = 0.2)
#text(d.regr, pretty = 0)
rpart.plot(d.regr, under=FALSE, fallen.leaves = FALSE, cex = 0.9)

5. Większe drzewo regresji
d.regr.duze <- rpart(quality ~. , data = train, cp = 0.003)
#plot(d.regr.duze, margin = 0.2)
#text(d.regr.duze, pretty = 0)
rpart.plot(d.regr.duze, under=FALSE, fallen.leaves = FALSE, cex = 0.5)

min.error <- which.min(d.regr.duze$cptable[,"xerror"])
opt.cp <- d.regr.duze$cptable[min.error,"CP"]

plotcp(d.regr.duze)
points(min.error, d.regr.duze$cptable[min.error, "xerror"], pch = 19, col = "red")

d.regr.przyciete <- prune.rpart(d.regr.duze, cp = opt.cp)
#plot(d.regr.przyciete, margin = 0.2)
#text(d.regr.przyciete, pretty = 0)
rpart.plot(d.regr.przyciete, under=FALSE, fallen.leaves = FALSE, cex = 0.7)

6. Variable Importance, RSS, MAE, RMSE, RAE, RRSE, R^2

varImp(lin_m)
d.regr$variable.importance
d.regr.przyciete$variable.importance


ZADANIE 1
Napisz funkcje, ktora na podstawie macierzy klasyfikacji oblicza i zwraca
3-elementowa nazwana liste zawierajaca informacje o accuracy, sensitivity i specificity modelu.
  # Sciagawka: https://en.wikipedia.org/wiki/Sensitivity_and_specificity#Confusion_matrix
  
EvaluateModel <- function(classif_mx)
{  true_positive <- classif_mx[1,1]
  true_negative <- classif_mx[2,2]
  condition_positive <- sum(classif_mx[ ,1])
  condition_negative <- sum(classif_mx[ ,2])
  accuracy <- (true_positive + true_negative) / sum(classif_mx)
  sensitivity <- true_positive / condition_positive
  specificity <- true_negative / condition_negative
  return(list(accuracy = accuracy, 
              sensitivity = sensitivity,
              specificity = specificity)) }
              
 ZADANIE 2
a) Wczytaj dane o czerwonych winach (plik "winequality-red.csv"). Zamien wartosc zmiennej quality na
binarna, przyjmujac, ze wina o jakosci 6 lub wyzszej sa wysokiej jakosci, a pozostale - niskiej jakosci.


dane <- read.csv2('data/winequality-white.csv',  stringsAsFactors = FALSE, dec = '.')
if (typeof(dane$quality) == "integer") dane$quality <- ifelse(dane$quality >= 6, 'high', 'low')

b) inicjalizacja danych 
set.seed(1)

c) Podziel zbior na uczacy i testowy losowo w proporcji 0.8:0.2.

train_proportion <- 0.8
train_index <- runif(nrow(dane)) < train_proportion
train <- dane[train_index,]
test <- dane[!train_index,]

d) Zbuduj drzewo klasfikacyjne przewidujce jakosc czerwonego wina na podstawie jego parametrow chemicznych.
Przyjmij na poczatek parametr zlozonosci (complexity parameter) rowny 0.005. Zwizualizuj drzewo.

cp_start <- 0.005
library(rpart)
library(rpart.plot)
d.klas <- rpart(quality~., data = train, method = "class", cp = cp_start)
rpart.plot(d.klas, under=FALSE, fallen.leaves = FALSE, cex = 0.3)

e) Narysuj wykres bledu w zaleznosci od wielkosci drzewa. 
Czerwona kropką oznacz na wykresie wielkosc drzewa o minimalnym bledzie.

plotcp(d.klas)
min.error <- which.min(d.klas$cptable[,"xerror"])
opt.cp <- d.klas$cptable[min.error,"CP"]
points(min.error, d.klas$cptable[min.error, "xerror"], pch = 19, col = "red")

f) Zbuduj nowe drzewo powstale przez przyciecie poprzedniego drzewa do wartosci optymalnego parametru zlozonosci.

d.klas.przyciete <- prune.rpart(d.klas, cp = opt.cp)
rpart.plot(d.klas.przyciete, under=FALSE, fallen.leaves = FALSE, cex = 0.5)

g) Policz macierze klasyfikacji dla obu drzew

CM <- list()
CM[["d.klas"]] <- table(predict(d.klas, new = test, type = "class"), test$quality)
CM[["d.klas.przyciete"]] <- table(predict(d.klas.przyciete, new = test, type = "class"), test$quality)

h) Na postawie macierzy klasyfikacji policz dla obu drzew accuracy, sensitivity i specificity.
Uzywajac funkcji napisanej w zadaniu 1:
lapply(CM, EvaluateModel) # lapply stosuje funkcje po kolei do kazdego elementu listy i zwraca liste wynikow
Narysuj krzywa ROC i lift oraz policz AUC dla obu drzew.
Zamiast pisac dwa razy to samo, wygodniej bedzie napisac funkcje, ktora to robi w sposob ogolny,
i zastosowac ja do obu drzew.


library(ROCR) # do krzywej ROC

EvaluateTree <- function(tree_model, data_set, response_column_name)
{
  prognoza_ciagla <- predict(tree_model, newdata = data_set)
  prognoza_ciagla <- as.vector(prognoza_ciagla[,2])
  
  # krzywa ROC - potrzebuje "ciaglej" prognozy
  plot(performance(prediction(prognoza_ciagla, data_set[[response_column_name]]),"tpr","fpr"),lwd=2, colorize=T) 
  
  # AUC (Area Under Curve) - pole pod krzywa ROC
  print(performance(prediction(prognoza_ciagla, data_set[[response_column_name]]),"auc"))
  
  # Sensitivity/specificity plots ~ trade-off
  plot(performance(prediction(prognoza_ciagla, data_set[[response_column_name]]),"sens","spec"),lwd=2) 
  
  # Lift chart
  plot(performance(prediction(prognoza_ciagla, data_set[[response_column_name]]),"lift","rpp"),lwd=2, 
       col = "darkblue") 
}

EvaluateTree(tree_model = d.klas, data_set = test, response_column_name = "quality")
EvaluateTree(tree_model = d.klas.przyciete, data_set = test, response_column_name = "quality")

# Za kazdym wywolaniem powstaje kilka wykresow, ale widac tylko ostatni.
# Wczesniejsze mozna obejrzec, uzywajac strzalki wstecz nad wykresem.








