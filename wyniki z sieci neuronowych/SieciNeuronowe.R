library(keras)

mnist <- dataset_mnist()
train_images <-mnist$train$x
train_labels <- mnist$train$y
test_images <-mnist$test$x
test_labels <- mnist$test$y

dim(train_images)
str(train_images)
str(train_labels)

dim(test_images)
str(test_images)
str(test_labels)

no_of_sample =55
digit <- train_images[no_of_sample,,]
plot(as.raster(digit,max = 255))

test_images_orginal <- test_images
test_labels_orginal <- test_labels

train_images <- train_images[1:10000,,]
train_labels <- train_labels[1:10000]

train_images <- array_reshape(train_images,c(10000,28*28))
train_images <- train_images/255
test_images <- array_reshape(test_images,c(10000,28*28))
test_images <- test_images/255

train_labels <- to_categorical(train_labels)
test_labels <- to_categorical(test_labels)

test_labels_orginal[1:20]
test_labels[1:20,1:10]

network <- keras_model_sequential() %>%
  layer_dense(units = 637, activation = "relu", input_shape = c(28*28)) %>%
  layer_dense(units = 10, activation = "softmax")

#layer_dense(units = 156, activation = "relu") %>%

network %>% compile(
  optimizer = "rmsprop",
  loss = "categorical_crossentropy",
  metrics = c("accuracy")
)

network %>% fit(train_images,train_labels,
                epochs = 24,
                batch_size = 128,
                validation_split = 0.2)
network %>% evaluate(test_images,test_labels)

predictions <- network %>% predict_classes(test_images[1:100,])
labels <- test_labels_orginal[1:100]
prediction_results <- data.frame(predictions,labels)
prediction_results
wrong <- prediction_results[predictions != labels,]
wrong
digit <- test_images_orginal[98,,]
plot(as.raster(digit,max = 255))

prediction <- network %>% predict(test_images[1:50,])
prediction <- round(prediction,3)
View(prediction)