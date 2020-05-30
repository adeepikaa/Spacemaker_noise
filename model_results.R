#######################


# File contains the output of both Model Sets:


#    Models Set 1

# Uses ONLY the non-specific train dataset as the train set
# and the specific train dataset as the test set


# FINAL OUTPUT of the 8 models


#  |method        |tuned |      RMSE|
#  |:-------------|:-----|---------:|
#  |Guess         |N     | 0.3247182|
#  |Random Forest |N     | 0.0864403|
#  |KNN_1         |N     | 0.0806087|
#  |KNN_2         |Y     | 0.0727067|
#  |Average       |N     | 0.0639357|
#  |Reg Tree      |N     | 0.0625724|
#  |Linear Reg    |N     | 0.0551626|
#  |GamLoess      |N     | 0.0537910|
#  |SVM Poly      |N     | 0.0509911|




#    Models Set 2



# Uses BOTH the non-specific train dataset and the specific train dataset combined together as 
# the train set and partitions it into train and test dataset

#  |method        |tuned |      RMSE|
#  |:-------------|:-----|---------:|
#  |Guess         |N     | 0.3665481|
#  |Average       |N     | 0.1691356|
#  |KNN_1         |N     | 0.0915582|
#  |KNN_2         |Y     | 0.0882236|
#  |Reg Tree      |N     | 0.0738796|
#  |Linear Reg.   |N     | 0.0734966|
#  |GamLoess      |N     | 0.0665017|
#  |SVM Poly      |N     | 0.0599199|
#  |Random Forest |N     | 0.0553471|
  
  
