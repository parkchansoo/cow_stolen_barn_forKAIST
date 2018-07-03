# BalloonGame

### Singly_Linked_List Example
Example to create and test `singly linked list` with some game named __balloon game__.


### Balloon Game Rule
There are `N circular singly linked balloons` in a __clockwisedirection__. The player pops every `M-th balloon` on the current position in balloons list. If the player pops a __boom__, the boom is removed but __two new balloons are added__ to the boom’s poistion. When a player pops all the balloons, the game ends. __Find the order of popping balloons!__.

  •	N , M >= 2
  1. create N balloon with singly linked lists
  2. move M start from last poped place (if you poped 1 last, and M=3, then you should go 4. not 3 or 5)
  3. after you move, your destination balloon booms.
  4. if the target balloon is on boom-list, then create new two balloon with increased number.
  5. and next time, your start point is poped place which first balloon created.

### Game Example

There are 6 balloons. Among them balloon 5 is a boom. In this example, 
let’s assume that M = 2. Pop every 2nd ballloon on the current position.
```
size = 6, M = 2, boom = {5}
```
1. Start with `balloon 1`.

 1 |  2  |  3  |  4  |  5  |  6 
------------ | ------------- | ------------- | ------------- | ------------- | -------------
:balloon::heavy_check_mark:| :balloon:| :balloon:| :balloon:| :red_circle:| :balloon:

2. Pop the __2nd balloon__ `balloon 3` by the position of the `balloon 1`.

 1 |  2  |  3  |  4  |  5  |  6 
------------ | ------------- | ------------- | ------------- | ------------- | -------------
:balloon:| :balloon:| :anger::heavy_check_mark:| :balloon:| :red_circle:| :balloon:


3. Pop next 2nd ballon `balloon 5` by the position of the `popped balloon 3`. The boom:boom: is popped!

 1 |  2  | - | 4  |  5 |  6 
------------ | ------------- | ------------- | ------------- | ------------- | -------------
:balloon:| :balloon:|  | :balloon:| :boom::heavy_check_mark: | :balloon:


4. __Add two__ new balloons to the __location of the popped boom__. 
    Label the new balloons with takes next sequence number of balloons. Then their numbers would be 7, 8 respectively.
    
1 | 2  |-  |  4  |  5 |  7  |  8 |  6 
------ | ----|----| ------------- | ------------- | ------------- | -------------| -------------
:balloon:| :balloon:| | :balloon:| :boom::heavy_check_mark: | :balloon: | :balloon: | :balloon:


5. After the boom is popped, begin at the balloon 7 which is the same position that the boom was. Pop next 2nd balloon `balloon 6` by the position of balloon 7.

 1 |  2  | -  |  4  |  - |  7  |  8 |  6 
--------- |--|---------- | ---------- | --------- | ---------- | --------- | ---------
:balloon:| :balloon:| |:balloon:| | :balloon: | :balloon: | :anger::heavy_check_mark:


6. Pop the 2nd balloon `balloon 2` by the position of the popped balloon 6.

 1 |  2  | -  |  4  |  - |  7  |  8 |  - 
--------- |--|---------- | ---------- | --------- | ---------- | --------- | ---------
:balloon:| :anger::heavy_check_mark:| |:balloon:| | :balloon: | :balloon: | 


7. Pop the 2nd balloon `balloon 7` by the position of the popped balloon 2.

 1 |  -  | -  |  4  |  - |  7  |  8 |  - 
--------- |--|---------- | ---------- | --------- | ---------- | --------- | ---------
:balloon:| | |:balloon:| | :anger::heavy_check_mark: | :balloon: | 


8. Pop the 2nd balloon `balloon 1` by the position of the popped balloon 7.

 1 |  -  | -  |  4  |  - |  -  |  8 |  - 
-- |-----|--- |---- |--- |---- |--- | -----
:anger::heavy_check_mark:| | |:balloon:| |  | :balloon: | 


9. Pop the 2nd balloon `balloon 8` by the position of the popped balloon 1.

 x |  -  | -  |    4    |  - |  -  |  8 |  - 
-- |-----|--- |-------- |--- |---- |--- | -----
   |     | |    |:balloon:|    |     | :anger::heavy_check_mark:



and keep pop the balloon until all balloons are poped.

### Test cases
```java
size = 2, M = 2, boom = {}
printed balloon order >>> 1-2
```

```java
size = 2, M = 2, boom = {2}
printed balloon order >>> 1-2-3-4
```

```java
size = 5, M = 3, boom = {2}
printed balloon order >>> 4-2-5-7-6-3-1
```

```java
size = 7, M = 11, boom = {1,4,6}
printed balloon order >>> 5-3-4-2-8-7-9-6-1-11-13-10-12
```
