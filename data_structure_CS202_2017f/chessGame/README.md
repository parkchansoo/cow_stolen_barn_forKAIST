# Chess Game

### 0. Data Structure : Queue
Example to create and test `Queue` with some game named chess game which is __Moving chess Knight to destination__

### 1. Rules
When the size of a square chessboard, the position of a Knight and position of a target are given, 
you should figure out how many steps a Knight should take to get to the position of the target.  
Your method __must return the minimum value.__
 - `N` = the size of a square chessboard (N x N)
 - `startPos` = the position of a Knight
 - `targetPos` = the postion of a target  
When the values of arguments are __infeasible__, your method should `return -1`.  

### 2. Example
Let's say there is square chess board and 
 - size N : 3
 - start positin : (1, 1)
 - target position : (2, 1)

knight always moves _(+-1, +-2) or (+-2, +-1)_  
#### `Good answer` : return 3
```
possible path : (1, 1) -> (3, 2) -> (1, 3) -> (2, 1)
```

#### `Bad answer` : return 5  
```java
// it's *wrong* even the Knight have path to reach target point.  
non optimal path : (1, 1) -> (2, 3) -> (3, 1) -> (1, 2) -> (3, 3) -> (2, 1)
```

and there are another example.  
 - size N : 3
 - start positin : (1, 1)
 - target position : (2, 2)  

In fact, there is no way to arrive. so you should __return -1__


### 3. Test Cases

```java
/* input */
Start Position: (1, 1)
Target Position: (4, 5)
/* printed output */
Result: 3
```
:arrow_right_hook: There is case (1, 1) -> (3, 2) -> (2, 4) -> (4, 5).  
<br>
  
```java
/* input */
Start Position: (5, 7)
Target Position: (15, 20)
/* printed output */
Result: 9
```
