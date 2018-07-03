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
