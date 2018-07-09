# Possible Largest Rectangle in Histogram

### 0. Data Structure: Stack
Example to build and test `stack` with proving optimal by __Possible Largest Rectangle in Histogram Problem__

### 1. Rules
In this exercise, you should implement a method which can calculate 
the area of the largest rectangle in a given histogram. 
Every patch in the given histogram has the same width and the width is 1.

### 2. Example
consider a histogram with 8 bars of heights 
```java
{10, 1.5, 6, 7.5, 8, 5.5, 2, 9}. 
```
The area of the largest possible rectangle is `22`  
because we should draw the largest possible rectangle with `width 4` and `height 5.5` in this example.  
which means we take area like belows.
```java
// {10, 1.5, 
    6, 7.5, 8, 5.5, 
//  2, 9}
Largest Rectangle in Histogram >>> 5.5 * 4 = 22
```
#### Bad example
```javascript
// {10, 
    1.5, 6, 7.5, 8, 5.5, 2, 
//  9}
Largest Rectangle in Histogram >>> 1.5 * 6 = 9
```


`__(Hint)__` You should use __Stack__ to solve this problem.  
You may save the __index of patches__ in a given histogram into Stack to figure out when the largest possible rectangle will be made.
