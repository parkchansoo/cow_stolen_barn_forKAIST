package myStack;

import java.util.*;

public class myStack<T> {
    /* myStack properties
     * variables : stack pointer, array to store
     * stack functions : constructor, get/set Size, push, pop, top, isEmpty
     */
    public Vector<T> arr;
    public int top;

    public myStack () {
        this.arr =  new Vector<>();
        this.top = -1;
    }

    public int getSize() { return top + 1; };

    /* actually you shouldn't touch this function */
    public int setTop(int top) { return this.top = top; }

    public boolean isEmpty() { return top == -1; }

    public T top() { return arr.get(top); }

    public void push(T item) {
        top ++;
        arr.add(top, item);
    }

    public T pop() {
        top --;
        return arr.get(top + 1);
    }
}