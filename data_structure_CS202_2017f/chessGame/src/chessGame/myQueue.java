package chessGame;
import java.util.*;


/*
 * actually we can easily implement queue data stucture with
 * LinkedList DS. but to learn how queue is work, let's use array.
 * */
public class myQueue<T> {
    /*
    * myQueue AST
    * 1. variables : last, size, maxSize
    * 2. methods : getSize, set/getFirst, set/getLast, enqueue, dequeue,
    * 3. helper functions : expandQ
    * 4. functions for chess game
    * */

    int first;
    int size;
    int maxSize;
    T[] container;

    myQueue () {
        this.size = 0;
        this.first = 0;
        this.maxSize = 100;
        this.container = (T[])new Object[maxSize];
    }

    private void expandQ() {
        maxSize *= 2;
        T[] newContainer = (T[])new Object[maxSize];
        for(int i = first; i < size; i ++ ) {
            /* this is implementation as easy to see what's going on
             * however, it takes huge time complexity.
             * if you want to reduce complexity, you need to use Java built-in function
             */
            newContainer[i] = container[i];
        }
        for(int i = 0; i < first; i ++) {
            newContainer[size + i] = container[i];
        }
        this.container = newContainer;
    }

    public int enqueue(T e) {
        if(size == maxSize)
            expandQ();
        int idx = (first + size) % maxSize;
        container[idx] = e;
        size ++;
        return idx;
    }

    public T dequeue() {
        T ret = container[first];
        first = (first + 1) % maxSize;
        size --;
        return ret;
    }

    public T first() {
        return container[first];
    }

    public boolean isEmpty() {
        return size == 0;
    }

    public int getSize() {
        return size;
    }
}