package balloonGame;

import java.util.Arrays;

public class LinkedBalloonList {
    public Balloon head;
    public Balloon tail;
    public int size;
    public int cnt;

    public LinkedBalloonList () {
        size = 0;
        cnt = 0;
        head = null;
        tail = null;
    }

    public int setSize(int size) {
        return this.size = size;
    }

    public int getSize() {
        return size;
    }

    public boolean isEmpty() {
        return (size == 0);
    }

    public Balloon getHead() { return head; }

    public Balloon getTail() { return tail; }

    /* build up balloon Linked list
     * with given number and boomlist
     * */
    public LinkedBalloonList setBalloonList(int listSize, int[] boomList) {
        Arrays.sort(boomList);
        int boomCnt;
        int boomLen = boomList.length;
        for(int i = 0; i < listSize; i ++)
            insertBalloon();

        Balloon tailing = head;
        int currBoomHead;
        int prevBoomHead = 0;
        for(int i = 0; i < boomLen; i ++) {
            currBoomHead = boomList[i];
            prevBoomHead = currBoomHead - prevBoomHead;
            rotate(prevBoomHead);
            head.setBoom();
            prevBoomHead = currBoomHead;
        }

        tail = tailing;
        head = tail.getNext();
        return this;
    }

    public Balloon rotate(int moves) {
        if (size > 1) {
            for (int i = 0; i < moves; i++) {
                tail = head;
                head = tail.getNext();
            }
        }
        return this.head;
    }

    public String remove() {
        /* remove curr head
         * if isBoom > insert two more balloons
         */
        String ret = "" + head.getNum();
        head = head.getNext();
        tail.setNext(head);
        size --;
        return ret;
    }

    public String boom() {
        insertBalloon();
        insertBalloon();
        rotate(size - 2);
        return remove();
    }


    public Balloon insertBalloon() {
        cnt ++;
        if(isEmpty()) {
            head = new Balloon(cnt);
            tail = head;
            head.setNext(tail);
            tail.setNext(head);
        } else {
            Balloon next = head.getNext();
            Balloon curr = new Balloon(cnt);
            curr.setNext(next);
            head.setNext(curr);
            tail = head;
            head = curr;
        }
        size ++;
        return head;
    }

    public String printLBL() {
        String ret = "";
        Balloon curr = head;
        ret += curr.getNum();
        for (int i = 1; i < size; i++) {
            ret += "-";
            curr = curr.next;
            ret += curr.getNum();
        }
        return ret;
    }

    public String printBoomList() {
        String ret = "[";
        Balloon curr = head;
        for(int i = 0; i < size; i ++) {
            if(curr.isBoom()) {
                ret += curr.getNum();
                ret += ", ";
            }
            curr = curr.next;
        }
        ret += "]";
        return ret;
    }
}
