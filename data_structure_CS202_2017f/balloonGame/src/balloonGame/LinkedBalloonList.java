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

    public int getSize(int size) {
        return size;
    }

    public boolean isEmpty() {
        return (size == 0);
    }

    /* build up balloon Linked list
     * with given number and boomlist
     * */
    public LinkedBalloonList setBalloonList(int listSize, int[] boomList) {
        Arrays.sort(boomList);
        int boomCnt;
        int boomLen = boomList.length;
        for(int i = 0; i < listSize; i ++)
            insertBalloon();

        Balloon tailing = tail;
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

    public void remove() {
        /* remove curr head
         * if isBoom > insert two more balloons
         */
        head = head.getNext();
        tail.setNext(head);
        size --;
    }

    public void boom() {
        Balloon heading = head;
        insertBalloon();
        insertBalloon();
        tail = head;
        head = tail.getNext();
        remove();
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
        for(int i = 0; i < size; i ++) {
            ret += "-";
            curr = curr.next;
            ret += curr.getNum();
        }
        return ret;
    }
}
