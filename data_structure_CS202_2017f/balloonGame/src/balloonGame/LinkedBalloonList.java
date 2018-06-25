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

    public LinkedBalloonList setBalloonList(int listSize, int[] boomList) {
        Arrays.sort(boomList);
        int boomCnt;
        int boomLen = boomList.length;
        for(int i = 0; i < listSize; i ++)
            insertBalloon();

        int currBoomHead;
        int prevBoomHead = 0;
        for(int i = 0; i < boomLen; i --) {
            currBoomHead = boomList[i];
            prevBoomHead = currBoomHead - prevBoomHead;
            // ratate moves
            // and mark isBoom

            prevBoomHead = currBoomHead;
        }
        return this;
    }

    public Balloon rotate(int moves) {
        // ToDo. make rotation
        return this.head;
    }

    public void remove() {
        /* remove curr head
         * if isBoom > insert two more balloons
         */
    }


    public Balloon insertBalloon() {
        cnt ++;
        if(isEmpty()) {

        }

        return head;
    }
}
