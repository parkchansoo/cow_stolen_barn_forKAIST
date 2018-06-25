package balloonGame;

public class Balloon {
    public int num;
    public boolean isBoom;
    public Balloon next;

    public Balloon (int num) {
        this.num = num;
        this.isBoom = false;
    }

    public Balloon (int num, boolean isBoom) {
        this.num = num;
        this.isBoom = isBoom;
    }

    public int getNum() {
        return num;
    }

    public boolean isBoom() {
        return isBoom;
    }

    public boolean setBoom() {
        return isBoom;
    }

    public Balloon getNext() {
        return next;
    }

    /* set Balloon as new and returns new set balloon */
    public Balloon setNext(Balloon next) {
        this.next = next;
        return next;
    }
}
