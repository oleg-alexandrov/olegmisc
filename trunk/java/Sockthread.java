class HelloRunnable implements Runnable {

    static public synchronized void doIncrement(){
        System.out.println("Now entering the lock");
        try{Thread.currentThread().sleep(5000);}
        catch(InterruptedException ie){}
        m_I[0]++;
        System.out.println("Count is " + m_I[0]);
        System.out.println("Now exiting the lock");
    }    
        
    public void run() {
        doIncrement();
    }

    static public Integer[] m_I;

    HelloRunnable(Integer[] I){
        m_I = I;
    }
    
}


class Sockthread{

    public static void main(String []args){

        // This array is shared by all threads
        Integer[] I = new Integer[5];
        I[0] = 0;
        
        Thread A = new Thread(new HelloRunnable(I));
        A.start();
        Thread B = new Thread(new HelloRunnable(I));
        B.start();
    }
    
}