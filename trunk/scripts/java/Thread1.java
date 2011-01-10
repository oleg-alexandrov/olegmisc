import java.lang.Math;
import java.util.Random;

class MyThread1 implements Runnable{
    public Thread t;
    Integer[] m_count;
    
    MyThread1(Integer[] count) {
        t = new Thread(this, "my thread 2");
        t.start();
        m_count = count;
    }

    void doWork(){
        System.out.println("start---");
        int q = m_count[0];
//         double w = Math.random();
//         int v = Math.max(1, (int)(4*w));
//         try {
//             Thread.sleep(v);
//         } catch (InterruptedException e) {
//             e.printStackTrace();
//         }
        m_count[0] = q + 1;
        System.out.println("----end");
    }
    
    public void run(){

        doWork();
        
        System.out.println("--Now in thread 1, value is " + m_count[0]);
    }
    
}


class Thread1{

    public static void main(String[] args){
        
        System.out.println("Hello ");
        System.out.println("Hello world2");
        
        Thread T = Thread.currentThread();
        T.setName("My thread 1");
        T.setPriority(6);
        System.out.println("Thread is " + T);
        
        System.out.println("Before starting new thread");

        Integer[] count = {2};

        System.out.println("Before: count is " + count[0]);

        // Start a new thread
        for (int s = 0; s < 6; s++){
            //System.out.println("s is " + s);
            MyThread1 p = new MyThread1(count);
            
            try { 
                // This will wait until the thread terminates
                p.t.join();
            }catch(InterruptedException e){
                e.printStackTrace();
            }
            
        }
        
        try {
            Thread.sleep(30);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }

        System.out.println("After starting new thread");
        System.out.println("After: count is " + count[0]);
        
        System.out.println("After2: count is " + count[0]);

    }
    
}
