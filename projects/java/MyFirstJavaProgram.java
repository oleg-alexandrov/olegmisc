public class MyFirstJavaProgram{

   /* This is my first java program.  
    * This will print 'Hello World' as the output
    */

    public static void main(String []args){
        subfun1();
        System.out.println("Hello World"); // prints Hello World
        subfun2();

        String a = "xuxa";
        int z = 5;
        System.out.println(a + z);

        byte c  = 4;
        short d = 5;
        int e   = 6;
        double f = 6.0/9.0;
        char   g = 'a';

        System.out.println(c + " " + d + " " + e + " " + f + " " + g);
    }

    public static void subfun1(){
        System.out.println("subfun1"); 
    }

    private static void subfun2(){
        System.out.println("subfun2");
    }
} 


