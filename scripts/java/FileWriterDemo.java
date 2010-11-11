import java.io.BufferedWriter;
import java.io.FileWriter;

public class FileWriterDemo {
    
    public static void main(String args[]) throws Exception{

        FileWriter fw = new FileWriter("mytest.txt");
        BufferedWriter bw = new BufferedWriter(fw);
        bw.write("test2");
        bw.close();
        fw=null;

    }
}
