import java.net.*;
import java.io.*;

// A server which will be queried by clients via a network socket.
// Will return an integer i, 0 <= i <= maxNum - 1. We guarantee
// that no clients will ever get the same number even if they
// query the server at precisely the same time. This is accomplished
// by using a lock on that number. After the numbers run out, return
// -1.
public class EchoServer{
    
    ServerSocket m_ServerSocket;
    
    
    public EchoServer(){

        Integer[] count = new Integer[1];
        count[0] = 0; // will keep here the number to return
        
        try{
            // Create the server socket.
            m_ServerSocket = new ServerSocket(12111);
        }catch(IOException ioe){
            System.out.println("Could not create server socket at 12111. Quitting.");
            System.exit(-1);
        }
        
        System.out.println("Listening for clients on 12111...");
        
        // Successfully created Server Socket. Now wait for connections.
        int id = 0;
        while(true){                        
            try{
                // Accept incoming connections.
                Socket clientSocket = m_ServerSocket.accept();
            
                // accept() will block until a client connects to the server.
                // If execution reaches this point, then it means that a client
                // socket has been accepted.
                
                // For each client, we will start a service thread to
                // service the client requests. This is to demonstrate a
                // multithreaded server, although not required for such a
                // trivial application. Starting a thread also lets our
                // EchoServer accept multiple connections simultaneously.
                
                // Start a service thread
                
                ClientServiceThread cliThread
                    = new ClientServiceThread(clientSocket, id++, count);
                cliThread.start();
            }catch(IOException ioe){
                System.out.println("Exception encountered on accept. Ignoring. Stack Trace :");
                ioe.printStackTrace();
            }
        }
    }
    
    public static void main (String[] args){
        new EchoServer();    
    }
    
}

class ClientServiceThread extends Thread{
    
    Socket m_clientSocket;        
    int m_clientID = -1;
    static Integer[] m_count;
        
    ClientServiceThread(Socket s, int clientID, Integer[] count){
        m_clientSocket = s;
        m_clientID     = clientID;
        m_count        = count;
    }
       
    static public synchronized int getNumLessThan(int maxNum){
        // See the information at the top of this file.

        // Lines for debugging. Test if only one thread accesses
        // this block at any time.
        //System.out.println("Now entering the lock");
        //try{Thread.currentThread().sleep(2000);}
        //catch(InterruptedException ie){}

        int val = m_count[0];
        
        if (m_count[0] >=0 ){
            m_count[0]++;
        }
        if (m_count[0] < 0 || m_count[0] >= maxNum){
            m_count[0] = -1;
        }
        System.out.println("Server count is " + m_count[0]);
        //System.out.println("Now exiting the lock");
        return val;
    }

    public void run(){            
        // Obtain the input stream and the output stream for the socket
        // A good practice is to encapsulate them with a BufferedReader
        // and a PrintWriter as shown below.
        BufferedReader inStream  = null; 
        PrintWriter    outStream = null;
            
        // Print out details of this connection
        System.out.println("Accepted Client : ID - " + m_clientID + " : Address - " + 
                           m_clientSocket.getInetAddress().getHostName());
                
        try{                                
            inStream  = new BufferedReader(new InputStreamReader (m_clientSocket.getInputStream()));
            outStream = new PrintWriter   (new OutputStreamWriter(m_clientSocket.getOutputStream()));
                
            // At this point, we can read for input and reply with appropriate output.
                
            // read incoming stream
            String clientCommand = inStream.readLine();
            int maxNum = Integer.parseInt(clientCommand);
            System.out.println("Client says: " + clientCommand);

            String answer = Integer.toString(getNumLessThan(maxNum));
          
            System.out.println("Will reply with: " + answer);
            outStream.println(answer);
            outStream.flush();
            
            
        }catch(Exception e){
            e.printStackTrace();
        }finally{
            // Clean up
            try{                    
                inStream.close();
                outStream.close();
                m_clientSocket.close();
                System.out.println("...Stopped");
            }catch(IOException ioe){
                ioe.printStackTrace();
            }
        }
    }
}
