public class MangoBase{
    
    private double m_x;
    public MangoBase(double x){
        System.out.println("Now in the constructor of MangoBase");
        m_x = x;
    }
    void setVal(double x){
        m_x = x;
    }
    double getVal(){
        return m_x;
    }
    
    public String toString(){ // Overwrite the default
        return "MangoBase: " + m_x;
    }
}
