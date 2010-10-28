class Pear{

    private int m_z;

    public Pear(int z){
        System.out.println("Now in the Pear constructor");
        m_z = z;
    }

    public int action1(){
        return m_z*m_z + action2();
    }

    private int action2(){
        return m_z + m_z;
    }
}