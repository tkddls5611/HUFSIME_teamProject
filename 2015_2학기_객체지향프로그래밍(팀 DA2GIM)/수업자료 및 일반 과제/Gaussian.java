public class Gaussian {
    
    public static double gaussian() {
        double r, x, y;
        do {
            x = uniform(-1.0, 1.0); 
            y = uniform(-1.0, 1.0); 
            r = x*x + y*y;
        } while (r >= 1 || r == 0);
        return x * Math.sqrt(-2.0 * Math.log(r) / r);
    }   

    
    public static double uniform(double a, double b) {
        return Math.random() * (b-a) + a;
    }
    
    public static void main(String[] args) {
        int N = Integer.parseInt(args[0]);
        
        int[] a = new int[20];
        
        for (int k=0; k<N; k++) {
            double r = gaussian();
            
            for (int i=0; i<20; i++) {
                if (r >= i*0.5-5 && r <= (i+1)*0.5-5) {
                    a[i]++;
                }
                
            }
            
        }
        
        int maxy = 0;
        for (int i=0; i<20; i++) {
            if (maxy < a[i]) {
                maxy = a[i];
            }
        }
        
        StdDraw.setXscale(0, 20);
        StdDraw.setYscale(0, maxy);
        
        for (int i=0; i<19; i++) {
            StdDraw.line(i, a[i], i+1, a[i+1]);
            
        }
        
        
    }

}