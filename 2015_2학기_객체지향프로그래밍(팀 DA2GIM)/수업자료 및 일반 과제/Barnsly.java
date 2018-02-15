public class Barnsly {
  public static void main(String[] args) {
    int T = Integer.parseInt(args[0]);
    
    double x = 0.5, y = 0.0;
    for (int t = 0; t < T; t++) {
      double r = Math.random();
      double x1=x;
      double y1=y;
      if (r <= 0.02) {
        x = 0.50;
        y = 0.27 * y1;
      } else if (r <= 0.15) {
        x = -.14*x1 + .26*y1 + .57; 
        y =  .25*x1 + .22*y1 - .04;
      } else if (r <= 0.30) {
        x =  .17*x1 - .21*y1 + .41;
        y =  .22*x1 + .18*y1 + .09;
      } else {
        x = .78*x1 + .03*y1 + .11;
        y = -.03*x1 + .74*y1 + .27;
      }
      StdDraw.point(x, y);
    }
  }
  


}