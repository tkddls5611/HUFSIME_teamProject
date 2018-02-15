public class ShuffleTest {
  public static void main(String[] args) {
    int M = Integer.parseInt(args[0]);
    int N = Integer.parseInt(args[1]);
    int[] a = new int[M];
    int[][] results = new int[M][M];
    
      
    for (int t = 0; t < N; t++) {
      for (int i = 0; i < M; i++) {
        a[i] = i;
      }
      
      for (int i = 0; i < M; i++) {
        int r = 0 + (int) (Math.random() * (M-i));
        int tmp = a[r];
        a[r] = a[i];
        a[i] = tmp;
      }
      
      for (int i = 0; i < M; i++) {
        int j = a[i];
        results[i][j]++;
      }
      
    }
    
    for (int i = 0; i < M; i++) {
      for (int j = 0; j < M; j++) {
        System.out.print(results[i][j] + " ");
      }
      System.out.println("");
    }
  } 
} 