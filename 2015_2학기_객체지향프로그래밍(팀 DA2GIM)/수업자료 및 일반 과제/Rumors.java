public class Rumors {
  public static void main(String[] args) {
    int N = Integer.parseInt(args[0]);
    int T = Integer.parseInt(args[1]);
    
    int num_all_heard = 0;
    int tot_num_heard = 0;
    
    for (int t = 0; t < T; t++) {
      boolean[] heard = new boolean[N];
      
      int num_heard = 0;
      
      int r = 0;
      heard[r] = true;
      num_heard++;
      while (num_heard < N) {
        
        int next = (int)(Math.random() * N);
        
        while (next == r) {
          next = (int)(Math.random() * N);
        }
        
        if (heard[next] == false) {
          heard[next] = true;
          num_heard++;
          r = next;
        } else {
          break;
        }
      }
      
      tot_num_heard += num_heard;
      if (num_heard == N) {
        num_all_heard++;
      }
      
    }
    
    System.out.println("Probability = " + (double)num_all_heard / T * 100);
    System.out.println("Expected number of people = " + (double)tot_num_heard / T);
  }
  
}