public class Main {

	public static void main(String[] args) {
		// TODO Auto-generated method stub
		
		int myValue = 10000;
		
		int myMinIntValue = Integer.MIN_VALUE;
		int myMaxIntValue = Integer.MAX_VALUE;
		System.out.println("Integer min Value = " + myMinIntValue);
		System.out.println("Integer max Value = " + myMaxIntValue);
		System.out.println("Busted min Value = " + (myMinIntValue-1));
		System.out.println("Busted max Value = " + (myMaxIntValue+1));
		
		int test = 2_147_483_647;
	}

}
