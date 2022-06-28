
public class OptionFlags {

	public static void main(String[] args) {
		//max integer for 1 byte = 256 --- [ 0 ; 255]
		/*
		 * 256=1111 1111
		 * 0x60 = 96
		 * */
		int optionFlags = 72;//128   96   256=1111 1111
		byte[] flagValidation = {
				(byte)0x8000, 
				(byte)0x4000, 
				(byte)0x2000, 
				(byte)0x1000, 
				(byte)0x800, 
				(byte)0x400, 
				(byte)0x200, 
				(byte)0x100,
				(byte)0x80, 
				(byte)0x40, 
				(byte)0x20, 
				(byte)0x10, 
				(byte)0x08, 
				(byte)0x04, 
				(byte)0x02, 
				(byte)0x01
		};
		
		IMillFlags signOnFlags;
		signOnFlags = new GenericMillFlags(optionFlags,flagValidation);
		signOnFlags.buildOptionFlags();
		
		
//		do {
//			activeFlag = optionFlags % k;
//			System.out.println("Active flag="+activeFlag);
//			if(activeFlag == 0 || activeFlag < optionFlags) {
//				flagList[i] = (true);
//				optionFlags -= activeFlag;
//			}
//			k /= 2;
//			System.out.println(flagList[i]);
//			i++;
//		} while(k != 0 && optionFlags != 0);
		

		
		
//		for(short i=0; i<flagValidation.length ;i++) {
//			flagResult = (byte) (optionFlags & flagValidation[i]);
//			System.out.printf("0x%02X\n",flagResult);
//			if(flagResult != 0x00) flag[i]=true;
//		}
//		
//		for(short t=0; t<flag.length ;t++) {
//			System.out.println(flag[t]);
//		}
		
		//buildOptionFlags(optionFlags,flagValidation);
		
	}
	
	/*
	public static void buildOptionFlags(int optionFlags,byte[] flagValidation) {
		byte flagResult;
		boolean[] flag = new boolean[8];
		
		for(short i=0; i<flagValidation.length ;i++) {
			flagResult = (byte) (optionFlags & flagValidation[i]);
			System.out.printf("0x%02X\n",flagResult);
			if(flagResult != 0x00) flag[i]=true;
		}
		
		for(short t=0; t<flag.length ;t++) {
			System.out.println(flag[t]);
		}
	}
	*/

}
 