C
C SUBROUTINE ASCEBC
C $Log:   GXAFXT:[GOLS]ASCEBC.FOV  $
C  
C     Rev 1.0   17 Apr 1996 12:13:08   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.0   21 Jan 1993 15:40:00   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - nrm_ascebc.for **
C
C ASCEBC.FOR
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C V01 01-FEB-89 XXX INITIAL RELEASE FOR SWEDEN
C                   ** RECORD IS NOW I*2 OR I*4 *** NOT CHARACTER **
C                   ** BEGINNING OFFSET IS SPECIFIED **
C
C
C     CONVERT AN ASCII STRING TO EBCDIC IN PLACE
C
C     CALL ASCEBC(I2STRING,OFF,LEN)
C               I2STRING=ASCII STRING (INTEGER*2 OR INTEGER*4 ARRAY)
C                 OFF   =BEGINNING OFFSET (1-N)
C                 LEN   =# OF CHARACTERS IN STRING TO CONVERT
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode
C Island, and contains confidential and trade secret information. It
C may not be transferred from the custody or control of GTECH except
C as authorized in writing by an officer of GTECH. Neither this item
C nor the information it contains may be used, transferred,
C reproduced, published, or disclosed, in whole or in part, and
C directly or indirectly, except as expressly authorized by an
C officer of GTECH, pursuant to written agreement.
C
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE ASCEBC(TXT,OFF,LEN)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INTEGER*2 TXT(*)
	INTEGER*4 OFF
	INTEGER*4 LEN
	INTEGER*4 TEMP, I, K
C
C
	INTEGER*4 EBC(0:255)
	DATA (EBC(K),K=0,127)/
     1	        000,001,002,003,055,045,046,047,022,005,
     1	        037,011,012,013,014,015,016,017,018,059,
     1	        060,061,050,038,024,025,063,039,034,111,
     1	        053,111,064,090,127,123,091,108,080,125,
     1	        077,093,092,078,107,096,075,097,240,241,
     1	        242,243,244,245,246,247,248,249,122,094,
     1	        076,126,110,111,124,193,194,195,196,197,
     1	        198,199,200,201,209,210,211,212,213,214,
     1	        215,216,217,226,227,228,229,230,231,232,
     1	        233,111,111,111,111,109,111,129,130,131,
     1	        132,133,134,135,136,137,145,146,147,148,
     1	        149,150,151,152,153,162,163,164,165,166,
     1	        167,168,169,111,079,111,111,007/
	DATA (EBC(K),K=128,255)/
     1	        000,001,002,003,055,045,046,047,022,005,
     1	        037,011,012,013,014,015,016,017,018,059,
     1	        060,061,050,038,024,025,063,039,034,111,
     1	        053,111,064,090,127,123,091,108,080,125,
     1	        077,093,092,078,107,096,075,097,240,241,
     1	        242,243,244,245,246,247,248,249,122,094,
     1	        076,126,110,111,124,193,194,195,196,197,
     1	        198,199,200,201,209,210,211,212,213,214,
     1	        215,216,217,226,227,228,229,230,231,232,
     1	        233,111,111,111,111,109,111,129,130,131,
     1	        132,133,134,135,136,137,145,146,147,148,
     1	        149,150,151,152,153,162,163,164,165,166,
     1	        167,168,169,111,079,111,111,007/
C
C
	DO 1010 I=OFF-1,OFF+LEN-2
	  CALL ILBYTE(TEMP,TXT,I)
	  CALL ISBYTE(EBC(TEMP),TXT,I)
1010	CONTINUE
C
	RETURN
C
	END
