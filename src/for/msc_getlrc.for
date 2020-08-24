C
C *** SUBROUTINE GETLRC ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_GETLRC.FOV                               $
C  $Date::   17 Apr 1996 14:07:40                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_getlrc.for ***
C
C V01 24-JAN-91 RRB INITIAL VAX RELEASE
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, Providence, Rhode Island,
C and contains confidential and trade secret information. It may not be
C transferred from the custody or control of GTECH except as authorized in
C writing by an officer of GTECH. Neither this item nor the information it
C contains may be used, transferred, reproduced, published, or disclosed,
C in whole or in part, and directly or indirectly, except as expressly
C authorized by an officer of GTECH, pursuant to written agreement.
C
C Copyright 1994 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C Purpose:
C	CALCULATE LRC CHECKSUM FOR TELENEX MATRIX SWITCH INTERFACE
C
C Calling Sequence:
C	CALL GETLRC(ARRAY, BEG_IDX, LEN, LRC)
C
C Input:	BTYE(*)		ARRAy   - ARRAY OF BYTES TO BE CHECKSUMMED
C		INTEGER*4	BEG_IDX - WHERE TO START IN ARRAY
C		INTEGER*4	END_IDX - WHERE TO END IN ARRAY
C
C Output:	INTEGER*4	LRC     - CHECKSUM
C
C Files:
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETLRC(ARRAY,BEG_IDX,END_IDX,LRC)
	IMPLICIT NONE
C
	BYTE	    ARRAY(*)	    !INPUT BYTE ARRAY
	INTEGER*4   BEG_IDX         !BEGIN INDEX
	INTEGER*4   END_IDX         !END INDEX
	INTEGER*4   I		    !LOOP VARIABLE
	INTEGER*4   LRC  	    !CHECKSUM RESULT
	INTEGER*4   TEMP            !TEMP I*4 VARIABLE USED IN IEOR
C
	LRC = 0		            !INITIALIZE RESULT
C
	DO 1000 I=BEG_IDX,END_IDX
	  TEMP = ZEXT(ARRAY(I))
	  LRC=IEOR(TEMP,LRC)
1000	CONTINUE
C
	RETURN
	END
