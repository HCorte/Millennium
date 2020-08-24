C
C PROGRAM X2TERMSTA
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2TERMSTA.FOV                                $
C  $Date::   17 Apr 1996 16:38:32                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C ** Source - X2TERMSTA.FOR **
C
C X2TERMSTA.FOR
C
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C This program will print a list of the status of the terminals, 
C sorted by error code and x2x-address. The terminals with a non-zero
C installation date are printed out first.
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
C Copyright 1994 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW

	PROGRAM X2XTERMSTA                       
	IMPLICIT NONE  
	INCLUDE 'INCLIB:X2TERMSTA.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
	INCLUDE 'INCLIB:SYSPARAM.DEF'
C
C	Variable Declarations:
C
	INTEGER*4 SORTARRAY(LEN_SORTARRAY,X2X_TERMS)
C
C THE PROGRAM USES SORTARRAY AS A STORAGE LOCATION FOR THREE DIFFERENT
C LISTS: INSTALLED TERMINALS, NON-INSTALLED TERMINALS, AND TERMINALS
C HAVING A NON-ZERO DELIVERY ACK, ZERO ERROR AND NO TRANSACTIONS. 
C THE SEPARATION BETWEEN LIST 1 AND 2 IS POINTED TO BY END_ARRAY, AND 
C BETWEEN 2 AND 3 BY POINTER_LIST2
C

	INTEGER*4 TEMPRECORD(LEN_SORTARRAY)
	INTEGER*4 OFFSET_FROM_END,BACKPOINTER,FORWPOINTER,I,END_ARRAY
	INTEGER*4 STN
	INTEGER*4 POINTER_LIST1,POINTER_LIST2,STORE_POINTER
        INTEGER*4 CLASS,SUBNET,STN_SUBNET,EXT
C
C Ask for subnetwork
C
        CALL INPNUM('Enter Subnetwork number ',SUBNET,0,20,EXT)
        IF(EXT.LT.0) CALL GSTOP(GEXIT_OPABORT)
C
C
C	Add 'terminal number', 'X2X Address' and 'Error number' fields
C	to program table.
	POINTER_LIST1 = 1
	POINTER_LIST2 = X2X_TERMS
C
	DO 1000,I=1,X2X_TERMS
	    STN = X2XT_STATION_NO(I)
	    IF (STN.EQ.0) GOTO 1000
            CLASS = X2XS_STNCLS(STN)
	    IF (CLASS.LE.0) GOTO 1000
            STN_SUBNET = X2XC_SUBNETWORK(CLASS)
	    IF (STN_SUBNET.NE.SUBNET) GOTO 1000
	    IF (X2XS_CNT_ACTIVE(STN).EQ.0.AND.
     *	        X2XS_ACK_CNT(STN).NE.0.AND.
     *		X2XS_LAST_ERR_CODE(STN).EQ.0) THEN
		   STORE_POINTER = POINTER_LIST2
		   POINTER_LIST2 = POINTER_LIST2-1
	    ELSE
		   STORE_POINTER = POINTER_LIST1
		   POINTER_LIST1 = POINTER_LIST1+1
	    ENDIF
C
	    SORTARRAY(TERM_NUMBER,STORE_POINTER) = I
	    SORTARRAY(ADDRESS1,STORE_POINTER) = X2XS_ADRESS(1,STN)
	    SORTARRAY(ADDRESS2,STORE_POINTER) = X2XS_ADRESS(2,STN)
	    SORTARRAY(ERR_NR,STORE_POINTER) = X2XS_LAST_ERR_CODE(STN)
1000	CONTINUE
C
C	Create internal program table and update 'Install date', 'First date',
C	and 'Last date' fields.
C
	TYPE *,POINTER_LIST1,POINTER_LIST2
C
	CALL X2UPDATE_INTERNALFILE(SORTARRAY)
C
	POINTER_LIST1 = POINTER_LIST1-1
	IF (POINTER_LIST1 .LE. 0) THEN
	    TYPE *,'NO STATIONS FOUND FOR ENTERED SUBNET, EXITING...'
	    CALL EXIT
	ENDIF
C
C	Sort the program according to the 'Installation date' field.
	CALL I4XSORT(SORTARRAY,LEN_SORTARRAY,POINTER_LIST1,INST_DATE,0,0)
C
C	Reverse the order of the table so that the most recently
C	active terminal is at the top.
	BACKPOINTER = POINTER_LIST1
	DO I=1,(POINTER_LIST1+1)/2
	    OFFSET_FROM_END = POINTER_LIST1-I+1
	    CALL X2COPY_RECORD(TEMPRECORD,SORTARRAY(1,OFFSET_FROM_END))
	    IF (TEMPRECORD(INST_DATE) .NE. 0) BACKPOINTER = OFFSET_FROM_END
	    IF (SORTARRAY(INST_DATE,I) .EQ. 0) FORWPOINTER = I
	    CALL X2COPY_RECORD(SORTARRAY(1,OFFSET_FROM_END),SORTARRAY(1,I))
	    CALL X2COPY_RECORD(SORTARRAY(1,I),TEMPRECORD)
	END DO
C
C	Get offset to first terminal in table with a zero 'Last date'.
	IF (BACKPOINTER .LE. (POINTER_LIST1+1)/2+1) END_ARRAY = 
     *	      POINTER_LIST1 - FORWPOINTER + 1
	IF (FORWPOINTER .GE. (POINTER_LIST1)/2) END_ARRAY = 
     *	      POINTER_LIST1 - BACKPOINTER + 1
C
C	Sort all terminal records with a positive 'installation date'
C        according to the 'Error number' and the 'X2X address'.
C
	CALL I4XSORT(SORTARRAY,LEN_SORTARRAY,END_ARRAY,ERR_NR,
     *	  ADDRESS1,ADDRESS2)
C
	IF (END_ARRAY .LT. POINTER_LIST1) THEN
		CALL I4XSORT(SORTARRAY(1,END_ARRAY+1),LEN_SORTARRAY,
     *	         POINTER_LIST1-END_ARRAY,ERR_NR,ADDRESS1,ADDRESS2)
	ENDIF
C
C	Generate a report file from the program table.
	CALL X2PRINT_REPORT(SORTARRAY,POINTER_LIST1+1,POINTER_LIST2+1,
     *	  END_ARRAY+1,SUBNET)

	END

C
C************** COPY_RECORD() **********************
C	Copies two records in the program table.
C
	SUBROUTINE X2COPY_RECORD(NEW_RECORD,OLD_RECORD)                       
	IMPLICIT NONE  
	INCLUDE 'INCLIB:X2TERMSTA.DEF'
C
	INTEGER*4 NEW_RECORD(LEN_SORTARRAY)	
	INTEGER*4 OLD_RECORD(LEN_SORTARRAY)
	INTEGER*4 I

C	Do the copy.
	DO I=1,LEN_SORTARRAY
	    NEW_RECORD(I) = OLD_RECORD(I)
	END DO
C
	RETURN

	END
