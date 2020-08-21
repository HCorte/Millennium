C
C *** SUBROUTINE GETPARM ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_GETPARM.FOV                              $
C  $Date::   17 Apr 1996 14:07:44                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_getparm.for ***
C
C V03 27-FEB-96 wsm Replaced SLEN with SPCLEN for Finland.
C V02 27-JAN-93 RRB DON'T CHECK FOR AUXILIARY PORTS HERE.
C                   CALL NOSPACES TO FIND BEGIN & END INDEX INTO TARGET STRING.
C V01 14-JAN-91 RRB EXTRACT NETWORK AND LOCAL PORT NUMBERS
C                   FROM TELENEX MATRIX SWITCH MESSAGES AND
C                   MESSAGE TRAP ROUTINE.
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
C Calling Sequence:
C	CALL GETPARM(MESS, SUBSTRING, VALUE)
C
C Input:	MESS      - MESSAGE FROM MSC OR MESTRAP
C	        SUBSTRING - PARAMETER PREFIX (TEXT TO SEARCH FOR)
C
C Output:	VALUE     - PARAMETER VALUE (0 - NOT FOUND, INVALID)
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GETPARM(MESS,SUBSTRING,VALUE)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:MSCCOM.DEF'
C
	CHARACTER*(*) MESS                 !MESSAGE TO SEARCH
	CHARACTER*(*) SUBSTRING            !TEXT TO SEARCH FOR
	INTEGER*4 VALUE                    !VALUE TO RETURN
C
	CHARACTER*1 CHAR
	BYTE        ICHAR
	EQUIVALENCE (CHAR,ICHAR)
	INTEGER*4   I, ST, DIG, SPCLEN, IND, IND1, IND2
	LOGICAL*4   FIRST
C
	VALUE = 0
C
	CALL NOSPACES(SUBSTRING,IND1,IND2,SPCLEN)	
C
D	TYPE*,IAM(),'Searching for ',SUBSTRING(IND1:IND2)
D	TYPE*,IAM(),'In ',MESS(1:SPCLEN)
	IND=INDEX(MESS(1:SPCLEN),SUBSTRING(IND1:IND2))
	IF(IND.GT.0) THEN
D	TYPE*,IAM(),'INDEX INTO STRING = ',IND
D	TYPE*,IAM(),'STRING LENGTH = ',SPCLEN
C
C FOUND TARGET STRING. EXTRACT VALUE.
C (SEARCH EIGHT CHARACTERS, IGNORING SPACES, OR UNTIL WE FIND A NON-ASCII
C DECIMAL VALUE.
C
	   IND = IND + SPCLEN
	   DO 100 I = IND,IND+8
	      CHAR = MESS(I:I)
D	      TYPE*,IAM(),'CONVERTING CHARACTER ',CHAR
	      IF(CHAR.NE.' ') THEN
	         CALL ASCBIN(ICHAR,1,1,DIG,ST)
                 IF(ST.NE.0) THEN
	            GOTO 200
	         ENDIF
	         VALUE = VALUE*10 + DIG
	      ENDIF
100	   CONTINUE
C
200        CONTINUE
C
	ENDIF
        RETURN
        END
