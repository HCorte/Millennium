C
C *** SUBROUTINE NOSPACES ***
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]MSC_NOSPACES.FOV                             $
C  $Date::   17 Apr 1996 14:07:52                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C *** Pre-Baseline Source - msc_nospaces.for ***
C
C V01 27-JAN-93 RRB INITIAL RELEASE.
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
C	RETURNS BEGINNING AND ENDING INDEX OF ACTUAL STRING 
C	WHICH MAY HAVE LEADING AND/OR TRAILING SPACES.
C
C Calling Sequence:
C	CALL NOSPACES(STRING, IND1, IND2, SLEN)
C
C	NOTE: STRING ITSELF SHOULD NOT HAVE EMBEDDED SPACES.
C
C Input:	STRING - STRING TO SUCK SPACES OUT OF
C
C Output:	IND1   - FIRST CHARACTER POSITION
C		IND2   - LAST CHARACTER POSITION
C		SLEN   - ACTUAL LENGTH OF STRINGS 
C		         W/O LEADING AND TRAILING SPACES
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE NOSPACES(STRING,IND1,IND2,SLEN)
C
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	CHARACTER*(*) STRING               !STRING TO EXTRACT 
	INTEGER*4 IND1                     !FIRST CHARACTER IN STRING
	INTEGER*4 IND2                     !LAST CHARACTER IN STRING
	INTEGER*4 SLEN                     !ACTUAL STRING LENGTH
	INTEGER*4 IND                      !POSITION OF ' '
C
	IND1 = 1
	IND2 = LEN(STRING)
100     CONTINUE
	IND = INDEX(STRING(IND1:),' ')
	IF(IND.NE.0) THEN           
	   IF(IND.EQ.1) THEN        !LEADING SPACES
	     IND1 = IND1 + 1
	     GOTO 100
	   ELSE		   	    !TRAILING SPACES
	     IND2 = IND1 + IND - 2
	   ENDIF
	ENDIF
	SLEN = LEN(STRING(IND1:IND2))
	RETURN
	END
