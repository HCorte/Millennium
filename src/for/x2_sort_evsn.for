C
C  GXSRC:X2_SORT_EVSN.FOR
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2_SORT_EVSN.FOV                             $
C  $Date::   17 Apr 1996 16:46:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C X2X_SORT_EVSN.FOR
C
C V01 28-MAY-92 JWE Initial relesae
C
	PROGRAM X2_SORT_EVSN
	IMPLICIT NONE
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
C When the GVTid table is full this program will clear out the
C overflows and resort the table...
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:GLOBAL.DEF'
C
	INCLUDE	'INCLIB:X2XCOM.DEF'
C
	INTEGER*4   STATUS
	INTEGER*4   ADDED_EVSN
	INTEGER*4   LENGTH_INDEX
	INTEGER*4   LOOP_COUNT
C
	CALL COPYRITE
C
1000	CONTINUE
	IF(LOKON(X2X_LOCK_EVSN))THEN
	    CALL XWAIT(1, 2, STATUS)
	    TYPE *, IAM(), 'Waiting for lock...'
	    GOTO 1000
	ENDIF
C
	LOOP_COUNT  =	 0
C
C Go through all of the added addresses
C
	DO 1100	ADDED_EVSN = X2X_STATIONS + 1, X2X_STATIONS
	1   + X2X_ADDED_EVSN
	    LOOP_COUNT	=   LOOP_COUNT + 1
C
C Make sure that the slot is empty
C
	    IF(X2X_SORTED_EVSN(1, LOOP_COUNT) .NE. 0 .AND.
	1	X2X_SORTED_EVSN(2, LOOP_COUNT))THEN
		TYPE *, IAM(), 'Station table is full...'
		GOTO 1100
	    ENDIF
C
C Start putting addressess into the start of the table were the zero's 
C end up...
C
C	move station #
C
	    X2X_SORTED_EVSN(0, LOOP_COUNT) =
	1	X2X_SORTED_EVSN(0, ADDED_EVSN)
	    DO 1200 LENGTH_INDEX = 1, X2X_EVSN_MAXLEN
		X2X_SORTED_EVSN(LENGTH_INDEX, LOOP_COUNT) =
	1		X2X_SORTED_EVSN(LENGTH_INDEX, ADDED_EVSN)
1200	    CONTINUE
1100	CONTINUE
C
	CALL I4XSORT(X2X_SORTED_EVSN, 3, X2X_STATIONS, 2, 3, 0)
C
	X2X_ADDED_EVSN	=   0
	X2X_SORTED_EVSN_UPDATE	=   1
C
8000	CONTINUE
	CALL LOKOFF(X2X_LOCK_EVSN)
C
	CALL GSTOP(GEXIT_SUCCESS)
	END
