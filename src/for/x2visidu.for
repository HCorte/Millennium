C
C SUBROUTINE X2VISIDU
C  
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2VISIDU.FOV                                 $
C  $Date::   17 Apr 1996 16:40:20                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C
C ** Source - x2visidu.for **
C
C V01 06-OCT-94 SCD CREATE ROUTINE TO HANDLE IDU IDs
C
C ===========================================================
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
	SUBROUTINE X2VISIDU(SCRIDX,LEVEL,IDUID,LEN,ERR)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:X2XCOM.DEF'
	INCLUDE 'INCLIB:X2VIS.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'				!V02
C
	CHARACTER   IDUID*(*)         !IDU ID in ascii		!V02
        CHARACTER   NULL_X2XIDU*(LGSER) ! ALL 0'S.		!V02
	INTEGER*4   SCRIDX          !Screen
	INTEGER*4   LEVEL           !Level of input
	INTEGER*4   LEN             !Length of input
	INTEGER*4   ERR             !Return error code
	INTEGER*4   BCDADR(2)       !BCD station address
	INTEGER*4   STN, I, OFF, OFF1
	INTEGER*4   EVSNNUM(X2X_EVSN_MAXLEN)

	LOGICAL     TESTBIT         !Test for valid bits - V02
C
C CONVERT THE BINARY STRING TO THE BCD STRING.
C
	ERR=0
	BCDADR(1)=0
	BCDADR(2)=0
        DO I = 1, LGSER
          NULL_X2XIDU(I:I) = '0'
	ENDDO
C
C IF INPUT DATA HAS BEEN PASSED, SEARCH COMMON FOR THE
C INPUT ADDRESS TO FIND THE STATION NUMBER.
C
	IF(IDUID.GT.NULL_X2XIDU) THEN
	  CALL ATOH(IDUID,1,MIN0(LEN,LGSER),EVSNNUM,ERR)
	  IF(ERR.NE.0) RETURN

C LINEARLY SEARCH THE EXTENDED VERIFICATION SEQUENCE NUMBER TABLE
C TO OBTAIN THE STATION NUMBER. IF IT IS NOT FOUND, THEN RETURN A STATION
C NUMBER OF 0.
C
	  STN = -1					!INITIALLY, NO MATCH
          DO 120 OFF=1,X2X_STATIONS
             IF(X2XS_EVSN(1,OFF).EQ.0 .AND.
     *          X2XS_EVSN(2,OFF).EQ.0) GOTO 120
             DO 110, OFF1=1,X2X_EVSN_MAXLEN
                IF(X2XS_EVSN(OFF1,OFF).NE.EVSNNUM(OFF1))GOTO 120
110          CONTINUE
             STN=OFF
120       CONTINUE
	ENDIF
C
C	  Save the station if the address was found
C
	  IF(STN.NE.-1) THEN
	    X2FLDINF(XSTNIDX)=STN
	    X2SCRN(X2SCRN_KEYLEV3+LEVEL,SCRIDX)=XIDUIDX
C
C	  Otherwise set station number to 0
C
	  ELSE
	   STN = 0
	  ENDIF
C
C ***** Start V02 changes *****
C	STATION bit, ADR bit and GVT bit are mutually exclusive.  If 
C	STATION bit or ADR bit are set, then clear them.

	TESTBIT=BJTEST(X2SCRN(X2SCRN_FILFLDS,SCRIDX),XSTNIDX)
	IF(TESTBIT) X2SCRN(X2SCRN_FILFLDS,SCRIDX) = 
     *		    JIBCLR(X2SCRN(X2SCRN_FILFLDS,SCRIDX),XSTNIDX)

	TESTBIT=BJTEST(X2SCRN(X2SCRN_FILFLDS,SCRIDX),XADRIDX)
	IF(TESTBIT) X2SCRN(X2SCRN_FILFLDS,SCRIDX) = 
     *		    JIBCLR(X2SCRN(X2SCRN_FILFLDS,SCRIDX),XADRIDX)

	TESTBIT=BJTEST(X2SCRN(X2SCRN_FILFLDS,SCRIDX),XGVTIDX)
	IF(TESTBIT) X2SCRN(X2SCRN_FILFLDS,SCRIDX) = 
     *		    JIBCLR(X2SCRN(X2SCRN_FILFLDS,SCRIDX),XGVTIDX)

C ***** End V02 changes *****

	RETURN
	END
