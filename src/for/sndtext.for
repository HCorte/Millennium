C
C SUBROUTINE SNDTEXT
C
C SNDTEXT.FOR
C
C V18 02-SEP-2013 FJG CR15(2) ADD DATE AND CLOSE TIME
C V17 26-AUG-2013 FJG CR15 SPORTS NEW TEAMS NAME REPORTS
C V16 01-DEC-2000 UXN TOTOGOLO ADDED.
C V15 17-MAY-1999 UXN Super Triple added.
C V14 18-MAR-1999 RXK Game type/game index change. Hack for V5 removed.
C V13 20-JAN-1998 UXN Super Score and Todays Triple added.
C V12 10-DEC-1995 HXK Removed unused code
C V11 10-NOV-1995 HXK Further changes for Double, Couple
C V10 05-MAY-1995 HXK V5 entered into database again!!!!
C V09 22-FEB-1995 HXK HACK FOR V5
C V08 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V07 13-AUG-1993 HXK CORRECTED PROBLEMS WITH INSTANT TICKETS
C V06 10-AUG-1993 HXK FIXED INSTANT TEXT MESSAGE
C V05 24-JUN-1993 GXA Released for Finland Dec Conversion / Oddset.
C V04 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V03 04-MAR-1996 wsm Do not include GLOBAL.DEF if including MSGCOM.DEF,
C                     removed one argument in READMSG call.
C V02 07-NOV-1991 GCAN INITIAL RELEASE FOR THE NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SUBROUTINE TO PROCESS TERMINAL TEXT MESSAGE REQUESTS.
C
C CALLING SEQUENCE:
C     CALL SNDTEXT(TRABUF,MESTAB,OUTLEN) 
C INPUT
C     TRABUF - INTERNAL TRANSACTION FORMAT
C     MESTAB - TERMINAL INPUT MESSAGE
C OUTPUT
C     MESTAB - TERMINAL OUTPUT MESSAGE
C     OUTLEN - OUTPUT LENGTH
C
C    ( MESNUM - MESSAGE NUMBER, WRITTEN TO TM AFTER USE)
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C---------------------------------------------------------------------------
C
C Text Message  Terminal --> Host
C -------------------------------
C
C Start Byte  End Byte	  Size	  Contents
C --------------------------------------------------------------------------
C	   1 -	     1	     1	  Control (0010)    , Sequence #
C	   2 -	     2	     1	  Type = 3	    , Subtype = 5
C	   3 -	     4	     2	  Checksum
C	   5 -	     5	     1	  Game Type	    , Game Index
C	   6 -       6       1    Segment Number
C --------------------------------------------------------------------------
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE SNDTEXT(TRABUF,MESTAB,OUTLEN)
	IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
	INCLUDE 'INCLIB:MSGCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:SPTCOM.DEF'        
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMDLL.DEF'
        INCLUDE 'INCLIB:CHKSUMCM.DEF'
C
        INTEGER*4   GNUM                         !Game Number
        INTEGER*4   GTYP                         !Game Type
        INTEGER*4   GIND                         !Game Index
	INTEGER*4   SEGNUM			 !Segment Number requested.
        INTEGER*4   I4TEMP                       !Temp. Variable
        INTEGER*4   CHKLEN                       !Length of checksumed msg.
        INTEGER*4   MYCHKSUM                     !Checksum of message.
	INTEGER*4   MESLEN                       !Length of mescom text
	INTEGER*4   MESNUM                       !Message common number
        INTEGER*2   OUTLEN                       !Output Length
        INTEGER*2   I2TEMP(2)                    !Temp. Variable
        INTEGER*2   OFFS                         !OFFSET
        INTEGER*2   MOFF
        INTEGER*2   XONE                         !Loop index
        INTEGER*2   XTWO                         !Loop index
        INTEGER*2   SIND                         !TEMP SEGMENT OFFSET   
        INTEGER*2   STMP                         !TEMP SEGMENT IN PLACE
        INTEGER*4   TEMP                         !TEMP VARIABLE
C
        INTEGER*1   TMP1                         !1 BYTE TEMP   
        INTEGER*2   TMP2                         !2 BYTE TEMP           
C
        BYTE        ERRTYP                       !Type Subtype for Error Message
        BYTE        I1TEMP(4)                    !Temp. Variable
        BYTE        MESTAB(*)                    !Table of I/O Messages
	BYTE        BMSGBUF(1000)                !Byte message buffer
	CHARACTER   MSGBUF*1000                  !text character buffer
	
C
        EQUIVALENCE (I4TEMP,I2TEMP(1),I1TEMP(1))
	EQUIVALENCE (MSGBUF,BMSGBUF)
        DATA ERRTYP/Z90/
C
	SYNTERRCOD = 0
	I4TEMP = 0
C
D	TYPE*,IAM(),'>>> SNDTEXT'
C
C GET GAME TYPE AND GAME INDEX
C
	GTYP = ZEXT(MESTAB(5))
	GIND = ZEXT(MESTAB(6))
D	TYPE*,IAM(),'>>> GTYP/GIND',GTYP,GIND
	TRABUF(TGAMTYP) = GTYP
	TRABUF(TGAMIND) = GIND

	IF(TRABUF(TGAMTYP).LT.1.OR.TRABUF(TGAMTYP).GT.MAXTYP) THEN
     	   TRABUF(TERR) = SYNT
	   SYNTERRCOD = 10
           GOTO 8000 
	ENDIF
C
	IF(TRABUF(TGAMIND).LT.1.OR.TRABUF(TGAMIND).GT.MAXIND) THEN
           TRABUF(TERR) = SYNT
	   SYNTERRCOD = 30
           GOTO 8000      
	ENDIF
C
C GET GAME NUMBER
C
	GNUM = GTNTAB(GTYP,GIND)
	IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
	   TRABUF(TERR) = SYNT
	   SYNTERRCOD = 40
           GOTO 8000 
	ENDIF
C
C CHECK IF WE SHOULD HAVE TEXT TO SEND
C
	IF(TRABUF(TGAMTYP).NE.TTSL.AND.TRABUF(TGAMTYP).NE.TWIT.AND.
     *	   TRABUF(TGAMTYP).NE.TSCR.AND.TRABUF(TGAMTYP).NE.TSPT.AND.
     *     TRABUF(TGAMTYP).NE.TDBL.AND.TRABUF(TGAMTYP).NE.TTGL.AND.
     *     TRABUF(TGAMTYP).NE.TCPL.AND.TRABUF(TGAMTYP).NE.TSSC.AND.
     *     TRABUF(TGAMTYP).NE.TTRP.AND.TRABUF(TGAMTYP).NE.TSTR) THEN
	   TRABUF(TERR) = SYNT
	   SYNTERRCOD = 20
           GOTO 8000
	ENDIF
C
C GET SEGMENT NUMBER
C
	SEGNUM = ZEXT(MESTAB(7))
	IF(SEGNUM.LT.0.OR.SEGNUM.GT.20) THEN
	   TRABUF(TERR)=SYNT
	   SYNTERRCOD = 50
           GOTO 8000 
	ENDIF
        MESNUM = (GAMTXT(GNUM)-1) + SEGNUM
C
C GET TEXT MESSAGE FROM MESSAGE COMMON , ONLY IF NO ERROR SO FAR!
C
        IF(GTYP.NE.TSPT) THEN
	  IF(TRABUF(TERR).EQ.NOER) THEN
	    IF(MESNUM.GT.1.AND.MESNUM.LT.GAMTXT(MAXGAM)+20) THEN
      	      CALL READMSG(MSGCHR,MESNUM,MSGBUF,MESLEN)
	      IF(MESLEN.LT.1.OR.MESLEN.GT.256) THEN
	        TRABUF(TERR)=SYNT
	        SYNTERRCOD = 60
                GOTO 8000
	      ENDIF
	    ENDIF
	  ENDIF
C
C FOR SPORTS IF DRAW IS NOT OPENED, RETURN ERROR
C
	ELSE
	  IF(SPTSTS(GIND).NE.GAMOPN) THEN
	    TRABUF(TERR) = RNIN
	    GOTO 8000
	  ENDIF
	ENDIF

8000   CONTINUE

C
C STORE SOME FUN STUFF
C
	TRABUF(TGAM) =  GNUM
	TRABUF(TSNEW) = SEGNUM
	TRABUF(TSDT1) = MESNUM
	TRABUF(TSDT2) = MESLEN
	TRABUF(TSDT3) = I4TEMP
	TRABUF(TSDT4) = SYNTERRCOD
C
C IF TRANSACTION STATUS IS NOT GOOD
C BUILD ERROR MESSAGE.
C
	IF(TRABUF(TERR).NE.NOER) TRABUF(TSTAT)=REJT
	IF(TRABUF(TSTAT).NE.GOOD) THEN
	  MESTAB(2) = ERRTYP
	  MESTAB(5) = TRABUF(TERR)
	  MESTAB(6) = 0
	  OUTLEN = 6
	  GOTO 9000
	ENDIF
C
C TRANSFER TEXT TO PROCOM BUFFER
C
        IF(GTYP.EQ.TSPT) THEN ! LET's BUILD IT CORRECTLY
D	  TYPE*,IAM(),'>>> BUILDING MESSAGE'
          OFFS = 8
          STMP = 1
          MESTAB(OFFS) = SPTMAX(GIND)  ! IT IS SEND IN ALL SEGMENTS
          OFFS = OFFS + 1
          SIND = OFFS
          MOFF = OFFS
D	  TYPE*,IAM(),'>>> (1) OFFS/STMP/SIND',OFFS,STMP,SIND
C+++++++++TEAM NAMES
          DO 10 XTWO=1,SPTMAX(GIND)
            DO 20 XONE=1,2
              SIND = SIND+SPNMS_LEN
              IF(SIND.GT.256) THEN
                STMP = STMP+1
                SIND = MOFF+SPNMS_LEN
              ENDIF
C
              IF(SEGNUM.EQ.STMP) THEN
                CALL MOVBYT(SPTNMS(1,XONE,XTWO,GIND),1,MESTAB,OFFS,SPNMS_LEN)
                OFFS = OFFS + SPNMS_LEN
              ENDIF
D	      TYPE*,IAM(),'>>> (2) OFFS/STMP/SIND',OFFS,STMP,SIND              
20          CONTINUE              
10        CONTINUE
C+++++++++DATE AND CLOSE TIME
          SIND = SIND + 7
          IF(SIND.GT.256) THEN
            STMP = STMP+1
            SIND = MOFF+7
          ENDIF
          IF(SEGNUM.EQ.STMP) THEN
            TEMP = SPTTIM(GIND)
            IF(TEMP.GT.'40000000'X) TEMP=TEMP-'40000000'X
D	    TYPE*,IAM(),'>>> SPTTIM ',TEMP
            CALL PUTIME(TEMP,MESTAB,OFFS)  ! CARE OFFS IS INCREASED INSIDE FUNCTION
            I4TEMP = SPTESD(GIND)
            MESTAB(OFFS+0) = I1TEMP(2)
            MESTAB(OFFS+1) = I1TEMP(1)
D	    TYPE*,IAM(),'>>> SPTESD ',SPTESD(GIND)
C
            CALL FIGWEK(SPTBSD(GIND),TMP2,TEMP)
            MESTAB(OFFS+2) = TMP2
            MESTAB(OFFS+3) = MOD(TEMP,100)
D	    TYPE*,IAM(),'>>> BSD/WEEK/YEAR ',SPTBSD(GIND),TMP2,TEMP
            OFFS = OFFS+4
          ENDIF
D	  TYPE*,IAM(),'>>> (3) OFFS/STMP/SIND',OFFS,STMP,SIND          
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C         DO XONE=OUTLEN+1,640
C           MESTAB(XONE)= 42
C         ENDDO
C         OUTLEN = 640
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
          IF(SEGNUM.EQ.STMP) MESTAB(7)=0        ! SEGNUM TO ZERO IF LAST SEGMENT
          OUTLEN = OFFS - 1
C         
      	  TRABUF(TSDT1) = 0
      	  TRABUF(TSDT2) = OUTLEN
        ELSE
    	  CALL MOVBYT(BMSGBUF,1,MESTAB,7,MESLEN)
    	  OUTLEN = MESLEN + 6
    	ENDIF
C
C CALCULATE CHECKSUM
C
9000	CONTINUE
	I4CCITT = TRABUF(TCHK)
	MESTAB(3) = I1CCITT(2)
	MESTAB(4) = I1CCITT(1)
	CHKLEN=OUTLEN-1
	CALL GETCCITT(MESTAB,1,CHKLEN,MYCHKSUM)
	I4CCITT = MYCHKSUM
	MESTAB(3) = I1CCITT(2)
	MESTAB(4) = I1CCITT(1)
	RETURN
	END
