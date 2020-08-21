C
C SUBROUTINE X2SHOW_TBL
C
C*************************** START X2X PVCS HEADER ****************************
C
C  $Logfile::   GXAFXT:[GOLS]X2SHOW_TBL.FOV                               $
C  $Date::   17 Apr 1996 16:34:22                                         $
C  $Revision::   1.0                                                      $
C  $Author::   HXK                                                        $
C
C**************************** END X2X PVCS HEADER *****************************
C  
C ** Source - x2gtxsnp.for **
C
C X2SHOW_TBL.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Added SYSPARAM.DEF, GLOBAL.DEF,
C                            PRMAGT.DEF, AGTINF.DEF for Finland.
C
C V01 13-DEC-94 GPR RELEASED FOR UK
C
C This routine will display a table with X.25 data for each port of the sap
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
C
	SUBROUTINE X2SHOW_TABLE
     *	  (ARRAY,LENGTH,SIZE,START_ELEMENT,BEG_SCR,END_SCR,LEFT,RIGHT,
     *		MOVE_DIR,NEW_SAP)
C
	IMPLICIT NONE
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:AGTINF.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'
C
	INTEGER*4   ARRAY(*)
	INTEGER*4   SIZE,LENGTH
	INTEGER*4   BEG_SCR,END_SCR
	INTEGER*4   LEFT,RIGHT
	INTEGER*4   MOVE_DIR
	INTEGER*4   SIZE_SCR,WIDTH
	INTEGER*4   START_ELEMENT
	INTEGER*4   GLOB_START/0/
	INTEGER*4   I,J
	INTEGER*4   NEW_SAP
C***	INTEGER*4   POS(*)
C	CHARACTER*5 STATE_SAP(3)
C	DATA STATE_SAP /'Idle ','Onl. ','Down '/
C
	COMMON /GLOBAL/ GLOB_START
C
	SIZE_SCR = END_SCR - BEG_SCR + 1
	WIDTH = RIGHT-LEFT
C
C***	TABPOS(1) = POS(1)+2+LEFT
C***	DO J=2,LENGTH
C***	    TABPOS(J) = TABPOS(J-1) + POS(J)+1
C***	END DO
C
	IF (MOVE_DIR.NE.0) THEN
	    GLOB_START = GLOB_START + MOVE_DIR*SIZE_SCR
C	    TYPE *,MOVE_DIR*SIZE_SCR,GLOB_START,SIZE
C	    PAUSE
	ELSE
	    GLOB_START = START_ELEMENT
	ENDIF
C
	IF (GLOB_START.GT.SIZE) GLOB_START=SIZE
	IF (GLOB_START.LT.1) GLOB_START=1
C
	J=1
	DO I=0,SIZE_SCR
	    IF (GLOB_START+I.GT.SIZE) GOTO 1000
	    WRITE (XNEW(I+BEG_SCR),9050) 
     *	       (ARRAY(J+(GLOB_START+I-1)*LENGTH),J=1,LENGTH)
C***9050	FORMAT(<LENGTH+1>(' ',T<TABPOS(J)>,I<POS(J)>,'|'))
	END DO
C
1000	CONTINUE
C
C CLEAR OTHER LINES
C
	IF (I.NE.SIZE_SCR) THEN
	    DO J=I,SIZE_SCR
		WRITE (XNEW(J+BEG_SCR),9010)
	    END DO
	ENDIF
C
	NEW_SAP = GLOB_START
C
	RETURN
C
9010	FORMAT(80X)
9050	FORMAT(<LEFT>X,I3,'|',I3,'|',2(I5,'|'),2(I6,'|'),7(I5,'|'))
C
	END
