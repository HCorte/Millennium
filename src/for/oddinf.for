C
C V02 10-MAY-1999 UXN Today's Triple changed to Today's Trio.
C                     Super Triple added.
C V01 20-MAY-1998 UXN Initial release. 
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
C Copyright 1998 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	PROGRAM ODDINF
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSDEFINE.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:ODDINF.DEF'
C
	INTEGER*4	ST,LEN,GAME_CODE
	COMMON		SCFREC
	CHARACTER*2	FUN
	INTEGER*4	MAX_GAMES	! how many games in ODDINF
	PARAMETER	(MAX_GAMES=9)
	CHARACTER*20	FILE_NAME(MAX_GAMES),TMP_NAME
	INTEGER*4	LUN
	DATA FILE_NAME	/'FILE:PITKA.FTP      ','FILE:TULOS.FTP      ',
     *                   'FILE:VOITTAJA.FTP   ','FILE:SUPER.FTP      ',
     *                   'FILE:COUPLE.FTP     ','FILE:SSCORE.FTP     ',
     *                   'FILE:TTRIO.FTP      ','FILE:VAKIO.FTP      ',
     *                   'FILE:STRIPLE.FTP    '/
	LOGICAL*4	UPDATE
C
C Get system configuration from SCF.FIL
C
	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C	
C Display main menu.
C
100	CONTINUE
	WRITE(6,9000) GTNAMES(TTSL),GTNAMES(TSCR),GTNAMES(TWIT),
     *                GTNAMES(TDBL),GTNAMES(TCPL),GTNAMES(TSSC),
     *                GTNAMES(TTRP),GTNAMES(TSPT),GTNAMES(TSTR)
C	
	CALL INPTEXT('Enter function ',FUN,LEN)
	IF(LEN.LE.0) GOTO 100
	CALL STR$UPCASE(FUN,FUN)
	IF(FUN.EQ.'EX') CALL GSTOP(GEXIT_SUCCESS)
	IF(FUN.EQ.'TS') THEN
	    GAME_CODE = 1
	ELSEIF(FUN.EQ.'SC') THEN
	    GAME_CODE = 2
	ELSEIF(FUN.EQ.'WT') THEN
	    GAME_CODE = 3
	ELSEIF(FUN.EQ.'DB') THEN
	    GAME_CODE = 4
	ELSEIF(FUN.EQ.'CP') THEN
	    GAME_CODE = 5
	ELSEIF(FUN.EQ.'SS') THEN
	    GAME_CODE = 6
	ELSEIF(FUN.EQ.'TR') THEN
	    GAME_CODE = 7
	ELSEIF(FUN.EQ.'VA') THEN
	    GAME_CODE = 8
	ELSEIF(FUN.EQ.'ST') THEN
	    GAME_CODE = 9
	ELSE
	    GOTO 100
	ENDIF
	CALL INPTEXT('Enter file name (RETURN for '//FILE_NAME(GAME_CODE)//') ',
     *               TMP_NAME,LEN)
	IF(LEN.LE.0) TMP_NAME=FILE_NAME(GAME_CODE)
C
C Open the ODDINF file and call proper subroutine.
C	
	LUN = 9
	OPEN(UNIT=LUN,FILE=TMP_NAME,IOSTAT=ST,STATUS='OLD',
     *       RECL=512,ORGANIZATION='SEQUENTIAL',ACCESS='SEQUENTIAL')
	IF(ST.NE.0) THEN
	    TYPE*,IAM(),'Error opening ',TMP_NAME,' status=',ST
	    CALL GSTOP(GEXIT_FATAL)
	ENDIF
C
C Update or report mode.
C
200	CONTINUE
	WRITE(6,9001)
	CALL INPTEXT('Enter selection ',FUN,LEN)	
	IF(LEN.LE.0) GOTO 200
	CALL STR$UPCASE(FUN,FUN)
	IF(FUN.EQ.'UP') THEN
	    UPDATE = .TRUE.
	ELSEIF(FUN.EQ.'RP') THEN
	    UPDATE = .FALSE.
	ELSE
	    GOTO 200
	ENDIF
	IF(GAME_CODE.EQ.1) THEN
	    CALL ODDINF_TTSL(LUN,UPDATE,ST)
	ELSEIF(GAME_CODE.EQ.2) THEN
	    CALL ODDINF_TSCR(LUN,UPDATE,ST)
	ELSEIF(GAME_CODE.EQ.3) THEN
	    CALL ODDINF_TWIT(LUN,UPDATE,ST)
	ELSEIF(GAME_CODE.EQ.4) THEN
	    CALL ODDINF_TDBL(LUN,UPDATE,ST)
	ELSEIF(GAME_CODE.EQ.5) THEN
	    CALL ODDINF_TCPL(LUN,UPDATE,ST)
	ELSEIF(GAME_CODE.EQ.6) THEN
	    CALL ODDINF_TSSC(LUN,UPDATE,ST)
	ELSEIF(GAME_CODE.EQ.7) THEN
	    CALL ODDINF_TTRP(LUN,UPDATE,ST)
	ELSEIF(GAME_CODE.EQ.8) THEN
	    CALL ODDINF_TSPT(LUN,UPDATE,ST)
	ELSEIF(GAME_CODE.EQ.9) THEN
	    CALL ODDINF_TSTR(LUN,UPDATE,ST)
	ENDIF
	IF(ST.NE.0) THEN
	    TYPE*,IAM(),'Errors found in ',TMP_NAME
	    IF(UPDATE) TYPE*,IAM(),'Files not updated for that draw.'
	ENDIF
	CLOSE(LUN)
	GOTO 100
C
C Format statements.
C
9000	FORMAT(//,T5,'VEDONLYÖNTI FTP functions:',
     *  //,T5,'TS',5X,'- Toto Select   game set - ',A8,
     *   /,T5,'SC',5X,'- Score         game set - ',A8,
     *   /,T5,'WT',5X,'- Winners tip   game set - ',A8,
     *   /,T5,'DB',5X,'- Super Double  game set - ',A8,
     *   /,T5,'CP',5X,'- Daily Couple  game set - ',A8,
     *   /,T5,'SS',5X,'- Super Score   game set - ',A8,
     *   /,T5,'TR',5X,'- Todays Trio   game set - ',A8,
     *   /,T5,'VA',5X,'- Vakio         game set - ',A8,
     *   /,T5,'ST',5X,'- Super Triple  game set - ',A8,
     *  //,T5,'EX',5X,'- Program exit',//)
9001	FORMAT(//,T5,'UPDATE OR REPORT MODE SELECTION:',
     *  //,T5,'RP',5X,'- REPORT MODE - files will not be updated',
     *   /,T5,'UP',5X,'- UPDATE MODE - files will be updated',//)
	END	
