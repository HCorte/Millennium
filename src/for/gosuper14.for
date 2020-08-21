C GOSUPER14.FOR
C
C V01 MTK 04_APR-2017 Modified Super 14 game
C This program will allow the user to set the SUPER14 game type
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
C Copyright(c) 2017 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
	OPTIONS /CHECK=NOOVERFLOW
	PROGRAM GOSUPER14
	IMPLICIT NONE
				
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:STANDARD.DEF'					       
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:RECSCF.DEF'
	INCLUDE 'INCLIB:RECDAF.DEF'
	INCLUDE 'INCLIB:DSPREC.DEF'
									       
	INTEGER*4 K
	INTEGER*4 ST
	INTEGER*4 EXT 
	INTEGER*4 FLAG
	INTEGER*4 GNUM
	INTEGER*4 GIND
	INTEGER*4 GTYP
	INTEGER*4 DRAW
	INTEGER*4 SDRAW
	INTEGER*4 S14TYP

	INTEGER*4 SFDB(7)
	CHARACTER*27 SDES(0:2)
	DATA SDES/'SUPER 14 game disabled     ',
     *            'SUPER 14 set to Match Score',
     *            'SUPER 14 set to 1X2        '/
								       
	TYPE*,'This program will change the sports SUPER 14 game option'
	CALL WIMG(5,'Do you want to continue [Y/N]')
	CALL YESNO(FLAG)
	IF(FLAG.NE.1) STOP

	CALL GETSCONF(SCFREC,ST)
	IF(ST.NE.0) THEN    
	  TYPE*,'SCF error ',ST
	  STOP
	ENDIF

10	CONTINUE
	CALL INPNUM('Enter sports game index ',GIND,1,NUMSPT,EXT)
	IF(EXT.LT.0) STOP
	
	GNUM = SCFGTN(TSPT,GIND)
	IF(GNUM.EQ.0) THEN
	  TYPE*,'Inactive game index entered'
	  GOTO 10
	ENDIF

	CALL OPENW(1,SCFGFN(1,GNUM),4,0,0,ST)
	CALL IOINIT(SFDB,1,DSPSEC*256)
	IF(ST.NE.0) THEN
	  CALL FILERR(SCFGFN(1,GNUM),1,ST,0)
	  STOP
	ENDIF

20	CONTINUE
	CALL INPNUM('Enter first draw to change ',SDRAW,1,9999,EXT)
	IF(EXT.LT.0) STOP
        CALL READW(SFDB,SDRAW,DSPREC,ST)
        IF(ST.NE.0) CALL FILERR(SCFGFN(1,2),2,ST,SDRAW)
        IF(DSPSTS.GT.GAMOPN) THEN
          TYPE*,'Start draw already closed'
	  GOTO 20
        ENDIF


	CALL INPNUM('Enter SUPER14 game type [0=None, 1=Match Score, 2=1X2]',
     *              S14TYP,0,2,EXT)
	IF(EXT.LT.0) STOP

	WRITE(5,900) SDRAW, SDES(S14TYP)
									     
	CALL WIMG(5,'Is this correct [Y/N]?')
	CALL YESNO (FLAG)
	IF(FLAG.NE.1) STOP

        WRITE(5,901) (SCFGFN(K,GNUM),K=1,5)

	DRAW = SDRAW
1000	CONTINUE
	CALL READW(SFDB,DRAW,DSPREC,ST)
        IF(ST.EQ.144) THEN				
          TYPE*,'Last draw updated ',DRAW-1	
          CLOSE(UNIT=1)
	  STOP		
        ENDIF	
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,2),2,ST,DRAW)
C
C SET SUPER 14 GAME TYPE FLAG
C
	DSPFRG = S14TYP

	CALL WRITEW(SFDB,DRAW,DSPREC,ST)
	IF(ST.NE.0) CALL FILERR(SCFGFN(1,2),3,ST,DRAW)
	DRAW = DRAW + 1
	GOTO 1000
	STOP

900     FORMAT(//,1X,'Start draw is ',I4,' for ',A)
901     FORMAT(1X,'Applying changes to ',5A4)		     
	END
