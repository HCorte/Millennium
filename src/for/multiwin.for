C------------------------------------------------------------------------------
C
C                      M U L T I W I N  P R O G R A M 
C
C------------------------------------------------------------------------------
C
C V12 20-APR-2000 UXN WININD added.
C V11 10-APR-2000 UXN VKDBLD added.
C V10 16-JAN-2000 UXN DISPWIN changed, SORT_WINSEL added.
C V09 05-JAN-2000 OXK WINYES set as status (=removed WINPRV)
C V08 28-DEC-1999 OXK Enabled re-adding a game that was removed earlied
C V07 13-DEC-1999 OXK Selection based on entered results.
C V06 11-NOv-1999 RXK World tour added.
C V05 27-AUG-1999 RXK Display of draw numbers changed 
C V04 28-JUN-1999 UXN Wait for completition of MLCOPVLF (Othrewise
C                     MLCOPVLF will be terminated when MULTIWIN exits.)
C                     Also Super Triple added.
C V03 25-MAY-1999 RXK VLF->VLC copy started here.
C V02 30-APR-1999 RXK Call of ISUBPROC added, draw numbers displayed
C V01 11-JAN-1999 GPW INITIAL RELEASE FOR FINLAND
C
C  
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
C=======OPTIONS /CHECK=NOOVERFLOW
      PROGRAM MULTIWIN
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
      INCLUDE 'INCLIB:MULNAM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
C
      CHARACTER*20 FILES(200)
      INTEGER*4 FTYPE(200)
      INTEGER*4 CNTG,GMN
      INTEGER*4 ST,FLAG
C
      INTEGER*4 I,J,FILCNT,NTSK,WININD
      INTEGER*4 COUNT
C
      LOGICAL*4 ISSUB  
      LOGICAL*4 NEED_TCC
      LOGICAL*4 NEED_VKSBLD

      COMMON/SCF/SCFREC
C
      CALL COPYRITE
C
      ISSUB = ISSUBPROC()
      IF(.NOT.ISSUB) THEN
        CALL PRMYESNO('Are you sure you want run (not SUBRUN) MULTIWIN [Y/N]?',
     *                FLAG)
        IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
      ENDIF
C
      CALL PRMYESNO('Are you sure you want MULTIWIN [Y/N]? ',FLAG)
      IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
      CALL GETSCONF(SCFREC,ST)
      IF(ST.NE.0) CALL GSTOP(GEXIT_FATAL)
C
      STOPMOD=WINMULTI
C
C CHECK GAME/WINSEL TODAY  (PRELIMINARY)  
C
      CALL SORT_WINSEL
      CALL DISPWIN(0)
C
      CALL ADREGAM

      DO 300 WININD=1,MAX_WINSEL
C
      MLWININD = WININD
C
C SET DAILY WINTASKS       (PRELIMINARY)
C
      CALL SETWTSK
      IF(WINCNT.EQ.0) GOTO 300
C
      IF(VLCSTS.NE.WCLR) CALL NRUNTSK(8HCRTVLC  )
      NEED_TCC = .FALSE. 
      DO I=1,MAXWTSK
	 IF(TCWNBR(I).NE.0) NEED_TCC = .TRUE.
      ENDDO
      IF(NEED_TCC .AND. TCCSTS.NE.WCLR) CALL NRUNTSK(8HCRTTCC  )
C
C START COPYING VLF TO VLC
C
      COUNT = 0
98    CONTINUE
      IF (VLCSTS.NE.WCLR) THEN
	COUNT = COUNT + 1
	IF(MOD(COUNT,60).EQ.1) TYPE*,IAM(),'Waiting for VLC to be cleared'
	CALL XWAIT(2,2,ST)
	GOTO 98
      ENDIF 

      TYPE*,IAM(),' Beginning the execution of MLCOPVLF'
      CALL NRUNTSK(8HMLCOPVLF)

      COUNT = 0
99    CONTINUE
      IF(NEED_TCC .AND. TCCSTS.NE.WCLR) THEN      
	COUNT = COUNT + 1
	IF(MOD(COUNT,60).EQ.1) TYPE*,IAM(),'Waiting for TCC to be cleared'
	CALL XWAIT(2,2,ST)
	GOTO 99
      ENDIF 
C
C SET DRAW FILES NAMES & COPY TO BUFFER ON STOPCOM
C
      DO 200 I=1,WINCNT
           NTSK=WINWTSK(I)
           GO TO (101,102,103,104,105,106,107,108,109,110,
     *            111,112,113,114,115) , NTSK
C
  101      CONTINUE
           CALL WIN_WININT(FILES,FTYPE,FILCNT)
           CALL STORFIL(NTSK,FILES,FTYPE,FILCNT,1,1)
           GO TO 222
  102      CONTINUE
           GO TO 222
  103      CONTINUE
           GO TO 222
  104      CONTINUE
           CALL TWIN_WININT(FILES,FILCNT)
           CALL STORFIL(NTSK,FILES,FTYPE,FILCNT,1,0)
           GO TO 222
  105      CONTINUE
           CALL SWIN_WININT(FILES,FILCNT)
           CALL STORFIL(NTSK,FILES,FTYPE,FILCNT,1,0)
           GO TO 222
  106      CONTINUE
           CALL WWIN_WININT(FILES,FILCNT)
           CALL STORFIL(NTSK,FILES,FTYPE,FILCNT,1,0)
           GO TO 222
  107      CONTINUE
           CALL RUNTSK(8HBNGFHPRG)
           CALL BWIN_WININT(FILES,FILCNT)
           CALL STORFIL(NTSK,FILES,FTYPE,FILCNT,1,0)
           GO TO 222
  108      CONTINUE
           CALL DWIN_WININT(FILES,FILCNT)
           CALL STORFIL(NTSK,FILES,FTYPE,FILCNT,1,0)
           GO TO 222
  109      CONTINUE
           CALL CWIN_WININT(FILES,FILCNT)
           CALL STORFIL(NTSK,FILES,FTYPE,FILCNT,1,0)
           GO TO 222
  110      CONTINUE
           CALL SSWIN_WININT(FILES,FILCNT)
           CALL STORFIL(NTSK,FILES,FTYPE,FILCNT,1,0)
           GO TO 222
  111      CONTINUE
           CALL TRWIN_WININT(FILES,FILCNT)
           CALL STORFIL(NTSK,FILES,FTYPE,FILCNT,1,0)
           GO TO 222
  112      CONTINUE
           CALL STWIN_WININT(FILES,FILCNT)
           CALL STORFIL(NTSK,FILES,FTYPE,FILCNT,1,0)
           GO TO 222
  113      CONTINUE
  114      CONTINUE
           GO TO 222
  115      CONTINUE
C
  222 CONTINUE
C
      IF(FILCNT.EQ.0) THEN
          CNTG=TSKCNT(NTSK)
	  TYPE*,IAM(),'Nothing to do for game ',TSKGAM(J,NTSK)
          DO J=1,CNTG
             GMN=TSKGAM(J,NTSK)
             DRWSTS(MLWININD,GMN)=WINCAN
          ENDDO
      ENDIF
  200 CONTINUE 
C
C  SET DAILY WINTASKS         (FINALLY)
C
      CALL SETWTSK
C
      CALL DISPWIN(1)
      IF(WINCNT.EQ.0) GOTO 300
C
C      CALL PRTTEST   ! (used for testing purposes only)
C
C
      CALL PRMYESNO('Do you want to run WINSELS Y/N ?',FLAG)
      IF(FLAG.NE.1) THEN
         STOPMOD=WINMANUAL
         CALL GSTOP(GEXIT_OPABORT)
      ENDIF

C
C CHECK WORKFILES & START WINSELS
C
      CALL STARTWINS
C
C CHECK & WAIT IF ALL WINSELS WERE DONE 
C
      CALL WINSEND
C
      CALL RUNTSK(8HWINRPT  )
      CALL RUNTSK(8HBKKREP  )
C
C SHARECLC CAN NOT BE RUNNING IN MULTIWIN BECAUSE WE DON'T HAVE OFF LINE
C INFORMATION, WHEN OFF LINE DESAPEAR WE CAN ADD AGAIN.
C
C     CALL RUNTSK(8HSHARECLC)
C
C CHANGE SHARE REPORT FOR PORTUGAL, NOW WE CALL THIS REPOR FROM VLFTSK TASK
C
C     CALL RUNTSK(8HSHARERPT)
C
C MAKE SURE VLF2VLC is done.
C
      COUNT = 0
500   CONTINUE
      IF(VLCSTS.NE.WCOP) THEN
          CALL XWAIT(2,2,ST)
          COUNT = COUNT + 1
          IF(MOD(COUNT,30).EQ.0) 
     *       TYPE*,IAM(),'Waiting for completion of VLF->VLC'
          GOTO 500
      ENDIF
C
      CALL NRUNTSK(8HMULTIVLF)
      CALL NRUNTSK(8HMULTITCF)
C
      COUNT = 0
600   CONTINUE
      IF((VLCSTS.NE.WMRG).OR.(TCCSTS.NE.WMRG)) THEN
          CALL XWAIT(2,2,ST)
          COUNT = COUNT + 1
          IF(MOD(COUNT,60).EQ.1)
     *       TYPE*,IAM(),'Waiting for completion of MULTIVLF and MULTITCF'
          GOTO 600
      ENDIF

C
300   CONTINUE
C 
C Run VKSBLD if needed
C
      NEED_VKSBLD = .FALSE.
      DO I=1, NUMSPT
      	  GMN = SCFGTN(TSPT,I)
      	  IF(GMN.GT.0.AND.GMN.LE.MAXGAM) THEN
             DO J=1,MAX_WINSEL
      	        IF(DRWGAM(J,GMN).GT.0) NEED_VKSBLD = .TRUE.
             ENDDO
      	  ENDIF
      ENDDO
      IF(NEED_VKSBLD) CALL RUNTSK(8HVKSBLD  )
C
      CALL GSTOP(GEXIT_SUCCESS)
C
1000  FORMAT(1X,A,A8,'  started.')
1001  FORMAT(1X,A,'Initialize ',5A4, ' with WINCLR and rerun MLCOPVLF')
1100  FORMAT(5A4)
1110  FORMAT(1X,A,' Error while clearing ',5A4)
C  
C
      END
C
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
      SUBROUTINE ADREGAM
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:STOPCOM.DEF'
      INCLUDE 'INCLIB:RECSCF.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:GTNAMES.DEF'
C
      COMMON/SCF/SCFREC

      INTEGER*4 GNUM,GTYP,GIND,DRAW,OPT,EXT,FLAG,I,K
      INTEGER*4 STSCOPY(MAX_WINSEL,MAXGAM)
      INTEGER*4 STSTMP (MAX_WINSEL,MAXGAM)
      LOGICAL   OK
C
C
        GNUM=0
	CALL FASTMOV(DRWSTS,STSCOPY,MAX_WINSEL*MAXGAM)

   10   CONTINUE
        TYPE*,' '
        WRITE(6,2000) IAM(),IAM(),IAM(),IAM(),IAM()
        TYPE*,' '
C
        CALL PRMNUM('Enter option #',OPT,1,3,EXT)
C
        IF(EXT.EQ.-1) THEN
           STOPMOD=WINMANUAL
           CALL GSTOP(GEXIT_OPABORT)
        ENDIF
C
        IF(OPT.EQ.3) RETURN
C
        WRITE(6,900) (K,GTNAMES(K),K=1,MAXTYP)
        CALL PRMNUM('Enter game type ',GTYP,1,MAXTYP,EXT)
        IF(EXT.LT.0) GOTO 10
        CALL PRMNUM('Enter game index ',GIND,1,MAXIND,EXT)
        IF(EXT.LT.0) GOTO 10
C
        GNUM=SCFGTN(GTYP,GIND)
        IF(GNUM.LT.1.OR.GNUM.GT.MAXGAM) THEN
            TYPE*,'Sorry, game selected is not active'
            GOTO 10
        ENDIF
C
C DO NOT ALLOW ADD LOTTO, JOKERI AND VAKIO GAMES
C
        IF(GNUM .EQ. 2 .OR. 
     *     GNUM .EQ. 4 .OR. 
     *     GNUM .EQ. 1 .OR.
     *     GNUM .EQ. 3 .OR.
     *     GNUM .EQ. 5 .OR. 
     *     GNUM .GT. 7) 
     *  THEN
           WRITE(6,2001) IAM()
           GOTO 10
        ENDIF 
C
        IF(GTYP.EQ.TINS) THEN
            TYPE*,'Sorry, not applicable to Instant games'
            GOTO 10
        ENDIF
C
C
        DRAW=DAYDRW(GNUM)
        CALL PRMNUM('Enter draw number [C-current draw] ',DRAW,1,99999,EXT)
        IF(EXT.LT.0.AND.EXT.NE.-5) GOTO 10
C
C
        WRITE(6,910) GTNAMES(GTYP),GIND,(SCFLGN(K,GNUM),K=1,4),DRAW
        CALL PRMYESNO('Is this correct (Y/N) ',FLAG)
        IF(FLAG.NE.1) GOTO 10
C
        GO TO (100,200) , OPT

C Add 
100     CONTINUE
	OK=.FALSE.
	DO I=1,MAX_WINSEL
	   IF((DRWSTS(I,GNUM).EQ.WINCAN).AND.(DRWGAM(I,GNUM).EQ.DRAW)) THEN
	      IF (STSCOPY(I,GNUM).EQ.WINCAN) THEN
  		WRITE(6,*)IAM(),
     *	      'Removed on an earlier run of MULTIWIN - restoring from gamefile.'
		CALL FASTMOV(DRWSTS,STSTMP,MAX_WINSEL*MAXGAM)
		CALL CHKWINS
		IF (DRWSTS(I,GNUM).EQ.RESNOT) DRWSTS(I,GNUM)=WINYES
		STSTMP(I,GNUM)=DRWSTS(I,GNUM)
		CALL FASTMOV(STSTMP,DRWSTS,MAX_WINSEL*MAXGAM)
	      ELSE
		DRWSTS(I,GNUM)=STSCOPY(I,GNUM)
	      ENDIF
	      OK=.TRUE.
	   ENDIF
	ENDDO
	
	IF (.NOT.OK) THEN
	   WRITE(6,*) IAM(),'Cannot add unless RESULTS are entered.'
	ENDIF

	CALL SORT_WINSEL
        CALL DISPWIN(0)
	GOTO 10


C Remove
200     CONTINUE
        OK=.FALSE.
        DO I=1,MAX_WINSEL
           IF((DRWSTS(I,GNUM).NE.WINNOT).AND.(DRWGAM(I,GNUM).EQ.DRAW)) THEN
              DRWSTS(I,GNUM)=WINCAN
              OK=.TRUE.
           ENDIF
        ENDDO

        IF (.NOT.OK) THEN
           WRITE(6,*) IAM(),'Cannot remove this game.' ! NOT_REACHED
        ENDIF
	CALL SORT_WINSEL
        CALL DISPWIN(0)
        GO TO 10
C
900     FORMAT(//,<MAXTYP>(1X,I2,' - ',A8,/))
910     FORMAT(1X,A8,I1,2X,4A4,'Draw ',I5,/)
2000    FORMAT(1X A,' === Select option ==='/
     *         1X,A,'  1 - Add new game'/
     *         1X,A,'  2 - Remove game'/
     *         1X,A,'  3 - Continue with Multiwin'/
     *         1X,A,'  E - Stop Multiwin')
2001    FORMAT(1X,A,1X,
     *     'You can not add or remove regular Lotto, Jokeri or Sport games')
C
      END
