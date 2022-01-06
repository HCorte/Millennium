C PROGRAM RESET
C
C V53 24-NOV-2020 SCML Added support for OLM
C V52 14-MAR-2016 SCML M16 PROJECT
C V51 13-FEB-2014 SCML Added support for IGS
C V50 11-MAY-2011 FJG EVO REPROX as independent reprocessing program
C V49 21-MAR-2011 RXK Call of not used LOADPRM commented out
C V48 06-JAN-2011 FJG MILLENNIUM MXSRV
C     24-JAN-2011 RXK Format 940 fixed.
C V47 13-JAN-2011 FJG Lotto2 Passive
C V46 01-JAN-2010 FJG ePassive
C V45 27-OCT-2003 FRP Modify for Batch2 Totobola Changes.
C V44 26-ABR-2001 JHR REMOVED ICSLOG TASK
C V43 05-FEB-2001 CS  INCLUDED PASSIVE GAME AND LOAD_WEKDRW
C V42 29-NOV-2000 UXN TOTOGOLO ADDED
C V41 02-SEP-2000 UXN RUNSYS improvements, SETUP() added, DAF loading
C                     sequence changed.
C V40 24-JUL-2000 UXN Added starting detached processes.
C V39 13-JUN-2000 UXN Don't start THRULOGs anymore.
C V38 05-JUN-2000 UXN RESCRS called before loading from checkpoints.
C V37 28-Feb-2000 RXK Promotion file load (LOADPRM) is back.
C V36 18-Feb-2000 OXK DRAW # as a parameter to INTGAM (Vakio changes)
C V35 14-DEC-1999 OXK STARTER no longer waited for to finish.
C V34 02-DEC-1999 UXN ICSLOG added.
C V33 26-NOV-1999 UXN LODIMG7 added.
C V32 07-JUL-1999 UXN DON'T START FTPPRO. LODAGT added.
C V31 27-MAY-1999 RXK STARTER added.
C V30 25-MAY-1999 UXN LODIMG6 ADDED.
C V29 11-JAN-1999 UXN ZAPCLERK changed to CLRCRK.
C V28 01-FEB-1998 WPW TVTPRO REMOVED
C V27 05-MAR-1997 HXK Change for checking GVT end time
C V26 27-FEB-1997 HXK Allow for GVT end time to differ from day to day
C V25 07-JAN-1997 HXK Added LODIMG4
C V24 05-DEC-1996 HXK Updated for Finland IPS pre-release
C V23 13-SEP-1996 HXK Do checkpoint after getting system configuration 
C                     (PXN ... Pat Nestor)
C V22 20-NOV-1995 HXK Load LODIMG3
C V21 20-AUG-1995 HXK Always store prizes for Ravi games
C V20 15-OCT-1994 HXK Adding /developing Bingo (15.Oct.94)
C V19 24-SEP-1994 HXK Added check for operators
C V18 07-JUN-1994 HXK ALWAYS RUN THRULOG TASKS
C V17 17-APR-1994 HXK LOAD THRULOG TASKS, DO NOT LOAD TVTPRO
C V16 04-FEB-1994 HXK Start TVTPRO if primary only.
C V15 16-JAN-1994 HXK CHANGED NAME OF STUTIMER TO TVTPRO.
C V14 16-JAN-1994 HXK REMOVED UNSPRO, ADDED STUTIMER.
C V13 22-OCT-1993 GXA Restore Lotto Pools, restore TMF names from the SCF at 
C                     checkpointing.
C V12 12-OCT-1993 GXA When restarting from checkpoint load all game text into 
C                     msgcom. (Text is not checkpointed (static) and is not 
C                     kept in the SMF, hence the need to restore it from the 
C                     game files.)
C V11 10-SEP-1993 HXK Uncommented LOADWRF call
C V10 27-AUG-1993 SXH SUBRUN V65SCAN
C V09 24-AUG-1993 GXA Added second LODIMG task (LODIMG_TSK2).
C V08 07-JUL-1993 HXK COMMENTED OUT LOADPRM TEMPORARILY
C V07 16-JUN-1993 SXH Released for Finland
C V06 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V05 20-JAN-1993 EBD changed call to startcom subroutine to reset_startcom 
C                     to avoid conflict with the other version of the 
C                     startcom subroutine
C V04 26-FEB-1996 wsm Added loading of NETMON task for new NETMGR,
C                            removed X2GETSAP call.
C V03 13-DEC-1991 DAS ADDED X2XCOM
C V02 07-OCT-1991 MTK INITAL RELEASE FOR NETHERLANDS
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C INITIALIZE COMMONS FOR TRANSACTION PROCESSING TASKS AND COMMUNICATIONS.
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
C Copyright 2000 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        PROGRAM RESET
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:DESNET.DEF'
        INCLUDE 'INCLIB:POOLLTO.DEF'
        INCLUDE 'INCLIB:LANCOM.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:LOGCOM.DEF'
        INCLUDE 'INCLIB:X2XCOM.DEF'
        INCLUDE 'INCLIB:LSYSCOM.DEF'
        INCLUDE 'INCLIB:PRZCOM.DEF'
        INCLUDE '($SSDEF)'
	INCLUDE 'INCLIB:SETUP.DEF'
	INCLUDE 'INCLIB:GTNAMES.DEF'
	RECORD/SETUP/ PAR
C
C
        INTEGER*2  DATE(12)          !
        LOGICAL    RTASK             !
        LOGICAL    TTASK             !

        INTEGER*4  FDB(7)            !
        INTEGER*4  CERR              !
        INTEGER*4  STATUS            !
        INTEGER*4  YNFLG             !
        INTEGER*4  GAM               !
        INTEGER*4  S                 !
        INTEGER*4  I                 !
        INTEGER*4  OFFSET            !
        INTEGER*4  INDEX             !
        INTEGER*4  BLOCK             !
        INTEGER*4  SER               !
        INTEGER*4  CHKERR            !
        INTEGER*4  EXT               !
        INTEGER*4  CDC               !
        INTEGER*4  FLAG              !
        INTEGER*4  ST                !
        INTEGER*4  STAT              !
        INTEGER*4  GTYP              !
        INTEGER*4  GIND              !
        INTEGER*4  DUMMY             !
!	INTEGER*4  EMISSION
!       INTEGER*4  DRAWPRG           !
!       INTEGER*4  DOFF              !
!       INTEGER*4  DOFFAUX           !
!       INTEGER*4  FIND_DRAW_PURGED  ! FUNCTION
!       INTEGER*4  DRAW              !
!	INTEGER*4  LSTEMIS(MAXGAM)
!       INTEGER*4 FRST_EMIS          ! FIRST EMISION TO SEARCH
!	INTEGER*4  BUF(CDLEN)
!       INTEGER*4  BITMAP(2)

        REAL*8     RESNAM            !
        REAL*8     TSTNAM            !
        REAL*8     TSTREC            !
        REAL*8     DCNTSK            !
        REAL*8     CTLTSK            !
        REAL*8     RELAPP            !
        REAL*8     NETMON            !
	REAL*8     REPROX            ! V50        
	REAL*8     STARTER
C	REAL*8     ICSLOG
C
C V07   TASK NAME OF THE 'LODIMG' - TO LOCK PAGES IN MEMORY
C
        REAL*8 LODIMG_TSK
        REAL*8 LODIMG_TSK2
        REAL*8 LODIMG_TSK3
        REAL*8 LODIMG_TSK4
        REAL*8 LODIMG_TSK5
C
        DATA RESNAM/'RESET   '/
        DATA TSTNAM/'RESTST  '/
        DATA TSTREC/'RECTST  '/
        DATA DCNTSK/'DCNPRO  '/
        DATA CTLTSK/'CTLPRO  '/
        DATA RELAPP/'X2XRAPP '/
        DATA NETMON/'NETMON  '/
        DATA REPROX/'REPROX  '/  ! V50
        DATA STARTER/'STARTER '/
C       DATA ICSLOG/'ICSLOG  '/

        DATA LODIMG_TSK/'LODIMG  '/
        DATA LODIMG_TSK2/'LODIMG2 '/     
        DATA LODIMG_TSK3/'LODIMG3 '/
        DATA LODIMG_TSK4/'LODIMG4 '/
        DATA LODIMG_TSK5/'LODIMG5 '/


C
C       CONCURRENT SOFTWARE USES PROCESS NAME OF THE RESET PROGRAM TO
C       DISTINGUISH BETWEEN NORMAL RESET AND REPROCESSING.
C       VAX USES LOGICAL NAME 'RESET_PRCNAM' WITH THE SAME VALUES
C       TO MAKE THE SAME DISTINCTION
C
        CHARACTER*255   RESET_SYMBOL, RESET_PRCNAM
        DATA            RESET_SYMBOL/'RESET_PRCNAM'/
        REAL*8          ACTNAM
        EQUIVALENCE     (ACTNAM, RESET_PRCNAM)
        INTEGER*4       LENGTH
        INTEGER*4       LIB$GET_SYMBOL
        CHARACTER*1     BELL/Z07/
C
	CALL COPYRITE
        CALL SNIF_AND_WRKSET
	CALL XWAIT(2,2,ST)
C
C       EQUIVALENCE 'MESLOG1' TO TERMINAL NAME
C
        CALL ASNTERM('MESLOG1', STAT)
C
C CHECK TASK NAME
C
C       I DID NOT FIND A WAY TO RUN RESET UNDER DIFFERENT NAME ON VAX
C       AND TO LEAVE SUBPROCESSES RUNNING AFTER TERMINATION.
C       I USE LOGICAL NAME 'RESET_PRCNAM' TO BE 'RESET' OR 'REPROC'
C       AND TO DEFINE PROPER BEHAVIOR.
C***    CALL GETNAM(ACTNAM)
C
        STATUS = LIB$GET_SYMBOL(RESET_SYMBOL,RESET_PRCNAM,LENGTH)
        IF (STATUS .NE. SS$_NORMAL) THEN
            TYPE *,IAM(),BELL,BELL
            TYPE *,IAM(),'RESET_PRCNAM IS NOT CURRENTLY DEFINED ****'
            TYPE *,IAM(),'THIS MEANS YOU PROBABLY RAN THIS PROGRAM WITHOUT'
            TYPE *,IAM(),'USING "RUNSYS" OR "RECOVER".  IF YOU CONTINUE,'
            TYPE *,IAM(),'THIS TASK WILL RUN AS "RESET" WHICH MEANS IT'
            TYPE *,IAM(),'WILL REPROCESS TRANSACTIONS IN THE TMF.'
            TYPE *,IAM(),'IF YOU ARE TRYING TO RECOVER INTO N-PLEX,'
            TYPE *,IAM(),'DO NOT CONTINUE'
            TYPE *,IAM(),BELL,BELL
            CALL PRMYESNO('Do you want to continue?',FLAG)
            IF(FLAG.NE.1)CALL GSTOP(GEXIT_SUCCESS)
            RESET_PRCNAM = 'RESET'
        ENDIF
        IF(LENGTH .GT. 15) THEN
            TYPE *,'PROGRAM NAME TOO LONG (OVER 15 CHARACTERS)'
            CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
C
	TYPE *,IAM()
	TYPE *,IAM()
        TYPE *,IAM(),'You have attempted to run RESET.'
        TYPE *,IAM(),'This program will perform start-of-day functions.'
        TYPE *,IAM(),'If you do not intend for this to happen,'
        TYPE *,IAM(),'you may abort now without any damage.'
	TYPE *,IAM()
	TYPE *,IAM()
C
        CALL PRMYESNO('Are you sure you want RESET [Y/N]? ',FLAG)
        IF(FLAG.NE.1) CALL GSTOP(GEXIT_OPABORT)
C
        RTASK = .FALSE.
        TTASK = .FALSE.
        IF (ACTNAM.EQ.RESNAM.OR.ACTNAM.EQ.TSTNAM) RTASK=.TRUE.
        IF (ACTNAM.EQ.TSTNAM.OR.ACTNAM.EQ.TSTREC) TTASK=.TRUE.
        IF (.NOT.RTASK) THEN
            WRITE(6,900) IAM()
            CALL PRMYESNO('Do you want to continue',FLAG)
            IF(FLAG.NE.1)CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
	TYPE*,IAM()
C
C CLEAR GLOBAL GAME COMMONS AND LOAD SYSTEM CONFIGURATION FILE
C
        CALL INTMEM
        CALL SYSCON
C
	TYPE*,IAM()
C
C SETUP SYSTEM CONFIGURATION
C
	CALL SETUP(PAR,TTASK)
	TYPE*,IAM()
C
C LOAD GAME CONFIGURATION FILES INTO MEMORY OF EUROMILLIONS GAMING SYSTEM
C
        CALL LOAD_EURSYSCONF !V52
C
C
C PROJECT EURO MIL - P(EUMILF) AND P(EUTIMOUT) = 40 Sec.
C
        P(EUMILF) = PAR.EURCON
C----+-----------------------------------------------------------------
C V51| Adding support for IGS
C----+------------------------------------------------------------------
C        P(EUTIMOUT) = 40 
C
        P(IGSCONF) = PAR.IGSCON
C        P(ODDSETF) = 1
C        P(ODTIMOUT) = 40 
C        P(ODFRTOUT) = 20
        P(IGSDEBUGF) = 0 
C----+-----------------------------------------------------------------
C V51| Adding support for IGS
C----+------------------------------------------------------------------
C----+-----------------------------------------------------------------
C V53| Adding support for OLM
C----+------------------------------------------------------------------
        P(OLMCONF) = PAR.OLMCON
C        P(REGLOG)  = PAR.REGLOG
C----+-----------------------------------------------------------------
C V53| Adding support for OLM
C----+------------------------------------------------------------------

	CDC = PAR.DAYCDC
        DATE(5)= CDC
        CALL CDATE(DATE)
        DAYYER = DATE(3)
        DAYCDC = CDC
        DAYJUL = DATE(4)
        CALL FIGWEK(CDC,DAYWEK,DUMMY)
	P(TSTMOD) = PAR.SYSMOD
	IF(PAR.RESCHK.NE.0) GOTO 210
C
C READ DAF RECORD
C
        CALL OPENW(3,SFNAMES(1,DAF),4,0,0,ST)
        CALL IOINIT(FDB,3,DAFSEC*256)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),1,ST,0)

        CALL READW(FDB,CDC,DAFREC,ST)
        IF(ST.NE.0) CALL FILERR(SFNAMES(1,DAF),2,ST,CDC)
	CALL CLOSEFIL(FDB)
C
        IF(DAFSTS.NE.DSOPEN) THEN
            WRITE(6,910) IAM(),DAFSTS
            CALL BELLS(2)
            CALL GPAUSE
        ENDIF
C
        P(LIVGAM)=0
C
C SET DAYDRW , DAYHDR AND DIPLAY TODAYS ACTIVE GAMES
C
	TYPE*,IAM()
        TYPE*,IAM(),' Games active today '
	TYPE*,IAM()
        DO 200 I = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,I)
            GIND = GNTTAB(GAMIDX,I)

            DAYDRW(I) = DAFDRW(I)
            DAYHDR(I) = DAFHDR(I)
            IF(DAYHDR(I).LT.DAYDRW(I)) DAYHDR(I)=DAYDRW(I)
            IF(DAFDRW(I).GT.0) P(LIVGAM)=P(LIVGAM)+1
            IF(DAYDRW(I).GT.0) THEN
!===============================================================================              
!               IF(GTYP.EQ.TPAS)THEN
!                 FRST_EMIS = PASEMIS(CURDRW,GIND)
!                 CALL GET_LAST_EMIS(GIND,EMISSION,FRST_EMIS,ST)
!                 IF(ST.NE.0) THEN
!                   TYPE *,IAM(),'ERRO OBTENDO ULTIMA EXTRACAO ABERTA'
!                   CALL GPAUSE()
!                 ENDIF
!                 LSTEMIS(I) = EMISSION + 2
!                 FRST_EMIS = EMISSION + 1
!
!	  ! SPECIAL CASE FOR " NATAL" DRAWS
!                 CALL GET_LAST_EMIS(GIND,EMISSION,FRST_EMIS,ST)	  
!                 IF(ST.NE.0) THEN
!                   TYPE *,IAM(),'ERRO OBTENDO ULTIMA EXTRACAO CADASTRADA'
!                   CALL GPAUSE()
!                 ENDIF
!	  IF (EMISSION .GT. LSTEMIS(I)) THEN
!                    LSTEMIS(I) = EMISSION
!                 ENDIF
!                 EMISSION = LSTEMIS(I)
!	  IF (EMISSION.NE.0) THEN
!                    WRITE(6,902) IAM(),(GLNAMES(S,I),S=1,4),DAYDRW(I),EMISSION
!	  ENDIF
!	ELSE
                  WRITE(6,901) IAM(),(GLNAMES(S,I),S=1,4),DAYDRW(I)
!	ENDIF
!===============================================================================              
            ENDIF
200     CONTINUE

	TYPE*,IAM()
        CALL PRMYESNO('Are these correct? (Y/N) ',FLAG)
        IF(FLAG.NE.1) CALL GSTOP(GEXIT_SUCCESS)
C
210	CONTINUE
C
C START LODIMG IF NEEDED
C
        IF (P(LOCK_PAGES_MEM) .NE. NOLOCK_PAGES_MEM_VALUE) THEN
            CALL START(LODIMG_TSK)
            CALL START(LODIMG_TSK2)
            CALL START(LODIMG_TSK3)
            CALL START(LODIMG_TSK4)
            CALL START(LODIMG_TSK5)
        ENDIF
C
C
C                                                      
C CHECK FOR OPINION POLLING STATUS.                    
C       
        DO I = 1, PRM_NUMOPN                           
            IF(SCC_OPNDATE(1,I) .GT. DAYCDC .OR.       
     *         SCC_OPNDATE(2,I) .LT. DAYCDC) SCC_OPNID(I) = 0

            IF(SCC_OPNDATE(1,I) .EQ. DAYCDC) THEN        
                SCC_OPNSEED(I)=0                         
            ENDIF                                        
        END DO

C
C If not restarting from checkpoint then clear CLERK.FIL if needed and
C load ASF into memory.
C
	IF(PAR.RESCHK .EQ. 0) THEN
           IF(P(CLRKACT).EQ.0) CALL NRUNTSK(8HCLRCRK  )
           CALL NRUNTSK(8HLODAGT  )
	ENDIF
C
C LOAD X2X COMMON BEFORE REPROCESSING.
C
        CALL X2INIT  
C
C SET GAMESTATE TO DAY GAME
C
        X2X_GAME_MODE=X2X_GAMEM_DAY 

        ! load system message file
        CALL LOADSMF(3)
C
C SET UP TIMER QUEUES FOR CRSPRO
C
        CALL RESCRS
C
C ASK IF RESTARTING FROM CHECKPOINT
C
        CHKERR = 1
        SER=0
	IF(PAR.RESCHK .NE. 0) THEN
            CALL RESCHK(CDC,SER,CHKERR)
            IF(CHKERR.EQ.0) THEN
                CALL GETBI(NXTSER,BLOCK,INDEX,OFFSET)
                HRSER  = NXTSER
                HSER   = NXTSER
                HBLOCK = BLOCK
                HBLKRDY = BLOCK-1
                WAYINP = 1
C
C LOAD ALL STATIC GAME TEXT THAT IS NOT CHECKPOINTED
C
                DO GAM = 1,MAXGAM
                   IF(DAYDRW(GAM).GT.0) CALL LODTXT(GAM)
                END DO
C
C RESTORE PRIMARY TMF NAME FROM THE SCF.FIL
C
                CALL TMFSWI
 		P(DISKSW) = PAR.BAKLOG
		P(TAPESW) = PAR.TAPLOG
		P(PRMSTR) = PAR.IPSCON
C
C LOAD LOTTO SYSTEM COMMON
C
                TYPE*,IAM(),'Loading Lotto system file '
                CALL OPENQW(3,SFNAMES(1,LSF),4,0,0,ST)
                IF(ST.NE.0) CALL FILERR(SFNAMES(1,LSF),1,ST,0)
                CALL IOQINIT(FDB,3,1*256)
                CALL READQIO(FDB,1,LSYS_ATR,LSYS_COMMON_LENGTH*4,ST)
                IF(ST.NE.0) CALL FILERR(SFNAMES(1,LSF),2,ST,1)
                CALL CLOSEQFIL(FDB)                     
C
                GOTO 1000
            ELSE
                WRITE(6,920) IAM()
                CALL BELLS(2)
                CALL GSTOP(GEXIT_FATAL)
            ENDIF
        ENDIF
C
C NOT A RESTART FROM CHECKPOINT
C SET SYSTEM PARAMETERS AND GAME DRAW NUMBERS
C
        NXTSER=1
        CALL SETPAR
        DAYSTS=DSOPEN
C
C INITIALIZE GAME COMMONS
C
        DO 220 GAM = 1, MAXGAM
            GTYP = GNTTAB(GAMTYP,GAM)
            GIND = GNTTAB(GAMIDX,GAM)
C
            IF (DAYDRW(GAM).GT.0) THEN
               IF (GTYP.EQ.TPAS) THEN
                  CALL INTPAS(GAM)                
C
C BRING ONLY NON-PURGED DRAWS TO MEMORY
C
!                 DRAWPRG = FIND_DRAW_PURGED(TPAS,GAM)
!        	  DOFFAUX = 2
!		  DOFF = 1				    !We start from 2 because the firt position in memory is reserved for 
!		  DO WHILE(DOFF.LE.PAGEMI)                  !CURDRW
!		     DRAW = LSTEMIS(GAM) - (DOFF-1)
!		     IF (DRAW.GT.DRAWPRG.AND.DRAW.GT.0) THEN
!		         IF (DRAW.EQ.DAYDRW(GAM)) THEN
!                             CALL INTGAM(GAM,DRAW,CURDRW)
!			 ELSE
!                             CALL INTGAM(GAM,DRAW,DOFFAUX)
!                             DOFFAUX = DOFFAUX + 1
!			 ENDIF
!	             ENDIF	             
!		     DOFF = DOFF + 1		     
!		  ENDDO
!
C***                  DO DOFF = 1, PAGEMI
C***                     DRAW = DAYDRW(GAM) - (DOFF-1)
C***                     IF (DRAW.GT.DRAWPRG) CALL INTGAM(GAM,DRAW,DOFF)
C***                  ENDDO
               ELSE
                  CALL INTGAM(GAM,DAYDRW(GAM))
               ENDIF
            ELSEIF (DAYDRW(GAM).EQ.0 .AND.
     *              (GTYP.EQ.TSPT.OR.GTYP.EQ.TTGL)) THEN
                IF (DAYHDR(GAM).GT.0) THEN
                    WRITE(6,930)IAM(),(GLNAMES(I,GAM),I=1,4),DAYHDR(GAM)
                    CALL INTGAM(GAM,DAYHDR(GAM))
                ENDIF
            ENDIF
220     CONTINUE
C
        CALL LOADTKM
        CALL LOADWRF 
        CALL LOAD_WEKDRW()

221     CONTINUE
        CALL XWAIT(2,2,ST)
        CALL STTSK(8HLODAGT  ,EXT, STATUS)
        IF(STATUS.NE.4) GOTO 221

CCC        CALL LOADPRM 
        CALL RECARY
C
C    INITIALIZE LOTTO POOLS, MUST BE RUN AFTER ALL GAMES INITIALIZED
C
        IF (P(POOLACT).EQ.0) CALL POOLRES
C
C INITIALIZE CHECKPOINT HEADERS
C
        CALL CHK_HEAD_INIT(3)              
C
	P(TAPESW) = PAR.TAPLOG
C
        P(DISKSW) = PAR.BAKLOG
C
	P(PRMSTR) = PAR.IPSCON
        IF(P(PRMSTR).EQ.0)  P(SUPFPT) = 1
        P(ENDTIM) = PAR.GVTTIM
C
C CHECK GVT END TIME
C
        IF(P(FLSTIM).GT.P(ENDTIM)) THEN
           WRITE(6,906) IAM(),P(FLSTIM),P(ENDTIM)
           WRITE(6,907) IAM(),DISTIM(P(FLSTIM)),DISTIM(P(ENDTIM))
           CALL PRMYESNO('Do you want to continue with Runsys [Y/N]? ',YNFLG)
           IF(YNFLG.NE.1) THEN
              CALL GSTOP(GEXIT_OPABORT)
           ELSE
              TYPE*,IAM(),'**************************************************'
              TYPE*,IAM(),'** RUNSYS will continue but GVT timers will not **'
              TYPE*,IAM(),'** be consistent ... this may cause problems!   **'
              TYPE*,IAM(),'**************************************************'
           ENDIF
        ENDIF
C
C REPROCESS TRANSACTIONS FROM THE TMF
C START LOGGER AND REPROCESS TASKS
C
1000    CONTINUE
        P(REPFLG)=0
        CALL POOLINI                     !INITIALIZE ALL POOL QUEUES
C
        NODEID = 0                                      ! Sep 04, 1996
                                                        ! NODEID#0 together with
                                                        ! LIVSYS are used to let
                                                        ! TCPPLINK to accept
                                                        ! connections (LIVSYS is
                                                        ! not enough after
                                                        ! recovery from
                                                        ! checkpoints.
C
        CALL START(TSKNAM(POL))
        CALL START(TSKNAM(LOG))
        CALL START(TSKNAM(TIM))
        CALL START(TSKNAM(RPC))
        CALL START(TSKNAM(VAL))
        CALL START(TSKNAM(ERR))
        CALL START(TSKNAM(PSV))		  !PASSIVE VALIDATION
        CALL START(TSKNAM(PST))		  !PASSIVE RETURN TICKETS
C
C WAIT FOR CLRCRK TO BE FINISHED...
C
2000	CONTINUE
	CALL XWAIT(2,2,ST)
	CALL STTSK(8HCLRCRK  ,EXT, STATUS)
	IF(STATUS.NE.4) GOTO 2000
C=======V50=====================================================================
C       CALL REPROC(RTASK)
        IF(RTASK) THEN
          P(REPROW) = ISWORK + ISRTSK
        ELSE
          P(REPROW) = ISWORK
        ENDIF
        CALL START(REPROX)        
C        
2020    CONTINUE
          CALL XWAIT(500,1,STATUS)
    	  CALL STTSK(8HREPROX  ,EXT, STATUS)
    	  IF(STATUS.EQ.4) THEN
    	    IF(P(REPROW).NE.0) THEN
    	      TYPE*,IAM(),'=============================================='
    	      TYPE*,IAM()
              TYPE*,IAM(),'Reprocessing task ABORTED. Reprocess cancelled'
    	      TYPE*,IAM()              
    	      TYPE*,IAM(),'=============================================='              
              CALL GPAUSE()
              P(REPROW) = 0
            ENDIF
            GOTO 2030
    	  ENDIF
        GOTO 2020
2030    CONTINUE   
        CALL XWAIT(1,2,STATUS)     
C=======V50=====================================================================        
        P(REPFLG)=1
C
C GET SYSTEM NETWORK CONFIGURATION
C
        CALL GETSYS
        IF(P(SYSTYP).EQ.LIVSYS.AND..NOT.RTASK) THEN
            TYPE*,IAM(),' Only backup or spare could'
            TYPE*,IAM(),' be started in recovery mode'
            CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        IF(.NOT.RTASK.AND.NODEID.EQ.NETBACKUP(WAYINP)) THEN
            TYPE *,IAM(),' System status changed to SPARE '
            TYPE *,IAM(),' Remember to set BACKUP ID, after resynchronization'
            NETBACKUP(WAYINP)=0
        ENDIF
C
C CHECKPOINT COMMONS AND SET DES ENCRYPTION KEYS
C
        CALL START(TSKNAM(CHK))
        P(LSTCMD)=NXTSER
        TYPE *,IAM(),' Checkpointing commons'
        CALL LODCHK(CERR)
        P(NETFLU)=NOFLU
        CALL ENCINI(1,NUMAGT)
C
        LCHKFIL = -2                         !TO CHK0 FILE
        LCHKSER = NXTSER                     !LAST VALID NXTSER
        LTQNUM  = MOD(LTQNUM,2)+1            !SWAP QUEUE FOR LOTTO POOLS
        LCHKPNT = LCMDCHK                    !FORCE CHECKPOINT
C
C
C START ALL TASKS
C
        DO 3000 I=1,NUMTSK
            IF(I .EQ. POL .OR.
     *         I .EQ. OVR .OR.
     *         I .EQ. LOG .OR.
     *         I .EQ. TIM .OR.
     *         I .EQ. RPC .OR.
     *         I .EQ. VAL .OR.
     *         I .EQ. ERR .OR.
     *         I .EQ. CHK .OR.
     *         I .EQ. PSV .OR.
     *         I .EQ. PST .OR.
C----+-----------------------------------------------------------------
C V51| Adding support for IGS
C----+------------------------------------------------------------------
C!     *         I .EQ. NBR .OR.
C!C    *         I .EQ. ODD .OR.
C!     *         I .EQ. ODS .OR.
C     *         I .EQ. IGI .OR.
C     *         I .EQ. IGO .OR.
C----+-----------------------------------------------------------------
C V51| Adding support for IGS
C----+------------------------------------------------------------------
C    *         I .EQ. PPP .OR.          !MXSRV
     *         I .EQ. UNS) GOTO 3000    !N.B. WE DON'T RUN UNSPRO
C
            IF(TSKNAM(I).NE.'        ') THEN
                TYPE '(X,A,X,A,A8)',IAM(),'STARTING ',TSKNAM(I)
                CALL START(TSKNAM(I))
            ENDIF
3000    CONTINUE
C
C START THRULOG PROCESSES
C
C        CALL NRUNTASK(THRULOG)
C        CALL NRUNTASK(THRULOG2)
C        TYPE*,IAM(),'Starting THRULOG and THRULOG2'
C
C START DCNPRO AND CTLPRO.
C
        CALL START(DCNTSK)
        CALL START(CTLTSK)
C
C START MATRIX SWITCH PROCESSING (NOT USED INT THE NETHERLANDS)
C
C.......CALL STARTMSC(TTASK)
C
C START RELAY APPLICAION PROCESSOR
C
        CALL START(RELAPP)
C
C START UP NETMGR MONITORING TASK (NETMON).
C
        CALL START(NETMON)
C
C ENABLE LANPRO
C
        LANGO=LANPROSTART
        IF(P(SYSTYP).EQ.LIVSYS) CALL RESET_STARTCOM
        TYPE *,IAM(),'Activating LANS'
        CALL LAN_START
C
C START STARTER FOR MULTIWINSEL
C
        CALL START(STARTER)
C
C RUN ICSLOG 
C
C	CALL START(ICSLOG)
C
C
C DO NOT ALLOW WAGERS AND CANCELS FOR RESULTS 1 GAME (TOTOGOLO)
C
!	GIND=1
!	GAM = GTNTAB(TTGL,GIND)
!	WRITE(6,940) IAM(),GAM,GTNAMES(TTGL),GIND
!C
!	CALL FASTSET(0,BUF,CDLEN)
!        BITMAP(1)=P(SUPGWA)
!        BITMAP(2)=P(SUPGWA1)
!        CALL BSET(BITMAP,GAM)
!        BUF(1)=SUPGWA
!        BUF(2)=BITMAP(1)
!        BUF(3)=TCPAR
!        BUF(9)=BITMAP(2)
!	CALL QUECMD(BUF,ST)
!        CALL XWAIT(2,1,ST)
!C
!	CALL FASTSET(0,BUF,CDLEN)
!        BITMAP(1)=P(SUPGCA)
!        BITMAP(2)=P(SUPGCA1)
!        CALL BSET(BITMAP,GAM)
!        BUF(1)=SUPGCA
!        BUF(2)=BITMAP(1)
!        BUF(3)=TCPAR
!        BUF(9)=BITMAP(2)
!	CALL QUECMD(BUF,ST)
!        CALL XWAIT(2,1,ST)
C
C END OK
C
        CALL GSTOP(GEXIT_SUCCESS)
C
C
900     FORMAT(//,1X,A18,' This program should only be run when you',/,
     *            19X,' want to recover to duplex mode from',/,
     *            19X,' simplex mode.',//)
 
901     FORMAT(1X,A18,1X,4A4,' - Draw - ',I4.4)
902     FORMAT(1X,A18,1X,4A4,' - Draw - ',I4.4,' - Last open - ',I4.4)

903     FORMAT(1X,A18,'0 - NO CONNECTION',/,
     *            19X,'1 - PRIMARY SITE ',/,
     *            19X,'2 - BACKUP SITE  ')

904     FORMAT(1X,A18,' GVT end time for today in seconds since midnight:',I5)
905     FORMAT(1X,A18,' GVT end time for today in HH:MM:SS           :',A8)

906     FORMAT(1X,A18,'INCONSISTENCY - GVT flush time    ',I5,
     *         '  is greater than GVT end time    ',I5,' (time in seconds)')
907     FORMAT(1X,A18,'INCONSISTENCY - GVT flush time ',A8,
     *         '  is greater than GVT end time ',A8,' (time in HH:MM:SS)')

910     FORMAT(1X,A18,' *** Invalid day status - ',I3,' ***')
920     FORMAT(1X,A18,' *** Checkpoint restart error ***')

930	FORMAT(1X,A18,'Loading inactive ',4A4,' draw ', I4)
940     FORMAT(1X,A18,'*** Setting SUPWAG and SUPCAN for game ',
     *         I2,1X,'(',A8,1X,I1') ***')

        END
