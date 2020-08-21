C
C SUBROUTINE XRUNTSK_DET
C
C V02 12-FEB-2014 SCML Allowing task run from another location
C V01 24-JUL-2000 UXN Initial release (produced from NRM_XRUNTSK.FOR) 
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
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE XRUNTSK_DET(PRGNAME,PRCNAME,WFLG)
	IMPLICIT NONE
C
	BYTE	PRGNAME(8),PRCNAME(8)
	LOGICAL WFLG
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE '($SYSSRVNAM)'
	INCLUDE '($PRVDEF)'
	INCLUDE '($PQLDEF)'
	INCLUDE '($ACLDEF)'
	INCLUDE '($PRCDEF)'

C
	BYTE	    NAMB
	CHARACTER*1 NAMC
	INTEGER*4   STATUS/1/
	INTEGER*4   I,IND, PREFIX_LEN, PROGNAM_LEN
	LOGICAL	    FIRST_TIME/.TRUE./
	CHARACTER*4 EXT/'.EXE'/
	CHARACTER*7 TASKLIB/'TSKLIB:'/
	INTEGER*4   TASKLIB_SZ/7/
	INTEGER*4   TSKSTS(2)

C----+------------------------------------------------------------------
C V02| Allowing task run from another location
C----+------------------------------------------------------------------
        LOGICAL     TASK_FOUND
        LOGICAL     AUX_CONDITION
        INTEGER*4   IDX
        INTEGER*4   IDX_LOCATION
        INTEGER*4   MAX_ALT_LOC
        PARAMETER  (MAX_ALT_LOC=4)
        INTEGER*4   DEFAULT_LOCATION
        PARAMETER  (DEFAULT_LOCATION = 1)
        CHARACTER*7 ALTLOC(MAX_ALT_LOC)
        INTEGER*4   IXI
        CHARACTER*255 OLD_DVOL
        INTEGER*4     OLD_DVOL_LEN
        CHARACTER*255 OLD_DDIR
        INTEGER*4     OLD_DDIR_LEN
        CHARACTER*255 DDIR
        INTEGER*4     DDIR_LEN
        CHARACTER*255 DVOL
        INTEGER*4     DVOL_LEN
        DATA (ALTLOC(IXI),IXI=1,MAX_ALT_LOC)/
     *          'TSKLIB:', 'IGSLIB:', 'UD1LIB:', 'UD2LIB:'
     *  / 
C----+------------------------------------------------------------------
C V02| Allowing task run from another location
C----+------------------------------------------------------------------
C
C	FOLLOWING ARE DATA SPACE ALLOCATION FOR
C	PARAMETERS THAT SERVICE SYS$CREPRC REQUIRES
C
	INTEGER*4	PIDADR		! ADDRESS OF CREATED PROCESS ID
	CHARACTER*63	IMAGE		! PROGRAM NAME TO LOAD
	CHARACTER*63	INPUT		! SYS$INPUT  DEFINITION
	CHARACTER*63	OUTPUT		! SYS$OUTPUT DEFINITION
	CHARACTER*63	ERROR		! SYS$ERROR  DEFINITION
	INTEGER*4	PRVADR(2)	! PRIVILEGES
C
	EQUIVALENCE	(IMAGE, TASKLIB)
C
	DATA	INPUT /'SYS$INPUT'/
	DATA	OUTPUT/'SYS$OUTPUT'/
	DATA	ERROR /'SYS$ERROR'/
C
CDEC$ OPTIONS /WARNING=NOALIGNMENT
	STRUCTURE	/QUOTA_DEF/
	    BYTE	QUOTA_TYPE	! QUOTA TYPE
	    INTEGER*4	QUOTA_AMOUNT	! QUOTA AMOUNT
	END STRUCTURE
CDEC$ END OPTIONS 
	RECORD /QUOTA_DEF/ QUOTA(PQL$_LENGTH+1)
C
	CHARACTER*15	PRCNAMEXP	! PROCESS NAME EXPANDED
	INTEGER*4	I4PRCNAM
	EQUIVALENCE	(I4PRCNAM, PRCNAMEXP)
	INTEGER*4	BASPRI		! BASE PRIORITY
	INTEGER*4	UIC		! UIC OF CREATED PROCESS
	INTEGER*2	I2UIC(2)
	EQUIVALENCE	(I2UIC,UIC)
C
	INTEGER*2	MBXCHAN, MBXUNT
        INTEGER*4       UNIT
 	INTEGER*4	STSFLG
C
	LOGICAL		PRGPRCDIF
C
	INTEGER*4	WSEXTENT,WSQUOTA,WSDEFAULT
	INTEGER*4	PRCNAMEXP_LEN
	CHARACTER*256	ERRMSG
	INTEGER*4       LEN

C
	PRGPRCDIF = .FALSE.
C
C	FOR MULTIPLE GAMING SYSTEMS ON ONE COMPUTER EACH PROCESS SHOULD
C	HAVE A PREFIX IDENTIFYING WICH GAMING ENVIRONMENT THE PROCESS
C	BELONGS
C
	IF (FIRST_TIME) THEN
	    FIRST_TIME = .FALSE.
	    CALL GETPRFX(I4PRCNAM, PREFIX_LEN)
	    CALL GET_WSDEFAULT(WSDEFAULT)
	    CALL GET_WSQUOTA(WSQUOTA)
	    CALL GET_WSEXTENT(WSEXTENT)
	ENDIF

C----+------------------------------------------------------------------
C V02| Allowing task run from another location
C----+------------------------------------------------------------------
        TASK_FOUND = .FALSE.
        IDX_LOCATION = DEFAULT_LOCATION
        DO WHILE(     TASK_FOUND .EQ. .FALSE.
     *        .AND. IDX_LOCATION .LE. MAX_ALT_LOC )

C----+------------------------------------------------------------------
C V02| Initializing TASKLIB value
C----+------------------------------------------------------------------
            DO IDX = 1, 7
                TASKLIB(IDX:IDX) = ALTLOC(IDX_LOCATION)(IDX:IDX)
            ENDDO

C----+------------------------------------------------------------------
C V02| Build program file name to be loaded
C    |     Length of IMAGE is changed later, so initialize it
C----+------------------------------------------------------------------
            DO I = TASKLIB_SZ + 1, 63
	    		IMAGE(I:I) = ' '
            ENDDO

            AUX_CONDITION = .FALSE.
            I = 1
            DO WHILE( AUX_CONDITION .EQ. .FALSE.
     *          .AND.             I .LE. 8 )
     
	   			NAMB = PRGNAME(I)
	   			NAMC = CHAR(NAMB)
               
               IF (NAMB .EQ. 0 .OR. NAMC .EQ. ' ') THEN
                  AUX_CONDITION = .TRUE.
               ELSE
	   			  IMAGE((TASKLIB_SZ+I):(TASKLIB_SZ+I))=NAMC
                  I = I + 1 
               ENDIF
            ENDDO

			I = I - 1
			PROGNAM_LEN = I
            
            DO IND = 1,4
	   			IMAGE(I+IND+TASKLIB_SZ:I+IND+TASKLIB_SZ)=EXT(IND:IND)
            ENDDO
            
			INQUIRE(FILE=IMAGE, EXIST=STATUS)
            IF (STATUS) THEN
                TASK_FOUND = .TRUE.
            ELSE
                IDX_LOCATION = IDX_LOCATION + 1
            ENDIF 
        ENDDO
        IF ( .NOT. STATUS) THEN
	    CALL WRITEBRK(IAM()//'FILE DOES NOT EXIST: '//
     *                    IMAGE(1:PROGNAM_LEN+TASKLIB_SZ+4))
	    RETURN
	ENDIF
C!C
C!C
C!C BUILD PROGRAM FILE NAME TO BE LOADED
C!C
C!C
C!C LENGTH OF IMAGE IS CHANGED LATER, SO INITIALIZE IT
C!C
C!	DO 6, I=TASKLIB_SZ+1,63
C!	    IMAGE(I:I) = ' '
C!6	CONTINUE
C!C
C!	DO 10 I=1,8
C!	   NAMB = PRGNAME(I)
C!	   NAMC = CHAR(NAMB)
C!	   IF (NAMB .EQ. 0 .OR. NAMC.EQ.' ') THEN
C!	      GOTO 20
C!	   ENDIF
C!	   IMAGE((TASKLIB_SZ+I):(TASKLIB_SZ+I))=NAMC
C!10	CONTINUE
C!C
C!20	CONTINUE
C!	I = I - 1
C!	PROGNAM_LEN = I
C!C
C!	DO 25 IND=1,4
C!	   IMAGE(I+IND+TASKLIB_SZ:I+IND+TASKLIB_SZ)=EXT(IND:IND)
C!25	CONTINUE
C!C
C!C	CHECK IF FILE EXISTS
C!C V02	EVEN WITH RECIEVING TERMINATION MESSAGE IT IS MORE MEANINGFULL
C!C V02	TO CHECK THE EXISTANCE OF THE FILE
C!C
C!	INQUIRE(FILE=IMAGE, EXIST=STATUS)
C!	IF (.NOT.STATUS) THEN
C!	    CALL WRITEBRK(IAM()//'FILE DOES NOT EXIST: '//
C!     *                    IMAGE(1:PROGNAM_LEN+TASKLIB_SZ+4))
C!	    RETURN
C!	ENDIF
C----+------------------------------------------------------------------
C V02| Allowing task run from another location
C----+------------------------------------------------------------------

C
C	LENGTH OF PRCNAMEXP IS CHANGED LATER, SO INITIALIZE IT
C
	DO 30 I=1,8
	   NAMB = PRCNAME(I)
	   IF (NAMB .NE. PRGNAME(I)) THEN
	     PRGPRCDIF = .TRUE.
	   ENDIF
	   NAMC = CHAR(NAMB)
	   IF (NAMB .EQ. 0 .OR. NAMC.EQ.' ') THEN
	      GOTO 40
	   ENDIF
	   PRCNAMEXP((PREFIX_LEN+I):(PREFIX_LEN+I))=NAMC
30	CONTINUE
C
40	CONTINUE
	I = I - 1
C
C	SET PROCESS NAME LENGTH PROPERLY FOR SYSTEM SERVICE CALL
C
C***************
	PRCNAMEXP_LEN = PREFIX_LEN+I
C
C	IF PROCESS NAME IS DIFFERENT FROM PROGRAM NAME, PRINT A MESSAGE
C
	IF (PREFIX_LEN .GT. 0 .OR. PRGPRCDIF) THEN
D	    TYPE *,IAM(),'ACTIVATING ',IMAGE,' AS ', PRCNAMEXP
	ENDIF
C
C	WE WILL GIVE ALL PRIVILEGES THAT THE FATHER HAS
C
	PRVADR(1) = 'FFFFFFFF'X
	PRVADR(2) = 'FFFFFFFF'X
C
C	SET QUOTAS
C
	QUOTA(1).QUOTA_TYPE = PQL$_WSDEFAULT
	QUOTA(1).QUOTA_AMOUNT = WSQUOTA
	QUOTA(2).QUOTA_TYPE = PQL$_WSQUOTA
	QUOTA(2).QUOTA_AMOUNT = WSQUOTA
	QUOTA(3).QUOTA_TYPE = PQL$_WSEXTENT
	QUOTA(3).QUOTA_AMOUNT = WSEXTENT
	QUOTA(4).QUOTA_TYPE = PQL$_LISTEND
C
C	SET STATUS FLAG
C
	STSFLG = PRC$M_DISAWS		! DISABLE WORKING SET ADJUSTMENT
     1  .OR.     PRC$M_PSWAPM	        ! INHIBIT PROCESS SWAPPING
     *  .OR.     PRC$M_DETACH	        ! MAKE A DETCHED PROCESS
C
C	USE UIC OF THE CREATING PROCESS
C
	I2UIC(1) = 0
	I2UIC(2) = 0
C
C Set output file name
C
	WRITE(OUTPUT,900) PRCNAMEXP(1:PRCNAMEXP_LEN)
900	FORMAT('GXLOG:',A,'.LOG')
	ERROR = OUTPUT			! SYS$ERROR to OUTPUT file
	INPUT = 'NL:'			! SYS$INPUT to NL:
C
C       GET MAILBOX UNIT
C
        CALL GET_MAIL_UNIT(UNIT, MBXCHAN)
        MBXUNT = UNIT
D        TYPE *,IAM(),'MBXUNT, MBXCHAN ', MBXUNT, MBXCHAN
C
C	ISSUE THE CALL
C
	IF (P(LOCK_PAGES_MEM) .NE. NOLOCK_PAGES_MEM_VALUE) THEN
		BASPRI = 6
	ELSE
		BASPRI = 4
	ENDIF
C
        IF(IDX_LOCATION .NE. DEFAULT_LOCATION) THEN
            CALL GET_DVOL(OLD_DVOL, OLD_DVOL_LEN)
            CALL GET_DDIR(OLD_DDIR, OLD_DDIR_LEN)
            CALL SET_DDIR(ALTLOC(IDX_LOCATION),7)
            CALL GET_DVOL(DVOL, DVOL_LEN)
            CALL GET_DDIR(DDIR, DDIR_LEN)
        ENDIF
	STATUS = SYS$CREPRC (PIDADR, IMAGE, INPUT,
     *			     OUTPUT, ERROR,
     *			     PRVADR, QUOTA,
     *			     %DESCR(PRCNAMEXP(1:PRCNAMEXP_LEN)),
     *			     %VAL(BASPRI),
     *			     %VAL(UIC), %VAL(MBXUNT), %VAL(STSFLG))
        IF(IDX_LOCATION .NE. DEFAULT_LOCATION) THEN
            CALL SET_DVOL(OLD_DVOL,OLD_DVOL_LEN)
            CALL SET_DDIR(OLD_DDIR,OLD_DDIR_LEN)
            CALL GET_DVOL(DVOL, DVOL_LEN)
            CALL GET_DDIR(DDIR, DDIR_LEN)
        ENDIF
C
C	CHECK THE STATUS
C
	IF (.NOT.STATUS) THEN
	    CALL SYS$GETMSG(%VAL(STATUS), LEN, ERRMSG, %VAL(1),)
	    CALL WRITEBRK(IAM()//'ERROR ACTIVATING '//
     *                    IMAGE(1:PROGNAM_LEN+TASKLIB_SZ+4)//' -> '//
     *                    ERRMSG(1:LEN))
	    GOTO 70
	ENDIF
C
C       WAIT UNTIL THE MAIL BOX HAS ANY MESSAGES OR TASK STATUS
C       INDICATES THAT THE PROCESS STARTED
C
D       TYPE *,IAM(),'PROCESS ID ', PIDADR
C
50      CONTINUE
C
        CALL CHECK_MAIL_BOX(MBXCHAN,PRCNAME,PRCNAMEXP(1:PRCNAMEXP_LEN),
     *                      PIDADR,STATUS)
	IF(STATUS .NE. 0) THEN
	    CALL WRITEBRK(IAM()//'ERROR ACTIVATING TASK: '//PRCNAMEXP)
	    GOTO 70
	ENDIF
CC
	IF (.NOT.WFLG) THEN
	   GOTO 10000
	ENDIF
60	CONTINUE
	CALL XWAIT(2,2,STATUS)
	CALL STTSK(PRCNAME,TSKSTS,STATUS)
	IF (STATUS.NE.4) THEN
	   GOTO 60
	ENDIF
C
C COMMON RETURN
C
10000	CONTINUE
	RETURN
C
C PROGRAM RUN ERROR
C
70	CONTINUE
	RETURN
C
	END




C----+------------------------------------------------------------------
C V02| Auxiliary subroutines: GET_DVOL - getting default volume
C----+------------------------------------------------------------------
        SUBROUTINE GET_DVOL(DVOL,DVOL_LEN)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE '(LIB$ROUTINES)'

        INCLUDE '($LNMDEF)' 
        INCLUDE '($SYSSRVNAM)' 
         
        STRUCTURE /LIST/ 
          INTEGER (KIND=2) BUF_LEN/255/ 
          INTEGER (KIND=2) ITEM_CODE/LNM$_STRING/ 
          INTEGER (KIND=4) TRANS_LOG 
          INTEGER (KIND=4) TRANS_LEN 
          INTEGER (KIND=4) END_ENTRY/0/ 
        END STRUCTURE    !LIST 
         
        CHARACTER (LEN=255) EQV_BUFFER 
        INTEGER (KIND=2)  W_NAMELEN 
        INTEGER*4 I,DVOL_LEN,LEN
        CHARACTER*255 AUX
        CHARACTER*255 DVOL
        
        LOGICAL ISTAT
C        INTEGER*4 IGS_DEBUG_LVL
C        IGS_DEBUG_LVL = 0
        
        RECORD/LIST/ ITEM_LIST 
        ITEM_LIST.TRANS_LOG = %LOC(EQV_BUFFER) 
        ITEM_LIST.TRANS_LEN = %LOC(W_NAMELEN) 
         
C        CALL GET_IGS_DEBUG(IGS_DEBUG_LVL)

        DO I=1,255
            DVOL(I:I)=CHAR(0)
        ENDDO
        ISTAT = SYS$TRNLNM(, 'LNM$PROCESS_TABLE', 'SYS$DISK', ,ITEM_LIST) 
        IF (ISTAT) THEN
            DO I=1,W_NAMELEN
                DVOL(I:I) = EQV_BUFFER(I:I)
            ENDDO
            DVOL_LEN = W_NAMELEN
        ENDIF         
        
C        IF (IGS_DEBUG_LVL .NE. 0) THEN
C            CALL OPS('GET_DVOL = ' // DVOL(1:DVOL_LEN),DVOL_LEN,DVOL_LEN)
C        ENDIF
        
        RETURN
        END
        
        


C----+------------------------------------------------------------------
C V02| Auxiliary subroutines: GET_DDIR - getting default directory
C----+------------------------------------------------------------------
        SUBROUTINE GET_DDIR(DDIR,DDIR_LEN)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE '(LIB$ROUTINES)'

        INCLUDE '($LNMDEF)' 
        INCLUDE '($SYSSRVNAM)' 
         
        STRUCTURE /LIST/ 
          INTEGER (KIND=2) BUF_LEN/255/ 
          INTEGER (KIND=2) ITEM_CODE/LNM$_STRING/ 
          INTEGER (KIND=4) TRANS_LOG 
          INTEGER (KIND=4) TRANS_LEN 
          INTEGER (KIND=4) END_ENTRY/0/ 
        END STRUCTURE    !LIST 
         
        CHARACTER (LEN=255) EQV_BUFFER 
        INTEGER (KIND=2)  W_NAMELEN 
        INTEGER*4 I,DDIR_LEN,LEN
        CHARACTER*255 AUX
        CHARACTER*255 DDIR
        
        LOGICAL ISTAT
C        INTEGER*4 IGS_DEBUG_LVL
C        IGS_DEBUG_LVL = 0
        
        RECORD/LIST/ ITEM_LIST 
        ITEM_LIST.TRANS_LOG = %LOC(EQV_BUFFER) 
        ITEM_LIST.TRANS_LEN = %LOC(W_NAMELEN) 
         
C        CALL GET_IGS_DEBUG(IGS_DEBUG_LVL)

        DO I=1,255
            DDIR(I:I)=CHAR(0)
        ENDDO
        ISTAT = SYS$SETDDIR(,LEN,AUX)
        DO I=1,LEN
            DDIR(I:I) = AUX(I:I) 
        ENDDO
        DDIR_LEN = LEN
C        IF (IGS_DEBUG_LVL .NE. 0) THEN
C            CALL OPS('GET_DDIR = ' // DDIR(1:DDIR_LEN),DDIR_LEN,DDIR_LEN)
C        ENDIF
        
        RETURN
        END
        
        
C----+------------------------------------------------------------------
C V02| Auxiliary subroutines: SET_DVOL - setting default volume
C----+------------------------------------------------------------------
        SUBROUTINE SET_DVOL(DVOL,DVOL_LEN)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE '(LIB$ROUTINES)'

        INCLUDE '($LNMDEF)' 
        INCLUDE '($SYSSRVNAM)' 
         
        INTEGER*4 I,DVOL_LEN
        CHARACTER*255 DVOL
C        INTEGER*4 IGS_DEBUG_LVL
C        IGS_DEBUG_LVL = 0
C        CALL GET_IGS_DEBUG(IGS_DEBUG_LVL)

        I = LIB$SET_LOGICAL('SYS$DISK',DVOL(1:DVOL_LEN),'LNM$PROCESS_TABLE',,,)
C        IF (IGS_DEBUG_LVL .NE. 0) THEN
C           CALL OPS('SET_DVOL = ' // DVOL(1:DVOL_LEN),DVOL_LEN,I)
C        ENDIF
        RETURN
        END
        
        
        
C----+------------------------------------------------------------------
C V02| Auxiliary subroutines: SET_DDIR - setting default directory
C----+------------------------------------------------------------------
        SUBROUTINE SET_DDIR(DDIR,DDIR_LEN)
        IMPLICIT NONE
        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE '(LIB$ROUTINES)'

        INCLUDE '($LNMDEF)' 
        INCLUDE '($SYSSRVNAM)' 
        
        
        INTEGER*4 I,DDIR_LEN
        CHARACTER*255 DDIR
C        INTEGER*4 IGS_DEBUG_LVL
C        IGS_DEBUG_LVL = 0
C        CALL GET_IGS_DEBUG(IGS_DEBUG_LVL)
        I = SYS$SETDDIR(DDIR(1:DDIR_LEN),,)
C        IF (IGS_DEBUG_LVL .NE. 0) THEN
C           CALL OPS('SET_DDIR = ' // DDIR(1:DDIR_LEN),DDIR_LEN,I)
C        ENDIF
        RETURN
        END

        

C----+------------------------------------------------------------------
C V02| Auxiliary subroutines: GET_IGS_DEBUG - getting IGS debug level
C----+------------------------------------------------------------------
C        SUBROUTINE GET_IGS_DEBUG(IGS_DEBUG_LVL)
C        IMPLICIT NONE
C        
C        INTEGER*4 IGS_DEBUG_LVL
C        INTEGER*4 IGS_DEBUG_LEVEL
C        COMMON /IGS_DEBUG_COM/ IGS_DEBUG_LEVEL
C        
C        IGS_DEBUG_LVL = IGS_DEBUG_LEVEL
C        
C        RETURN
C        END
        
C----+------------------------------------------------------------------
C V02| Auxiliary subroutines: SET_IGS_DEBUG - setting IGS debug level
C----+------------------------------------------------------------------
C        SUBROUTINE SET_IGS_DEBUG(IGS_DEBUG_LVL)
C        IMPLICIT NONE
C        
C        INTEGER*4 IGS_DEBUG_LVL
C        INTEGER*4 IGS_DEBUG_LEVEL
C        COMMON /IGS_DEBUG_COM/ IGS_DEBUG_LEVEL
C        
C        IGS_DEBUG_LEVEL = IGS_DEBUG_LVL
C        
C        RETURN
C        END
        
