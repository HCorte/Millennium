C
C NRM_PASSUBS.FOR
C
C V04 13-MAY-2011 FJG PMAXNUMCLA Classica
C V03 24-JAN-2011 RXK IF command split
C V02 01-JAN-2010 FJG ePassive
C V01 11-DEC-00 CS  INITIAL RELEASE FOR PORTUGAL
C
C ROUTINES TO ACCESS PASSIVE LOTTERY FILES
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
C************************************************************
C ROUTINE TO RETURN SCML PASSIVE DRAW # USING WEEK/YEAR FORMAT
C************************************************************
C=======OPTIONS    /CHECK=NOOVERFLOW
        SUBROUTINE GETPASDRW(SCML,WEEK,YEAR)
        IMPLICIT   NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
C
C ROUTINE VARIABLES
C
	INTEGER*4   SCML, WEEK, YEAR
C
C START OF CODE
C
	WEEK = SCML/10000
	YEAR = MOD(SCML,10000)
C
        RETURN
        END       
C===============================================================================        
C===============================================================================
C===============================================================================
C*************************************************************
C SUBROUTINE TO OPEN/WRITE AND CLOSE WORK PASSIVE LOTTERY FILE
C*************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PAS_WINLOD(FUN,RECORD)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PRMVPF.DEF'
	INCLUDE 'INCLIB:RECVLW.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:STANDARD.DEF'

	! CONSTANT DEFINITIONS
	INTEGER*4   OPEN_FILE
	PARAMETER   (OPEN_FILE   = 1)

	INTEGER*4   WRITE_FILE
	PARAMETER   (WRITE_FILE = 2)

	INTEGER*4   CLOSE_FILE
	PARAMETER   (CLOSE_FILE = 3)

	INTEGER*4 K, VBLOCK, VIND, ST, VPFCNT
	INTEGER*4 VPFREC(I4VLWSIZ)/I4VLWSIZ*0/

	INTEGER*4 VFDB(7)
	INTEGER*4 RECORD(*),FUN
	INTEGER*4 LENGTH, NUMCHUNK
	DATA VPFCNT/0/

	! OPEN VPW FILE
	IF(FUN .EQ. OPEN_FILE) THEN 

	  CALL OPENW(VPW,SFNAMES(1,VPW),4,0,0,ST)
	  CALL IOINIT(VFDB,VPW,I4VLWSIZ*4)
	  IF(ST.NE.0) CALL FILERR(SFNAMES(1,VPW),OPEN_ERROR,ST,0)

	  VIND   = 1
	  VBLOCK = 0

	! WRITE VPW FILE
	ELSEIF(FUN .EQ. WRITE_FILE) THEN

          NUMCHUNK = ISHFT(RECORD(VFSSER),-30) + 1

          LENGTH = NUMCHUNK * VPFLEN
          VPFCNT = VPFCNT + 1

          CALL FASTMOV(RECORD,VPFREC(VIND),LENGTH)
          VIND = VIND + LENGTH

          IF(VIND.GT.I4VLWSIZ-(VPFLEN*VPFMAXREC)+1) THEN
            VBLOCK=VBLOCK+1
            CALL WRITEW(VFDB,VBLOCK,VPFREC,ST)
            IF(ST.NE.0) CALL FILERR(SFNAMES(1,VPW),WRITE_ERROR,ST,VBLOCK)
            CALL FASTSET(0,VPFREC,I4VLWSIZ)
            VIND=1
          ENDIF

	! CLOSE VPW FILE
	ELSEIF(FUN .EQ. CLOSE_FILE) THEN
	
	  VBLOCK=VBLOCK+1

	  CALL WRITEW(VFDB,VBLOCK,VPFREC,ST)
	  IF(ST.NE.0) CALL FILERR(SFNAMES(1,VPW),WRITE_ERROR,ST,VBLOCK)
	  CALL CLOSEFIL(VFDB)

	  WRITE(5,9100) IAM(),VPFCNT,(SFNAMES(K,VPW),K=1,5)

	ENDIF

	RETURN

9100	FORMAT(1X,A,1X,I8,' records loaded to ',5A4)
	END

C*******************************************************************************
C**** VERIFY IF WE HAVE WINNER SELECTION FOR PASSIVE TODAY
C*******************************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE HAS_PAS_WINSEL(EMINUM, INDPAS)
	IMPLICIT NONE

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
C
C ROUTINE PARAMETERS
C
	INTEGER*4   EMINUM, INDPAS
C
C LOCAL VARIABLES
C
	INTEGER*4   INDEMI

	EMINUM = 0
	DO INDEMI=1, PAGEMI

	    ! TEST IF TODAY IS A DRAW DAY FOR PASSIVE LOTTERY
	    IF (PASSTS(INDEMI, INDPAS) .EQ. GAMDON)
     *	      EMINUM = PASEMIS(INDEMI, INDPAS)

	ENDDO

	RETURN
	END

C*********************************************************************
C**** CREATE WINNER SELECTION FILES IN ACCORDING TO NUMBER OF WINNERS
C*********************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE PAS_CRTFIL(FILNAM, EMINUM, INDPAS, STATUS)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
C
C ROUTINE PARAMETERS
C
	INTEGER*4   FILNAM(5)
	INTEGER*4   EMINUM, INDPAS
        INTEGER*4   MULTIP
C
C LOCAL VARIABLES
C
	INTEGER*4   OFFEMI, INDDIV, TOTSHR, TOTSEC, STATUS, TEMP
C
C CALCULATE NUMBER OF WINNERS
C
	TOTSHR = 0
	OFFEMI = -1
	DO TEMP=1,PAGEMI
	    IF (PASEMIS(TEMP, INDPAS) .EQ. EMINUM) THEN
               OFFEMI = TEMP
	    ENDIF
	ENDDO

	IF (OFFEMI.LT.0) THEN
	    TYPE*,IAM(),'ERROR FINDING OFFSET ON MEMORY (PAS_CRTFIL)'
	    CALL GPAUSE()
	    STATUS = -1
	    RETURN
	ENDIF

        IF(INDPAS.EQ.PSBCLA) THEN   ! CLASSICA
          MULTIP = 60 
        ELSE                        ! POPULAR
          MULTIP = 30
        ENDIF

	DO INDDIV=1, PAGDIV
	  TOTSHR = TOTSHR +
     *		   (PASSHR(INDDIV, OFFEMI, INDPAS) * 
     *		    PASNUMSER(OFFEMI, INDPAS) * PASNOFFRA(OFFEMI, INDPAS))
	ENDDO
C
C CALCULATE FILE LENGTH TO SUPPORT THESE NUMBER OF WINNERS WHERE
C MULTIP - AVERAGE SIDE
C 512    - NUMBER OF BYTES PER BLOCK
C
	TOTSEC = (MULTIP * TOTSHR) / 512
C
C CREATE FILE
C
	CALL CRTFIL(FILNAM, TOTSEC, STATUS)

	RETURN
	END

C*********************************************************************
C**** CREATE TICKET FILES IN ACCORDING TO NUMBER OF TICKETS
C*********************************************************************
C=======OPTIONS /CHECK=NOOVERFLOW
C SUBROUTINE PAS_IDX_CRTFIL(LUN, FILNAM, NUMTCK, STATUS) !(PLCS)
	SUBROUTINE PAS_IDX_CRTFIL(LUN, FILNAM, NUMTCK, STATUS, NEWLOT) !(PLCS)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'

C
C ROUTINE PARAMETERS
C
	CHARACTER   FILNAM*(*)
	INTEGER*4   NUMTCK, LUN
	LOGICAL*1 NEWLOT !(PLCS)
C
C LOCAL VARIABLES
C
	INTEGER*4   TOTSEC, STATUS

        INTEGER*4   CFILCNTG
        EXTERNAL    CFILCNTG        !USEROPEN FOR CONTIGUOUS FILE

	RECORD /STCIDX/ RECIDX
	RECORD /NEWSTCIDX/ NEWRECIDX !(PLCS)

C
C CALCULATE NUMBER OF TICKETS
C
C(PLCS)
      IF (NEWLOT) THEN
        TOTSEC = (NUMTCK * SIZEOF(NEWRECIDX)) / 512
      ELSE
        TOTSEC = (NUMTCK * SIZEOF(RECIDX)) / 512
      ENDIF
C
C CREATE FILE
C

      IF (NEWLOT) THEN

	TYPE*,IAM(),'Creating file ',FILNAM
        OPEN( UNIT         = LUN, 
     *        FILE         = FILNAM, 
     *        ACCESS       = 'DIRECT',
     *        INITIALSIZE  = TOTSEC+1, 
     *        STATUS       = 'NEW', 
     *        RECL         = SIZEOF(NEWRECIDX),
     *        ORGANIZATION = 'SEQUENTIAL', 
     *        IOSTAT       = STATUS,
     *        RECORDTYPE   = 'FIXED', 
     *        FORM         = 'FORMATTED',
     *        USEROPEN     = CFILCNTG)
     
      ELSE

	TYPE*,IAM(),'Creating file ',FILNAM
        OPEN( UNIT         = LUN, 
     *        FILE         = FILNAM, 
     *        ACCESS       = 'DIRECT',
     *        INITIALSIZE  = TOTSEC+1, 
     *        STATUS       = 'NEW', 
     *        RECL         = SIZEOF(RECIDX),
     *        ORGANIZATION = 'SEQUENTIAL', 
     *        IOSTAT       = STATUS,
     *        RECORDTYPE   = 'FIXED', 
     *        FORM         = 'FORMATTED',
     *        USEROPEN     = CFILCNTG)
    
      ENDIF 
	IF (STATUS.NE.0) THEN
	    TYPE*,IAM(),'ERROR CREATING FILE ',FILNAM,' ST = ',STATUS
	    CALL GPAUSE()
	ENDIF
C(PLCS)

	CLOSE(LUN)

	RETURN
	END
C


C
C *******************************************
C RETURNS LAST DRAW WITH WINNER SELECTION
C *******************************************
C
C=======OPTIONS   /CHECK=NOOVERFLOW
	INTEGER*4 FUNCTION FIND_DRAW_WINSEL(GTYP,GIND,GAME)
        IMPLICIT  NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:STANDARD.DEF'
C
C FUNCTION PARAMETERS
C
	INTEGER*4 GTYP,GIND,GAME
C
C LOCAL VARIABLES
C	
	INTEGER*4 UNIT,DRAW,ST
	INTEGER*4 EMISIND

	LOGICAL	  DRAW_FOUND
C
C GET AVAILABLE HANDLE FILE
C
	CALL FIND_AVAILABLE_LUN(UNIT,ST)
	IF(ST.NE.0) CALL FILERR(GFNAMES(1,GAME),HANDLE_ERROR,ST,0)

	DRAW_FOUND = .TRUE.

	IF  (GTYP.EQ.TPAS) THEN
	    DRAW    = PASEMIS(CURDRW,GIND)
C**	    EMISIND = 1
	    EMISIND = 0

	    DO WHILE(DRAW_FOUND .AND. DRAW.GT.0)
		EMISIND = EMISIND + 1
		IF(EMISIND.LE.PAGEMI) THEN
		    DRAW = PASEMIS(EMISIND,GIND)
		    IF(DRAW.GT.0) THEN
			IF(PASSTS(EMISIND,GIND).EQ.GFINAL) DRAW_FOUND = .FALSE.
		    ENDIF
		ELSE
		    DRAW = 0
		ENDIF
	    ENDDO

	ENDIF

	FIND_DRAW_WINSEL = DRAW
	RETURN
	END
C
C *******************************************
C RETURN LAST PASSIVE PURGED DRAW
C *******************************************
C
C=======OPTIONS	    /CHECK=NOOVERFLOW
	INTEGER*4   FUNCTION FIND_DRAW_PURGED(GTYP,GAME)
        IMPLICIT    NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE	'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DPAREC.DEF'
        INCLUDE	'INCLIB:STANDARD.DEF'
C
C FUNCTION PARAMETERS
C
	INTEGER*4   GTYP,GAME
C
C LOCAL VARIABLES
C	
	INTEGER*4   FIND_DAF_DRWPRG
	INTEGER*4   FDB(7),UNIT,DRAW,ST
C
	DRAW = FIND_DAF_DRWPRG(GAME) ! FIND DRAW IN DAF FILE
C
	IF  (DRAW.GT.0)	THEN
C
C GET AVAILABLE HANDLE FILE
C
	    CALL FIND_AVAILABLE_LUN(UNIT,ST)
	    IF  (ST.NE.0) THEN
		CALL FILERR(GFNAMES(1,GAME),HANDLE_ERROR,ST,0)
	    ELSE
		CALL OPENW(UNIT,GFNAMES(1,GAME),4,0,0,ST)
		IF  (ST.NE.0) THEN
		    CALL FILERR(GFNAMES(1,GAME),OPEN_ERROR,ST,0)
		ELSE
		    IF (GTYP.EQ.TPAS) THEN
			CALL IOINIT(FDB,UNIT,DPASEC*256)
			DPAPUP = 0	    !ONLY FOR SECURITY

			DO  WHILE(DPAPUP.EQ.0 .AND. ST.EQ.0 .AND.
     *                            DRAW-PAS_DRW_OFFSET.GT.0)
			    CALL READW(FDB,DRAW-PAS_DRW_OFFSET,DPAREC,ST)
			    IF	(ST.NE.0) THEN
				CALL FILERR(GFNAMES(1,GAME),READ_ERROR,ST,0)
			    ELSE
				IF  (DPAPUP.EQ.0) DRAW = DRAW - 1
			    ENDIF
			ENDDO
		    ENDIF
		    CALL CLOSEFIL(FDB)
		ENDIF
	    ENDIF
	ENDIF

	FIND_DRAW_PURGED = DRAW
	RETURN
	END
C
C *******************************************
C FIND A DRAW IN DAF FILE USING CDC
C *******************************************
C
C=======OPTIONS	  /CHECK=NOOVERFLOW
        INTEGER*4 FUNCTION FIND_DAF_DRWPRG(GNUM)
C
        IMPLICIT  NONE

        INCLUDE	  'INCLIB:SYSPARAM.DEF'
        INCLUDE	  'INCLIB:SYSEXTRN.DEF'
        INCLUDE	  'INCLIB:GLOBAL.DEF'
        INCLUDE	  'INCLIB:CONCOM.DEF'
	INCLUDE	  'INCLIB:RECDAF.DEF'
        INCLUDE	  'INCLIB:STANDARD.DEF'
C
C ROUTINE PARAMETERS
C
	INTEGER*4 PCDC, GNUM
C
C LOCAL VARIABLES
C
	INTEGER*4 ST, DAF_FDB(7), UNIT, DRAW
C
	DRAW = 0

	IF (GNUM.GT.0) THEN
          IF(PRGDAY(GNUM).GT.0) THEN
	    PCDC = DAYCDC - PRGDAY(GNUM)
	    IF	(PCDC.GT.0) THEN
C
C FIND UNIT FOR DAF FILE
C
		CALL FIND_AVAILABLE_LUN(UNIT,ST)
		IF  (ST.NE.0) THEN
		    CALL FILERR(SFNAMES(1,DAF),HANDLE_ERROR,ST,0)
		ELSE
C
C OPEN DAF
C
		    CALL OPENW(UNIT,SFNAMES(1,DAF),4,0,0,ST)
		    IF	(ST.NE.0) THEN
			CALL FILERR(SFNAMES(1,DAF),OPEN_ERROR,ST,0)
		    ELSE
			CALL IOINIT(DAF_FDB,UNIT,DAFSEC*256)
C
C READ DAF WITH PURGE CDC
C
			CALL READW(DAF_FDB,PCDC,DAFREC,ST)
C
C CLOSE DAF FILE
C
			CALL CLOSEFIL(DAF_FDB)

			IF  (ST.NE.0) THEN 
			    CALL FILERR(SFNAMES(1,DAF),READ_ERROR,ST,PCDC)
			ELSE
			    DRAW = DAFHDR(GNUM)
			ENDIF
		    ENDIF
		ENDIF
	    ENDIF
	  ENDIF
	ENDIF
C
C UPDATE DRAW
C
	FIND_DAF_DRWPRG = DRAW
	RETURN
	END
C
C *****************************************************************
C FILLS "EMISION" IN FILEPAS (FILESTRUCS.DEF) WITH ACTIVE EMISSIONS
C *****************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE FILL_FILEPAS_ACTIVE (ST)
C
	IMPLICIT  NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'

	INTEGER*4 INDEMI, ST, POS, INDPAS, QTD
       
	ST  = 0
	QTD = 0
	CALL FASTSET(0,FILEPAS,SIZEOF(FILEPAS)/4)

	DO INDPAS = 1, NUMPAS
	  POS = 0
	  DO INDEMI = 1,PAGEMI
	    IF (PASEMIS(INDEMI,INDPAS).GT.0 .AND.
     *          PASSTS(INDEMI,INDPAS).EQ.GFINAL) THEN
              POS = POS + 1
              FILEPAS(POS,INDPAS).EMISION = PASEMIS(INDEMI,INDPAS)
            ENDIF
          ENDDO
	  QTD = QTD + POS
	ENDDO

        IF (QTD.EQ.0) ST = -1       !NAO HA EMISSOES ATIVAS

        RETURN
	END
C
C ************************************************************
C OPEN PASSIVE FILES
C ************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE OPENPAS (ST)
C
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMVPF.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'
C
C 	LOCAL VARIABLES
C
	INTEGER*4    ST, 
     *               INDEMI,
     *               INDPAS,
     *               FILENAME(5)
        CHARACTER*20 FILENAMECHAR

C
C	SET STATUS TO ZERO
C
	ST = 0
C
C	LOOP FOR ALL THE FILES TO BE OPEN
C
	DO INDPAS = 1, NUMPAS
	   INDEMI  = 1
	
	   DO WHILE (INDEMI.LE.PAGEMI .AND.
     *               FILEPAS(INDEMI,INDPAS).EMISION.GT.0)
C
C	      DEFINE FILE NAME TO OPEN
C
	      CALL BUILD_FILENAME (SFNAMES(1,VPF), INDPAS,
     *                             FILEPAS(INDEMI,INDPAS).EMISION,
     *                             FILENAMECHAR, FILENAME)
C
C	      FIND AVAILABLE LUN AND FILL FILEPAS.LUN WITH IT
C
	      CALL FIND_AVAILABLE_LUN (FILEPAS(INDEMI,INDPAS).LUN, ST)
	      IF (ST.NE.0) THEN
		 TYPE*,IAM(),' NAO FOI POSSIVEL ENCONTRAR UMA',
     *                       ' LOGICAL UNIT PARA ABRIR O ARQUIVO ', FILENAMECHAR
                 RETURN
              ENDIF
C
C 	      OPEN ONE OF THE PASSIVE LOTTERY VALIDATION FILE 
C
	      CALL IOPEN(FILENAME, FILEPAS(INDEMI,INDPAS).LUN, VPFLEN*2,
     *                   VFSCDC, VFSSER*2-1,ST)
	      IF (ST.NE.0) THEN
	         TYPE*,IAM(),' ERRO ABRINDO ', FILENAMECHAR
                 RETURN
	      ENDIF
C
C	      GO TO NEXT EMISION IN FILEPAS
C              
	      INDEMI = INDEMI + 1
	   ENDDO
	ENDDO

	RETURN
	END
C
C ************************************************************
C CLOSE PASSIVE FILE
C ************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE CLOSEPAS (ST)
C
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMVPF.DEF'
	INCLUDE 'INCLIB:DESLOG.DEF'
	INCLUDE 'INCLIB:PRMLOG.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'
C
C 	LOCAL VARIABLES
C
	INTEGER*4    ST, 
     *               INDEMI,
     *               INDPAS,
     *		     BUF(2048),
     *		     FILENAME(5)
        CHARACTER*20 FILENAMECHAR
C
C	SET STATUS TO ZERO
C
	ST = 0
C
C	LOOP FOR ALL THE FILES TO CLOSE
C
	DO  INDPAS = 1, NUMPAS

	    INDEMI  = 1
	    DO WHILE (INDEMI.LE.PAGEMI .AND.
     *                FILEPAS(INDEMI,INDPAS).EMISION.GT.0)
C
C	      DEFINE FILE NAME (CASE OF ERROR)
C
	      CALL BUILD_FILENAME (SFNAMES(1,VPF), INDPAS,
     *                             FILEPAS(INDEMI,INDPAS).EMISION,
     *                             FILENAMECHAR, FILENAME)
C
C             CLOSE PASSIVE LOTTERY VALIDATION FILE
C
	      CALL ICLOSE(FILEPAS(INDEMI,INDPAS).LUN, BUF, ST)
	      IF (ST.NE.0) THEN
	         TYPE*,IAM(),' ERRO AO FECHAR ', FILENAMECHAR
		 CALL GPAUSE
C
C	         DO NOT RETURN HERE BECAUSE MAYBE WE WANT TO CONTINUE
C                TRYING TO CLOSE OTHER FILES
C
	      ENDIF
C
C	      CLEAN STRUCTURE
C
	      FILEPAS(INDEMI,INDPAS).EMISION = 0
	      FILEPAS(INDEMI,INDPAS).LUN     = 0
C
C	      GO TO NEXT EMISION IN FILEPAS
C              
	      INDEMI = INDEMI + 1
            ENDDO

	ENDDO

	RETURN
	END
C
C ******************************************************************
C READ PASSIVE FILE
C ******************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE READPAS (TPASIND, EMISION, BILHETE,
     *                      SERIE,   DECIMO,  V4BUF_PAS, ST)
C
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:VALPASFIL.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'
C
C 	ROUTINE PARAMETERS
C
	INTEGER*4    ST                   ! ST=-1 (FILE IS NOT OPEN)
C                                         ! ST= 0 (OK) / ST<-1 (IREAD ST ERROR)
	INTEGER*4    TPASIND,
     *               EMISION,
     *               SERIE,
     *               BILHETE,
     *               DECIMO
C
C	LOCAL VARIABLES
C
	LOGICAL	     FOUND
	INTEGER*4    I4TEMP,INDPAS(2), INDEMI
	INTEGER*2    I2TEMP(2)
        BYTE         I1TEMP(4)

	EQUIVALENCE (I2TEMP,I4TEMP,I1TEMP)
C
C	SET STATUS TO ZERO (OK)
C
	ST    = 0
	FOUND = .FALSE.
C
C	LOOP TO LOCATE EMISION IN FILEPAS.EMISION AND GET
C       CORRESPONDING FILEPAS.LUN TO BE ABLE TO ACCESS THE DESIRED FILE
C
	INDEMI   = 1

	DO  WHILE( INDEMI.LE.PAGEMI .AND. (.NOT.FOUND) )

	    IF (FILEPAS(INDEMI,TPASIND).EMISION.EQ.EMISION) THEN
C
C	      BUILD KEY TO LOCATE RECORD
C
	      I4TEMP      = 0			    !KEY2BYTES
	      I1TEMP(2)   = 0			    !FREE
	      I1TEMP(1)   = DECIMO
              INDPAS(1)   = I4TEMP
	      I4TEMP      = ISHFT(SERIE,24)+BILHETE !KEY4BYTES
              INDPAS(2)   = I4TEMP
C
C	      READ FILE
C
	      CALL IREAD(INDPAS ,V4BUF_PAS, FILEPAS(INDEMI,TPASIND).LUN, ST)

	      FOUND = .TRUE.

	    ELSE

	      INDEMI = INDEMI + 1

	    ENDIF

	ENDDO
C
C PLEASE CHECK ALL WRONG STATUS USING PRMHSH.DEF
C THIS ROUTINE WILL RETURN ONE OF THEM
C
	IF (.NOT.FOUND) THEN
           TYPE*,IAM(), 'ARQUIVO DA EMISION ',EMISION, 'INDICE', TPASIND,
     *                  ' DA LOTARIA NACIONAL NAO ESTA ABERTO.'
	   ST = ERRERR
        ENDIF

	RETURN
	END
C
C ******************************************************************
C WRITE PASSIVE FILE
C ******************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE WRITEPAS (V4BUF_PAS, ST)
C
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:DESVAL.DEF'
	INCLUDE 'INCLIB:VALPASFIL.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PRMHSH.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'
C
C 	ROUTINE PARAMETERS
C
	INTEGER*4    ST         ! ST = -1  (FILE IS NOT OPEN)
C                               ! ST =  0  (OK) / ST<-1 (IWRITE ST ERROR)
C
C	LOCAL VARIABLES
C
	LOGICAL      FOUND 
	INTEGER*4    TPASIND,
     *               EMISION,
     *               INDEMI
C
C	TRANSLATE V4BUF_PAS
C
	CALL LOGPAS (VALREC, V4BUF_PAS)

	TPASIND = VALREC(VGIND)
	EMISION = VALREC(VEXTR)
C
C	SET STATUS TO ZERO (OK)
C
	ST = 0
C
C	TEST FOR VALID VALREC VALUES
C	
	IF (EMISION.LE.0) THEN
	   TYPE*,IAM(),'EMISSAO',EMISION,
     *                 'INVALIDA NA GRAVACAO DA LOTARIA NACIONAL'
           ST = ERRERR
	   RETURN
	ELSEIF (TPASIND.LE.0 .OR. TPASIND.GT.NUMPAS) THEN
	   TYPE*,IAM(),'INDICE', TPASIND,
     *                 'INVALIDO NA GRAVACAO DA LOTARIA NACIONAL'
           ST = ERRERR
	   RETURN
	ENDIF
C
	FOUND  = .FALSE.
C
C	LOOP TO LOCATE EMISION IN FILEPAS.EMISION AND GET
C       CORRESPONDING FILEPAS.LUN TO BE ABLE TO ACCESS THE DESIRED FILE
C
	INDEMI = 1

	DO WHILE ( INDEMI.LE.PAGEMI .AND. (.NOT.FOUND) )
	
	   IF (FILEPAS(INDEMI,TPASIND).EMISION.EQ.EMISION) THEN  
C
C	      FOUND EMISION IN FILEPAS
C
	      FOUND = .TRUE.
C
C	      WRITE TO FILE
C
	      CALL IWRITE(V4BUF_PAS, FILEPAS(INDEMI,TPASIND).LUN, ST)
	   ELSE
	      INDEMI = INDEMI + 1
           ENDIF

	ENDDO
C
C PLEASE CHECK ALL WRONG STATUS USING PRMHSH.DEF
C THIS ROUTINE WILL RETURN ONE OF THEM
C
	IF (.NOT.FOUND) THEN
           TYPE*,IAM(), 'ARQUIVO DA EMISION ',EMISION, 'INDICE', TPASIND,
     *                  ' DA LOTARIA NACIONAL NAO ESTA ABERTO.'
	   ST = ERRERR
        ENDIF

	RETURN
	END
C
C *************************************************************************
C COMMON ROUTINE
C *************************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE BUILD_FILENAME(GENERAL_NAME, INDPAS, EMISION,
     *                            FILENAMECHAR, FILENAME)
	IMPLICIT NONE
C
C ROUTINE PARAMETERS
C
	INTEGER*4    GENERAL_NAME(5),
     *               EMISION,
     *               INDPAS
	INTEGER*4    FILENAME(5)
	CHARACTER*20 FILENAMECHAR
C
C LOCAL VARIABLES
C
	CHARACTER*20 GENERAL_NAMECHAR

	INTEGER*4    IND
C
C
C       BUILD GENERAL_NAMECHAR
C
	WRITE (GENERAL_NAMECHAR,FMT='(5A4)') (GENERAL_NAME(IND), IND=1,5)

	FILENAMECHAR = '                    '
	CALL FASTSET(0,FILENAME,SIZEOF(FILENAME)/4)
C
C	GET INITIAL PART OF THE NAME (BEFORE *)
C
	IND = 1
	DO  WHILE (IND.LE.SIZEOF(GENERAL_NAMECHAR) .AND.
     *             GENERAL_NAMECHAR(IND:IND).NE.'*')
	    IND = IND + 1
        ENDDO
	IND = IND - 1
C
C 	11 = SIZEOF (INDPAS+'_'+EMISION+'.FIL'
C
C	IF ( (IND+11) .LE. SIZEOF(FILENAMECHAR) ) THEN
	IF ( (IND+10) .LE. SIZEOF(FILENAMECHAR) ) THEN

C	   WRITE (FILENAMECHAR, FMT='(A<IND>,I2.2,A1,I4.4,A4)')
C     *           GENERAL_NAMECHAR,INDPAS,'_',EMISION,'.FIL'
	   WRITE (FILENAMECHAR, FMT='(A<IND>,I2.2,I4.4,A4)')
     *           GENERAL_NAMECHAR,INDPAS,EMISION,'.FIL'

C
C	   FILL FILENAME WITH FILENAMECHAR
C
	   CALL MOVBYT(%REF(FILENAMECHAR),1,FILENAME(1),1,SIZEOF(FILENAMECHAR))
	ENDIF

	RETURN
	END
C
C*******************************************************************
C
C ROUTINES TO ACESS IDX FILES (FOR RETURN TICKETS)
C
C*******************************************************************
C
C
C ********************************************************************
C FILLS "EMISION" IN FILEIDXPAS (FILESTRUCS.DEF) WITH ACTIVE EMISSIONS
C ********************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE FILL_FILEIDX_ACTIVE (ST)
C
	IMPLICIT  NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'

	INTEGER*4 INDEMI, ST, POS, INDPAS, QTD
       
	ST  = 0
	QTD = 0
	CALL FASTSET(0,FILEIDXPAS,SIZEOF(FILEIDXPAS)/4)

	DO INDPAS = 1, NUMPAS
	  POS = 0
	  DO INDEMI = 1,PAGEMI
	    IF (PASSTS(INDEMI,INDPAS).GE.GAMOPN .AND.
     *          PASSTS(INDEMI,INDPAS).LE.GFINAL) THEN
              POS = POS + 1
              FILEIDXPAS(POS,INDPAS).EMISION = PASEMIS(INDEMI,INDPAS)
            ENDIF
          ENDDO
	  QTD = QTD + POS
	ENDDO

        IF (QTD.EQ.0) ST = -1       !NAO HA EMISSOES ATIVAS

        RETURN
	END
C
C ************************************************************
C OPEN PASSIVE RETURN IDX FILES
C ************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE OPENIDX_PAS (ST)
C
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'
	INCLUDE 'INCLIB:STANDARD.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF' !(PLCS)
	
C
C 	LOCAL VARIABLES
C
	INTEGER*4    ST, 
     *               INDEMI,
     *               INDPAS,
     *               FILENAME(5)
        CHARACTER*20 FILENAMECHAR
C
	RECORD /STCIDX/ RECIDX
	RECORD /NEWSTCIDX/ NEWRECIDX !(PLCS)
C
C	SET STATUS TO ZERO
C
	ST = 0
C
C	LOOP FOR ALL IDX FILES TO BE OPEN
C
	DO INDPAS = 1, NUMPAS
	   INDEMI  = 1
	
	   DO WHILE (INDEMI.LE.PAGEMI .AND.
     *               FILEIDXPAS(INDEMI,INDPAS).EMISION.GT.0)
C
C	      DEFINE FILE NAME TO OPEN
C
	      CALL BUILD_FILENAME (SFNAMES(1,TPF), INDPAS,
     *                             FILEIDXPAS(INDEMI,INDPAS).EMISION,
     *                             FILENAMECHAR, FILENAME)

C
C 	      OPEN ONE OF THE PASSIVE LOTTERY VALIDATION FILE 
C (PLCS)
           
              CALL FIDX_OPEN(FILEIDXPAS(INDEMI,INDPAS).FDB, FILENAMECHAR,
     *                       SIZEOF(RECIDX),'OLD',ST)            
              IF (ST.NE.0) THEN
                CALL FIDX_OPEN(FILEIDXPAS(INDEMI,INDPAS).FDB, FILENAMECHAR,
     *                       SIZEOF(NEWRECIDX),'OLD',ST)
                IF (ST.NE.0) THEN
                    CALL FILERR(FILENAME, OPEN_ERROR, ST, 0)
                ENDIF
              ENDIF
C (PLCS)            
C
C	      GO TO NEXT EMISION IN FILEIDXPAS
C              
	      INDEMI = INDEMI + 1
	   ENDDO
	ENDDO

	RETURN
	END
C
C ************************************************************
C CLOSE PASSIVE FILE
C ************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE CLOSEIDX_PAS (ST)
C
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'
	INCLUDE 'INCLIB:STANDARD.DEF'
C
C 	LOCAL VARIABLES
C
	INTEGER*4    ST, 
     *               INDEMI,
     *               INDPAS,
     *               FILENAME(5)
        CHARACTER*20 FILENAMECHAR

C
C	SET STATUS TO ZERO
C
	ST = 0
C
C	LOOP FOR ALL THE FILES TO CLOSE
C
	DO  INDPAS = 1, NUMPAS

	    INDEMI  = 1
	    DO WHILE (INDEMI.LE.PAGEMI .AND.
     *                FILEIDXPAS(INDEMI,INDPAS).EMISION.GT.0)
C
C             DEFINE FILE NAME (CASE OF ERROR)
C
              CALL BUILD_FILENAME (SFNAMES(1,TPF), INDPAS,
     *                             FILEIDXPAS(INDEMI,INDPAS).EMISION,
     *                             FILENAMECHAR, FILENAME)
C
C             CLOSE PASSIVE LOTTERY VALIDATION FILE
C
	      CALL FIDX_CLOSE(FILEIDXPAS(INDEMI,INDPAS).FDB,ST)
              IF (ST.NE.0) CALL FILERR(FILENAME, CLOSE_ERROR, ST, 0)
C
C	      CLEAN STRUCTURE
C
	      FILEIDXPAS(INDEMI,INDPAS).EMISION = 0
	      CALL FASTSET(0,FILEIDXPAS(INDEMI,INDPAS).FDB,FDB_IDXLENGTH)
C
C	      GO TO NEXT EMISION IN FILEIDXPAS
C              
	      INDEMI = INDEMI + 1
            ENDDO

	ENDDO

	RETURN
	END
C
C ******************************************************************
C READ PASSIVE FILE
C ******************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE READIDX_PAS (TPASIND, TICKET, EMISION, RECIDX, ST,NEWRECIDX,NEWLOT)
C
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF' !(PLCS)
	
C
C 	ROUTINE PARAMETERS
C
	INTEGER*4    ST                   ! ST=-1 (FILE IS NOT OPEN)
C                                         ! ST= 0 (OK) / ST<-1 (IREAD ST ERROR)
	INTEGER*4    TPASIND,
     *               EMISION,
     *               TICKET
C
	RECORD /STCIDX/	RECIDX
	RECORD /NEWSTCIDX/	NEWRECIDX !(PLCS)		
	LOGICAL*1	     NEWLOT  		
C
C	LOCAL VARIABLES
C
	LOGICAL	     FOUND
	INTEGER*4    INDEMI
C
C	SET STATUS TO ZERO (OK)
C
	ST    = 0
	FOUND = .FALSE.
C
C	LOOP TO LOCATE EMISION IN FILEIDXPAS.EMISION AND GET
C       CORRESPONDING FILEIDXPAS.LUN TO BE ABLE TO ACCESS THE DESIRED FILE
C
	INDEMI   = 1

	DO  WHILE( INDEMI.LE.PAGEMI .AND. (.NOT.FOUND) )

	    IF (FILEIDXPAS(INDEMI,TPASIND).EMISION.EQ.EMISION) THEN
C
C	      READ FILE
C
C (PLCS)
            IF (NEWLOT) THEN
              CALL FIDX_READ(FILEIDXPAS(INDEMI,TPASIND).FDB,TICKET,SIZEOF(NEWRECIDX),
     *                   NEWRECIDX,ST)
            ELSE
              CALL FIDX_READ(FILEIDXPAS(INDEMI,TPASIND).FDB,TICKET,SIZEOF(RECIDX),
     *                   RECIDX,ST)
            
            ENDIF     
C (PLCS)                   
	      FOUND = .TRUE.

	    ELSE

	      INDEMI = INDEMI + 1

	    ENDIF

	ENDDO

	IF (.NOT.FOUND) THEN
           TYPE*,IAM(), 'ARQUIVO IDX DA EMISION ',EMISION, 'INDICE', TPASIND,
     *                  ' DA LOTARIA NACIONAL NAO ESTA ABERTO.'
	   ST = -1
	ELSEIF(ST.NE.0) THEN
           TYPE*,IAM(), 'ERRO NA LEITURA DO ARQUIVO IDX DA EMISION ',EMISION,
     *                  'INDICE', TPASIND, 'BILHETE', TICKET,
     *                  'DA LOTARIA NACIONAL.'
        ENDIF

	RETURN
	END
C
C ******************************************************************
C WRITE PASSIVE FILE
C ******************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE WRITEIDX_PAS (TPASIND, TICKET, EMISION, RECIDX, ST, NEWRECIDX, NEWLOT)
C
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'
	INCLUDE 'INCLIB:PASCOM.DEF' !(PLCS)

C
C 	ROUTINE PARAMETERS
C
	INTEGER*4    ST         ! ST = -1  (FILE IS NOT OPEN)
C                               ! ST =  0  (OK) / ST<-1 (IWRITE ST ERROR)
	INTEGER*4    TPASIND,
     *               EMISION,
     *               TICKET
C
	RECORD /STCIDX/	RECIDX
	RECORD /NEWSTCIDX/	NEWRECIDX
	LOGICAL*1 NEWLOT	
	
C
C	LOCAL VARIABLES
C
	LOGICAL      FOUND 
	INTEGER*4    INDEMI
C
C	SET STATUS TO ZERO (OK)
C
	ST = 0
C
C	TEST FOR VALID VALREC VALUES
C	
	FOUND  = .FALSE.
C
C	LOOP TO LOCATE EMISION IN FILEIDXPAS.EMISION AND GET
C       CORRESPONDING FILEIDXPAS.LUN TO BE ABLE TO ACCESS THE DESIRED FILE
C
	INDEMI = 1

	DO WHILE ( INDEMI.LE.PAGEMI .AND. (.NOT.FOUND) )
	
	   IF (FILEIDXPAS(INDEMI,TPASIND).EMISION.EQ.EMISION) THEN  
C
C	      FOUND EMISION IN FILEIDXPAS
C
	      FOUND = .TRUE.
C
C	      WRITE TO FILE
C
C (PLCS)
            IF (NEWLOT) THEN
              CALL FIDX_WRITE(FILEIDXPAS(INDEMI,TPASIND).FDB,TICKET,SIZEOF(NEWRECIDX),
     *                        NEWRECIDX,ST       )
            ELSE
              CALL FIDX_WRITE(FILEIDXPAS(INDEMI,TPASIND).FDB,TICKET,SIZEOF(RECIDX),
     *                        RECIDX,ST       )            
            ENDIF


	   ELSE
	      INDEMI = INDEMI + 1
           ENDIF

	ENDDO

	IF (.NOT.FOUND) THEN
           TYPE*,IAM(), 'ARQUIVO DA EMISION ',EMISION, 'INDICE', TPASIND,
     *                  ' DA LOTARIA NACIONAL NAO ESTA ABERTO.'
	   ST = -1
	ELSEIF(ST.NE.0) THEN
           TYPE*,IAM(), 'ERRO NA GRAVACAO DO ARQUIVO DA EMISION ',EMISION,
     *                  'INDICE', TPASIND, 'BILHETE', TICKET,
     *                  ' DA LOTARIA NACIONAL.'
        ENDIF

	RETURN
	END

C
C ******************************************************************
C UNLOCK PASSIVE FILE
C ******************************************************************
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE UNLOCKIDX_PAS (TPASIND, EMISION, ST)
C
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:FILESTRUCS.DEF'
C
C 	ROUTINE PARAMETERS
C
	INTEGER*4    ST         ! ST = -1  (FILE IS NOT OPEN)
C                               ! ST =  0  (OK) / ST<-1 (IWRITE ST ERROR)
	INTEGER*4    TPASIND,
     *               EMISION

C
C	LOCAL VARIABLES
C
	LOGICAL      FOUND 
	INTEGER*4    INDEMI
C
C	SET STATUS TO ZERO (OK)
C
	ST = 0
C
C	TEST FOR VALID VALREC VALUES
C	
	FOUND  = .FALSE.
C
C	LOOP TO LOCATE EMISION IN FILEIDXPAS.EMISION AND GET
C       CORRESPONDING FILEIDXPAS.LUN TO BE ABLE TO ACCESS THE DESIRED FILE
C
	INDEMI = 1

	DO WHILE ( INDEMI.LE.PAGEMI .AND. (.NOT.FOUND) )
	
	   IF (FILEIDXPAS(INDEMI,TPASIND).EMISION.EQ.EMISION) THEN  
C
C	      FOUND EMISION IN FILEIDXPAS
C
	      FOUND = .TRUE.
C
C	      UNLOCK FILE
C
	      UNLOCK (FILEIDXPAS(INDEMI,TPASIND).FDB(FDB_IDXLUN),IOSTAT = ST)
	   ELSE
	      INDEMI = INDEMI + 1
           ENDIF

	ENDDO

	RETURN
	END
C
C ******************************************************************
C EXTRACT TICKET STATUS AND ALGORITMO
C ******************************************************************
C

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE GET_TICK_STATUS(VALNUM_STATUS,ALGORITMO,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INTEGER*4 VALNUM_STATUS, ALGORITMO, STATUS

	ALGORITMO = ISHFT(VALNUM_STATUS,-8)
	STATUS    = IAND(VALNUM_STATUS,'000000FF'X)

	RETURN
	END


C
C ******************************************************************
C UPDATE TICKET STATUS
C ******************************************************************
C

C=======OPTIONS /CHECK=NOOVERFLOW
	SUBROUTINE UPD_TICK_STATUS(VALNUM_STATUS,STATUS)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'

	INTEGER*4 VALNUM_STATUS, STATUS

	VALNUM_STATUS = IOR(IAND(VALNUM_STATUS,'FFFFFF00'X),STATUS)	

	RETURN
	END

C ******************************************************************
C UPDATE TICKET ALGORITMO
C ******************************************************************
C

C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE UPD_TICK_ALGORITMO(VALNUM_STATUS,ALGORITMO)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'

        INTEGER*4 VALNUM_STATUS, ALGORITMO

        VALNUM_STATUS = IOR(IAND(VALNUM_STATUS,'000000FF'X),ISHFT(ALGORITMO,8))
        
        RETURN
        END
C
C *************************************
C ROUDING VALUES ACCORDING EURO'S RULE
C *************************************
C
C=======OPTIONS   /CHECK=NOOVERFLOW
	INTEGER*4 FUNCTION PAS_ROUND_VALUE(I8_VALUE)
        IMPLICIT  NONE

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C
C FUNCTION PARAMETERS
C
	INTEGER*8   I8_VALUE
C
C LOCAL VARIABLES
C	
	INTEGER*4   I4_VALUE
C
C
	IF  ( P(PRFACTOR).GT.0 ) THEN
	    I4_VALUE = I8_VALUE/P(PRFACTOR)
	    IF	( MOD(I8_VALUE,P(PRFACTOR)).GE.50 ) I4_VALUE = I4_VALUE + 1
	ELSE
	    I4_VALUE = I8_VALUE
	ENDIF
C
	PAS_ROUND_VALUE = I4_VALUE
C
	RETURN
	END
!===============================================================================
!       FUNCTION TO GET THE ENTRY IN PASCOM BASED ON DRAW NUMBER (EMISION)
!===============================================================================
C=======OPTIONS   /check=nooverflow
        integer*4 function getpagemi(draw,gind)
        implicit none
!       
        include 'inclib:sysparam.def'
        include 'inclib:global.def'
        include 'inclib:pascom.def'
!       
        integer*4 draw
        integer*4 gind
        integer*4 emis
        integer*4 temp
!
        temp = -1        
        if(gind.gt.0.and.gind.le.numpas) then
          temp = 0
          do emis=1,pagemi
            if(pasemis(emis,gind).gt.0.and.pasemis(emis,gind).eq.draw) then
              temp = emis
              exit
            endif
          enddo
        endif
!
        getpagemi = temp
        return
        end
