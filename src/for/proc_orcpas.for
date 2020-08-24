C
c V04 16-SEP-2010 FRP Allow returned tickets to be loaded.
C V03 09-JUL-2010 FRP All Passive games in 1 file
C V02 15-APR-2010 FRP ePassive
C V01 13-FEB-01ANG  INITIAL RELEASE FOR PORTUGAL
C
C PROGRAM PROCESS FILE FROM SCML ORACLE SYSTEM.
C
C ORACLE FILE: ORCPAS.DAT - CONTAINS PASSIVE TICKETS SOLD.
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
C Copyright 1993 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
      PROGRAM PROC_ORCPAS
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 ST,GIND,IND
      INTEGER*4 YEAR,MONTH,DAY
      INTEGER*4 IGEN_DAT,GENDAT_POS
      CHARACTER CGEN_DAT*8
      LOGICAL*1 ISTHERE,WRITE_TCKSTS
C
      COMMON /TCKSTS/ GIND,WRITE_TCKSTS
C
      TYPE*,IAM()
      TYPE*,IAM(),'****************************************************'
      TYPE*,IAM(),'PROCESS PASSIVE TICKETS STATUS (ORCPAS_AAAAMMDD.ASC)'
      TYPE*,IAM(),'*****************************************************'
      TYPE*,IAM()
C
C Check if System Is Up
      CALL CHCKDIS(ST)
      IF(ST .EQ. 0) THEN
        TYPE*,IAM()
        TYPE*,IAM(),'System is ACTIVE!'
        TYPE*,IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
C Ask Generation Date
      CALL INPNUM('Enter File Generation Date (AAAAMMDD): ',IGEN_DAT,1,99999999,ST)
      IF(ST .LT. 0) CALL GSTOP(GEXIT_OPABORT)
C
      CGEN_DAT = ITOC(IGEN_DAT,IND)
C
      GENDAT_POS = 1
      CALL GET_VALUE(GENDAT_POS,4,YEAR,CGEN_DAT)
      IF(YEAR .LT. 2010) THEN
        TYPE*,IAM(),'YEAR LOWER THAN 2010'
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      GENDAT_POS = 5
      CALL GET_VALUE(GENDAT_POS,2,MONTH,CGEN_DAT)
      IF(MONTH .LT. 1 .OR. MONTH .GT. 12) THEN
        TYPE*,IAM(),'MONTH LOWER THAN 1 OR HIGHER THAN 12'
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
      GENDAT_POS = 7
      CALL GET_VALUE(GENDAT_POS,2,DAY,CGEN_DAT)
      IF(DAY .LT. 1 .OR. DAY .GT. 31) THEN
        TYPE*,IAM(),'DAY LOWER THAN 1 OR HIGHER THAN 31'
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
C Inquire for Oracle File
      WRITE(ORCNAM,100) CGEN_DAT
      INQUIRE(FILE=ORCNAM,EXIST=ISTHERE)
      IF(.NOT. ISTHERE) THEN
        TYPE*,IAM(),'ERROR: FILE ',ORCNAM,' NOT FOUND'
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
C Open Oracle File
      CALL OPEN_ORC_FILE
C
C Open Ticket Status Files
      CALL OPNIDX
C
C Open Log File
      WRITE(LOGNAM,200) CGEN_DAT
      CALL OPEN_LOG_FILE
C
C Load Oracle File
      WRITE_TCKSTS = .FALSE.
      CALL LOAD_ORC_FILE
      IF(ORC_ERR_CNT .NE. 0) THEN
        TYPE*,IAM()
        TYPE*,IAM(),ORC_ERR_CNT,' ERRORS FOUND'
        TYPE*,IAM(),'SEE LOG FILE - ',LOGNAM
        TYPE*,IAM(),'FILE WILL NOT BE PROCESSED'
        TYPE*,IAM()
        CALL GSTOP(GEXIT_FATAL)
      ENDIF
C
C Rewind Oracle File
      REWIND(ORCLUN)
C
C Load Oracle File
      WRITE_TCKSTS = .TRUE.
      CALL LOAD_ORC_FILE
C
C Close Oracle File
      CLOSE(ORCLUN)
C
C Close Ticket Status Files
      CALL CLSIDX
C
C Close Log File
      CLOSE(LOGLUN)
C
C Formats
100   FORMAT('SYSX:ORCPAS_',A8,'.ASC')
200   FORMAT('SYSX:ORCPAS_',A8,'.LOG')
C
C End
      CALL GSTOP(GEXIT_SUCCESS)
      END
C
C******************
C SUBROUTINE OPNIDX
C******************
C OPEN TPF FILES
C
C=======OPTIONS    /check=nooverflow
	subroutine opnidx
	implicit none
!
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'
        include 'inclib:pascom.def'
	include 'inclib:pasiosubs.def'
!
        integer*4 gind
        integer*4 emis
!
        record /stpasfdb/  pasfdb(pagemi,numpas)
!
        common /pastruct/ pasfdb        
!
        do gind=1,numpas
          do emis=1,pagemi
            if(passts(emis,gind).eq.gamopn .and.
     *        (passubsts(emis,gind).eq.pdrwwag .or. passubsts(emis,gind).eq.pdrwret)) then
              call pasio_init(pasfdb(emis,gind),gind,pasemis(emis,gind),pasnumtck(emis,gind)-1,
     *                        pasnumser(emis,gind),pasnoffra(emis,gind),cpastpffil(emis,gind))     
              call pasio_openro(pasfdb(emis,gind))  
              if(pasfdb(emis,gind).err.ne.ioe_noerr) then
                call opstxt('Open error for TPF ' // cpastpffil(emis,gind))
                passubsts(emis,gind) = pdrwerr
              endif
            endif
          enddo
        enddo
!
	return
	end
C
C******************
C SUBROUTINE CLSIDX
C******************
C CLOSE TPF FILES
C
C=======OPTIONS    /check=nooverflow
	subroutine clsidx
	implicit none
!
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
        include 'inclib:global.def'
	include 'inclib:pasiosubs.def'
!
        integer*4 gind
        integer*4 emis
!
        record /stpasfdb/  pasfdb(pagemi,numpas)
!
        common /pastruct/ pasfdb        
!
        do gind=1,numpas
          do emis=1,pagemi
            if(pasfdb(emis,gind).lun.ne.0) call pasio_close(pasfdb(emis,gind))        
          enddo
        enddo   	
	return
	end
C
C*************************
C SUBROUTINE LOAD_ORC_FILE
C*************************
C LOAD ORACLE INTERFACE FILE
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE LOAD_ORC_FILE
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:STANDARD.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 ST
C
      ORC_LIN_CNT = 0
      ORC_ERR_CNT = 0
      ORC_ERR_STR = ' '
C
      CALL LOAD_ORC_HEADER(ST)
      IF(ST .EQ. EOF) RETURN
C
      CALL LOAD_ORC_BODY_TRAILER(ST)
      IF(ST .EQ. EOF) RETURN
C
      RETURN
      END
C
C***************************
C SUBROUTINE LOAD_ORC_HEADER
C***************************
C READ ORACLE INTERFACE FILE HEADER RECORD
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE LOAD_ORC_HEADER(ST)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:STANDARD.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 ST
      INTEGER*4 POS
      CHARACTER REC_TYPE*2
C
      CALL READ_ORC_FILE(ST)
      IF(ST .EQ. EOF) THEN
        ORC_ERR_STR = IAM()//' INVALID EOF. SHOULD BE A HEADER RECORD'
        CALL WRITE_LOG_FILE
        RETURN
      ENDIF
C
      POS=1
C
      CALL GET_REC_TYPE(POS,REC_TYPE)
      IF(REC_TYPE .NE. 'HP') THEN
        ORC_ERR_STR = IAM()//' HEADER RECORD INVALID. SHOULD BE TYPE HP'
        CALL WRITE_LOG_FILE
      ENDIF
C
      CALL GET_GEN_DATE(POS)
      IF(ORC_GEN_DAT .NE. ORCNAM(13:20)) THEN
        ORC_ERR_STR = IAM()//' GENDATE NOT EQUAL TO GENDATE IN FILE NAME'
        CALL WRITE_LOG_FILE
      ENDIF
C
      RETURN
      END
C
C*********************************
C SUBROUTINE LOAD_ORC_BODY_TRAILER
C*********************************
C READ ORACLE INTERFACE FILE BODY AND TRAILER RECORDS
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE LOAD_ORC_BODY_TRAILER(ST)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:STANDARD.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
C
      INTEGER*4 ST,POS
      CHARACTER REC_TYPE*2
      LOGICAL*1 IS_TRAILER
C
C Read Oracle File
      ST = 0
      ORC_TOT_REC_01 = 0
      IS_TRAILER = .FALSE.
C
      DO WHILE(ST .EQ. 0)
C
        CALL READ_ORC_FILE(ST)
        IF(ST .EQ. EOF) THEN
          IF(ORC_TOT_REC_01 .EQ. 0) THEN
            IF(IS_TRAILER .EQ. .TRUE. .AND.
     *         ORC_ERR_CNT .EQ. 0) CALL GSTOP(GEXIT_SUCCESS) !Only header&trailer: end
            ORC_ERR_STR = IAM()//' INVALID EOF. SHOULD BE A RECORD 01'
            CALL WRITE_LOG_FILE
          ENDIF
          IF(IS_TRAILER .EQ. .FALSE.) THEN
            ORC_ERR_STR = IAM()//' INVALID EOF. SHOULD BE A TRAILER RECORD'
            CALL WRITE_LOG_FILE
          ENDIF
          RETURN
        ENDIF
C
        POS=1
C
        CALL GET_REC_TYPE(POS,REC_TYPE)
C
        IF(REC_TYPE .EQ. '01') THEN
          ORC_TOT_REC_01 = ORC_TOT_REC_01+1
          CALL GET_REC_01(POS)
C
        ELSEIF(REC_TYPE .EQ. 'TP') THEN
          IS_TRAILER = .TRUE.
          CALL GET_VALUE(POS,8,ORC_TOT_LIN,ORC_REC)
          IF(ORC_TOT_LIN .NE. ORC_LIN_CNT) THEN
            ORC_ERR_STR = IAM()//' TOTRECS IN TRAILER NOT EQUAL TO TOTRECS IN FILE'
            CALL WRITE_LOG_FILE
          ENDIF
C
        ELSE
          ORC_ERR_STR = IAM()//' INVALID RECORD. SHOULD BE TYPE 01 OR TYPE TP'
          CALL WRITE_LOG_FILE
        ENDIF
C
      ENDDO
C
      RETURN
      END
C
C**********************
C SUBROUTINE GET_REC_01
C**********************
C GET ORACLE INTERFACE FILE BODY RECORD '01'
C
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE GET_REC_01(POS)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
      INCLUDE 'INCLIB:CONCOM.DEF'
      INCLUDE 'INCLIB:ORCCOM.DEF'
      INCLUDE 'INCLIB:AGTCOM.DEF'
      include 'inclib:pascom.def'
      include 'inclib:pasiosubs.def'
C
      INTEGER*4 POS
      INTEGER*4 PTYP,PIND,GNUM,GIND,IND
      INTEGER*4 WEEK,YEAR,YIND,EMIS
      INTEGER*4 XSER,XNUM,XFRA,XAGT,TERN
      INTEGER*4 MXSER,MXNUM,MXFRA,MXAGT
      CHARACTER CYEAR*4
      LOGICAL*1 WRITE_TCKSTS
C
      COMMON /TCKSTS/ GIND,WRITE_TCKSTS
C
      record /stpasfdb/  pasfdb(pagemi,numpas)
      record /stpasrec/  pasrec 
!
      common /pastruct/ pasfdb        
C
C Passive Lottery Type
      CALL GET_VALUE(POS,1,PTYP,ORC_REC)
      IF(PTYP .NE. 0 .AND. PTYP .NE. 5) THEN
        ORC_ERR_STR = IAM()//' INVALID PASSIVE TYPE. SHOULD BE 0 OR 5'
        CALL WRITE_LOG_FILE
      ENDIF
C
      IF(PTYP .EQ. 0) PIND = PSBCLA
      IF(PTYP .EQ. 5) PIND = PSBPOP
C
      GNUM  = GTNTAB(TPAS,PIND)
      IF(GNUM .LE. 0) THEN
        ORC_ERR_STR = IAM()//' GAME NOT ACTIVE FOR PASSIVE TYPE '//ITOC(PIND,IND)
        CALL WRITE_LOG_FILE
      ENDIF
      GIND = PIND
C
C Extraction
      CALL GET_VALUE(POS,2,WEEK,ORC_REC)
      IF(WEEK .LT. 1 .OR. WEEK .GT. PMAXWEK) THEN
        ORC_ERR_STR = IAM()//' INVALID EXTRACTION WEEK. SHOULD BE UP TO '//ITOC(PMAXWEK,IND)
        CALL WRITE_LOG_FILE
      ENDIF
C
      CALL GET_VALUE(POS,4,YEAR,ORC_REC)
      IF(YEAR .LT. 2010) THEN
        ORC_ERR_STR = IAM()//' INVALID EXTRACTION YEAR. SHOULD START FROM 2010'
        CALL WRITE_LOG_FILE
      ENDIF
C
      CYEAR = ITOC(YEAR,IND)
      YIND = CTOI(CYEAR(4:4),IND)
C
      EMIS = PASEXTDRW(WEEK,YIND,GIND)
      IF(EMIS .LE. 0) THEN
        ORC_ERR_STR = IAM()//' INVALID WEEK-YEAR. EXTRACTION NOT IN MEMORY'
        CALL WRITE_LOG_FILE
      ENDIF
C
      IF(PASSTS(EMIS,GIND) .NE. GAMOPN) THEN
        ORC_ERR_STR = IAM()//' INVALID EXTRACTION STATUS. SHOULD BE OPEN'
        CALL WRITE_LOG_FILE
      ENDIF
C
      IF(PASSUBSTS(EMIS,GIND) .NE. PDRWWAG .AND.
     *   PASSUBSTS(EMIS,GIND) .NE. PDRWRET) THEN
        ORC_ERR_STR = IAM()//' INVALID EXTRACTION SUBSTATUS. SHOULD BE SAL OR RET'
        CALL WRITE_LOG_FILE
      ENDIF
C
C Serie
      CALL GET_VALUE(POS,2,XSER,ORC_REC)
C
      MXSER = PASNUMSER(EMIS,GIND)
      IF(XSER .LE. 0 .OR. XSER .GT. MXSER) THEN
        ORC_ERR_STR = IAM()//' INVALID SERIE NUMBER. SHOULD BE UP TO '//ITOC(MXSER,IND)
        CALL WRITE_LOG_FILE
      ENDIF
C
C Number
      CALL GET_VALUE(POS,5,XNUM,ORC_REC)
C
      MXNUM = PASNUMTCK(EMIS,GIND)-1
      IF(XNUM .LT. 0 .OR. XNUM .GT. MXNUM) THEN
        ORC_ERR_STR = IAM()//' INVALID BILLET NUMBER. SHOULD BE UP TO '//ITOC(MXNUM,IND)
        CALL WRITE_LOG_FILE
      ENDIF
C
C Fraction
      CALL GET_VALUE(POS,2,XFRA,ORC_REC)
C
      MXFRA = PASNOFFRA(EMIS,GIND)
      IF(XFRA .LE. 0 .OR. XFRA .GT. MXFRA) THEN
        ORC_ERR_STR = IAM()//' INVALID FRACTION NUMBER. SHOULD BE UP TO '//ITOC(MXFRA,IND)
        CALL WRITE_LOG_FILE
      ENDIF
C
C Agent
      CALL GET_VALUE(POS,7,XAGT,ORC_REC)
C
      MXAGT = 9999999
      IF(XAGT .LE. 0 .OR. XAGT .GT. MXAGT) THEN
        ORC_ERR_STR = IAM()//' INVALID AGENT NUMBER. SHOULD BE UP TO '//ITOC(MXAGT,IND)
        CALL WRITE_LOG_FILE
      ENDIF
C
      TERN = 0
      DO IND = 1,NUMAGT
        IF(XAGT .EQ. AGTTAB(AGTNUM,IND)) THEN
          TERN = IND
          GOTO 1000
        ENDIF
      ENDDO
C
      IF(TERN .LE. 0) THEN
        ORC_ERR_STR = IAM()//' INVALID AGENT NUMBER. TERMINAL NUMBER NOT FOUND '
        CALL WRITE_LOG_FILE
      ENDIF
C
1000  CONTINUE
C
C Read TPF File
      CALL PASIO_READ(PASFDB(EMIS,GIND),XNUM,XSER,XFRA,PASREC)
      if(pasfdb(emis,gind).err .ne. ioe_noerr) then
        call opstxt('Read error for TPF ' // cpastpffil(emis,gind))
        passubsts(emis,gind) = pdrwerr
      endif
C
      IF(PASREC.STAT .NE. PBILOFF .AND.
     *   PASREC.STAT .NE. PBILCOF) THEN
        ORC_ERR_STR = IAM()//' INVALID TCKT STS. SHOULD BE OFFSALE OR RETURNED'
        CALL WRITE_LOG_FILE
      ENDIF
C
C Write TPF File
      IF(WRITE_TCKSTS .EQ. .TRUE. .AND. ORC_ERR_CNT .EQ. 0 .AND.
     *   passubsts(emis,gind) .ne. pdrwerr) THEN
        PASREC.STAT = PBILSOF
        PASREC.AGT = TERN
C
        CALL PASIO_WRITE(PASFDB(EMIS,GIND),XNUM,XSER,XFRA,PASREC)
        if(pasfdb(emis,gind).err .ne. ioe_noerr) then
          call opstxt('Write error for TPF ' // cpastpffil(emis,gind))
          passubsts(emis,gind) = pdrwerr
        endif
      ENDIF
C
      RETURN
      END
