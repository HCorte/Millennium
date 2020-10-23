      PROGRAM COMOLM
      IMPLICIT NONE

      INCLUDE 'INCLIB:SYSPARAM.DEF'
!      INCLUDE 'INCLIB:SYSEXTRN.DEF'
      INCLUDE 'INCLIB:GLOBAL.DEF'
!      INCLUDE 'INCLIB:CONCOM.DEF'
!      INCLUDE 'INCLIB:PROCOM.DEF'
!      INCLUDE 'INCLIB:AGTCOM.DEF'
!      INCLUDE 'INCLIB:TASKID.DEF'
      INCLUDE 'INCLIB:PRMLOG.DEF'
      INCLUDE 'INCLIB:CHKSUMCM.DEF'
!      INCLUDE 'INCLIB:QUECOM.DEF'
!      INCLUDE 'INCLIB:APUCOM.DEF'
!      INCLUDE 'INCLIB:IGSCON.DEF'
      INCLUDE '(LIB$ROUTINES)' 

      INTEGER*4  MESS(EDLEN)       ! EDLEN is defined GLOBAL and its a constant INTEGER*4  EDLEN PARAMETER (EDLEN=20) !MESSAGE DATA LENGTH 
      INTEGER*4  TASK              !
      INTEGER*4  BUF               !
      INTEGER*4  STATUS,ST
      INTEGER*4  MESSERIAL
      LOGICAL    CONOLM, FIRSTRUN

!      INTEGER*4  TEOLM !temporary this variable will come from global.def
!      TEOLM = 9 !temporary this value will come from global.def

      character*8 DATEI
      character*10 TIMEI
      character*20 LOGDATEI!could be 18 caracters with
      character*80 PATHI    

C
C     Begin Constants defined in INCLIB:IGSCON.DEF
C
      INTEGER*4  PAMS__NOMOREMSG
      PARAMETER (PAMS__NOMOREMSG      = 139756347)
      INTEGER*4  PAMS__SUCCESS
      PARAMETER (PAMS__SUCCESS        = 1)
      INTEGER*4  PAMS__DECLARED
      PARAMETER (PAMS__DECLARED       = 139761972)
      INTEGER*4  PAMS__NOLINK
      PARAMETER (PAMS__NOLINK         = 139759394)
      INTEGER*4  PAMS__LINK_DOWN
      PARAMETER (PAMS__LINK_DOWN      = 139759266)
      INTEGER*4  PAMS__NOTACTIVE
      PARAMETER (PAMS__NOTACTIVE      = 139759474)
      INTEGER*4  PAMS__NETLINKLOST
      PARAMETER (PAMS__NETLINKLOST    = 139759730)
      INTEGER*4  PAMS__STOPPED
      PARAMETER (PAMS__STOPPED        = 139759962)
      INTEGER*4  PAMS__NETNOLINK
      PARAMETER (PAMS__NETNOLINK      = 139762140)
      INTEGER*4  PAMS__FATAL
      PARAMETER (PAMS__FATAL          = 139762156)
      INTEGER*4  PAMS__NETERROR 
      PARAMETER (PAMS__NETERROR       = 139759722)
      INTEGER*4  PAMS__STUB
      PARAMETER (PAMS__STUB           = 139757248)
      INTEGER*4 PAMS__TIMEOUT
      PARAMETER (PAMS__TIMEOUT        = 139757256)
C
C     End Constants defined in INCLIB:IGSCON.DEF
C
            
      PATHI = 'DKD10:[DMIL.WRK.HMC.EXAMPLES.LOGFILES]logs.dat' 

      CALL OPSTXT(' Copyright 2014 SCML. All rights reserved. ') !um print para command line
      CALL SNIF_AND_WRKSET !SIZE OF THE WORKING SET;;;SETS THE WORKING SET TO THE REQUIRED VALUE Alocar memoria necessaria para o programa

      !PARAMETER (PSV=28)        !PASSIVE VALIDATION TASK # (PASVAL)
      !PARAMETER (PST=29)        !PASSIVE PROCESSING TASK # (PASPRO)
      !TASK    = 28 !OLM !it will be a constant defined in taskid.def thats PARAMETER (OLM=28) but still in study if its one of this (PSV=28) or (PST=29) or another task id
      !CALL BUILD_MSG(MESS,1, TASK) !queue interna do openvms/millenium

      MESSERIAL = 0 !comolm message number
      CONOLM = .FALSE.
      CALL OPSTXT(' ******************* COMOLM ******************* ')

C----+------------------------------------------------------------------
C    | Entry Point: wait for something to do
C----+------------------------------------------------------------------
10      CONTINUE

C----+------------------------------------------------------------------
C    | If there is no connection to OLM then try to connect to MessageQ
C----+------------------------------------------------------------------
        IF (CONOLM .EQ. .FALSE.) THEN !or could be (.NOT. CONIGS) thats would be true when CONIGS IS FALSE that it not connected to IGS
            CALL MESSQ_ATTACH(%REF(ST))
            IF (ST .NE. PAMS__SUCCESS) THEN !if not attach successfully then exit 
                  CALL MESSQ_EXIT(%REF(ST))
                  !P(OLMCONF) = 0 !so it indicates that its not connected to the MessageQ
                  !CALL OPSTXT('ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!')
                  !GOTO 10           ! ST = PAMS__SUCCESS : Attach successful.

C----+------------------------------------------------------------------
C    | Begin Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------   
                  CALL date_and_time(DATEI,TIMEI) 

                  LOGDATEI = DATEI(7:8)//'/'//DATEI(5:6)//'/'//DATEI(1:4)
     &            //'  '//TIMEI(1:2)//':'//TIMEI(3:4)//':'//TIMEI(5:6)
             
                  open(UNIT=1234, FILE=PATHI, ACCESS='append', STATUS='old')
                  write(1234,*) LOGDATEI//' - ERROR!!!! CAN NOT ATTACH TO MESSAGEQ!!!!'
                  close(1234)   
C----+------------------------------------------------------------------
C    | End Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------                                     

            ENDIF

C----+------------------------------------------------------------------
C    | Send message - OLM connect
C----+------------------------------------------------------------------
            !global.def -> ERRLOG MESAGE TYPE DEFINITIONS new one !PARAMETER (TEOLM=9)   !OLM - Olimpo Messages
            
            !CALL BUILD_MSG(MESS,2, TEOLM) !PARAMETER (TEOLM=9)   !OLM - Olimpo Messages
            !CALL BUILD_MSG(MESS,3, 1)
            !CALL QUEMES(MESS) !writes the message into the queue/buffer of 768 bytes of body size with header of 64 bytes generated/obtain from the list of buffers/queues called FREEQ
            
            CONOLM = .TRUE. !the flag is true since its connected
            !P(IGSCONF) = 1 !this position in P indicates that its connected and configurated IGS
            
        ENDIF

C----+------------------------------------------------------------------
C    | Get buffer number from queue top.
C    | If there are no wagers queued, then go back to wait state.
C----+------------------------------------------------------------------        
        CALL XWAIT(250,1,ST)      ! Wait for 250 milliseconds

C----+------------------------------------------------------------------
C    | Call GetFromIGS function is used to get from IGS all response
C    | messages -- only if status is 'NO MORE MESSAGES'
C----+------------------------------------------------------------------
      ST = PAMS__NOMOREMSG !PARAMETER (PAMS__NOMOREMSG      = 139756347)
      IF (CONOLM) THEN !P(IGSCONF) .NE. 0 !that is its connected to MessageQ
            CALL GETFROMOLM(ST,MESSERIAL) !then reads message from MessageQ from IGS
            !READS ONE MESSAGE AND NOW DETACH AND EXITS
            !CALL MESSQ_EXIT(%REF(ST))
      ENDIF

      END      
C----+-----------------------------------------------------------------
C    | SUBROUTINE GETFROMOLM
C    |    This subroutine receives messages from OLM
C    +-----------------------------------------------------------------
C    | OUTPUT PARAMETERS:
C    |    ST         Process status
C    |    MESSERIAL  Received messages' sequence number
C----+-----------------------------------------------------------------
C=======OPTIONS /CHECK=NOOVERFLOW
      SUBROUTINE GETFROMOLM(ST,MESSERIAL)
      IMPLICIT NONE
C**************************************************
            INCLUDE 'INCLIB:SYSPARAM.DEF'
!            INCLUDE 'INCLIB:SYSEXTRN.DEF'
            INCLUDE 'INCLIB:GLOBAL.DEF'
!            INCLUDE 'INCLIB:CONCOM.DEF'
!            INCLUDE 'INCLIB:PROCOM.DEF'
!            INCLUDE 'INCLIB:AGTCOM.DEF'
!            INCLUDE 'INCLIB:TASKID.DEF'
            INCLUDE 'INCLIB:PRMLOG.DEF'
            INCLUDE 'INCLIB:CHKSUMCM.DEF'
!            INCLUDE 'INCLIB:QUECOM.DEF'
!            INCLUDE 'INCLIB:DESTRA.DEF'
!            INCLUDE 'INCLIB:APUCOM.DEF'
!            INCLUDE 'INCLIB:EURCON.DEF'
!            INCLUDE 'INCLIB:DATBUF.DEF'
            !INCLUDE 'INCLIB:IGSDEBUG.DEF'

            INTEGER*4  MESS(EDLEN)
            INTEGER*4 I4TEMP
            INTEGER*2 I2TEMP(2)
            BYTE      I1TEMP(4)
            EQUIVALENCE (I4TEMP,I2TEMP,I1TEMP)

            INTEGER*4 ST,STATUS,MTYPE,MSUBTYPE
            INTEGER*4 XRFNUM,AGENTNR   !AGENTNR may not be needed here
            
            character*34 XRFNUMSTR, AGENTNRSTR, MTYPESTR, MSUBTYPESTR

            COMMON /FROM_OLM/ MESS_FROM_OLM, MESS_FROM_LEN
            BYTE MESS_FROM_OLM(1024) !so the message is 1024 bytes of length
            INTEGER*4 MESS_FROM_LEN
            INTEGER*4 MESSERIAL

C----+------------------------------------------------------------------
C    | Begin Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------
            character*8 DATE
            character*10 TIME
            character*20 LOGDATE!could be 18 caracters with 
            character*80 PATH  
            
            INTEGER*4  PAMS__SUCCESS
            PARAMETER (PAMS__SUCCESS        = 1)            
            
            PATH = 'DKD10:[DMIL.WRK.HMC.EXAMPLES.LOGFILES]logs.dat'            
C----+------------------------------------------------------------------
C    | End Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------

            !MESS(1) = 28!OLM ! OLM=28 !its a constant defined in taskid.def thats PARAMETER (IGC=42) 
            ST = 0

20          CONTINUE

            CALL MESSQ_GET(%REF(STATUS)) !this is the function in MessageQ library in c not the one messageget.for that one is ignored  

            IF (STATUS .EQ. PAMS__SUCCESS) THEN   
                  
C----+------------------------------------------------------------------
C    | Get AGENTNR from message (bytes 1-4)
C----+------------------------------------------------------------------
                  I1TEMP(4) = ZEXT (MESS_FROM_OLM(1))
                  I1TEMP(3) = ZEXT (MESS_FROM_OLM(2))
                  I1TEMP(2) = ZEXT (MESS_FROM_OLM(3))
                  I1TEMP(1) = ZEXT (MESS_FROM_OLM(4))  
                  AGENTNR = I4TEMP    
                  
                  write(AGENTNRSTR,30) AGENTNR !convert the integer value to string using internal file log-> (write)
30                format(I4)

C----+------------------------------------------------------------------
C    | Get XREFNUM from message (bytes 5-8)
C----+------------------------------------------------------------------
                  I1TEMP(1) = ZEXT (MESS_FROM_OLM(8))
                  I1TEMP(2) = ZEXT (MESS_FROM_OLM(7))
                  I1TEMP(3) = ZEXT (MESS_FROM_OLM(6))
                  I1TEMP(4) = ZEXT (MESS_FROM_OLM(5))
                  XRFNUM = I4TEMP 

                  write(XRFNUMSTR,40) XRFNUM !convert the integer value to string using internal file log-> (write)
40                format(I4)                  
C----+------------------------------------------------------------------
C    | Clear variable memory
C----+------------------------------------------------------------------
                  I4TEMP = 0 !validar se não fazer este reset influencia os proximos valores...

C----+------------------------------------------------------------------
C    | check for unsolicited messages from OLM (check type and subtype)
C----+------------------------------------------------------------------                  
                  MTYPE = ZEXT(MESS_FROM_OLM(16))
                  MSUBTYPE = IAND(  MOD(MTYPE, 16), '0F'X)
                  MTYPE    = IAND(ISHFT(MTYPE, -4), '0F'X)

                  write(MTYPESTR,50) MTYPE !convert the integer value to string using internal file log-> (write)
50                format(I4)     

                  write(MSUBTYPESTR,60) MSUBTYPE !convert the integer value to string using internal file log-> (write)
60                format(I4) 

                  IF(.TRUE.) THEN!IGSDEBUG(IA_COMIGS)
                        !CALL OPS('517:GETFROMIGS:XRFNUM   ',XRFNUM,XRFNUM)

C----+------------------------------------------------------------------
C    | Begin Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------
                        CALL date_and_time(DATE,TIME) 

                          LOGDATE = DATE(7:8)//'/'//DATE(5:6)//'/'//DATE(1:4)
     &                   //'  '//TIME(1:2)//':'//TIME(3:4)//':'//TIME(5:6)

                        open(UNIT=123456, FILE=PATH, ACCESS='append', STATUS='old')
                        write(123456,*) LOGDATE//' - XRFNUM:  '//XRFNUMSTR//' AGENTNR: '//AGENTNRSTR//' MSUBTYPE: '//MSUBTYPESTR
     &                  //' MTYPE: '//MTYPESTR 
                        close(123456)   
                        
                        ST = STATUS !confirm if its this value thats returned

                        CALL MESSQ_EXIT(%REF(ST))
C----+------------------------------------------------------------------
C    | End Extra for Initial testing (remove before going to production)
C----+------------------------------------------------------------------

                        !CALL OPS('517:GETFROMIGS:MESSERIAL',MESSERIAL,MESSERIAL)
                  ENDIF
C----+--------------                  
            ENDIF      

      RETURN
      END