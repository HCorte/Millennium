C
C SUBROUTINE OLMSNP
C
C VIS_OLMSNP.FOR
C
C V01 26-OCT-2021 SCML New Terminals Project
C
C OLM SYSTEM CONTROL SNAPSHOT
C
C
C ERROR MESSAGE #  DESCRIPTION
C ---------------  -----------------------------------------------------
C       5          PASSWORD ERROR WHILE TRYING TO CHANGE EM TIMEOUT
C       6          PASSWORD ERROR WHILE TRYING TO CHANGE EM FIN TIMEOUT
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2021 SCML Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE OLMSNP(CLINE)
        IMPLICIT NONE
C
        INCLUDE '(LIB$ROUTINES)' 
        INCLUDE '($SSDEF)'     ! SS$_ symbols/definitions              
        INCLUDE '($JPIDEF)' 
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
C
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
        INCLUDE 'INCLIB:VISCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PROCOM.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:ENCCOM.DEF'
        INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:X2XQUE.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:OLMCOM.DEF'           
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       INPUT ARGUMENTS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        INTEGER*4  CLINE(20)
C        INTEGER*4  EGAM    !EXTERNAL GAME NUMBER (IN OLIMPO SYSTEM)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       LOCAL VARIABLES
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
        INTEGER*4  GSOLM    !GAME SUPRESS (LI, Totoloto and Totobola from Olimpo)
        INTEGER*4  GSGRR    !GAME SUPRESS GAME RESULTS REPORT
        INTEGER*4  GSICA    !GAME SUPRESS INTERNAL CANCELLATION

        INTEGER*4  ST
        INTEGER*4  KEYNUM        

        INTEGER*4 OLMLST(3),OLMTTL,INPLST(3)   
        INTEGER*4 DECLST(3),WAGLST(3),CANLST(3),VALLST(3)  
        INTEGER*4 INILST(3),CRSLST(3),INOLST(3)
        INTEGER*4 SPELST(3),DISLST(3)  
        INTEGER*4  BUF(CDLEN)     
        INTEGER*4  MESS(EDLEN)  
        CHARACTER*20  PASPAS        
        CHARACTER*7  DEFTPASS                                                   !DEFAULT PASSWORD
        CHARACTER*7  PASSENT                                                    !ENTERED PASSWORD
        CHARACTER*134 MILLCON
        INTEGER*4  ISTAT
        INTEGER*4  STATUS
        INTEGER*4  VALUE
        INTEGER*4  POS
        INTEGER*4  pidadr,STATLOG,STPROC,POS_AUX,STATDAY
        INTEGER*4  CURRENT_DAYS,FILE_DAYS
        INTEGER*8  FILE_DAYS_AUX
        INTEGER*1  FILE_DAYS_OLD
        INTEGER*4  FILES_DAY,FILESTAT,FILE_CONTEXT /0/,FILE_SCRIPT_CONT /0/
        CHARACTER*50 FILES_NAMES_PATH 
        CHARACTER*25 FILE_NAME
        CHARACTER*2 FILE_DAY
        CHARACTER*3 FILE_MONTH
        CHARACTER*4 FILE_YEAR
        CHARACTER*20 FILE_SYS_DATE        
        INTEGER*4 STATUS_ELEM, STATUS_BINTIM, STATUS_DELETE
        INTEGER*4  SYS$CREPRC,STR$ELEMENT,SYS$BINTIM           
C        INTEGER*4  STR$ELEMENT,STR$TRIM,SIZE_AUX
        CHARACTER*40 MESSCON
        LOGICAL    MILL_CON_STATUS /0/, REGLOG /1/, REGLOGER /1/  
        LOGICAL    PURGE_LOG_VALIDATION /0/      
        CHARACTER*20 PROCESS_ID,OUTPUT_PATH_AUX,ERROR_PATH_AUX
        CHARACTER*34 OUTPUT_PATH,ERROR_PATH,SCRIPT_PATH
        CHARACTER*25 HOST
        CHARACTER*40 ERR_MSG
        CHARACTER*13 IP_ADDRESS
        CHARACTER*11 TODAY_DATE
        INTEGER*1  MAX_WRITES_LOG /0/, MAX_WRITES_ERR /0/
        
C
        INTEGER*4  MAXPRM
        PARAMETER (MAXPRM=16)
C
        REAL*8       K(MAXPRM)                                                  !SNAPSHOT PARAMETER DESCRIPTION
C
        EQUIVALENCE(PASPAS,PASSENT)
C
        DATA   K/'COMOLM  ','OLMCOn  ','INPUT   ','OUTPUT  ',
     *           'WAGPRO  ','CANPRO  ','VALPRO  ',
     *           'INSPRO  ','CRSPRO  ','INSOUT  ',
     *           'OLMTMO  ','FINTMO  ','SUPLOG  ','SUPELOG',
     *           'SPESRV  ','DISPAT  '/
        DATA DEFTPASS/'SUPORTE'/
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       BEGIN PROCESS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
        MESS(1) = 63 
        MESS(2) = TEOLM
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C EURSNP INPUT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC  
        VALUE = 0
        !TEMP  = 0 parece que esta variavel n?o est? a ser usada no euromil
        POS   = 1      
        CALL KEY(CLINE,K,MAXPRM,POS,KEYNUM)
C
        IF(POS.GT.40) GOTO 300                                                  !NO INPUT
        IF(KEYNUM.EQ.0)GOTO 200                                                 !INPUT ERROR        
C
        CALL NUMB(CLINE,POS,VALUE)                                              !GET VALUE
        IF(VALUE.LT.0)  GOTO 205    
        
C
C CLEAR COMMAND MESSAGE BUFFER
C
2       CONTINUE
        CALL FASTSET(0,BUF,CDLEN)
        GOTO(200,506,200,200,200,200,200,200,200,200,200,200
     *  ,507,509,200,200) KEYNUM   
        
        GOTO 200          
C
C force some changes to enter pass first the allow those changes
C
506     CONTINUE
C        CALL PASSWORD(5,PASPAS)
C        IF (PASSENT .NE. DEFTPASS) THEN
C          MESS(3) = 5
C          CALL QUEMES(MESS)
C          GOTO 206
C        ENDIF
        IF(VALUE.LT.0.OR.VALUE.GT.1) GOTO 210
        BUF(1)=OLMCONF
        BUF(2)=VALUE
        BUF(3)=TCPAR
        GOTO 250        
C       will change the value of the connection to Olimpo directly on system parameter variable  
C        P(OLMCONF)=VALUE
C        GOTO 300
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C
C Register the output logs of Script run in process
C
507     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.1) GOTO 211
        REGLOG = VALUE       
        GOTO 300
C
C Register the error logs of Script run in process
C
509     CONTINUE
        IF(VALUE.LT.0.OR.VALUE.GT.1) GOTO 211
        REGLOGER = VALUE       
        GOTO 300

C
C INPUT ERROR
C
200     CONTINUE
        WRITE(CLIN23,800)
800     FORMAT('Input error')

        RETURN   
C
C VALUE ERROR
C
205     CONTINUE
        WRITE(CLIN23,801)
801     FORMAT('Value error')          

        RETURN
C
C INVALID PASSWORD
C
206     CONTINUE
        WRITE(CLIN23,802)
802     FORMAT('Invalid Password')

        RETURN

210     CONTINUE
        WRITE(CLIN23,805)
805     FORMAT('Invalid value (0-Disconnect 1-Connect /to Olimpo)')

        RETURN

211     CONTINUE
        WRITE(CLIN23,806)
806     FORMAT('Invalid value (0-No Log 1-Log)')

        RETURN        
C
C QUEUE COMMAND BUFFER TO SYSTEM INPUT INPUT QUEUE
C     
250     CONTINUE
        BUF(6)=IDNUM
        CALL VISCMD(BUF,ST)
        CALL XWAIT(2,1,ST)   
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       BUILD OLMSNP SCREEN IMAGE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
300     CONTINUE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       GET BUFFER UTILIZATION INFORMATION 
C       To do Later(press BUF followed by the number of buffs used to show)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
!        CALL QIMAGE(QUETAB(1,OLM),OLMLST,3) !from reuse paspro application queue
C        CALL OPSTXT('Starting')
        CALL QIMAGE(COMOLMQUE(1),OLMLST,3) !use the new queue for the app queue
        CALL QIMAGE(INQUE,INPLST,3)
        CALL NQIMAGE(GAME_OUTQUE,DECLST,3)
        CALL QIMAGE(QUETAB(1,WAG),WAGLST,3)
        CALL QIMAGE(QUETAB(1,CAN),CANLST,3)
        CALL QIMAGE(QUETAB(1,VAL),VALLST,3)
        CALL QIMAGE(QUETAB(1,INI),INILST,3)
        CALL QIMAGE(QUETAB(1,CRS),CRSLST,3)
        CALL QIMAGE(QUETAB(1,INO),INOLST,3)  
        CALL QIMAGE(QUETAB(1,SPE),SPELST,3)  
        CALL QIMAGE(QUETAB(1,DIS),DISLST,3)      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
C        What MessageQ MILL is connected to (Primary or Failover)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C        CALL OPSTXT('Middle')
        IF(MILL_CON_STATUS .EQ. 0) THEN
          IF(REGLOG .EQ. 0) THEN
C                OUTPUT_PATH = "GXOLM:MILLCON.LOG"
                IF(PURGE_LOG_VALIDATION .EQ. 0) THEN
                        STATDAY = LIB$DAY(CURRENT_DAYS)
C                       FILES_DAY 
C                        CALL OPS("CURRENT_DAYS: ",CURRENT_DAYS,0)
C                       FILE_CONTEXT = 0

c                       FILESTAT = LIB$FIND_FILE("GXOLM:MILLCON*.LOG;1",FILES_NAMES_PATH,FILE_CONTEXT)
C                        FILESTAT = 65537
                        DO WHILE(LIB$FIND_FILE("GXOLM:MILLCON*.LOG;1",FILES_NAMES_PATH,FILE_CONTEXT) .EQ. 65537 )
C                           FILESTAT = LIB$FIND_FILE("GXOLM:MILLCON*.LOG;1",FILES_NAMES_PATH,FILE_CONTEXT)    
C                           CALL OPS("MAX_WRITES_LOG",MAX_WRITES_LOG,MAX_WRITES_LOG)
C                           CALL OPS("FILESTAT",FILESTAT,FILESTAT)
C                           CALL OPSTXT("NAME PATH: "//FILES_NAMES_PATH)
                           STATUS_ELEM = STR$ELEMENT(FILE_DAY,1,"-",FILES_NAMES_PATH)
                           STATUS_ELEM = STR$ELEMENT(FILE_MONTH,2,"-",FILES_NAMES_PATH)
                           STATUS_ELEM = STR$ELEMENT(FILE_YEAR,3,"-",FILES_NAMES_PATH)
C                           CALL OPSTXT("FILE DATE: "//FILE_DAY//"-"//FILE_MONTH//"-"//FILE_YEAR)
                           FILE_SYS_DATE = FILE_DAY//"-"//FILE_MONTH//"-"//FILE_YEAR
                           STATUS_BINTIM = SYS$BINTIM(FILE_SYS_DATE,FILE_DAYS_AUX)
C                           CALL OPS("FILE_DAYS_AUX: ",FILE_DAYS_AUX,FILE_DAYS_AUX)
C                           CALL OPSTXT("FILE SYS DATE: "//FILE_SYS_DATE)
                           STATDAY = LIB$DAY(FILE_DAYS,FILE_DAYS_AUX)
C                           CALL OPS('FILE DAYS STATUS: ',STATDAY,0)
C                           CALL OPS("FILE DAYS: ",FILE_DAYS,0)
                           FILE_DAYS_OLD = CURRENT_DAYS-FILE_DAYS 
C                           CALL OPS("FILE DAYS OLD: ",FILE_DAYS_OLD,0)  
                           IF(FILE_DAYS_OLD .GT. 7) THEN
C                             CALL OPSTXT('REMOVE FILE: '//FILES_NAMES_PATH)
                             IF(LIB$MATCHC("MILLCON",FILES_NAMES_PATH)) THEN
C                                CALL OPSTXT('...VALID LOG FILE TO REMOVE...')
                                STATUS_ELEM = STR$ELEMENT(FILE_NAME,1,"]",FILES_NAMES_PATH)
C                                CALL OPSTXT('...VALID LOG FILE TO REMOVE ('//FILE_NAME//')...')
                                STATUS_DELETE = LIB$DELETE_FILE('GXOLM:'//FILE_NAME)
                                IF(STATUS_DELETE .EQ. SS$_NORMAL) THEN
                                  CALL OPSTXT('FILED REMOVED: '//FILES_NAMES_PATH)    
                                ENDIF
                             ENDIF
                           ENDIF  
                        ENDDO 
                        PURGE_LOG_VALIDATION = 1
                ENDIF       
                
C               AFTER 7 DAYS THE OLD LOGS ARE PURGE/DELETED FROM SYSTEM                


C                CALL OPS("STATLOG DATE TIME: ",STATLOG,STATLOG)
C                CALL OPSTXT(TODAY_DATE)
                STATLOG = LIB$DATE_TIME(TODAY_DATE)
                IF(STATLOG .NE. SS$_NORMAL .AND. STATLOG .NE. 1409041) THEN
                  TODAY_DATE = ""     
                ENDIF        
                STATLOG = lib$get_logical("MSQLOG",OUTPUT_PATH_AUX)
                OUTPUT_PATH = OUTPUT_PATH_AUX//"-"//TODAY_DATE//".LOG;1"
C                STR$ELEMENT(MESSCON,0,",",MILLCON)
C                STR$ELEMENT(MESSCON,0,",",MILLCON)
                IF(STATLOG .NE. SS$_NORMAL) THEN
                  OUTPUT_PATH = "GXOLM:MILLCON-"//TODAY_DATE//".LOG;1"        
                ENDIF
                MAX_WRITES_LOG = MAX_WRITES_LOG + 1
                IF(MAX_WRITES_LOG .EQ. 6) THEN 
                    REGLOG = 1   
                    MAX_WRITES_LOG = 0 
                    FILE_CONTEXT = 0
                ENDIF        
          ELSE
                OUTPUT_PATH = "NLA0:" !its null device
          ENDIF
          IF(REGLOGER .EQ. 0) THEN
                STATLOG = LIB$DATE_TIME(TODAY_DATE)
                STATLOG = lib$get_logical("MERLOG",ERROR_PATH_AUX) 
                ERROR_PATH = ERROR_PATH_AUX//"-"//TODAY_DATE//".LOG;1"
                IF(STATLOG .NE. SS$_NORMAL) THEN
                  ERROR_PATH = "GXOLM:ERR_MILLCON-"//TODAY_DATE//".LOG;1"       
                ENDIF   
                MAX_WRITES_ERR = MAX_WRITES_ERR + 1
                IF(MAX_WRITES_ERR .EQ. 6) THEN 
                    REGLOG = 1    
                    MAX_WRITES_ERR = 0
                ENDIF                                            
          ELSE
                ERROR_PATH = "NLA0:" !its null device
          ENDIF
CCCCCCCCCC Confirm that the script exists CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C                LIB$FIND_FILE("GXOLM:MESSAGEQCONNECTION.COM",,FILE_SCRIPT_CONT
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                        

          STATLOG = lib$get_logical("OLM_MESSQ_ATTACH_SCRIPT",SCRIPT_PATH)
          IF(STATLOG .NE. SS$_NORMAL) THEN
             SCRIPT_PATH = "GXOLM:MESSAGEQCONNECTION.COM"       
          ENDIF

          ISTAT = SYS$CREPRC(pidadr,"SYS$SYSTEM:LOGINOUT.EXE",
C     *    "GXOLM:MESSAGEQCONNECTION.COM",          !input
     *    SCRIPT_PATH,                             !input
     *    OUTPUT_PATH,                             !output log
     *    ERROR_PATH,                              !error log
     *    ,,"MILLCON",,,,)                         !process name       

CCCCCCCCCC GET THE PID OF PROCESS MILLCON (optional)CCCCCCCCCCCCCCCC     
C          STPROC = LIB$GETJPI(JPI$_PID,pidadr,,,PROCESS_ID,)
C          MILL_CON_STATUS = 1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          STATLOG = lib$get_logical("MILLCONNECT",MILLCON)
          IF(STATLOG .NE. SS$_NORMAL) THEN
                MILLCON = "FAIL TO GET LOGICAL MILLCONNECT,ERR"        
          ENDIF          
C          IF(.NOT. STATLOG) CALL LIB$SIGNAL(%VAL(STATLOG))          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C  COULD BE USED STR$ELEMENT(MESSCON,0,",",MILLCON) as alternative C
C  COULD BE USED STR$ELEMENT(IP_ADDRESS,1,",",MILLCON)             C 
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC         
C          POS_AUX = LIB$INDEX(MILLCON,",")
C          IP_ADDRESS = MILLCON(POS_AUX+1:LIB$LEN(MILLCON)-(POS_AUX+1))
          STATLOG = STR$ELEMENT(MESSCON,0,",",MILLCON)
          STATLOG = STR$ELEMENT(IP_ADDRESS,1,",",MILLCON)
          STATLOG = STR$ELEMENT(HOST,2,",",MILLCON)
C          CALL OPSTXT('MESSCON:'//MESSCON)
C          CALL OPSTXT('IP_ADDRESS:'//IP_ADDRESS)
C          CALL OPSTXT('HOST:'//HOST)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ENDIF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      GET GAME FLAGS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       WRITE(CLIN1,901)
C---- System 
C       WRITE(CLIN4,903)K(1),OLMLST(1),K(3),INPLST(1),k(4),DECLST(1),k(15),SPELST(1),k(16),DISLST(1)
       WRITE(CLIN4,903)K(1),OLMLST(1),K(3),INPLST(1),k(4),DECLST(1)
       WRITE(CLIN5,910),OLMLST(2),INPLST(2),DECLST(2)
       WRITE(CLIN6,913),OLMLST(3),INPLST(3),DECLST(3)
C---- Connection & Mes Flux
C       WRITE(CLIN8,912) !K(2),P(OLMCONF)
       WRITE(CLIN8,912) k(5),WAGLST(1),k(6),CANLST(1),k(7),VALLST(1),k(16),DISLST(1)
       WRITE(CLIN9,914) k(8),INILST(1),k(9),CRSLST(1),k(10),INOLST(1),k(15),SPELST(1)
C       WRITE(CLIN11,915) k(15),SPELST(1),k(16),DISLST(1)

C       WRITE(CLIN11,916) "GETSUC",OLMS_GETMESSUC,"PUTSUC",OLMS_PUTMESSUC,"GETFAI",OLMS_GETMESFAI,"PUTFAI",OLMS_PUTMESFAI

C       WRITE(CLIN11,900)
       WRITE(CLIN10,900)  
C----- MessageQ attach and detach
       IF(OLMS_ATTACHSTS.NE.0) THEN                                          !ATTACH HAS BEEN DONE 
         WRITE(CLIN11,919)  K(2), P(OLMCONF)                                   !OLMCOn PARAMETER
         IF(IP_ADDRESS .EQ. 'ERR') THEN
            WRITE(CLIN12,9110) K(11),
     *                         MESSCON               
         ELSE
            WRITE(CLIN12,9101) K(11),                               
     *                         HOST,
     *                         MESSCON                 
         ENDIF       
C     *                        OLMS_ATTACHDAT(3),                                !DAY ATTACHED (DD)
C     *                        OLMS_ATTACHDAT(2),                                !MONTH ATTACHED (MM)
C     *                        OLMS_ATTACHDAT(1),                                !YEAR ATTACHED (YYYY)
C     *                        OLMS_ATTACHTIM                                    !TIME ATTACHED (H24:MI:SS)
        WRITE(CLIN13,9104) K(12), 
     *                     OLMS_ATTACHDAT(3),                                !DAY ATTACHED (DD)
     *                     OLMS_ATTACHDAT(2),                                !MONTH ATTACHED (MM)
     *                     OLMS_ATTACHDAT(1),                                !YEAR ATTACHED (YYYY)
     *                     OLMS_ATTACHTIM                                    !TIME ATTACHED (H24:MI:SS)              
       ELSE
        WRITE(CLIN11,9102) K(2), P(OLMCONF)                                   !EURCOn PARAMETER     
        IF(IP_ADDRESS .EQ. 'ERR') THEN
           WRITE(CLIN12,9110) K(11),
     *                        MESSCON 
        ELSE
           WRITE(CLIN12,9101) K(11),                               
     *                        HOST,
     *                        MESSCON                 
        ENDIF
        WRITE(CLIN13,9103) K(12)                                
       ENDIF

       IF(OLMS_DETACHFLG.NE.0) THEN
         WRITE(CLIN14,9108) K(13),REGLOG,         
     *                        OLMS_DETACHDAT(3),                                !LAST DAY DETACHED (DD)
     *                        OLMS_DETACHDAT(2),                                !LAST MONTH DETACHED (MM)
     *                        OLMS_DETACHDAT(1),                                !LAST YEAR DETACHED (YYYY)
     *                        OLMS_DETACHTIM                                    !LAST TIME DETACHED (H24:MI:SS)
       ELSE                                                                  !DETACH HAS NOT BEEN DONE
        WRITE(CLIN14,9106) K(13),REGLOG
       ENDIF

       WRITE(CLIN15,9107) K(14),REGLOGER

C       IF(IP_ADDRESS .EQ. 'ERR') THEN
C         WRITE(CLIN16,923) MESSCON
C       ELSE 
C        WRITE(CLIN16,922) MESSCON,IP_ADDRESS,HOST
C        WRITE(CLIN17,924) K(13),REGLOG,k(14),REGLOGER 
C       ENDIF

       WRITE(CLIN17,950)
       WRITE(CLIN18,9502) OLMS_TOTOKYPUT,                                      !TOTAL # OF MESSAGES SENT TO OLIMPO SYSTEM
     *                     OLMS_TOTOKYGET                                      !TOTAL # OF MESSAGES RECEIVED FROM OLIMPO SYSTEM

       WRITE(CLIN19,9503) OLMS_TOTERRPUT,                                      !TOTAL # OF ERRORS WHILE SENDING MESSAGES TO OLIMPO SYSTEM
     *                     OLMS_TOTERRGET                                      !TOTAL # OF ERRORS WHILE GETTING MESSAGES FROM OLIMPO SYSTEM
       WRITE(CLIN20,9510) OLMS_GETMESSUC,
     *                    OLMS_PUTMESSUC  
       WRITE(CLIN21,9511) OLMS_GETMESFAI,
     *                    OLMS_PUTMESFAI     
       WRITE(CLIN22,9513) OLMS_GETTERFAI       
C       WRITE(CLIN22,9504) OLMS_TOTNOMGET                                         !TOTAL # OF TIMES THERE IS NO MORE MESSAGES TO READ FROM MESSAGEQ

  
  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C----- FORMAT STATEMENTS
C
900     FORMAT(80(' '))
901     FORMAT('**** OLM control snapshot ****')
903     FORMAT('QUEUES  >  ',A7,'<',I4.0,'>',1X,A7,'<',I4.0,'>',
     *  1X,A7,'<',I4.0,'>')
C     *  1X,A7,'<',I4.0,'>',
C     *  1X,A7,'<',I4.0,'>')
910     FORMAT('BUFF 1  >  ',8X,I4.0,10X,I4.0,10X,I4.0,10X)
C     *  ,I4.0,10X,I4.0)
913     FORMAT('BUFF 2  >  ',8X,I4.0,10X,I4.0,10X,I4.0,10X)
C     *  ,I4.0,10X,I4.0)
C911     FORMAT('            ',1('*',A7,I6,3X))
C912     FORMAT('OLM      >   ',1('*',A7,I6,3X))
912     FORMAT('OLM     >  ',4(A7,I6,1X))     
914     FORMAT('           ',4(A7,I6,1X))
C915     FORMAT('           ',2(A7,I6,3X))
916     FORMAT('MESID   >  ',4(A6,1X,I6,1X))

919     FORMAT('        > ',1('*',A7,I6,1X)
     *          ,'COMOLM Attached?',2X,'Yes')                                 !IS COMMGR ATTACHED TO MESSAGEQ SERVER?
C922     FORMAT('   (',A8,')',3X,A60)
C922     FORMAT('MESSQ   > ',A8,9X,'IP:',A13,2X,'PID:',A8,2X,A6,2X,I1)
C922     FORMAT('MESSQ   > ',A8,9X,'IP:',A13,2X,'HOST:',A20,2X,A6,2X,I1)
922     FORMAT('MESSQ   >  ',A8,6X,'IP ',A13,2X,'HOST ',A25)
923     FORMAT('MESSQ ER>  ',A40)
924     FORMAT('           ',A8,4X,I1,1X,A8,4X,I1)
9101    FORMAT('          ',1X,1(A7)
     *          7X,'Attached to',7X,A20,1X,'(',A8,')')
9110    FORMAT('          ',1X,1(A7)
     *          7X,'Attached to',7X,A25)     
C     *          7X,'Time Attached',5X,I2.2,'.',I2.2,'.',I4.4,1X,2A4)            !TIME COMOLM ATTACHED TO MESSAGEQ SERVER IN OLIMPO SYSTEM

9103    FORMAT('          ',1X,1(A7)
     *          7X,'Time Attached',5X,'??.??.???? ??:??:??')                    !COMOLM IS NOT ATTACHED
9104    FORMAT('          ',1X,1(A7),
     *          7X,'Time Attached',5X,I2.2,'.',I2.2,'.',I4.4,1X,2A4)
C     *          7X,'Time Last Detach',2X,I2.2,'.',I2.2,'.',I4.4,1X,2A4)         !LAST TIME COMOLM DETACHED FROM MESSAGEQ SERVER IN OLIMPO SYSTEM
9108    FORMAT('          ',1X,1(A7),
     *          5X,I1,1X,'Time Last Detach',2X,I2.2,'.',I2.2,'.',I4.4,1X,2A4)
C9105    FORMAT('          ',1X,1(A7),
C     *          7X,'Time Last Detach',2X,'??.??.???? ??:??:??')  
C     *          7X,'Time Last Detach',2X,'??.??.???? ??:??:??')                 !LAST TIME COMOLM DETACHED FROM MESSAGEQ SERVER IN OLIMPO SYSTEM
9106   FORMAT('          ',1X,1(A7),5X,I1,
     *          1X,'Time Last Detach',2X,'??.??.???? ??:??:??')    
9107    FORMAT('          ',1X,1(A7),5X,I1)
9102    FORMAT('        > ',1('*',A7,I6,1X)
     *          ,'COMOLM Attached?',2X,'No')                                  !IS COMMGR ATTACHED TO MESSAGEQ SERVER?
950     FORMAT(19('-'),2X,'O L M   S Y S   S T A T I S T I C S',2X,19('-'))
9502    FORMAT('Msg   PutQcount/GetQcount',4X,I0,'/',I0,                                   !TOTAL # OF MESSAGES SENT TO/RECEIVED FROM EUROMILLIONS SYSTEM
     *         T37)                                      !TOTAL # OF WAGERS TIMED OUT/ALREADY TIMED OUT
9503    FORMAT('Err   PutQcount/GetQcount',4X,I0,'/',I0,                                   !TOTAL # OF ERRORS SENDING TO/RECEIVING FROM EUROMILLIONS SYSTEM
     *         T37)                                     !TOTAL # OF CANCELS TIMED OUT/ALREADY TIMED OUT
9510    FORMAT('MsgId PutQsucce/GetQsucce',4X,I0,'/',I0) 
9511    FORMAT('MsgId PutQerror/GetQerror',4X,I0,'/',I0)
9513    FORMAT('MsgId GetTererror        ',4X,I0) 
905     FORMAT(2X,I4.0)        
        END        


        