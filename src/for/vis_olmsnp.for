C
C SUBROUTINE OLMSNP
C
C
C VIS_OLMSNP.FOR
C
C V03 30-MAR-2016 SCML M16 PROJECT
C V02 20-JUL-2015 SCML Adding support for IGS internal cancel flags
C V01 08-APR-2014 SCML Placard Project
C
C EUR SYSTEM CONTROL SNAPSHOT
C
C
C ERROR MESSAGE #  DESCRIPTION
C ---------------  -----------------------------------------------------
C       5          PASSWORD ERROR WHILE TRYING TO CHANGE EM TIMEOUT
C       6          PASSWORD ERROR WHILE TRYING TO CHANGE EM FIN TIMEOUT
C
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2016 SCML Corporation. All rights reserved.
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
        INTEGER*4  pidadr,STATLOG,STPROC,POS_AUX
       INTEGER*4  SYS$CREPRC,STR$ELEMENT          
C        INTEGER*4  STR$ELEMENT,STR$TRIM,SIZE_AUX
        CHARACTER*40 MESSCON
        LOGICAL    MILL_CON_STATUS /0/, REGLOG /0/, REGLOGER /0/        
        CHARACTER*20 PROCESS_ID,OUTPUT_PATH,ERROR_PATH
        CHARACTER*25 HOST
        CHARACTER*40 ERR_MSG
        CHARACTER*13 IP_ADDRESS
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
     *           'OLMTMO  ','FINTMO  ','REGLOG  ','REGERLOG',
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
          IF(REGLOG) THEN
                OUTPUT_PATH = "GXOLM:MILLCON.LOG"
          ELSE
                OUTPUT_PATH = "NLA0:" !its null device
          ENDIF
          IF(REGLOGER) THEN
                ERROR_PATH = "GXOLM:ERR_MILLCON.LOG"
          ELSE
                ERROR_PATH = "NLA0:" !its null device
          ENDIF
CCCCCCCCCC Confirm that the script exists CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

          ISTAT = SYS$CREPRC(pidadr,"SYS$SYSTEM:LOGINOUT.EXE",
     *    "GXOLM:MESSAGEQCONNECTION.COM",          !input
     *    OUTPUT_PATH,                             !output log
     *    ERROR_PATH,                 !error log
     *    ,,"MILLCON",,,,)                  !process name       

CCCCCCCCCC GET THE PID OF PROCESS MILLCON (optional)CCCCCCCCCCCCCCCC     
C          STPROC = LIB$GETJPI(JPI$_PID,pidadr,,,PROCESS_ID,)
C          MILL_CON_STATUS = 1
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          STATLOG = lib$get_logical("MILLCONNECT",MILLCON)
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
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      ENDIF

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      GET GAME FLAGS
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
       WRITE(CLIN1,901)
C---- System 
       WRITE(CLIN4,903)K(1),OLMLST(1),K(3),INPLST(1),k(4),DECLST(1)
       WRITE(CLIN5,910),OLMLST(2),INPLST(2),DECLST(2)
       WRITE(CLIN6,913),OLMLST(3),INPLST(3),DECLST(2)
C---- Connection & Mes Flux
C       WRITE(CLIN8,912) !K(2),P(OLMCONF)
       WRITE(CLIN9,912) k(5),WAGLST(1),k(6),CANLST(1),k(7),VALLST(1)
       WRITE(CLIN10,914) k(8),INILST(1),k(9),CRSLST(1),k(10),INOLST(1)
       WRITE(CLIN11,915) k(15),SPELST(1),k(16),DISLST(1)

C       WRITE(CLIN11,900)
       WRITE(CLIN12,900)  
C----- MessageQ attach and detach
       IF(OLMS_ATTACHSTS.NE.0) THEN                                          !ATTACH HAS BEEN DONE 
         WRITE(CLIN13,919)  K(2), P(OLMCONF)                                   !OLMCOn PARAMETER
         WRITE(CLIN14,9101) K(11),                                
     *                        OLMS_ATTACHDAT(3),                                !DAY ATTACHED (DD)
     *                        OLMS_ATTACHDAT(2),                                !MONTH ATTACHED (MM)
     *                        OLMS_ATTACHDAT(1),                                !YEAR ATTACHED (YYYY)
     *                        OLMS_ATTACHTIM                                    !TIME ATTACHED (H24:MI:SS)
       ELSE
        WRITE(CLIN13,9102) K(2), P(OLMCONF)                                   !EURCOn PARAMETER     
        WRITE(CLIN14,9103) K(11)                                 
       ENDIF

       IF(OLMS_DETACHFLG.NE.0) THEN
         WRITE(CLIN15,9104) K(12),         
     *                        OLMS_DETACHDAT(3),                                !LAST DAY DETACHED (DD)
     *                        OLMS_DETACHDAT(2),                                !LAST MONTH DETACHED (MM)
     *                        OLMS_DETACHDAT(1),                                !LAST YEAR DETACHED (YYYY)
     *                        OLMS_DETACHTIM                                    !LAST TIME DETACHED (H24:MI:SS)
       ELSE                                                                  !DETACH HAS NOT BEEN DONE
        WRITE(CLIN15,9105) K(12)
       ENDIF

C       WRITE(CLIN17,922) PROCESS_ID,MILLCON
       IF(IP_ADDRESS .EQ. 'ERR') THEN
C         STATLOG = STR$TRIM(ERR_MSG,MILLCON(1:POS_AUX-1),SIZE_AUX)
C         ERR_MSG = MILLCON(1:POS_AUX-1)
         WRITE(CLIN17,923) MESSCON
       ELSE 
C         WRITE(CLIN17,922) MESSCON,IP_ADDRESS,PROCESS_ID,K(13),REGLOG
        WRITE(CLIN17,922) MESSCON,IP_ADDRESS,HOST
        WRITE(CLIN18,924) K(13),REGLOG,k(14),REGLOGER 
       ENDIF

       WRITE(CLIN20,950)
       WRITE(CLIN21,9502) OLMS_TOTOKYPUT,                                      !TOTAL # OF MESSAGES SENT TO OLIMPO SYSTEM
     *                     OLMS_TOTOKYGET                                      !TOTAL # OF MESSAGES RECEIVED FROM OLIMPO SYSTEM

       WRITE(CLIN22,9503) OLMS_TOTERRPUT,                                      !TOTAL # OF ERRORS WHILE SENDING MESSAGES TO OLIMPO SYSTEM
     *                     OLMS_TOTERRGET                                      !TOTAL # OF ERRORS WHILE GETTING MESSAGES FROM OLIMPO SYSTEM

C       WRITE(CLIN22,9504) OLMS_TOTNOMGET                                         !TOTAL # OF TIMES THERE IS NO MORE MESSAGES TO READ FROM MESSAGEQ

  
  
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C----- FORMAT STATEMENTS
C
900     FORMAT(80(' '))
901     FORMAT('**** OLM control snapshot ****')
903     FORMAT('QUEUES  >  ',A7,'<',I4.0,'>',3X,A7,'<',I4.0,'>',3X,A7,'<',I4.0,'>',3X,A7,'<',I4.0,'>',3X)
910     FORMAT('BUFF 1  >  ',2X,I4.0,5X,I4.0,5X,I4.0,5X)
913     FORMAT('BUFF 2  >  ',2X,I4.0,5X,I4.0,5X,I4.0,5X)
C911     FORMAT('            ',1('*',A7,I6,3X))
C912     FORMAT('OLM      >   ',1('*',A7,I6,3X))
912     FORMAT('OLM     >  ',3(A7,I6,3X))     
914     FORMAT('           ',3(A7,I6,3X))
915     FORMAT('           ',2(A7,I6,3X))

919     FORMAT('        > ',1('*',A7,I6,3X)
     *          ,'COMOLM Attached?',2X,'Yes')                                 !IS COMMGR ATTACHED TO MESSAGEQ SERVER?
C922     FORMAT('   (',A8,')',3X,A60)
C922     FORMAT('MESSQ   > ',A8,9X,'IP:',A13,2X,'PID:',A8,2X,A6,2X,I1)
C922     FORMAT('MESSQ   > ',A8,9X,'IP:',A13,2X,'HOST:',A20,2X,A6,2X,I1)
922     FORMAT('MESSQ   >  ',A8,8X,'IP ',A13,2X,'HOST ',A25)
923     FORMAT('MESSQ ER>  ',A40)
924     FORMAT('           ',A8,4X,I1,3X,A8,4X,I1)
9101    FORMAT('          ',1X,1(A7)
     *          9X,'Time Attached',5X,I2.2,'.',I2.2,'.',I4.4,1X,2A4)            !TIME COMOLM ATTACHED TO MESSAGEQ SERVER IN OLIMPO SYSTEM
9103    FORMAT('          ',1X,1(A7)
     *          9X,'Time Attached',5X,'??.??.???? ??:??:??')                    !COMOLM IS NOT ATTACHED
9104    FORMAT('          ',1X,1(A7),
     *          9X,'Time Last Detach',2X,I2.2,'.',I2.2,'.',I4.4,1X,2A4)         !LAST TIME COMOLM DETACHED FROM MESSAGEQ SERVER IN OLIMPO SYSTEM
9105    FORMAT('          ',1X,1(A7),
     *          9X,'Time Last Detach',2X,'??.??.???? ??:??:??')                 !LAST TIME COMOLM DETACHED FROM MESSAGEQ SERVER IN OLIMPO SYSTEM
9102    FORMAT('        > ',1('*',A7,I6,3X)
     *          ,'COMOLM Attached?',2X,'No')                                  !IS COMMGR ATTACHED TO MESSAGEQ SERVER?
950     FORMAT(19('-'),2X,'O L M   S Y S   S T A T I S T I C S',2X,19('-'))
9502    FORMAT('Msg  PutQ/GetQ',4X,I0,'/',I0,                                   !TOTAL # OF MESSAGES SENT TO/RECEIVED FROM EUROMILLIONS SYSTEM
     *         T37)                                      !TOTAL # OF WAGERS TIMED OUT/ALREADY TIMED OUT
9503    FORMAT('Err  PutQ/GetQ',4X,I0,'/',I0,                                   !TOTAL # OF ERRORS SENDING TO/RECEIVING FROM EUROMILLIONS SYSTEM
     *         T37)                                     !TOTAL # OF CANCELS TIMED OUT/ALREADY TIMED OUT
C9504    FORMAT('No More Msg  GetQ',4X,I0)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C904     FORMAT(1X,'<',I4.0,'>')
905     FORMAT(2X,I4.0)        
        END        


        