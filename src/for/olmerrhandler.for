C-----------------------------------------------------------------------
C PROGRAM OLMERRHANDLER
C-----------------------------------------------------------------------
C OLMERRHANDLER.FOR
C
C V01 2022-MAY-05 SCML NEW TERMINALS PROJECT - OLM - Creation
C
C This function is used to turn all errors into warnings.  To use this,
C you must declare this function as an external (i.e.,:
C
C	INTEGER*4   OLMERRHANDLER
C	EXTERNAL    OLMERRHANDLER
C
C Then you must establish this as the condition handler as follows:
C
C	CALL LIB$ESTABLISH( OLMERRHANDLER )
C
C Any time an exception occurs during this routine or any lower subroutine
C (i.e., one called by this routine or its subroutines), this program will
C intercept the condition, turn it into a non-fatal error, and resignal to the
C next highest routine in the calling chain (which will normally simply print
C the warning and traceback info).
C
C After the establishing routine returns to its caller, this signal handler
C is disabled.  (You could also revert back to the normal condition handler
C by calling LIB$REVERT).
C
C
C logic replicated from NOFTLSIG.FOR with the addition of logs specific to OLM
C to register the error's that were originated during its normal working of the 
C process
C
C Contents of SIGARGS
C
C Array Element         CHFDEF1 Field Name            Contents
C SIGARGS(1)            CHF$IS_SIG_ARGS               Argument count (n)
C SIGARGS(2)            CHF$IS_SIG_NAME               Condition code
C SIGARGS(3 to n-1)     CHF$IS_SIG_ARG1 (first        Zero or more additional arguments,
C                                   argument)         specific to the condition code in
C                                                     SIGARGS(2)
C SIGARGS(n)            None                          PC (program counter)
C SIGARGS(n+1)          None                          PS (processor status), lower 32-bits of
C                                                     the 64-bit OpenVMS processor status
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C Copyright 2022 SCML All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT

      INTEGER*4 FUNCTION OLMERRHANDLER(SIGARGS, MECHARGS)
      IMPLICIT NONE
C
      INCLUDE 'INCLIB:SYSPARAM.DEF'
      INCLUDE '($LIBDEF)'
      INCLUDE '($STSDEF)'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INCLUDE '($FORDEF)' ! Declare the FOR$_ symbols      
      INCLUDE '($SSDEF)'  ! Declare the SS$_ symbols
C
      INTEGER*4   SIGARGS(*)
      INTEGER*4   MECHARGS(*)
C
      INTEGER*4   ERRORCODE
C
	CHARACTER*18 IAM
      INTEGER*4    STATUS,MSGLEN

      CHARACTER*256 MSG
      INTEGER*4    ERR_NUM
      INTEGER*4    LIB$SYS_GETMSG
C	EXTERNAL     IAM 
      ! Declare procedures
      INTEGER LIB$MATCH_COND,INDEX
      
C
C Get the error code.  If it is a fatal error, change severity to a non-fatal
C error, then resignal.
C
      ERRORCODE = 0
      CALL MVBITS( SIGARGS(2), 0, 3, ERRORCODE, 0)
C     the error code is the full 4 bytes that is 32 bits of SIGARGS(2) to retrive the error msg
      CALL MVBITS( SIGARGS(2), 0, 32, ERR_NUM, 0)   
      CALL LIB$SYS_GETMSG(ERR_NUM,MSGLEN,MSG)
C      CALL LIB$PUT_OUTPUT(MSG(1:MSGLEN))

C      CALL OPSTXT("----------Error Handler---------")
C      CALL OPS("ERR_NUM:",ERR_NUM,ERR_NUM)   
C      CALL OPS("ERRORCODE:",ERRORCODE,ERRORCODE)
C      CALL OPSTXT("ERR_MSG:"//MSG(1:MSGLEN))
C      CALL OPSTXT("--------------------------------")

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC           
C Value   Symbol            Severity          Response
C 0       STS$K_WARNING     Warning           Execution continues, unpredictable results
C 1       STS$K_SUCCESS     Success           Execution continues, expected results
C 2       STS$K_ERROR       Error             Execution continues, erroneous results
C 3       STS$K_INFO        Information       Execution continues, informational message displayed
C 4       STS$K_SEVERE      Severe error      Execution terminates, no output
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

CCCCCCCCCCCCCCCCCCC begin - More controled error handlingCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
      INDEX = LIB$MATCH_COND (SIGARGS(2),
     *        FOR$_FILNOTFOU,
     *        FOR$_OPEFAI,
     *        FOR$_NO_SUCDEV,
     *        FOR$_FILNAMSPE)

      IF (INDEX .EQ. 0) THEN
            ! Not an expected condition code, resignal
            IF( ERRORCODE.EQ.STS$K_SEVERE ) THEN
C                  CALL OPSTXT("...Not an expected condition code - REDUCE THE SEVERITY OF ERROR and resignal...")    
                  CALL MVBITS( STS$K_ERROR, 0, 3, SIGARGS(2), 0)
            ENDIF
            !HANDLER = SS$_RESIGNAL
      ELSE IF (INDEX .GT. 0) THEN
            ! Expected condition code, handle it
C            CALL OPSTXT("...Expected condition code - handle it...")
C            CALL OPS("error detected index:",INDEX,INDEX)              
            IF( ERRORCODE.EQ.STS$K_SEVERE ) THEN
                  CALL MVBITS( STS$K_ERROR, 0, 3, SIGARGS(2), 0)
            ENDIF
      ENDIF
CCCCCCCCCCCCCCCCCCC end - More controled error handlingCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC


      CALL OPSTXT("!!!error exception detected in COMOLM!!!")
      TYPE *,IAM(),''
      TYPE *,IAM(),''
      TYPE *,IAM(),'Error in COMOLM ocurred the severity is:',ERRORCODE
      TYPE *,IAM(),'Error type:',MSG(1:MSGLEN)
      IF( ERRORCODE.EQ.STS$K_SEVERE ) THEN
            TYPE *,IAM(),'The error is Severe/fatal that'
            TYPE *,IAM(),'would kill COMOLM if not handled'
C            CALL OPSTXT("!!!Fatal/Severe error exception detected in COMOLM!!!")
      ENDIF
      TYPE *,IAM(),''
      TYPE *,IAM(),''
C      Unwinding the Call Stack (when triggered by LIB$STOP(%VAL(0))  ) only way to avoid closing the image aka kill the process
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      To return control to the program unit that established the handler, invoke SYS$UNWIND and pass the     C                      
C      call depth                                                                                             C    
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      CALL SYS$UNWIND (MECHARGS(3),)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      To return control to the caller of the program unit that established the handler, invoke SYS$UNWIND    C                      
C      without passing any arguments.                                                                         C    
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C     CALL SYS$UNWIND (,)
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C      The first argument SYS$UNWIND specifies the number of program units to unwind (remove from the stack)  C                      
C      The second argument SYS$UNWIND contains the location of the next statement to be executed.             C 
C      Typically, you omit the second argument to indicate that the program should resume execution at the    C  
C      the statement following the last statement executed in the program unit that is regaining control.     C    
C                                                                                                             C                      
C                                                                                                             C                
C      Each time SYS$UNWIND removes a program unit from the stack, it invokes any condition handler           C
C      established by that program unit and passes the condition handler the SS$_UNWIND condition             C
C      code. To prevent the condition handler from resignaling the SS$_UNWIND condition code (and so          C
C      complicating the unwind operation), include SS$_UNWIND as an expected condition code when you          C
C      invoke LIB$MATCH_COND. When the condition code is SS$_UNWIND, your condition handler                   C
C      might perform necessary cleanup operations or do nothing.                                              C                
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

      OLMERRHANDLER = SS$_RESIGNAL
C
      RETURN
      END


C      CHARACTER*18 FUNCTION IAM()
C	IMPLICIT NONE
CC
C	CHARACTER*8 MYTIME
C	CHARACTER*8 PROC_NAME
CC
CC	CALL GETNAM(%REF(PROC_NAME))
C      PROC_NAME = "COMOLM"
C	CALL TIME (MYTIME)
C	IAM = MYTIME // ' ' // PROC_NAME // ' '
CC
C	RETURN
C	END

