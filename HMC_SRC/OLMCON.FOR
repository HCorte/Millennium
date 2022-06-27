      PROGRAM OLMCON
        IMPLICIT NONE
        INCLUDE 'INCLIB:SETUP.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:SPTCOM.DEF'
C        INCLUDE 'OLMCON.DEF'


C       show the status of P(SUPSYN)

        RECORD/SETUP/ PAR
        CHARACTER*40  FILENAME

        INTEGER*4  TEMP
        INTEGER*2  I2TEMP(2)

        EQUIVALENCE (TEMP,       I2TEMP)

        PRINT *, 'CDC DATE:',P(NXTCDC)

        PRINT *,'P(REGLOG)=',P(REGLOG)
        PRINT *,'P(OLMCONF)=',P(OLMCONF)
        TEMP = SPTREV(3) !3 game index for totobola extra
        PRINT *,'Game Control Revision: ',I2TEMP(1) 
        PRINT *,'Game Text Revision: ',I2TEMP(2) 
        TEMP = 0
        PAR.OLMCON = P(OLMCONF)    
C        PAR.REGLOG = P(REGLOG)

        PRINT *,'P(SYSTYP)',P(SYSTYP)
        IF(P(SYSTYP) .NE. LIVSYS) THEN 
            PRINT *,'Nao e Sistema primario (not live system):',LIVSYS
        ELSE 
            PRINT *,'E sistema primario (live system):',LIVSYS
        ENDIF

        PRINT *,'DAYSTS:',DAYSTS
        PRINT *,'DSSUSP:',DSSUSP
        PRINT *,'DSOPEN:',DSOPEN
        
        
C        GNUM=GTNTAB(TLTO,GIND)
C        PRINT *,'File Name:',GFNAMES(1,GNUM)  


        FILENAME = "GXOLM:L3F_AUX.FIL;1"
        CALL DISP_CONFIG(PAR,FILENAME)
        PRINT *,'GOOD .....EXITED SUBROUTINE'

        P(OLMCONF) = PAR.OLMCON
C        P(REGLOG) = PAR.REGLOG
        PRINT *,'P(OLMCONF)=',P(OLMCONF)
        PRINT *,'P(REGLOG)=',P(REGLOG)
C        PRINT *,P(OLMCONF)


      END

      SUBROUTINE DISP_CONFIG(PAR, FILENAME)
        IMPLICIT NONE  
        INCLUDE 'INCLIB:SETUP.DEF'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:WINCOM.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:KIKCOM.DEF'
C        INCLUDE 'INCLIB:DLTREC.DEF'
C        INCLUDE 'INCLIB:DSPREC.DEF'
C        INCLUDE 'INCLIB:DTGREC.DEF'
C        INCLUDE 'INCLIB:DKKREC.DEF'
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:STOPCOM.DEF'
        INCLUDE 'INCLIB:MULNAM.DEF'
C
        INCLUDE	'INCLIB:DISKIO.DEF'
C        INCLUDE 'INCLIB:ASFSUBS.DEF'
C        INCLUDE 'OLMCON.DEF'
        INCLUDE '(LIB$ROUTINES)'
	INCLUDE	'($SYSSRVNAM)'
	INCLUDE '($RABDEF)'
        INCLUDE '($RMSDEF)'  
C
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:AGTCOM.DEF'
        INCLUDE 'INCLIB:SPECOM.DEF'   
C
C        INCLUDE 'INCLIB:LLTREC.DEF'    
        INCLUDE 'INCLIB:LTOCOM.DEF'   
C       CONSTANTS for TCF file logic              
        INCLUDE 'INCLIB:PRMLOG.DEF'
        INCLUDE 'INCLIB:DESLOG.DEF'
        INCLUDE 'INCLIB:PRMHSH.DEF'
C       buffer TRABUF
        INCLUDE 'INCLIB:DESTRA.DEF'
C       get process information
C	INCLUDE	    '($SYSSRVNAM)'
        INCLUDE	    '($JPIDEF)' 
C
C        INCLUDE 'INCLIB:GLOBAL.DEF'                

        CHARACTER*3 YESNO(0:1)
        DATA YESNO  /' No','Yes'/
        INTEGER*4 OPT,ST,CLOSE_ST,STATUS
        CHARACTER OPTION
C        BYTE	    I4FILE(*)
        CHARACTER   FILENAME*(*)
        LOGICAL*1    ISTHERE
        CHARACTER*20 ISDIRECT
	CHARACTER*20 ORGTYPE
	CHARACTER*20 RECTYPE
c      LOGICAL 
        INTEGER*4   SHRIND /4/, LUN /3/
        INTEGER*4   DISKOPEN_AUX
        EXTERNAL    DISKOPEN_AUX


        RECORD/SETUP/ PAR
        INTEGER*4 GNUM, DRAW, GIND, k, DAY
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC   
C read binary file variables
        RECORD	    /RABDEF/ RAB
        INTEGER*4   FDB(FDB_LENGTH)
        
        INTEGER*4   NUMBYTES
        INTEGER*4   BEGBLK
        INTEGER*4   BLOCK_AUX
C
C        INTEGER*4   BUFFER(*)   
        INTEGER*4   FOR$RAB     
C
        INTEGER*4 NAMVLW(5),NAMTCW(5)  
        INTEGER*4 AMT      
        INTEGER*4  TUBSIZ
	PARAMETER (TUBSIZ=I4BUCSIZ*7)
        INTEGER*4 TCFBUF(TUBSIZ)
        INTEGER*4 LOGBUF(LREC*3)	!     

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C       option 6 variables
        INTEGER*4  GTYP                ! game type
        INTEGER*4  EXT                 ! status
C        
	INTEGER*4	I4ITEM(4)
	INTEGER*2	I2ITEM(6)
	EQUIVALENCE	(I4ITEM,I2ITEM)     
        CHARACTER*7 TN
        INTEGER*4   TNL		! LENGTH OF THE 'TN'
        INTEGER*4   NDX
        CHARACTER*8 TNAME	! OUTPUT
C   begin gninvall
        integer*4    xday   
        character*20 gfil(7,maxgam)   
        character*20 cfil
        logical      xfil
C        integer*4    gnum
C        integer*4    gtyp
        integer*4    gcdc
        integer*4    icdc          
        integer*4    fcdc
        integer*4    offs

        integer*4    temp
        byte         i1temp(4)
        equivalence (temp,i1temp)         
C   end gninvall

C begin nrm_csmonyi8
        DOUBLE PRECISION BIG_VALUE,BIG  !8 bytes since its REAL*8 equivalent
        DOUBLE PRECISION BIG_VALUE_3 
C        REAL*8 BIG_VALUE  
C        REAL*16 BIG_VALUE
C        PARAMETER (BIG=9223372036854775000.0)
        CHARACTER*22 BIG_STR

	CHARACTER*32	CTEMP		!TEMPORARY CHR VARIABLE
	INTEGER*4	ITEMP(8)	!TEMPORARY INT VARIABLE
	EQUIVALENCE    (CTEMP, ITEMP)

C end nrm_csmonyi8

10	CONTINUE        

        WRITE(6,9012) YESNO(PAR.OLMCON)
        print '(A)' !new line - NEW_LINE(A) (Fortran 2003)
        PRINT *, 'ENTER OPTION 9  AND ACTIVE/DISABLE MESSAGEQ MESSAGES READ'
C        PRINT *, 'ENTER OPTION 1  AND ACTIVE/DISABLE LOG REGISTRATION'
        PRINT *, 'ENTER OPTION 2 to see system variables'
        PRINT *, 'ENTER OPTION 3 to see game data'
        PRINT *, '4 - Print file names defined in the MILL system'
        PRINT *, '5 - Read TCF file information'
        PRINT *, '6 - Test read parameter input'
        print '(A)'
        READ *, OPTION

        IF(OPTION .EQ. 'E' .OR. OPTION .EQ. 'e') THEN
                PRINT *, 'exited'
                RETURN
        ENDIF

C       ICHAR gets the first char and gets its position in the ASCII TABLE  
C       knowing that 0 char corresponde to position 48 then subtrating this
C       value with the caracther insert input return the numerial value inserted     
        OPT = ICHAR(OPTION) - ICHAR('0') 

        PRINT *, 'GO TO OPTION:',OPT
        GOTO(100,200,300,400,500,600,700,800,900) OPT

        
100     CONTINUE
        PRINT *, 'INVALID OPTION'
C        IF(PAR.REGLOG .GT. 1 .OR. PAR.REGLOG .LT. 0) THEN
C          PAR.REGLOG = 0           
C        ELSE        
C          PAR.REGLOG = 1 - PAR.REGLOG
C        ENDIF
        GOTO 10  

200     CONTINUE
        print '(A)'
        print '(A)'
        PRINT *,'     BEGIN---SYS VARIABLES---       '
        PRINT *,'P(DISKSW)=',P(DISKSW)
        PRINT *,'P(SUPSYN)=',P(SUPSYN) 
        PRINT *,'P(SUPWEL)=',P(SUPWEL) 
        PRINT *,'P(NUMENC)=',P(NUMENC) 
        PRINT *,'P(DESFLG_TYPE)=',P(DESFLG_TYPE) 
C       (DESFLG_HARD=1)    !HARD DES ENCRYPTIO        
        PRINT *,'DESFLG_HARD=',1 
        PRINT *,'P(ENCPRO_WAIT)=',P(ENCPRO_WAIT) 
        PRINT *,'P(LOGTIM)=',P(LOGTIM) 
        PRINT *,'P(SUPSUM)=',P(SUPSUM) 

        PRINT *,'     END---SYS VARIABLES---       '
        print '(A)'
        print '(A)'
        GOTO 10      
        
300     CONTINUE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C
C \FILE (or NAME ) and DEFAULTFILE 1 specify the directory and/or file name of an external file.
C \IOSTAT specifies the integer variable to receive the error (IOSTAT) number if an error occurs.
C See Chapter 7.
C \DIRECT -- Value of Variable : 'YES' 'NO' 'UNKNOWN' ||| Data Type of Variable:CHARACTER
C DIRECT=dir -- dir is a character variable that is assigned the value 'YES' if the file could 
C be connected for direct I/O, 'NO' if the file could not be connected for direct I/O,
C and 'UNKNOWN' if the system can't tell
C The EXIST specifier variable indicates (true or false) whether the named file exists on the
C device or not.
C \ORGANIZATION 1 indicates the file organization (sequential, relative, or indexed).
C \RECORDTYPE 1 indicates which record type to use.
C
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

        INQUIRE(FILE=FILENAME, EXIST=ISTHERE,
     *	          IOSTAT=ST,
     *	          DIRECT=ISDIRECT,
     *	          ORGANIZATION=ORGTYPE,
     *	          RECORDTYPE=RECTYPE)

        print *,'ISTHERE:',ISTHERE
        print *,'ST:',ST
        print *,'DIRECT:'//ISDIRECT
        print *,'ORGTYPE:'//ORGTYPE
        print *,'RECTYPE:'//RECTYPE

        IF(.NOT.ISTHERE)THEN
          PRINT 'THE FILE DOES NOT EXISTS:'//FILENAME      
          GOTO 10
        ENDIF

C
C Now open the file
C OPEN (UNIT=2, FILE=DOC, DEFAULTFILE='DISK4:[PROJ]',STATUS='OLD')
C The DEFAULTFILE specification overrides your process default device and directory.
C 

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C Fortran for OpenVMS Language Reference Manual - January 2005.pdf    C
C USEROPEN func User program option No default                        C                                
C 
C 12.6.31 USEROPEN Specifier
C The USEROPEN specifier indicates a user-written external function that
C controls the opening of the file. It takes the following form:
C USEROPEN = function-name
C function-name
C
C Is the name of the user-written function to receive control.
C The function must be declared in a previous EXTERNAL statement; if it is
C typed, it must be of type INTEGER(4) (INTEGER*4). 
C
C The USEROPEN specifier lets experienced users use additional features of the
C operating system that are not normally available in Fortran.
C
C For More Information:
C On user-supplied functions to use with USEROPEN, including examples, see
C the HP Fortran for OpenVMS User Manual.                      
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C
C 12.6.28 SHARED Specifier
C The SHARED specifier indicates that the file is connected for shared access by
C more than one program executing simultaneously. It takes the following form:
C SHARED

C Explicit file-sharing information supplied by accessing programs – Whether file sharing actually
C takes place depends on information provided to OpenVMS RMS by each program accessing the
C file. In VSI Fortran programs, this information is supplied by the ACTION specifier (or VSI
C extension READONLY specifier) and the SHARED specifier in the OPEN statement.

	IF(ISDIRECT.EQ.'YES' .AND.
     *	     ORGTYPE.EQ.'RELATIVE' .AND.
     *	     RECTYPE.EQ.'FIXED')THEN
C
          IF(SHRIND.EQ.0)THEN  
   	    OPEN(UNIT=LUN, FILE=FILENAME, ORGANIZATION='RELATIVE',
     *	           ACCESS='DIRECT', STATUS='OLD', IOSTAT=ST,
     *	           USEROPEN=DISKOPEN_AUX, SHARED)
          ELSE
            print *,'---open file as expected---'      
   	    OPEN(UNIT=LUN, FILE=FILENAME, ORGANIZATION='RELATIVE',
     *	           ACCESS='DIRECT', STATUS='OLD', IOSTAT=ST,
     *	           USEROPEN=DISKOPEN_AUX, SHARED)
C
	  ENDIF
	ELSE
	  IF(SHRIND.EQ.0)THEN
	    OPEN(UNIT=LUN, FILE=FILENAME, STATUS='OLD',
     *	           IOSTAT=ST, SHARED, READONLY)
	  ELSE
	    OPEN(UNIT=LUN, FILE=FILENAME, STATUS='OLD',
     *	           IOSTAT=ST, SHARED)
	  ENDIF
	ENDIF
C
	IF(ST)THEN
	  ST = 0
        ENDIF

C        CLOSE(UNIT=LUN,STATUS='KEEP',ERR=2000, IOSTAT=CLOSE_ST)
C        print *,'close file status:',CLOSE_ST
        
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                         
C Document (OpenVMS 7.3-1 Record Management Services Reference Manual - RMS .pdf)                     C  
C SYS$READ $READ Retrieves a specified number of bytes from a file, beginning on block boundaries     C                          
C Format SYS$READ rab [,[err] [,suc]]                                                                 C                     
C                                                                                                     C
C Returns                                                                                             C  
C OpenVMS usage: cond_value                                                                           C     
C type: longword                                                                                      C
C access: write only                                                                                  C     
C mechanism: by value                                                                                 C     
C The value is returned in symbolic offset RAB$L_STS. Symbolic offset RAB$L_                          C                        
C STV may contain additional status information.                                                      C
C                                                                                                     C  
C Arguments                                                                                           C
C -rab                                                                                                C  
C OpenVMS usage: rab                                                                                  C
C type: longword (unsigned)                                                                           C
C access: modify                                                                                      C
C mechanism: by reference                                                                             C
C RAB control block whose contents are to be used as indirect arguments for the                       C              
C Read service call. The rab argument is the address of the RAB control block.                        C 
C -err                                                                                                C
C OpenVMS usage: ast_procedure                                                                        C                        
C type: procedure value                                                                               C               
C access: call without stack unwinding                                                                C       
C mechanism: by reference                                                                             C               
C AST-level error completion routine that the service invokes if the operation                        C                                                                          
C is unsuccessful. The err argument is the address of the entry mask of this                          C                                                                          
C user-written completion routine.                                                                    C                                  
C -suc                                                                                                C  
C OpenVMS usage: ast_procedure                                                                        C                          
C type: procedure value                                                                               C                  
C access: call without stack unwinding                                                                C                                  
C mechanism: by reference                                                                             C                          
C AST-level success completion routine that the service invokes if the operation                      C                                                                                  
C is successful. The suc argument is the address of the entry mask of this userwritten                C                                                                                  
C completion routine.                                                                                 C          
C                                                                                                     C                          
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                         
 
        NUMBYTES = DLTSEC*256
        IF(NUMBYTES.LT.256)THEN
	  PRINT *,'IOINIT LENGTH TOO SHORT = ',NUMBYTES
	  CALL LIB$STOP(%VAL(0))
	ENDIF
	IF(NUMBYTES.GT.65535)THEN
          PRINT *,'IOINIT LENGTH TOO  LONG = ',NUMBYTES
	  CALL LIB$STOP(%VAL(0))
        ENDIF
        
        DO 110 K = 1, FDB_LENGTH
                FDB(K) = 0
110	CONTINUE

        FDB(FDB_LUN)   = LUN		    !LUN #
        FDB(FDB_BYTSZ) = NUMBYTES	    !BYTES  PER I/O
        FDB(FDB_BLKSZ) = (NUMBYTES+511)/512 !BLOCKS PER I/O
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C
C       Totoloto de Sabado
        DRAW = 557
        BLOCK_AUX = DRAW
C
	LUN    = FDB(FDB_LUN)
        BEGBLK = (BLOCK_AUX-1)*FDB(FDB_BLKSZ) + 2
C       RAB = Record Access Block
C       The %VAL() Construct
C            %VAL(arg)
C            
C       The %VAL() construct specifies that an argument, arg, is to be passed by value, instead of by reference or descriptor.
C       
C       %VAL() is restricted to actual arguments in invocations of external procedures

C       Implementation Note: Currently, g77 passes all arguments either by reference or by descriptor.

C       Thus, use of %VAL() tends to be restricted to cases where the called procedure is written in a language other than Fortran 
C       that supports call-by-value semantics. (C is an example of such a language.)

C        RAB = %VAL(FOR$RAB(LUN))
C        RAB = FOR$RAB(LUN)                 
C
        CALL READXX_AUX(%VAL(FOR$RAB(LUN)), FDB, BEGBLK, DLTREC, STATUS)        
        
        IF(STATUS.NE.0) print *,'deu erro na leitura do stream...'
C       CALL FILERR(GFNAMES(1,GNUM),2,ST,DRAW)
C       Totoloto de Sabado Game Type = 1 and GameIndex = 3 
        GIND = 3
        DO 1100 DAY=1,LTGENT
        print '(A)'
        print '(A)'                
        print *,'DAY:',DAY        
	IF(DAY.EQ.LTGENT.OR.DAY.EQ.2) THEN
            LLTSAL(DAY,GIND) = LLTSAL(DAY,GIND) + DLTSAL(DAY)
            PRINT *,'...first if...'
            PRINT *,'LLTSAL(DAY,GIND)=',LLTSAL(DAY,GIND)
	    GOTO 1100
	ENDIF


        PRINT *,'DLTSAL(DAY)=',DLTSAL(DAY)
        PRINT *,'LLTSAL(DAY,GIND)=',LLTSAL(DAY,GIND)

        IF(DLTSAL(DAY).NE.LLTSAL(DAY,GIND).AND.DAY.NE.2) THEN
          PRINT *,'Discrepancie error ups....'      
C	  WRITE(6,901) IAM(),GTNAMES(TLTO),GIND,DAY,
C     *	               CMONY(DLTSAL(DAY),12,BETUNIT),
C     *		       CMONY(LLTSAL(DAY,GIND),12,BETUNIT)
C	  CALL GPAUSE
	ENDIF
1100	CONTINUE

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC               
C       The record access block (RAB) defines run-time options for a record stream and   C                                       
C       for individual operations within a predefined record stream context. After you   C                                       
C       connect the file to a record stream and associate the record stream with a FAB   C                                       
C       you use the RAB fields to specify the next record you want to access and to      C                                       
C       identify appropriate record characteristics.                                     C       
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC

C        print '(A)',GFNAMES(1,7)
C        print *,GFNAMES(1,7)


C        print *,GLNAMES(1,7)
C        print *,GLNAMES(2,7)
C        print *,GLNAMES(3,7)
C        print *,GLNAMES(4,7)
C        print '(A)',GLNAMES(1,7)+GLNAMES(2,7)+GLNAMES(3,7)+GLNAMES(4,7)
        print '(A)'
        print '(A)'

        GOTO 10  

400     CONTINUE
        

        print '(A)'
        print '(A)'
C       PARAMETER(NUMLTO=4)   !NUMBER OF LOTTO GAMES  

        PRINT *,'----------BEGIN--------GNINVALL---------------------'
C        DO gnum = 1,MAXGAM
C            WRITE(6,2100) 'File Name:',(GFNAMES(K,GNUM),K=1,5),GNUM
C        ENDDO

        WRITE(6,2100) '(totobola normal):',(GFNAMES(K,1),K=1,5),1
        WRITE(6,2100) '(totoloto Sabado):',(GFNAMES(K,6),K=1,5),6
        WRITE(6,2100) '(totoloto Quarta):',(GFNAMES(K,7),K=1,5),7
        WRITE(6,2100) '(totobola extra):',(GFNAMES(K,10),K=1,5),10

        WRITE(6,2110) 'Validation file:',(SFNAMES(k,VLF),k=1,5) !sfnames(1,vlf)

        icdc = daycdc-1
        fcdc = icdc
C-----------------------------------------------------------------------
C V06 - Allowing daily generations of AM_WAG file
C       initializing gfil array with default values
C-----------------------------------------------------------------------
        do gnum = 1,MAXGAM
            do xday = 1,7
                gfil(xday,gnum) = 'NONE'
            enddo
        enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C-----------------------------------------------------------------------
C V06 - Allowing daily generations of AM_WAG file
C-----------------------------------------------------------------------
      do gnum = 1,MAXGAM
        gtyp = gnttab(GAMTYP,gnum)
        if(gtyp.gt.0.and.gtyp.ne.TPAS) then   ! NO PASSIVE, ONLY ACTIVE GAMES
          temp = 0
          do gcdc = icdc,fcdc
            write(cfil,9000) P(REG_DRWPCK),GSNAMES(gnum),gcdc
            write(6,9010) P(REG_DRWPCK),GSNAMES(gnum),gcdc
            inquire(file=cfil,exist=xfil)
            if(xfil)then
              gfil(fcdc-gcdc+1,gnum) = cfil
              temp = temp + 1
            else
              gfil(fcdc-gcdc+1,gnum) = 'NONE'
            endif
          enddo
          write(cfil,'(4A4)') (glnames(offs,gnum),offs=1,4)
          if(temp.eq.0) then
            if(daydrw(gnum).gt.0) then
              WRITE(*,*),'>>> ERROR: ',cfil,' ACTIVE but no files were FOUND'
            else
              WRITE(*,*),cfil,' NOT ACTIVE'
            endif
          elseif(temp.eq.7) then
            if(daydrw(gnum).gt.0) then
              WRITE(*,*),cfil,' ACTIVE'              
            else
              WRITE(*,*),'>>> WARNING: ',cfil,' ACTIVE but no draws are configured'
            endif
          else
            WRITE(*,*),'>>> WARNING: ',cfil,' ACTIVE ',temp,' days <<<'              
          endif
        else
          do gcdc = icdc,fcdc
            gfil(fcdc-gcdc+1,gnum) = 'NONE'
          enddo
        endif
      enddo
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC        
        do gnum=1,MAXGAM
            do xday=1,7          
                IF(gfil(xday,gnum) .NE. 'NONE')  WRITE(6,2120) 'gfil file:',gfil(xday,gnum)
            enddo
        enddo 

        PRINT *,'----------END--------GNINVALL---------------------'

        DO GIND=1,NUMLTO
          DRAW=LLTDRW(GIND)
          GNUM=GTNTAB(TLTO,GIND)
          
                  
          PRINT *,'DRAW:',DRAW
          PRINT *,'GNUM;',GNUM

          WRITE(6,1900) (GLNAMES(K,GNUM),K=1,4),(GFNAMES(K,GNUM),K=1,5),GNUM
          WRITE(6,2900) (GAMFDB(k,GNUM),k=1,7)

        ENDDO

        print '(A)'
C        print *,'Validation File Name:',SFNAMES(1,VLF)
        WRITE(6,3010) SFNAMES(1,VLF)
        WRITE(6,3010) SFNAMES(2,VLF)
        WRITE(6,3010) SFNAMES(3,VLF)
        WRITE(6,3010) SFNAMES(4,VLF)
        WRITE(6,3010) SFNAMES(5,VLF)
        WRITE(6,3000) (SFNAMES(k,VLF),k=1,5),(SFNAMES(k,TCF),k=1,5)
        print '(A)'

        GIND=4
        DO 1200 DAY=1,LTGENT
          print '(A)'               
          print *,'DAY:',DAY  
          print *,'LLTSAL(DAY,GIND)=',LLTSAL(DAY,GIND)  
          print *,'LTOSAL(DAY,GIND)=',LTOSAL(DAY,GIND)    
          IF(DAY.EQ.LTGENT.OR.DAY.EQ.2) THEN
                  LLTSAL(DAY,GIND) = LLTSAL(DAY,GIND) + DLTSAL(DAY)
                  PRINT *,'...Day 2 or Last Day...'
                  PRINT *,'LLTSAL(DAY,GIND)=',LLTSAL(DAY,GIND)
                  GOTO 1200
          ENDIF 
          print '(A)'          
1200      CONTINUE

        GOTO 10      

500     CONTINUE

        print '(A)','----Here----'  
        print '(A)'
        print '(A)'  
        PRINT *,'OPENING TCF FILE FOR READ'
C       HEADER OFFSETS          
C       PARAMETER (LCDC = 3)       !INTEGER*2 OFFSET FOR HASH FILES
C       PARAMETER (LSER = 1)       !INTEGER*4   
        WRITE(6,3000) (SFNAMES(k,VLF),k=1,5),(SFNAMES(k,TCF),k=1,5)  
        CALL IOPEN(SFNAMES(1,TCF),TCF,LREC*2,LCDC,LSER*2-1,ST)
        IF(ST.NE.0) THEN
C          CALL FILERR(SFNAMES(1,TCF),1,ST,0)
          PRINT *,'ERROR OPENING TCF FILE (Carry Over), error',ST 
          CALL ICLOSE(TCF,TCFBUF,ST)      
          CALL GSTOP(GEXIT_SUCCESS)
        ENDIF
        PRINT *,'START READ OF TCF FILE'
        CALL ISREAD(LOGBUF,TCF,TCFBUF,ST)
        IF(ST.EQ.ERREND) THEN
          PRINT *,'ERROR IN READ TCF FILE STATUS'
     *      //'END OF FILE DETECTED:',ST      
          CALL ICLOSE(TCF,TCFBUF,ST)
          GOTO 10
        ENDIF
        CALL LOGTRA(TRABUF,LOGBUF)
        AMT = TRABUF(TWAMT)
        PRINT *,'Amount of Sales in TCF:',AMT
        CALL ICLOSE(TCF,TCFBUF,ST)
C        LLTSAL(DATIND,GIND)

        GOTO 10         

600     CONTINUE
C        WRITE(5,3900) (K,GTNAMES(K),K=1,MAXTYP) 
C        CALL INPNUM_AUX('Enter game type ',GTYP,1,MAXTYP,EXT)
C        PRINT *,'GAME TYPE CHOSEN IS:',GTYP

C	SET PARAMETERS FOR 'GETJPI' CALL
C
	I2ITEM(1) = 7
	I2ITEM(2) = JPI$_TERMINAL
C       Getting addresses by the %LOC function
	I4ITEM(2) = %LOC(TN)
	I4ITEM(3) = %LOC(TNL)
	I4ITEM(4) = 0		    !TO TERMINATE LIST
        ST = SYS$GETJPIW( ,%VAL(0),,I4ITEM,,,)

        TNAME = TN
	NDX=INDEX(TNAME,':')
	IF(NDX.EQ.0)THEN
	  TNAME(8:8)=':'
	ENDIF

        PRINT *,'process name:',TN
        PRINT *,'process name length:',TNL
        PRINT *,'process final name:',TNAME
        print '(A)'
        print '(A)'

        GOTO 10  

700     CONTINUE
 
C       8 bytes = 18446744073709551615 max value unsigned
C       signed is 9 223 372 036 854 775 807,5 -> 9 223 372 036 854 775 807
C                BIG_VALUE = 9223372036854775808.0 !FFFFFFFFFFFFFFFF (8000000000000000) hexa signed
C                BIG_VALUE = 18446744073709551615.0 !FFFFFFFFFFFFFFFF hexa unsigned 
C       DOUBLE PRECISION means 64 bits thats 8 bytes

C                BIG_VALUE = 18446744073709551615.0 !9223372036854775000.0 
C                BIG_VALUE = DFLOAT('FFFFFFFF'X) !4 bytes

CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C err at compilation time:
C %F90-E-ERROR, This is an illegal octal, hexadecimal, or binary character; 
C or, the value of the constant is too
C large or too small.   ['FFFFFFFFFFFFFFFFFF'X]
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C                BIG_VALUE = 4722366482869645213695 !9 bytes
C                BIG_VALUE = DFLOAT('FFFFFFFFFFFFFFFFFF'X) !9 bytes
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                
                BIG_VALUE = DFLOAT('FFFFFFFFFFFFFFFF'X) !8 bytes gives -1 so end of overflow no more positve range from here

C                BIG_VALUE = DFLOAT('8000000000000000'X) !negative value (overflow) -> -9223372036854775808.0
C                BIG_VALUE = DFLOAT('7FFFFFFFFFFFFFFF'X) !positive value -> 9223372036854775808.0
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
C %F90-E-ERROR, When converting this constant to a REAL(KIND=4) number, underflow or overflow occurred.   
C [340282366920938463463374607431768211455.0]
C                BIG_VALUE = 340282366920938463463374607431768211455.0 !16bytes REAL*16 test for double precision (8 or 16 bytes)???
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC                
                WRITE(6,970) BIG_VALUE
                WRITE(BIG_STR,990) BIG_VALUE
                WRITE(6,980) BIG_STR
C                BIG_VALUE_3 = BIG_VALUE*3
C                WRITE(6,1000) BIG_VALUE_3 
        BIG = 999999.96 !the max value before giving error is 999999.96 that in SYS$OUTPUT converts to 999999.94 for some reason...(compile related specific??)
C        BIG=(DFLOAT(AMOUNT(1))*MON_UNIT+DFLOAT(AMOUNT(2)))/DOLL_BASE
        ENCODE(9,8200,CTEMP) BIG
        WRITE(6,8300) CTEMP


        GOTO 10           

800     CONTINUE
        PRINT *, 'WRONG OPTION .... ITS NOT 8 BUT 9'
        GOTO 10         

900     CONTINUE
        PAR.OLMCON = 1 - PAR.OLMCON
        GOTO 10 
        
C1000    CONTINUE
C        PAR.REGLOG = 1 - PAR.REGLOG
C        GOTO 10   

2000    CONTINUE
        print *,'!!!Error in closing file!!!'
        GOTO 10

3900     FORMAT(//,' Game share/tax report',//,                               
     *         <MAXTYP>(1X,I2,' - ',A8,/))    
9012    FORMAT(15X,'9. Connect to Olimpo',T45,1X,A3)  
1900	FORMAT(1X,"Game Name:",4A4,3X,'File Name: ',5A4,3X,'For Game Num:',I1)
2500    FORMAT('Validation File Name',5A4,3X,'Validation File Name',5A4,3X)


2100    FORMAT(1X,'gninvall - File Name',15A,5A4,3X,'For Game Num:',I1)
2110    FORMAT(1X,15A,3X,5A4)
2120    FORMAT(1X,15A,3X,20A)
9000    format(A4,':',A4,I4.4,'.FIL') 
9010    format(3X,A4,':',A4,I4.4,'.FIL') 


2900    FORMAT(1X,'File Name: ',7A4)
3000    FORMAT(2X,'Validation File Name:',5A4,2X,'Carry Over File:',5A4)
3010    FORMAT(2X,'System File Name:',A4)

C990     FORMAT(F22.0)
970     FORMAT(3X,"8 bytes       : ",F22.1)
990     FORMAT(F22.0)
980     FORMAT(3X,"(Double Float): ",22A)
1000    FORMAT(3X,"> 8 bytes     : ",F22.1)
8200    FORMAT(F9.2)
8300    FORMAT(3X,A9)

      END

      
      SUBROUTINE INPNUM_AUX(STRING,NUM,LOW,HIGH,EXT)
      IMPLICIT NONE
C
      CHARACTER   STRING*(*)
      INTEGER*4   NUM,LOW,HIGH,EXT
C
      CALL XXXNUM(.FALSE., STRING,NUM,LOW,HIGH,EXT)
      RETURN
      END