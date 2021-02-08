      PROGRAM OLMCON
        IMPLICIT NONE
        INCLUDE 'INCLIB:SETUP.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
C        INCLUDE 'OLMCON.DEF'


C       show the status of P(SUPSYN)

        RECORD/SETUP/ PAR

        PRINT *, 'CDC DATE:',P(NXTCDC)

        PRINT *,'P(OLMCONF)=',P(OLMCONF)
        PAR.OLMCON = P(OLMCONF)    

        PRINT *,'P(SUPSYN)=',P(SUPSYN) 
        PRINT *,'P(SUPWEL)=',P(SUPWEL) 
        PRINT *,'P(NUMENC)=',P(NUMENC) 
        PRINT *,'P(DESFLG_TYPE)=',P(DESFLG_TYPE) 
C       (DESFLG_HARD=1)    !HARD DES ENCRYPTIO        
        PRINT *,'DESFLG_HARD=',1 
        PRINT *,'P(ENCPRO_WAIT)=',P(ENCPRO_WAIT) 
        PRINT *,'P(LOGTIM)=',P(LOGTIM) 
        PRINT *,'P(SUPSUM)=',P(SUPSUM) 

C        GNUM=GTNTAB(TLTO,GIND)
C        PRINT *,'File Name:',GFNAMES(1,GNUM)     

        CALL DISP_CONFIG(PAR)
        PRINT *,'GOOD .....EXITED SUBROUTINE'

        P(OLMCONF) = PAR.OLMCON
        PRINT *,'P(OLMCONF)=',P(OLMCONF)
C        PRINT *,P(OLMCONF)


      END

      SUBROUTINE DISP_CONFIG(PAR)
        IMPLICIT NONE  
        INCLUDE 'INCLIB:SETUP.DEF'
C        INCLUDE 'OLMCON.DEF'


        CHARACTER*3 YESNO(0:1)
        DATA YESNO  /' No','Yes'/
        INTEGER*4 OPT,ST
        CHARACTER OPTION
c      LOGICAL 

        RECORD/SETUP/ PAR

10	CONTINUE        

        WRITE(6,9012) YESNO(PAR.OLMCON)
        PRINT *, 'ENTER OPTION 9 AND ACTIVATE OMLCON'
        READ *, OPTION

        IF(OPTION .EQ. 'E' .OR. OPTION .EQ. 'e') THEN
                PRINT *, 'pressed e...'
                RETURN
        ENDIF

C       ICHAR gets the first char and gets its position in the ASCII TABLE  
C       knowing that 0 char corresponde to position 48 then subtrating this
C       value with the caracther insert input return the numerial value inserted     
        OPT = ICHAR(OPTION) - ICHAR('0') 

        PRINT *, 'GO TO OPT....'
        GOTO(100,200,300,400,500,600,700,800,900) OPT

        
100     CONTINUE
        PRINT *, 'WRONG OPTION .... ITS NOT 1 BUT 9'
        GOTO 10  

200     CONTINUE
        PRINT *, 'WRONG OPTION .... ITS NOT 2 BUT 9'
        GOTO 10      
        
300     CONTINUE
        PRINT *, 'WRONG OPTION .... ITS NOT 3 BUT 9'
        GOTO 10  

400     CONTINUE
        PRINT *, 'WRONG OPTION .... ITS NOT 4 BUT 9'
        GOTO 10      

500     CONTINUE
        PRINT *, 'WRONG OPTION .... ITS NOT 5 BUT 9'
        GOTO 10         

600     CONTINUE
        PRINT *, 'WRONG OPTION .... ITS NOT 6 BUT 9'
        GOTO 10  

700     CONTINUE
        PRINT *, 'WRONG OPTION .... ITS NOT 7 BUT 9'
        GOTO 10           

800     CONTINUE
        PRINT *, 'WRONG OPTION .... ITS NOT 8 BUT 9'
        GOTO 10         

900     CONTINUE
        PAR.OLMCON = 1 - PAR.OLMCON
        GOTO 10        

9012    FORMAT(15X,'9. Connect to Olimpo',T45,1X,A3)  


      END