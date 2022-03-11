        PROGRAM DISCREPANCY
          IMPLICIT NONE
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          INCLUDE 'INCLIB:GLOBAL.DEF'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          INCLUDE 'INCLIB:LTOCOM.DEF'      
          INCLUDE 'INCLIB:CONCOM.DEF'
CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          INCLUDE 'INCLIB:SYSDEFINE.DEF'      
          INCLUDE 'INCLIB:DLTREC.DEF'
          INCLUDE 'INCLIB:LLTREC.DEF'
CCCCCCCCCCCCCCCCCCC Data in Memory of each day after each wagpro/cancellationCCCCCCCCCCCCCCCCCCCCCCCCCCC
C         DATIND ==== 3
          INTEGER*4 DATIND,GIND

          GIND = 4 !NUMLTO
          DATIND = LTODAT(CURDRW,GIND) - DAYCDC + 3
          
          WRITE(*,*) 'DATIND: ',DATIND 
          WRITE(*,*) 'WAGPRO (Sales Data in Memory): ',LTOSAL(DATIND,GIND),' (Centimos)'

CCCCCCCCCCC Read the Data from file in format CCCCCCCCCCCCCCCCCCCCCCCCCCCC

C          CALL OPENW(1,GFNAMES(1,GNUM),4,0,0,ST)      
C          CALL IOINIT(FDB,1,DLTSEC*256)   
           
C this point GAMLOG basicaly is making a copy o DLTREC into LTCOM where LTOSTS is the first field of
C LTCOM that serves as pointer to this structer/array  
C          CALL GAMLOG(TLTO,GIND,DLTREC,LTOSTS)
C          CALL WRITEW(FDB,DRAW,DLTREC,ST) 
          
C          CALL READW(FDB,DRAW,DLTREC,ST)
CCCCCCCCCCCCCCCCCCCCC Data in Memory of each day after rotation of the day CCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCCC
          WRITE(*,*) 'MULTIWI (Sales Data in Memory from Record): ',LLTSAL(DATIND,GIND),' (Centimos)'
          

        END