C
C V01 30-MAR-2010 RXK Initial release.
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
C Copyright 1997 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW/EXT
        SUBROUTINE UPDPAS(TRABUF)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'
        INCLUDE 'INCLIB:PASIOSUBS.DEF'
C
        INTEGER*4 GIND
        INTEGER*4 DOFF
        INTEGER*4 RNUM
        INTEGER*4 EMIS
        INTEGER*4 XNUM
        INTEGER*4 XFRA
        INTEGER*4 XSER
        INTEGER*4 I        

        INTEGER*4 GETPAGEMI

!===============================================================================
!       THIS SHOULD BE INCLUDED IN SUBRUTINES THAT USES TPFS ONLINE
!===============================================================================
        RECORD /STPASFDB/  PASFDB(PAGEMI,NUMPAS)
        RECORD /STPASREC/  PASREC
!
        COMMON /PASTRUCT/ PASFDB        
!=============================================================================== 
        GIND = TRABUF(TGAMIND)
        EMIS = GETPAGEMI(TRABUF(TWBEG),GIND)
        DOFF = PASSALTAB(EMIS,GIND)  ! SHOULD BE CHECKED IN DNAC
C
C EPASSIVE WAGERS
C MARK NUMBER AS RESERVED
C
       IF(TRABUF(TTYP).EQ.TWAG) THEN  
          IF(TRABUF(TWEPOP).EQ.EPASRES) THEN
             DO I=0,TRABUF(TWEPNR)-1 
                RNUM = TRABUF(TWEPRES1+I)
                IF(GIND.EQ.PSBCLA) THEN     
                   PASNUMCLA(RNUM,DOFF).RESTER = TRABUF(TTER)
                   PASNUMCLA(RNUM,DOFF).RESTIM = TRABUF(TWEPSD)
                ELSE 
                   PASNUMPOP(RNUM,DOFF).RESTER = TRABUF(TTER)
                   PASNUMPOP(RNUM,DOFF).RESTIM = TRABUF(TWEPSD)
                ENDIF
             ENDDO
             RETURN
C
C MARK FRACTION AS SOLD
C
          ELSEIF(TRABUF(TWEPOP).EQ.EPASSAL) THEN
            XNUM = TRABUF(TWEPSN)
            XSER = TRABUF(TWEPSS)
            XFRA = TRABUF(TWEPSF)
            CALL PASIO_READ(PASFDB(EMIS,GIND),XNUM,XSER,XFRA,PASREC)
!-----------INI IF 01-----------------------------------------------------------
            IF(PASFDB(EMIS,GIND).ERR.NE.IOE_NOERR) THEN
              TRABUF(TSTAT)= REJT
              TRABUF(TERR) = EPIO
              PASIOERRS(IOREANO,EMIS,GIND) = PASIOERRS(IOREANO,EMIS,GIND) + 1
            ELSE
              PASIOERRS(IOREAOK,EMIS,GIND) = PASIOERRS(IOREAOK,EMIS,GIND) + 1     
!-------------INI IF 02---------------------------------------------------------
              IF(PASREC.STAT.EQ.PBILCON) THEN
                 IF(DAYCDC-PASREC.CDC.GE.P(PDAYRSL)) PASREC.STAT = PBILONL
              ENDIF
              IF(PASREC.STAT.EQ.PBILONL) THEN
                PASREC.STAT   = PBILSON
                PASREC.CDC    = DAYCDC
                PASREC.SERIAL = TRABUF(TSER)
                PASREC.AGT    = TRABUF(TTER)
                CALL PASIO_SWRITE(PASFDB(EMIS,GIND),XNUM,XSER,XFRA,PASREC)
!---------------INI IF 03-------------------------------------------------------
                IF(PASFDB(EMIS,GIND).ERR.NE.IOE_NOERR) THEN
                  TRABUF(TSTAT)= REJT
                  TRABUF(TERR) = EPIO
                  PASIOERRS(IOWRINO,EMIS,GIND) = PASIOERRS(IOWRINO,EMIS,GIND) + 1                  
                ELSE
                  PASIOERRS(IOWRIOK,EMIS,GIND) = PASIOERRS(IOWRIOK,EMIS,GIND) + 1                                    
!-----------------INI IF 04-----------------------------------------------------
                  IF(GIND.EQ.PSBCLA) THEN     
                    PASNUMCLA(XNUM,DOFF).BILLET(XSER,XFRA) = PBILSON
                    PASNUMCLA(XNUM,DOFF).FORSAL = PASNUMCLA(XNUM,DOFF).FORSAL - 1
                  ELSE
                    PASNUMPOP(XNUM,DOFF).BILLET(XSER,XFRA) = PBILSON
                    PASNUMPOP(XNUM,DOFF).FORSAL = PASNUMPOP(XNUM,DOFF).FORSAL - 1
                  ENDIF                  
!-----------------FIN IF 04-----------------------------------------------------
                ENDIF
!---------------FIN IF 03-------------------------------------------------------                
              ELSE
                TRABUF(TSTAT)= REJT
                TRABUF(TERR) = EPNR
              ENDIF                             
!-------------FIN IF 02---------------------------------------------------------              
            ENDIF 
!-----------FIN IF 01-----------------------------------------------------------            
            RETURN
C
C RELEASE FRACTIONS
C
          ELSEIF(TRABUF(TWEPOP).EQ.EPASREL) THEN  
             DO I=0,TRABUF(TWEPNR)-1
                RNUM = TRABUF(TWEPRES1+I) 
                IF(GIND.EQ.PSBCLA) THEN
                   IF(PASNUMCLA(RNUM,DOFF).RESTER.EQ.TRABUF(TTER)) THEN
                      PASNUMCLA(RNUM,DOFF).RESTER = 0
                      PASNUMCLA(RNUM,DOFF).RESTIM = 0
                   ENDIF
                ELSE
                   IF(PASNUMPOP(RNUM,DOFF).RESTER.EQ.TRABUF(TTER)) THEN
                      PASNUMPOP(RNUM,DOFF).RESTER = 0
                      PASNUMPOP(RNUM,DOFF).RESTIM = 0
                   ENDIF
                ENDIF
             ENDDO 
             RETURN
          ENDIF
C
C CANCELLATIONS
C RELEASE CANCELLED NUMBER
C
100    ELSEIF(TRABUF(TTYP).EQ.TCAN) THEN 
          XNUM = TRABUF(TWEPSN)
          XSER = TRABUF(TWEPSS)
          XFRA = TRABUF(TWEPSF)
          CALL PASIO_READ(PASFDB(EMIS,GIND),XNUM,XSER,XFRA,PASREC)
!---------INI IF 01-------------------------------------------------------------
          IF(PASFDB(EMIS,GIND).ERR.NE.IOE_NOERR) THEN
            TRABUF(TSTAT)= REJT
            TRABUF(TERR) = EPIO
            PASIOERRS(IOREANO,EMIS,GIND) = PASIOERRS(IOREANO,EMIS,GIND) + 1
          ELSE
            PASIOERRS(IOREAOK,EMIS,GIND) = PASIOERRS(IOREAOK,EMIS,GIND) + 1     
!-----------INI IF 02-----------------------------------------------------------
            IF(PASREC.STAT.EQ.PBILSON) THEN
              IF(P(PDAYRSL).LE.0) THEN
                PASREC.STAT   = PBILONL
              ELSE
                PASREC.STAT   = PBILCON                  
              ENDIF
              PASREC.CDC    = DAYCDC
              PASREC.SERIAL = TRABUF(TSER)
              PASREC.AGT    = TRABUF(TTER)
              CALL PASIO_SWRITE(PASFDB(EMIS,GIND),XNUM,XSER,XFRA,PASREC)
!-------------INI IF 03---------------------------------------------------------
              IF(PASFDB(EMIS,GIND).ERR.NE.IOE_NOERR) THEN
                TRABUF(TSTAT)= REJT
                TRABUF(TERR) = EPIO
                PASIOERRS(IOWRINO,EMIS,GIND) = PASIOERRS(IOWRINO,EMIS,GIND) + 1                  
              ELSE
                PASIOERRS(IOWRIOK,EMIS,GIND) = PASIOERRS(IOWRIOK,EMIS,GIND) + 1                                    
!---------------INI IF 04-------------------------------------------------------
                IF(P(PDAYRSL).LE.0) THEN
                  IF(GIND.EQ.PSBCLA) THEN     
                    PASNUMCLA(XNUM,DOFF).BILLET(XSER,XFRA) = PBILONL
                    PASNUMCLA(XNUM,DOFF).FORSAL = PASNUMCLA(XNUM,DOFF).FORSAL + 1
                  ELSE
                    PASNUMPOP(XNUM,DOFF).BILLET(XSER,XFRA) = PBILONL
                    PASNUMPOP(XNUM,DOFF).FORSAL = PASNUMPOP(XNUM,DOFF).FORSAL + 1
                  ENDIF                  
                ELSE
                  IF(GIND.EQ.PSBCLA) THEN     
                    PASNUMCLA(XNUM,DOFF).BILLET(XSER,XFRA) = PBILCON
                  ELSE
                    PASNUMPOP(XNUM,DOFF).BILLET(XSER,XFRA) = PBILCON                      
                  ENDIF
                ENDIF
!---------------FIN IF 04-------------------------------------------------------
              ENDIF
!-------------FIN IF 03---------------------------------------------------------              
            ELSE
              TRABUF(TSTAT)= REJT
              TRABUF(TERR) = EPNR
            ENDIF                             
!-----------FIN IF 02-----------------------------------------------------------              
          ENDIF 
!---------FIN IF 01-------------------------------------------------------------   
       ENDIF
       RETURN
C
       END
