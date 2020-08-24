C
C SUBROUTINE CMDPAR
C
C CMDPAR.FOR
C
C V09 31-MAR-2016 SCML M16 PROJECT: added EUSPGWAG, EUSPGGRR and EUSPGICA
C                      Fixed message number of IGSPGICAN from 2 to 36
C V08 28-APR-2014 SCML Placard Project
C V07 01-JAN-2010 FJG ePassive
C V06 21-JAN-2000 OXK Refix for SUPRPT.
C V05 19-JAN-2000 UXN Fix for SUPRPT.
C V04 16-MAR-1997 WPW Changed to process PRMSTR command.
C V03 21-JAN-1993 DAB Initial Release
C                     Based on Netherlands Bible, 12/92, and Comm 1/93 update
C                     DEC Baseline
C V02 08-MAR-1991 JPJ INITIAL RELEASE FOR MARYLAND
C V01 01-AUG-1990 XXX RELEASED FOR VAX
C
C SUBROUTINE TO CHANGE SYSTEM PARAMETERS FOR COMMAND PROCESSING
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
C Copyright 1991 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS /CHECK=NOOVERFLOW
        SUBROUTINE CMDPAR(TRABUF,MESS)
        IMPLICIT NONE
C
        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:TASKID.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:DESTRA.DEF'
        INCLUDE 'INCLIB:PRMPRO.DEF'
        INCLUDE 'INCLIB:CRSCOM.DEF'
        INCLUDE 'INCLIB:TCPEVN.DEF'

        INCLUDE '($SYSSRVNAM)'

        INTEGER*4 MESS(EDLEN), ST, PARNUM
        INTEGER*4 STATUS
C
C
C
        PARNUM=TRABUF(TCMNUM)
        IF(PARNUM.LT.1.OR.PARNUM.GT.NUMPAR) THEN
          TRABUF(TSTAT)=REJT
          TRABUF(TERR)=INVL
          MESS(2)=TECMD
          MESS(3)=1
          MESS(4)=TRABUF(TCMTYP)
          MESS(5)=TRABUF(TCMNUM)
          RETURN
        ENDIF
C
C CHECK FOR PRIMARY/SECONDARY TAPE SWITCH
C
        IF(TRABUF(TCMNUM).EQ.TAPESW) THEN
          MESS(2)=0
          IF(TRABUF(TCMDT1).EQ.0.AND.P(SYSTYP).EQ.LIVSYS) THEN
            TRABUF(TCMOLD)=P(TAPESW)
            P(TAPESW)=TRABUF(TCMNEW)
            CALL RELSE(TSKNAM(TAP),ST)
            MESS(2)=TECMD
            MESS(3)=2
            MESS(8)=TRABUF(TCMOLD)
            MESS(9)=TRABUF(TCMNEW)
            RETURN
          ENDIF
          IF(TRABUF(TCMDT1).NE.0.AND.P(SYSTYP).NE.LIVSYS) THEN
            TRABUF(TCMOLD)=P(TAPESW)
            P(TAPESW)=TRABUF(TCMNEW)
            CALL RELSE(TSKNAM(TAP),ST)
            MESS(2)=TECMD
            MESS(3)=2
            MESS(8)=TRABUF(TCMOLD)
            MESS(9)=TRABUF(TCMNEW)
            RETURN
          ENDIF
          RETURN
        ENDIF
C
C SET PRIMARY STRATUS NUMBER
C
        IF(TRABUF(TCMNUM).EQ.PRMSTR) THEN

          STATUS=SYS$ASCEFC(%VAL(TC_EVNTIMER),TC_EVNNAME,0,0)
          IF(.NOT.STATUS) CALL LIB$SIGNAL(%VAL(STATUS))

          IF(P(SYSTYP).EQ.LIVSYS.AND.TCP_CONNSTS.EQ.TCCONN) THEN
            CALL TCPQUEUE(DISASST,ST)
          ENDIF
          TRABUF(TCMOLD)=P(PARNUM)
          P(PARNUM)=TRABUF(TCMNEW)
C
C         IF PRIMARY STRATUS HAS CHANGED RE-QUE THE TASK
C
          IF(P(SYSTYP).EQ.LIVSYS) THEN
            CALL TCPQUEUE(ACONASST,ST)
          ENDIF
          MESS(2)=TECMD
          MESS(3)=2
          MESS(8)=TRABUF(TCMOLD)
          MESS(9)=TRABUF(TCMNEW)
          RETURN
        ENDIF
C
C
        IF(PARNUM.EQ.SUPGWA.OR.PARNUM.EQ.SUPGCA.OR.
     *     PARNUM.EQ.SUPGVA.OR.PARNUM.EQ.SUPGRE) THEN
            TRABUF(TCMOLD) = P(PARNUM)
            TRABUF(TCMDT1) = P(PARNUM+1)
            P(PARNUM)      = TRABUF(TCMNEW)
            P(PARNUM+1)    = TRABUF(TCMDT2)
            MESS(2)  = TECMD
            MESS(3)  = 30
            MESS(9)  = JISHFT(TRABUF(TCMOLD),-1)
            IF(IAND(TRABUF(TCMDT1),1).EQ.1) MESS(9)=IOR(MESS(9),'80000000'X)
            MESS(8)  = JISHFT(TRABUF(TCMDT1),-1)
            MESS(11) = JISHFT(TRABUF(TCMNEW),-1)
            IF(IAND(TRABUF(TCMDT2),1).EQ.1) MESS(11)=IOR(MESS(11),'80000000'X)
            MESS(10) = JISHFT(TRABUF(TCMDT2),-1)
            RETURN
        ENDIF
C
C SUPRPT NEEDS MORE THHAN 64 BITS SO IT'S DONE SEPARATELY
C
        IF(PARNUM.EQ.SUPRPT) THEN
            TRABUF(TCMOLD) = P(PARNUM)
            TRABUF(TCMDT1) = P(PARNUM+1)
            TRABUF(TCMDT3) = P(PARNUM+2)
            P(PARNUM)      = TRABUF(TCMNEW)
            P(PARNUM+1)    = TRABUF(TCMDT2)
            P(PARNUM+2)    = TRABUF(TCMDT4)
            MESS(2)  = TECMD
            MESS(3)  = 30
            MESS(9)  = JISHFT(TRABUF(TCMOLD),-1)
            IF(IAND(TRABUF(TCMDT1),1).EQ.1) MESS(9)=IOR(MESS(9),'80000000'X) 
            MESS(8)  = JISHFT(TRABUF(TCMDT1),-1)
            MESS(11) = JISHFT(TRABUF(TCMNEW),-1)
            IF(IAND(TRABUF(TCMDT2),1).EQ.1) MESS(11)=IOR(MESS(11),'80000000'X) 
            MESS(10) = JISHFT(TRABUF(TCMDT2),-1)
            RETURN
        ENDIF
C
C V08 - Start
C
        IF(PARNUM .EQ. IGSPGWAG .OR. PARNUM .EQ. IGSPGCAN .OR.
     *     PARNUM .EQ. IGSPGVAL .OR. PARNUM .EQ. IGSPGREP .OR.
     *     PARNUM .EQ. IGSPGFIN .OR. PARNUM .EQ. IGSPGRNT .OR.
     *     PARNUM .EQ. IGSPGICAN) THEN                                          !V09
            TRABUF(TCMOLD) = P(PARNUM)
            P(PARNUM)      = TRABUF(TCMNEW)
            MESS(2)  = TECMD
            MESS(3)  = 36
            MESS(9)  = TRABUF(TCMOLD)
            MESS(11) = TRABUF(TCMNEW)
            RETURN
        ENDIF
C
C V08 - End
C
C----+---+-------------+------------------------------------------------
C V09|BEG| M16 PROJECT | New EUR (VISION) commands
C----+---+-------------+------------------------------------------------
        IF(PARNUM .EQ. EUSPGWAG .OR. PARNUM .EQ. EUSPGGRR .OR.
     *     PARNUM .EQ. EUSPGICA) THEN
            TRABUF(TCMOLD) = P(PARNUM)
            P(PARNUM)      = TRABUF(TCMNEW)
            MESS(2)  = TECMD
            MESS(3)  = 36                                                       !MESSAGE NUMBER (SEE MESCMD.FOR FOR MESSAGE FORMAT)
            MESS(9)  = TRABUF(TCMOLD)
            MESS(11) = TRABUF(TCMNEW)
            RETURN
        ENDIF
C----+---+-------------+------------------------------------------------
C V09|END| M16 PROJECT | New EUR (VISION) commands
C----+---+-------------+------------------------------------------------
C
        TRABUF(TCMOLD)=P(PARNUM)
        P(PARNUM)=TRABUF(TCMNEW)
        MESS(2)=TECMD
        MESS(3)=2
        MESS(8)=TRABUF(TCMOLD)
        MESS(9)=TRABUF(TCMNEW)
        RETURN
        END
