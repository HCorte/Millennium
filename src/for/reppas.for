C
C REPPAS.FOR
C
C V01 23-APR-2010 RXK Separated from PASPRO. Changes for ePassive
C
C PASSIVE RE-PROCESSING TASK
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
C Copyright 2010 GTECH Corporation. All rights reserved.
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C=======OPTIONS    /CHECK=NOOVERFLOW
	SUBROUTINE REPPAS(STAT,OPENST)
	IMPLICIT   NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:PROCOM.DEF'
	INCLUDE 'INCLIB:DESTRA.DEF'
	INCLUDE 'INCLIB:TASKID.DEF'
	INCLUDE 'INCLIB:QUECOM.DEF'
        INCLUDE 'INCLIB:GLIST.DEF'
        INCLUDE 'INCLIB:PASCOM.DEF'

	INTEGER*4 STAT, OPENST
        INTEGER*4 ERRCNT,BUF,ST,EMIS,ELIST(10)
        INTEGER*4 I

        INTEGER*4 GETPAGEMI

        CHARACTER*1  BELL/Z07/
    
        INTEGER*4 INAME(5)
        CHARACTER*20 CNAME
        EQUIVALENCE(INAME,CNAME)
C
        CALL LISTTOP(BUF,REPQUEPAS(1,RQPASPRO),STAT)
        IF(STAT.EQ.GLIST_STAT_EMPTY) RETURN
        IF(OPENST.NE.0) THEN
           CALL OPNIDX
           OPENST=0
        ENDIF
        CALL LOGTRA(TRABUF,PRO(WRKTAB,BUF))
C
        ERRCNT = 0
C
        IF(TRABUF(TTYP).EQ.TWAG) GOTO 100
        IF(TRABUF(TTYP).EQ.TCAN) GOTO 200
        IF(TRABUF(TTYP).EQ.TINC) GOTO 300
        IF(TRABUF(TTYP).EQ.TRET) GOTO 500
C
C REPROCESS WAGERS
C
100     CONTINUE
        IF(TRABUF(TWEPOP).EQ.EPASSAL) THEN
           CALL UPDSUB(TRABUF)
           CALL UPDPAS(TRABUF)
           IF(TRABUF(TERR).EQ.EPIO) GOTO 900
        ELSE
           CALL UPDPAS(TRABUF)
        ENDIF 
        GOTO 1000
C
C REPROCESS CANCELLATIONS AND DELETIONS
C
200     CONTINUE
300     CONTINUE
        CALL UPDSUB(TRABUF)
        CALL UPDPAS(TRABUF)
        IF(TRABUF(TERR).EQ.EPIO) GOTO 900
        GOTO 1000
C
C REPROCESS RETURNS
C
500     CONTINUE
        CALL UPDSUB(TRABUF)
        CALL FASTSET(0,ELIST,10) 
	CALL RPROUPD_PAS(TRABUF,ERRCNT,ELIST)
        IF(TRABUF(TERR).EQ.EPIO) THEN
           DO I=1,10
              IF(ELIST(I).EQ.0) GOTO 1000
              EMIS = ELIST(I)
              CALL FASTMOV(PASTPFFIL(1,EMIS,TRABUF(TGAMIND)),INAME(1),5)
              CALL OPSTXT('IO error if reprocessing '//CNAME//', serial '//
     *                     ITOC(TRABUF(TSER))//BELL//BELL//BELL) 
           ENDDO    
        ENDIF
        GOTO 1000
C
C IN THE CASE OF INPUT/OUTPUT ERROR SEND ERROR MESSAGE 
C
900     CONTINUE
        EMIS = GETPAGEMI(TRABUF(TWBEG),TRABUF(TGAMIND))
        CALL FASTMOV(PASTPFFIL(1,EMIS,TRABUF(TGAMIND)),INAME(1),5)
        CALL OPSTXT('IO error if reprocessing '//CNAME//', serial '//
     *               ITOC(TRABUF(TSER))//BELL//BELL//BELL)
C
C RELEASE BUFFERS
C
1000    CONTINUE
        CALL RELBUF(BUF)
        CALL RTL(BUF,REPQUEPAS(1,RQPASPRO),ST)
C
	RETURN
	END
C
!===============================================================================
!       SUBROUTINE TO OPEN TPF FILES
!===============================================================================
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
!===============================================================================
!       THIS SHOULD BE INCLUDED IN SUBRUTINES THAT USES TPFS
!===============================================================================
        record /stpasfdb/  pasfdb(pagemi,numpas)
!
        common /pastruct/ pasfdb        
!===============================================================================   
        do gind=1,numpas
          do emis=1,pagemi
            if(passubsts(emis,gind).eq.pdrwval.or. ! RETURNS AFTER DRAW FOR PRIV
     *         passubsts(emis,gind).eq.pdrwwag.or.
     *         passubsts(emis,gind).eq.pdrwret) then
              call pasio_init(pasfdb(emis,gind),gind,pasemis(emis,gind),pasnumtck(emis,gind)-1,
     *                        pasnumser(emis,gind),pasnoffra(emis,gind),cpastpffil(emis,gind))     
              call pasio_open(pasfdb(emis,gind))  
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
!===============================================================================
!       SUBROUTINE TO CLOSE TPF FILES
!===============================================================================
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
!===============================================================================
!       THIS SHOULD BE INCLUDED IN SUBRUTINES THAT USES TPFS
!===============================================================================
        record /stpasfdb/  pasfdb(pagemi,numpas)
!
        common /pastruct/ pasfdb        
!===============================================================================  
        do gind=1,numpas
          do emis=1,pagemi
            if(pasfdb(emis,gind).lun.ne.0) call pasio_close(pasfdb(emis,gind))        
          enddo
        enddo   	
	return
	end
