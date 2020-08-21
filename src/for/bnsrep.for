C BNSREP.FOR
C
C V14 27-JUN-2000 OXK DPOOL(11) from shares (FullHouse has fixed prize)
C V13 24-MAY-2000 OXK Calculate pools from sales, not from shares etc
C V12 20-APR-2000 UXN Fix for TOTPER
C V11 13-JAN-2000 RXK Def-file for Bingo division names added
C V10 02-DEC-1999 OXK COPY = 0 removed
C V09 25-OCT-1999 UXN Additional field added.
C V08 01-OCT-1999 UXN Fix for coupon count.
C V07 01-FEB-1995 HXK Rearranged division for Veikkaus
C V06 13-JAN-1995 HXK Calculate count from DAF if board price is zero.
C V05 12-JAN-1995 HXK Final adjustments per Veikkaus instructions
C V04 07-JAN-1995 HXK Removed WFUND report, print as controller's page
C V03 07-JAN-1995 HXK Minor corrections to WFUND part
C V02 12-DEC-1994 PXB Bug fixes. Now reports all divisions.
C V01 08-DEC-1994 PXB Initial revision.
C  
C SUBROUTINE TO PRINT BINGO SHARE REPORT.
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C This item is the property of GTECH Corporation, W.Greenwich, Rhode            
C Island, and contains confidential and trade secret information. It            
C may not be transferred from the custody or control of GTECH except            
C as authorized in writing by an officer of GTECH. Neither this item            
C nor the information it contains may be used, transferred,                     
C reproduced, published, or disclosed, in whole or in part, and                 
C directly or indirectly, except as expressly authorized by an                  
C officer of GTECH, pursuant to written agreement.                              
C                                                                               
C Copyright 2000 GTECH Corporation. All rights reserved.                        
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++            

C=======OPTIONS/CHECK=NOOVERFLOW/EXT                                       

        SUBROUTINE BNSREP (GNUM,GIND,DRAW,COPY)                             

        IMPLICIT NONE

C---- Include files used.

        INCLUDE 'INCLIB:SYSPARAM.DEF'
        INCLUDE 'INCLIB:SYSEXTRN.DEF'
        INCLUDE 'INCLIB:GLOBAL.DEF'                                        
        INCLUDE 'INCLIB:RECSCF.DEF'                                        
        INCLUDE 'INCLIB:GTNAMES.DEF'
        INCLUDE 'INCLIB:DBNREC.DEF'
        INCLUDE 'INCLIB:DATBUF.DEF'
        INCLUDE 'INCLIB:CONCOM.DEF'
        INCLUDE 'INCLIB:RECDAF.DEF'
        INCLUDE 'INCLIB:BNGDNAM.DEF'

C---- Arguments.

        INTEGER*4  GNUM             ! game number
        INTEGER*4  GIND             ! game index
        INTEGER*4  DRAW             ! draw number
        INTEGER*4  COPY             ! number of report copies

C---- Variables.

        INTEGER*4  FDB(7)           ! BnF file dscription block
        INTEGER*4  DFDB(7)          ! DAF file dscription block
        INTEGER*4  DPOOL(BGODIV+1)  ! 9 divisions in total.
        INTEGER*4  NETSH(BGODIV+1)  ! (in pennies)                           
        INTEGER*4  GROSS(BGODIV+1)  ! (in pennies)
        INTEGER*4  GROSS2(BGODIV+1) ! without rounding to DYN_VALUNITs (5p)
        INTEGER*4  I                ! counter
        INTEGER*4  K                ! counter
        INTEGER*4  WRK_I            ! counter
        INTEGER*4  PIND             ! 
        INTEGER*4  TOTSAL           ! total sales
        INTEGER*4  TOTCNT           ! total sold count
        INTEGER*4  TOTASH           !
        INTEGER*4  ABTOTASH         ! AB Totash
        INTEGER*4  FHTOTASH         ! Fullhouse totash.
        INTEGER*4  TOTWON           !
        INTEGER*4  ABTOTWON         ! AB Total won.
        INTEGER*4  FHTOTWON         ! Fullhouse Total won.
        INTEGER*4  TOTCAR           !
        INTEGER*4  TOTPER           !
        INTEGER*4  ABTOTPER         ! AB Total Percent
        INTEGER*4  FHTOTPER         ! Fullhouse Total Percent
        INTEGER*4  TOTSHR           !
        INTEGER*4  ABTOTSHR         ! AB Total Share
        INTEGER*4  FHTOTSHR         ! Fullhouse Total Share
        INTEGER*4  TOTWPL           !
        INTEGER*4  TOTPOL           !
        INTEGER*4  TOTGRS           ! 
        INTEGER*4  ABTOTGRS         ! AB total gross sales
        INTEGER*4  FHTOTGRS         ! Fullhouse total gross sales
        INTEGER*4  TOTNET           ! 
        INTEGER*4  ABTOTNET         ! AB total NET sales
        INTEGER*4  FHTOTNET         ! Fullhouse total NET sales
        INTEGER*4  TPOOL            !
        INTEGER*4  ABTPOOL          ! AB total pool.
        INTEGER*4  FHTPOOL          ! Fullhouse total pool.
        INTEGER*4  REV              !
        INTEGER*4  PAGE             ! page number of report
        INTEGER*4  WEEK             ! week no for report
        INTEGER*4  ST               ! error status
        INTEGER*4  REC              ! record counter used to read DAF

        INTEGER*4  YEAR2
     
        CHARACTER*12  REPNAM        !                                      
        CHARACTER     HEAD1*49      !
        CHARACTER*7   DNAME(BGODIV+1) !                                      
        CHARACTER*16  SUBNAME(3)

        INTEGER*4  TOTWPL_PRN       ! Print summary with 'target' 40% 
        INTEGER*4  TOTPOL_PRN       !	  + difference between target & actual
        INTEGER*4  TOTPER_PRN/40000/!

        COMMON SCFREC

        DATA DNAME/'  SUPER','  TUPLA','  BINGO',
     *             '  TAYSI','  HIT24','KOLMOIS',
     *             '  TUPLA','  BINGO','HUONOIN',7*'       '/

        DATA REPNAM/'            '/                                        

        DATA SUBNAME /'BINGO LOTTO     ',
     *                'BINGO TAYSKASI  ',
     *                'BINGO           '/


C--------------------- Start of Program Code ---------------------------
C---- Open game file.
        CALL OPENW (3,SCFGFN(1,GNUM),4,0,0,ST)                              
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),1,ST,0)                     
        CALL IOINIT (FDB,3,DBNSEC*256)                                      

C---- Read record for DRAW.
        CALL READW(FDB,DRAW,DBNREC,ST)                                     
        IF (ST .NE. 0) CALL FILERR(SCFGFN(1,GNUM),2,ST,DRAW)                  

C---- Open report file.
        WRITE(REPNAM,800) GIND                                             
        CALL ROPEN (REPNAM,8,ST)                                            

        CALL FIGWEK(DBNDAT(CURDRW),WEEK, YEAR2)   

        PAGE = 0

        WRITE(HEAD1,9001) WEEK,YEAR2                                 

        IF(DRAW.LE.DBNLOB) THEN
           CALL TITLE(HEAD1,REPNAM(1:8),REV,8,PAGE,DAYCDC)                     
           WRITE(8,9002)                                                      
           WRITE(8,900) SUBNAME(1),WEEK,YEAR2
        ENDIF
                                                                               
C---- Get total sales.

        TOTSAL = 0                                                   
        TOTCNT = 0

        DO I = 1, BGOENT
           TOTSAL = TOTSAL + DBNSAL(I)                            
        END DO

        TOTASH = 0                                                 
        ABTOTASH = 0                                                 
        FHTOTASH = 0                                                 
        TOTWON = 0                                                 
        ABTOTWON = 0                                                 
        FHTOTWON = 0                                                 
        TOTCAR = 0                                                 
        TOTPER = 0                                                 
        ABTOTPER = 0                                                 
        FHTOTPER = 0                                                 
        TOTSHR = 0                                                 
        ABTOTSHR = 0                                                 
        FHTOTSHR = 0                                                 

        DO K = BGOBAB,BGOFHS
           DO I = 1, DBNDIV(K)
              TOTWON = TOTWON + DBNSHR(I,K) * DBNSHV(I,K)
              TOTSHR = TOTSHR + DBNSHR(I,K)
              TOTASH = TOTASH + DBNASH(I,K)
              TOTCAR = TOTCAR + DBNPOL(I,K)
              IF (K .EQ. BGOBAB) THEN
                 ABTOTWON = ABTOTWON + DBNSHR(I,K) * DBNSHV(I,K)
                 ABTOTSHR = ABTOTSHR + DBNSHR(I,K)
                 ABTOTASH = ABTOTASH + DBNASH(I,K)
              ELSE
                 FHTOTWON = FHTOTWON + DBNSHR(I,K) * DBNSHV(I,K)
                 FHTOTSHR = FHTOTSHR + DBNSHR(I,K)
                 FHTOTASH = FHTOTASH + DBNASH(I,K)
              END IF
           END DO
        END DO

C---- Get pool amounts.

        TPOOL  = 0                                                 

        !--- Bingo AB.

        DO I = 1,DBNDIV(BGOBAB)
C           DPOOL(I) = DBNSHV(I,BGOBAB) * DBNSHR(I,BGOBAB)
           DPOOL(I) = TOTSAL * CALPER(DBNPER(I,BGOBAB))
           TOTPER = TOTPER + DBNPER(I,BGOBAB)
           ABTOTPER = ABTOTPER + DBNPER(I,BGOBAB)
           TPOOL = TPOOL + DPOOL(I)
           ABTPOOL = ABTPOOL + DPOOL(I)
        END DO

        !--- Bingo Fullhouse.

        DO I = 1,DBNDIV(BGOFHS)
           IF(DRAW.LE.DBNLOB) THEN
              WRK_I = I + 3
           ELSE
              WRK_I = I
           ENDIF 
C           DPOOL(WRK_I) = DBNSHV(I,BGOFHS) * DBNSHR(I,BGOFHS)
	   IF (I.EQ.11) THEN	! FullHouse has fixed prize, so ...
	       DPOOL(WRK_I) = DBNSHV(I,BGOFHS) * DBNSHR(I,BGOFHS)
	       DBNPER(I,BGOFHS) = 
     *		      IDNINT(DFLOAT(DPOOL(WRK_I))*100000.0D0/DFLOAT(TOTSAL))
	   ELSE
      	       DPOOL(WRK_I) = TOTSAL * CALPER(DBNPER(I,BGOFHS))
	   ENDIF
           TOTPER = TOTPER + DBNPER(I,BGOFHS)
           FHTOTPER = FHTOTPER + DBNPER(I,BGOFHS)
           TPOOL = TPOOL + DPOOL(WRK_I)
           FHTPOOL = FHTPOOL + DPOOL(WRK_I)
        END DO

        TOTPOL = IDNINT(DFLOAT(TOTSAL)*CALPER(TOTPER))

        TOTWPL = TOTPOL + TOTASH * DYN_BETUNIT + TOTCAR

        PIND = 0

        !--- Bingo AB.

        DO 40 I = 1,DBNDIV(BGOBAB)
           PIND = PIND + 1
           IF (DBNSHR(I,BGOBAB) .EQ. 0) THEN
              DBNBRK(I,BGOBAB) = DPOOL(PIND)
	      DBNSHV(I,BGOBAB) = 0
	      NETSH(I) = 0
           ELSE
              NETSH(I) = DBNSHV(I,BGOBAB)
              TOTNET = TOTNET + NETSH(I)*DBNSHR(I,BGOBAB)
              ABTOTNET = ABTOTNET + NETSH(I)*DBNSHR(I,BGOBAB)
              GROSS(I) = DPOOL(I) / DBNSHR(I,BGOBAB)
              GROSS2(I) = DYN_VALUNIT*DPOOL(I) / DBNSHR(I,BGOBAB)
              TOTGRS = TOTGRS + GROSS(I)*DBNSHR(I,BGOBAB)
              ABTOTGRS = ABTOTGRS + GROSS(I)*DBNSHR(I,BGOBAB)
           END IF
40      CONTINUE

        !--- Bingo Fullhouse.

        PIND = 0
        DO 41 I = 1,DBNDIV(BGOFHS)
           IF(DRAW.LE.DBNLOB) THEN
              WRK_I = I + 3
           ELSE
              WRK_I = I
           ENDIF 
           PIND = PIND + 1
           IF (DBNSHR(I,BGOFHS) .EQ. 0) THEN
              DBNBRK(I,BGOFHS) = DPOOL(WRK_I)
	      DBNSHV(I,BGOFHS) = 0
	      NETSH(WRK_I) = 0
           ELSE                                                  
              NETSH(WRK_I) = DBNSHV(I,BGOFHS)
              TOTNET = TOTNET + NETSH(WRK_I)*DBNSHR(I,BGOFHS)
              FHTOTNET = FHTOTNET + NETSH(WRK_I)*DBNSHR(I,BGOFHS)
              GROSS(WRK_I) = DPOOL(I) / DBNSHR(I,BGOFHS)
              GROSS2(WRK_I) = DYN_VALUNIT*DPOOL(I) / DBNSHR(I,BGOFHS)
              TOTGRS = TOTGRS + GROSS(WRK_I)*DBNSHR(I,BGOFHS)
              FHTOTGRS = FHTOTGRS + GROSS(WRK_I)*DBNSHR(I,BGOFHS)
           END IF
41      CONTINUE

        IF (TOTSAL .GT. 0) THEN
          TOTPER=IDNINT(DFLOAT(TOTPOL)/DFLOAT(TOTSAL)*1.D5)
        END IF

	TOTGRS=TOTPOL
	ABTOTGRS=TOTPOL
	FHTOTGRS=TOTPOL


C---- If old bingo then write Bingo AB page of report.

        IF(DRAW.LE.DBNLOB) THEN
           WRITE(8,901) CSMONY(TOTSAL,12,VALUNIT),
     *               CSMONY(TOTPOL,12,VALUNIT),
     *               DISPER(TOTPER),
     *               CSMONY(TOTCAR,14,VALUNIT),
     *               CSMONY(TOTASH,14,VALUNIT),
     *               CSMONY(TOTWPL,14,VALUNIT)


           PIND = 0

           DO 50 I = 1,DBNDIV(BGOBAB)
              PIND = PIND + 1
              WRITE (8,902) DNAME(I),
     *                 CSMONY(DPOOL(PIND),13,VALUNIT),
     *                 DISPER(DBNPER(PIND,BGOBAB)),
     *                 CSMONY(DBNASH(PIND,BGOBAB),13,VALUNIT),
     *                 DBNSHR(I,BGOBAB),
     *                 CSMONY(GROSS2(I),15,1),
     *                 CSMONY((GROSS2(I)-DYN_VALUNIT*DBNSHV(PIND,BGOBAB)),10,1),
     *                 CSMONY(NETSH(I),14,VALUNIT)
50         CONTINUE

           WRITE(8,904) CSMONY(ABTPOOL,11,VALUNIT),
     *               DISPER(ABTOTPER),
     *               CSMONY(ABTOTASH,13,VALUNIT),
     *               ABTOTSHR,
     *               CSMONY(ABTOTGRS,15,VALUNIT),                               
     *               CSMONY(ABTOTGRS-ABTOTNET,10,VALUNIT),
     *               CSMONY(ABTOTNET,14,VALUNIT)

C---- Now write old Bingo Fullhouse page.

           PAGE = 1

           CALL TITLE(HEAD1,REPNAM(1:8),REV,8,PAGE,DAYCDC)                     

           WRITE(8,9002)                                                      

           WRITE(8,900) SUBNAME(2),WEEK,YEAR2

           WRITE(8,901) CSMONY(TOTSAL,12,VALUNIT),
     *               CSMONY(TOTPOL,12,VALUNIT),
     *               DISPER(TOTPER),
     *               CSMONY(TOTCAR,14,VALUNIT),
     *               CSMONY(TOTASH,14,VALUNIT),
     *               CSMONY(TOTWPL,14,VALUNIT)

           PIND = 0

           DO I = 1,2 !DBNDIV(BGOFHS)
              WRK_I = I + 3
              PIND = PIND + 1
              WRITE (8,902) DNAME(WRK_I),
     *                 CSMONY(DPOOL(WRK_I),13,VALUNIT),
     *                 DISPER(DBNPER(PIND,BGOFHS)),
     *                 CSMONY(DBNASH(PIND,BGOFHS),13,VALUNIT),
     *                 DBNSHR(I,BGOFHS),
     *                 CSMONY(GROSS2(WRK_I),15,1),
     *                 CSMONY((GROSS2(I)-DYN_VALUNIT*DBNSHV(PIND,BGOFHS)),10,1),
     *                 CSMONY(NETSH(WRK_I),14,VALUNIT)
           ENDDO

           I = DBNDIV(BGOFHS)
           WRK_I = I + 3
           PIND = PIND + 1
           WRITE (8,902) DNAME(WRK_I),
     *                CSMONY(DPOOL(WRK_I),13,VALUNIT),
     *                DISPER(DBNPER(PIND,BGOFHS)),
     *                CSMONY(DBNASH(PIND,BGOFHS),13,VALUNIT),
     *                DBNSHR(I,BGOFHS),
     *                CSMONY(GROSS2(WRK_I),15,1),
     *                CSMONY((GROSS2(I)-DYN_VALUNIT*DBNSHV(PIND,BGOFHS)),10,1),
     *                CSMONY(NETSH(WRK_I),14,VALUNIT)

           DO I = 3,DBNDIV(BGOFHS)-1
              WRK_I = I + 3
              PIND = PIND + 1
              WRITE (8,902) DNAME(WRK_I),
     *                 CSMONY(DPOOL(WRK_I),13,VALUNIT),
     *                 DISPER(DBNPER(PIND,BGOFHS)),
     *                 CSMONY(DBNASH(PIND,BGOFHS),13,VALUNIT),
     *                 DBNSHR(I,BGOFHS),
     *                 CSMONY(GROSS2(WRK_I),15,1),
     *                 CSMONY((GROSS2(I)-DYN_VALUNIT*DBNSHV(PIND,BGOFHS)),10,1),
     *                 CSMONY(NETSH(WRK_I),14,VALUNIT)
           ENDDO

           WRITE(8,904) CSMONY(FHTPOOL,11,VALUNIT),
     *               DISPER(FHTOTPER),
     *               CSMONY(FHTOTASH,13,VALUNIT),
     *               FHTOTSHR,
     *               CSMONY(FHTOTGRS,15,VALUNIT),                               
     *               CSMONY(FHTOTGRS-FHTOTNET,10,VALUNIT),
     *               CSMONY(FHTOTNET,14,VALUNIT)

C---- Old Bingo Totals Page.

           WRITE(8,9058)

           WRITE(8,904) CSMONY(TPOOL,11,VALUNIT),
     *               DISPER(TOTPER),
     *               CSMONY(TOTASH,13,VALUNIT),
     *               TOTSHR,
     *               CSMONY(TOTGRS,15,VALUNIT),                               
     *               CSMONY(TOTGRS-TOTWON,10,VALUNIT),
     *               CSMONY(TOTWON,14,VALUNIT)
        ELSE
C---- Now write new Bingo Fullhouse page.

	   TOTPOL_PRN = TOTSAL * CALPER(TOTPER_PRN)
	   TOTWPL_PRN = TOTPOL_PRN + TOTASH * DYN_BETUNIT + TOTCAR

           PAGE = 1

           CALL TITLE(HEAD1,REPNAM(1:8),REV,8,PAGE,DAYCDC)                     

           WRITE(8,9002)                                                      

           WRITE(8,900) SUBNAME(2),WEEK,YEAR2

           WRITE(8,901) CSMONY(TOTSAL,12,VALUNIT),
     *               CSMONY(TOTPOL_PRN,12,VALUNIT),
     *               DISPER(TOTPER_PRN),
     *               CSMONY(TOTCAR,14,VALUNIT),
     *               CSMONY(TOTASH,14,VALUNIT),
     *               CSMONY(TOTWPL_PRN,14,VALUNIT)

           DO I = 1,DBNDIV(BGOFHS)
              IF(DBNDNR(I).NE.0) THEN
                 WRITE (8,902) BNGDNAMES(DBNDNR(I)),
     *                 CSMONY(DPOOL(I),13,VALUNIT),
     *                 DISPER(DBNPER(I,BGOFHS)),
     *                 CSMONY(DBNASH(I,BGOFHS),13,VALUNIT),
     *                 DBNSHR(I,BGOFHS),
     *                 CSMONY(GROSS2(I),15,1),
     *                 CSMONY((GROSS2(I)-DYN_VALUNIT*DBNSHV(I,BGOFHS)),10,1),
     *                 CSMONY(NETSH(I),14,VALUNIT)
              ENDIF
           ENDDO

           WRITE(8,904) CSMONY(FHTPOOL,11,VALUNIT),
     *               DISPER(FHTOTPER),
     *               CSMONY(FHTOTASH,13,VALUNIT),
     *               FHTOTSHR,
     *               CSMONY(FHTOTGRS,15,VALUNIT),                               
     *               CSMONY(FHTOTGRS-FHTOTNET,10,VALUNIT),
     *               CSMONY(FHTOTNET,14,VALUNIT)

C 2 totals: 1st = 'target - actual'; 2nd = 'target - actual - penny money'
c           WRITE(8,9041) CSMONY(TOTWPL_PRN-FHTOTNET,13,VALUNIT)
           WRITE(8,9042) CSMONY(TOTWPL_PRN-FHTOTGRS,13,VALUNIT)

C---- New Bingo Totals Page.

        ENDIF 

C---  Print State Controller's Page

        CALL TITLE(HEAD1,REPNAM(1:8),REV,8,PAGE,DAYCDC)                     
        WRITE(8,9002)                                                      
        WRITE(8,8007) SUBNAME(3),WEEK,YEAR2

        IF(TOTSAL.EQ.0) THEN   !allow for zero price ticket
           CALL OPENW(18,SFNAMES(1,DAF),0,0,0,ST)
           IF(ST.NE.0) THEN
              CALL USRCLOS1(18)
              WRITE(6,8008) IAM(),(SFNAMES(K,DAF),K=1,5),ST
              RETURN
           ENDIF
           CALL IOINIT(DFDB,18,DAFSEC*256)
           TOTCNT = 0
           DO REC = DBNDAT(CURDRW)-6,DBNDAT(CURDRW),1 !read last 7 sales days
              CALL READW(DFDB,REC,DAFREC,ST)
              IF(ST.NE.0) THEN
                 CALL USRCLOS1(18)
                 WRITE(6,8009) IAM(),(SFNAMES(K,DAF),K=1,5),ST,REC
                 RETURN
              ENDIF
              TOTCNT = TOTCNT + DAFTYP(TRACNT,TWAG,GNUM)
           ENDDO
           CALL CLOSEFIL(DFDB)
        ELSE
           TOTCNT = TOTSAL/DBNPRC
        ENDIF
        IF(DRAW.LE.DBNLOB) THEN
           WRITE(8,8001) TOTCNT
        ELSE
           WRITE(8,80011) TOTCNT
        ENDIF

C---- Old Bingo AB bingo shares.

        IF(DRAW.LE.DBNLOB) THEN
           DO I = 1,DBNDIV(BGOBAB)
              WRITE(8,8002) DNAME(I),DBNSHR(I,BGOBAB)
           END DO
           WRITE (8,8003) ABTOTSHR

C---- Old Fullhouse bingo shares.

           WRITE (8,8004)
           DO I = 1,2
              WRK_I = I + 3
              WRITE(8,8002) DNAME(WRK_I),DBNSHR(I,BGOFHS)
           ENDDO
           I = DBNDIV(BGOFHS)
           WRK_I = I + 3
           WRITE(8,8002) DNAME(WRK_I),DBNSHR(I,BGOFHS)
           DO I = 3,DBNDIV(BGOFHS)-1
              WRK_I = I + 3
              WRITE(8,8002) DNAME(WRK_I),DBNSHR(I,BGOFHS)
           ENDDO
           WRITE (8,8003) FHTOTSHR

C---- New Fullhouse bingo shares.

        ELSE

           WRITE (8,8004)
           DO I = 1,DBNDIV(BGOFHS)
              IF(DBNDNR(I).NE.0) WRITE(8,8002) BNGDNAMES(DBNDNR(I)),
     *           DBNSHR(I,BGOFHS)
           ENDDO
           WRITE (8,8003) FHTOTSHR
        ENDIF

C---- Total Shares.

        IF(DRAW.LE.DBNLOB) WRITE (8,8005) TOTSHR

C---- Signature part of report.

        WRITE (8,8006)

C---- Close file and print.

        CLOSE(UNIT=8)                                                        
        CALL SPOOL(REPNAM,COPY,ST)                                           
        CALL CLOSEFIL(FDB)                                                   

        RETURN                                                               

C--------------------- Format Statements -------------------------------
800     FORMAT('BI',I1,'SHARE.REP')                                        

900     FORMAT (1X,107('-'),//,                                               
     *          ' OY VEIKKAUS AB',24X,'VOITTOLASKELMA / ',A16,
     *          24X,'KIERROS ',I2,'/',I4,//,                                  
     *          1X,107('-'),//,                                               
     *          4X,'VAIHTO MK',5X,'VOITTOIHIN MK      %',5X,                  
     *          'ED. KIERROKSELTA MK',5X,'VOITTORAHASTOSTA MK',5X,            
     *          'JAETTAVA SUMMA MK',/)                                        

901     FORMAT (1X,A12,6X,A12,1X,F6.2,10X,A14,10X,
     *          A14,7X,A14,/,
     *          1X,107('-'),////,                                             
     *          1X,'VOITTO-',5X,'JAKOSUMMA   JAKO   VOITTOIHIN  ',            
     *          'VOITTOJEN  ',7X,'YKSIKKOVOITTO',13X,                         
     *          'PENNI- YKSIKKOVOITTO',/,                                     
     *          1X,'LUOKKA',12X,'MK       %',6X,'LISATTY',8X,                 
     *          'LKM',16X,'BRUTTO',10X,'RAHASTOON',                           
     *          9X,'NETTO',/,                                                 
     *          1X,107('-'))                                                  

902     FORMAT (1X,A8,A13,F7.2,A13,I11,7X,A15,9X,
     *          A10,A14)

904     FORMAT (1X,107('='),//,
     *          1X,'YHTEENSA: ',A11,1X,F6.2,A13,I11,8X,A15,//,
     *          1X,'YKSIKKOVOITOT YHTEENSA:',60X,A10,A14,//)
C9041    FORMAT(' JAKOSUMMA - NETTOVOITOT  :', A13)
9042    FORMAT(' PYÖRISTYS PENNIRAHASTOON :', A13,///)

9058    FORMAT (///,1X,107('='),//,                                            
     *          1X,'KAIKKI PELIT YHTEENSA',//)                

C---- Header format Statements.

9001    FORMAT ('BINGO  WINNING SHARES REPORT FOR ',I2.2,              
     *          '/',I4.4)                                                     

9002    FORMAT (1X)                                                           

C---- Format statements for shares by subgame and divisions page

8001    FORMAT (/,1X,'OSALLISTUNEET PELIT',32X,I11,1X,'KPL',/,
     *          1X,107('-'),/,1X,'VOITOT VOITTOLUOKITTAIN',
     *          //,1X,'BINGO LOTTO',/)
80011   FORMAT (/,1X,'OSALLISTUNEET PELIT',32X,I11,1X,'KPL',/,
     *          1X,107('-'),/,1X,'VOITOT VOITTOLUOKITTAIN')

8002    FORMAT (1X,'VOITTOLUOKKA         ',13X,A8,1X,'OIKEIN',
     *          3X,I11,1X,'KPL')

8003    FORMAT (1X,67('='),/,
     *          1X,'YHTEENSA             ',30X,I11,1X,'KPL')

8004    FORMAT (//,1X,'BINGO TAYSKASI',/)

8005    FORMAT (//,1X,67('='),/,
     *          1X,'YHTEENSA             ',30X,I11,1X,'KPL')

8006    FORMAT (//,1X,107('-'),///,                                             
     *          1X,T35,'TULOKSEN VAHVISTI',///,                               
     *          1X,T35,'VANTAALLA  ___/___  20____',///,                     
     *          1X,T35,25('_'))                                               

8007    FORMAT (1X,107('-'),//,
     *          ' OY VEIKKAUS AB',24X,'VOITTOLASKELMA / ',A16,
     *          24X,'KIERROS ',I2,'/',I4,/)

8008    FORMAT (1X,A,1X,5A4,' file open error ',I4)

8009    FORMAT (1X,A,1X,5A4,' file read error ',I4,' record - ',I4)

        END                                                                  
