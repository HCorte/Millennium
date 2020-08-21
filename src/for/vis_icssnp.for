C  
C ICSSNP.FOR
C
C V01 25-NOV-1999 UXN Initial release.
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
C Copyright 1999 GTECH Corporation. All rights reserved.
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

C=======OPTIONS /CHECK=NOOVERFLOW/EXT
	SUBROUTINE ICSSNP(CLN_ID)
	IMPLICIT NONE

	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
        INCLUDE 'INCLIB:AGTINF.DEF'
        INCLUDE 'INCLIB:PRMAGT.DEF'
	INCLUDE 'INCLIB:VISCOM.DEF'   
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DATBUF.DEF'
	INCLUDE 'INCLIB:ICSCOM.DEF'
	INCLUDE 'INCLIB:LOGCOM.DEF'
C
	INTEGER*4 CLN_ID
C
	CHARACTER*20 CLN_STATUS(0:7)
	DATA CLN_STATUS/'Never connected     ','Disconnected (own)  ',
     *                  'Disconnected by host','Connected           ',
     *                  'Receiving data      ','Requested XOFF      ',
     *                  'Inter-batch break   ','Retry break         '/

	CHARACTER*8  TMF_STATUS(0:3)
	DATA TMF_STATUS/'  No TMF', 'Name req','Got name','   Open'/
	CHARACTER*11 T1,T2
C
	INTEGER*4 IND, I, K, ST, LENGTH
	BYTE      CLN_NAME(20)
	BYTE      IPADDR(15)
	INTEGER*4 FLG/1/
C
	WRITE(CLIN23,950)
C
	IF(CLN_ID.LE.0.OR.CLN_ID.GT.MAX_CLIENTS) CLN_ID = -1
	IF(CLN_ID.GT.0) THEN
	   IF(ICSLOG_GLOB.CLIENT(CLN_ID).STATUS.EQ.0) CLN_ID = -1
	ENDIF
C
	IND = 1
	WRITE(XNEW(IND),900)
	IND = IND + 2 
	
	IF(CLN_ID.NE.-1) GOTO 500

	WRITE(XNEW(IND),901) ICSLOG_GLOB.MAX_CLIENTS, ICSLOG_GLOB.ACT_CLIENTS,
     *                       ICSLOG_GLOB.MAX_OUT
	IND = IND + 1
	WRITE(XNEW(IND),902) ICSLOG_GLOB.KEEP_ALIVE, ICSLOG_GLOB.SLEEP_TIME,
     *                       ICSLOG_GLOB.BATCH_SIZE
	IND = IND + 1
	WRITE(XNEW(IND),903) ICSLOG_GLOB.BATCH_BREAK, ICSLOG_GLOB.RETRY_DELAY,
     *                       ICSLOG_GLOB.MAX_RETRIES
	IND = IND + 1
	WRITE(XNEW(IND),904) ICSLOG_GLOB.MAX_ERRORS, ICSLOG_GLOB.MAX_BADMSG,
     *                       ICSLOG_GLOB.DO_CHECKSUM

        IND = IND + 2
	CALL RIGHT_JUSTIFY(ICSLOG_GLOB.HOSTNAME, CLN_NAME, 20)
	WRITE(XNEW(IND),910) (CLN_NAME(K),K=1,20), ICSLOG_GLOB.TCP_PORT

	IND = IND + 1
	WRITE(XNEW(IND),905) ICSLOG_GLOB.CYCLES, ICSLOG_GLOB.IO_CYCLES

	IND = IND + 1
	WRITE(XNEW(IND),906) ICSLOG_GLOB.LOST_CONN, ICSLOG_GLOB.DISCONN

	IND = IND + 1
	WRITE(XNEW(IND),907) ICSLOG_GLOB.ERRCNT, ICSLOG_GLOB.BADMSG
        
        IND = IND + 1
	WRITE(XNEW(IND),909) ICSLOG_GLOB.TOTNACKS, ICSLOG_GLOB.TOT_INMSG

	IND = IND + 1
	WRITE(XNEW(IND),908) ICSLOG_GLOB.TOTBUCK, ICSLOG_GLOB.TOT_CACHE	

	IND = IND + 1
	WRITE(XNEW(IND),914) HBLOCK, HBLKRDY	

	IND = IND + 2
	WRITE(XNEW(IND),911)

	IND = IND + 1	
	WRITE(XNEW(IND),913)
	
	DO 100 I=1, MAX_CLIENTS
	   IND = IND + 1
	   ST = ICSLOG_GLOB.CLIENT(I).STATUS
	   IF(ST.EQ.0) GOTO 100

	   CALL LEFT_JUSTIFY(ICSLOG_GLOB.CLIENT(I).NAME, CLN_NAME, 20)
	   WRITE(XNEW(IND),912) I, (CLN_NAME(K),K=1,20), 
     *          CLN_STATUS(ST)

100	CONTINUE
	RETURN
C
C Client specific snapshot
C
500	CONTINUE
C	
	CALL RIGHT_JUSTIFY(ICSLOG_GLOB.CLIENT(CLN_ID).NAME, CLN_NAME, 20)
	CALL RIGHT_JUSTIFY(ICSLOG_GLOB.CLIENT(CLN_ID).IPADDR,IPADDR, 15)

	WRITE(XNEW(IND),920) (CLN_NAME(K),K=1,20),
     *                       (IPADDR(K),K=1,15)
	IND = IND + 1

	CALL SYS$ASCTIM(LENGTH,T1,ICSLOG_GLOB.CLIENT(CLN_ID).CONNTIME,%VAL(FLG))
	IF(LENGTH.LE.0) T1 = '00:00:00.00'
	CALL SYS$ASCTIM(LENGTH,T2,ICSLOG_GLOB.CLIENT(CLN_ID).DISCTIME,%VAL(FLG))
	IF(LENGTH.LE.0) T2 = '00:00:00.00'

	WRITE(XNEW(IND),921) T1(1:8),T2(1:8)
	IND = IND + 1

	WRITE(XNEW(IND),922) ICSLOG_GLOB.CLIENT(CLN_ID).IOTIME,
     *                       ICSLOG_GLOB.CLIENT(CLN_ID).XFRCDC
	IND = IND + 1

	WRITE(XNEW(IND),923) ICSLOG_GLOB.CLIENT(CLN_ID).START,
     *                       ICSLOG_GLOB.CLIENT(CLN_ID).FINISH
	IND = IND + 1

	WRITE(XNEW(IND),924) ICSLOG_GLOB.CLIENT(CLN_ID).LAST,
     *                       ICSLOG_GLOB.CLIENT(CLN_ID).BATCHBLK
	IND = IND + 1

	WRITE(XNEW(IND),925) ICSLOG_GLOB.CLIENT(CLN_ID).BATCHTICK,
     *                       ICSLOG_GLOB.CLIENT(CLN_ID).TOTBLKS
	IND = IND + 1

	WRITE(XNEW(IND),926) ICSLOG_GLOB.CLIENT(CLN_ID).TOTERR,
     *                       ICSLOG_GLOB.CLIENT(CLN_ID).RETRIES
	IND = IND + 1

	WRITE(XNEW(IND),927) ICSLOG_GLOB.CLIENT(CLN_ID).RTRYTICK,
     *                       TMF_STATUS(ICSLOG_GLOB.CLIENT(CLN_ID).MJFSTS)
	IND = IND + 1

	WRITE(XNEW(IND),928) ICSLOG_GLOB.CLIENT(CLN_ID).TOTXOFF,
     *                       ICSLOG_GLOB.CLIENT(CLN_ID).THRUPUT
	IND = IND + 1

	WRITE(XNEW(IND),929) ICSLOG_GLOB.CLIENT(CLN_ID).BLKCNT,
     *                       ICSLOG_GLOB.CLIENT(CLN_ID).NACKCNT
	IND = IND + 1

	CALL RIGHT_JUSTIFY(ICSLOG_GLOB.CLIENT(CLN_ID).MJFNAME, CLN_NAME, 20)

	WRITE(XNEW(IND),930) ICSLOG_GLOB.CLIENT(CLN_ID).BADMSG,
     *                       (CLN_NAME(K),K=1,20)
	IND = IND + 1

	WRITE(XNEW(IND),931) ICSLOG_GLOB.CLIENT(CLN_ID).CACHECNT
	IND = IND + 1
		
	RETURN
C
900	FORMAT('ICSLOG server snapshot')
901     FORMAT('Max clients ',T25,I4,3X,
     *         'Active clients',T50,I4,3X,
     *         'Max out     ',T75,I4)
902     FORMAT('Keep alive  ',T25,I4,3X,
     *         'Sleep time  ',T50,I4,3X,
     *         'Batch size  ',T75,I4)
903     FORMAT('Batch break ',T25,I4,3X,
     *         'Retry delay ',T50,I4,3X,
     *         'Max retries ',T75,I4)
904     FORMAT('Max errors  ',T25,I4,3X,
     *         'Max badmsg  ',T50,I4,3X,
     *         'Do checksum ',T75, I4)
905     FORMAT('Wakeup counter',T30,I8,T41,
     *         'io_cycles',T71,I8)
906     FORMAT('Total # of disconnects (own)',T30,I8,T41,
     *         'Total # of server disconnects',T71,I8)
907	FORMAT('Total i/o error count', T30,I8,T41,
     *	       'Total # of bad messages',T71,I8)
908     FORMAT('Total log blocks xfered', T30, I8,T41,
     *         'Total # of cache hits',T71,I8)
909     FORMAT('Total # of all NACKs',T30,I8,T41,
     *         'Total messages from clients',T71,I8)
910	FORMAT('Host name',T18,20A1,T41,
     *         'Port #',T71,I8)
911     FORMAT('  Id  Client name             Client status')
912     FORMAT(I4,2X,20A1,T31,A)
913     FORMAT(80('-'))
914     FORMAT('Current system block #',T30,I8,T41,
     *         'Highest block full',T71,I8)
920     FORMAT('Client name',6X,20A1,T41,
     *         'Client IP address',6X,15A1)
921     FORMAT('Time of last connect',T30,A8,T41,
     *         'Time of last disconnect',T71,A8)
922     FORMAT('Time of last good i/o(cycles)',T30,I8,T41,
     *         'Cdc of current TMF',T71,I8)
923     FORMAT('First block # to send',T24,I14,T41,
     *         'Last block # to send',T65,I14)
924     FORMAT('Last sent block #',T24,I14,T41,
     *         '# blocks in current batch',T71,I8)
925     FORMAT('Batch break tick count',T30,I8,T41,
     *         '# of all sent blocks',T71,I8)
926     FORMAT('Total I/O error count',T30,I8,T41,
     *         '# of retries',T71,I8)
927     FORMAT('Retry break tick count',T30,I8,T41,
     *         'TMF status',T71,A8)
928     FORMAT('Total XOFF messages',T30,I8,T41,
     *         'blks/sec in prev minute',T71,I8)
929     FORMAT('# blocks in current minute',T30,I8,T41,
     *         '# no data responses',T71,I8)
930     FORMAT('Total bad messages',T30,I8,T41,
     *         'TMF name',T59,20A1)
931     FORMAT('# of cache hits',T30,I8)

950     FORMAT('Enter client id or vision command')
	END
C
	SUBROUTINE RIGHT_JUSTIFY(INARR, OUARR, LENGTH)
	IMPLICIT NONE
	INTEGER*4 LENGTH
	BYTE INARR(LENGTH)
	BYTE OUARR(LENGTH)
	INTEGER*4 I,J
C
	DO I=1,LENGTH
	   OUARR(I) = ICHAR(' ')
	ENDDO
	J = LENGTH
	DO I=LENGTH,1,-1
	   IF(INARR(I).NE.0.AND.INARR(I).NE.ICHAR(' ')) THEN
	      OUARR(J) = INARR(I)
	      J = J - 1
	   ENDIF
	ENDDO
	END
C
C
C
	SUBROUTINE LEFT_JUSTIFY(INARR, OUARR, LENGTH)
	IMPLICIT NONE
	INTEGER*4 LENGTH
	BYTE INARR(LENGTH)
	BYTE OUARR(LENGTH)
	INTEGER*4 I,J
C
	DO I=1,LENGTH
	   OUARR(I) = ICHAR(' ')
	ENDDO
	J = 1
	DO I=1,LENGTH
	   IF(INARR(I).NE.0.AND.INARR(I).NE.ICHAR(' ')) THEN
	      OUARR(J) = INARR(I)
	      J = J + 1
	   ENDIF
	ENDDO
	END
