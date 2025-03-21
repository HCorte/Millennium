C
C SUBROUTINE MESNET
C $Log:   GXAFXT:[GOLS]MESNET.FOV  $
C  
C     Rev 1.0   17 Apr 1996 14:02:18   HXK
C  Release of Finland for X.25, Telephone Betting, Instant Pass Thru Phase 1
C  
C     Rev 1.2   26 Aug 1993  3:50:24   JWE
C  Replace incorrect version which I mistakenly checked in.
C  
C     Rev 1.1   26 Aug 1993  3:47:26   JWE
C  
C     Rev 1.0   21 Jan 1993 17:00:14   DAB
C  Initial Release
C  Based on Netherlands Bible, 12/92, and Comm 1/93 update
C  DEC Baseline
C
C ** Source - mesnet.for **
C
C MESNET.FOR
C
C
C X2X Upgrade: 22-FEB-96 wsm Changed IDSYMB(NETSYS) to IDSYMB(MAX_SYSTEMS)
C
C V01 01-AUG-90 XXX RELEASED FOR VAX
C
C SUBROUTINE TO FORMAT NETWORK MESSAGES FOR ERRLOG
C
C
C V01 01-FEB-89 MTK INITIAL RELEASE FOR SWEDEN
C
C
C MESSAGE #               MESSAGE
C     1                   <TYPE><NAME> FROM SER><XXXXXXX>
C     2                   <TYPE><NAME> SYS>X CHANGED FROM <XX> TO <XX>
C     3                   <TYPE><NAME> SYS>X ADDED AS <AAAA>
C     4                   <TYPE><NAME> LINK WITH SYS>X REMOVED
C     5                   <TYPE><NAME> SYS>X TAKEOVER SERIAL> XXXXXXX
C     6                   RCV SER> <X> SHOULD BE <X> IN <A> MODE
C     7                   AVAILABLE
C     8                   AVAILABLE
C     9                   AVAILABLE
C    10                   AVAILABLE
C    11                   CANNOT SEND - NO BUFFERS
C    12                   ADR: ZZZZ I/O ABORTED MODE <XX>
C    13                   NETWORK COMMAND <AAAAAAAA>
C    14                   ABORT OF SYSTEM>X IN PROGRESS
C    15                   SYSTEM DISCREPENCY - TYPE<XXX>
C    16                   SYS>X RECOVERED, SER>XXXXXXXX
C    17                   ADR: ZZZZ NEW MASTER COMING: <A>
C    18                   ADR: ZZZZ ADD NEW LINK MANUALLY <A>
C    19                   ADR: ZZZZ DATA IN INVALID MODE
C    20                   SYSTEM>X
C    21                   DATA; VALUE: <XXXXXXXXX>
C    22                   SYS>X RECOV. STAGE <XXXXXXXX>
C    23                   LUN:XX PPI/SSA ERROR ZZZZ
C    24                   LINK WITH SYSTEM>X INITIALIZED
C    25                   WAITING TOO LONG FOR DIAG FROM SYS>X
C    26                   ATTEMPTING TO RESYNC SYSTEM>X
C    27                   CROSS-COMMAND BETWEEN PATH X AND X
C    28                   REM CHKPNT ERR STAT <XXXX> R-SER <XXXX>
C    29                   REM CHKP XX R-SER <XXXXXXXXX>
C    30                   REM CHKPNT RESTORED FROM <AAA> R-SER <XXX>
C    31                   COMMONS RESTORED FROM <AAAA> R-SER <XXXX>
C    32                   POOLS RESTORED FROM <AAAA>
C    33                   COMMUNICATIONS ON PROC>X NOT RESPONDING
C    34                   COMMUNICATIONS ON PROC>X NOT ACTIVE
C    35                   COMMUNICATIONS ACTIVE SYSTEM STARTED
C    36                   RTS ERROR TYPE>X STATUS>ZZZZ
C    37                   DIGITAL COM SWITCH ERROR, TYPE>X STATUS>ZZZZ
C    38                   DISCREPENCY: XXX/XXXXXXXXX
C    39                   ACTIVE MASTER CONNECTION FROM X WAS FROM X
C
C
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
	SUBROUTINE MESNET(MNUM,DBUF,MBUF,ALARM)
	IMPLICIT NONE
C
	INCLUDE 'INCLIB:SYSPARAM.DEF'
	INCLUDE 'INCLIB:SYSEXTRN.DEF'
	INCLUDE 'INCLIB:GLOBAL.DEF'
	INCLUDE 'INCLIB:CONCOM.DEF'
	INCLUDE 'INCLIB:DESNET.DEF'
	INTEGER*4 DBUF(*),IDSYMB(0:MAX_SYSTEMS), K, MNUM
	CHARACTER*20 SITES(4)
	CHARACTER*140 MBUF
	CHARACTER*8 RELAT(2),DMODE(2),NETCM(6)
	LOGICAL ALARM
	DATA IDSYMB/'*','A','B','C','D','E'/
	DATA RELAT/'SECON-DY','PRIMARY '/
	DATA DMODE/'NORMAL  ','RECOVERY'/
	DATA NETCM/'ADD LINK','REM LINK','RESYNC  ','SET MAST',
     *	           'DIAGNCS ','REMCLSED'/
	DATA SITES/'                    ',
     *	           'ON LOCAL IN --------',
     *	           'ON REM FROM --------',
     *	           'ON REM FROM --------'/
C
	GOTO (1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,
     *	      16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,
     *	      31,32,33,34,35,36,37,38,39) MNUM
	GOTO 99
C
C
1	CONTINUE
	WRITE (MBUF,901) (DBUF(K),K=1,4),
     *	                 IDSYMB(DBUF(5)),DBUF(6),SITES(DBUF(7))
901	FORMAT(2A4,1X,2A4,' SYS>',A1,' FROM SER>',I9,1X,A20)
	RETURN
C
2	CONTINUE
	WRITE (MBUF,902) (DBUF(K),K=1,4),
     *	                 IDSYMB(DBUF(5)),DBUF(6),DBUF(7)
902	FORMAT(2A4,1X,2A4,' SYS>',A1,' CHANGED FROM ',I4,' TO ',I4)
	RETURN
C
3	CONTINUE
	WRITE (MBUF,903) (DBUF(K),K=1,4),
     *	                 IDSYMB(DBUF(5)),RELAT(DBUF(6)),SITES(DBUF(7))
903	FORMAT(2A4,1X,2A4,' SYS>',A1,' ADDED AS ',A8,1X,A20)
	RETURN
C
4	CONTINUE
	WRITE (MBUF,904) (DBUF(K),K=1,4),
     *	                 IDSYMB(DBUF(5)),SITES(DBUF(6))
904	FORMAT(2A4,1X,2A4,' LINK WITH SYS>',A1,' REMOVED ',A20)
	RETURN
C
5	CONTINUE
	WRITE (MBUF,905) (DBUF(K),K=1,4),
     *	                 IDSYMB(DBUF(5)),DBUF(6),SITES(DBUF(7))
905	FORMAT(2A4,1X,2A4,' SYS>',A1,' TAKEOVER SER> ',I9,1X,A20)
	RETURN
C
6	CONTINUE
	WRITE (MBUF,906) DBUF(1),DBUF(2),DMODE(DBUF(3))
906	FORMAT('RCV SER>',I9,' SHD BE>',I9,' IN ',A8,' MODE')
	RETURN
C
7	CONTINUE
8	CONTINUE
9	CONTINUE
10	CONTINUE
	GOTO 99
C
11	CONTINUE
	WRITE (MBUF,911) SITES(DBUF(3))
911	FORMAT('CANNOT SEND - NO BUFFERS ',1X,A20)
	RETURN
C
12	CONTINUE
	WRITE (MBUF,912) DBUF(1),DBUF(2),SITES(DBUF(3))
912	FORMAT('ADR: ',Z4,' I/O ABORTED, MODE ',I2,1X,A20)
	RETURN
C
13	CONTINUE
	WRITE (MBUF,913) NETCM(DBUF(2)),SITES(DBUF(3))
913	FORMAT('NETWORK COMMAND ',A8,1X,A20)
	RETURN
C
14	CONTINUE
	WRITE (MBUF,914) IDSYMB(DBUF(2)),SITES(DBUF(3))
914	FORMAT('ABORT OF SYSTEM>',A1,' IN PROGESS ',A20)
	RETURN
C
15	CONTINUE
	WRITE (MBUF,915) DBUF(2),SITES(DBUF(3))
915	FORMAT('SYSTEM DISCREPENCY, TYPE> ',I3,1X,A20)
	RETURN
C
16	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,916) IDSYMB(DBUF(1)),DBUF(2),SITES(DBUF(3))
916	FORMAT('SYS> ',A1,' RECOVERED, SER>',I9,1X,A20)
	RETURN
C
17	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,917) DBUF(1),IDSYMB(DBUF(2)),SITES(DBUF(3))
917	FORMAT('ADR: ',Z4,' NEW MASTER COMING: ',A1,1X,A20)
	RETURN
C
18	CONTINUE
	WRITE (MBUF,918) DBUF(1),IDSYMB(DBUF(2)),SITES(DBUF(3))
918	FORMAT('ADR: ',Z4,' ADD NEW LINK MANUALLY ',A1,1X,A20)
	RETURN
C
19	CONTINUE
	WRITE (MBUF,919) DBUF(1),SITES(DBUF(3))
919	FORMAT('ADR: ',Z4,' DATA IN INVALID MODE ',A20)
	RETURN
C
20	CONTINUE
	WRITE (MBUF,920) IDSYMB(DBUF(2)),SITES(DBUF(3))
920	FORMAT('SYSTEM> ',A1,1X,A20)
	RETURN
C
21	CONTINUE
	WRITE (MBUF,921) DBUF(2),SITES(DBUF(3))
921	FORMAT('DATA! VALUE: ',I9,1X,A20)
	RETURN
C
22	CONTINUE
	WRITE (MBUF,922) IDSYMB(DBUF(1)),DBUF(2),SITES(DBUF(3))
922	FORMAT('SYS> ',A1,' RECOV. STAGE ',I9,1X,A20)
	RETURN
C
23	CONTINUE
	WRITE (MBUF,923) DBUF(1),DBUF(2),SITES(DBUF(3))
923	FORMAT('LUN: ',I2,' PPI/SSA I/O ERROR ',Z4,1X,A20)
	RETURN
C
24	CONTINUE
	WRITE (MBUF,924) IDSYMB(DBUF(2)),SITES(DBUF(3))
924	FORMAT('LINK WITH SYSTEM ',A1,' INITIALIZED ',A20)
	RETURN
C
25	CONTINUE
	WRITE (MBUF,925) IDSYMB(DBUF(2)),SITES(DBUF(3))
925	FORMAT('WAITING TOO LONG FOR DIAG FROM ',A1,1X,A20)
	RETURN
C
26	CONTINUE
	WRITE (MBUF,926) IDSYMB(DBUF(2)),SITES(DBUF(3))
926	FORMAT('ATTEMPTING TO RESYNC SYSTEM ',A1,1X,A20)
	RETURN
C
27	CONTINUE
	WRITE (MBUF,927) DBUF(2),DBUF(3)
927	FORMAT('CROSS-COMMAND BETWEEN PATH ', I1,' AND ',I1)
	RETURN
C
28	CONTINUE
	WRITE (MBUF,928) DBUF(2),SITES(DBUF(3)),DBUF(1)
928	FORMAT('REM CHKPNT ERR STAT ',I4,1X,A20,' R-SER ',I9)
	RETURN
C
29	CONTINUE
	WRITE (MBUF,929) DBUF(2),SITES(DBUF(3)),DBUF(1)
929	FORMAT('REM CHKP ',I2,1X,A20,' R-SER ',I9)
	RETURN
C
30	CONTINUE
	WRITE (MBUF,930) DBUF(2),DBUF(3)
930	FORMAT('REM CHKPNT RESTORED FROM FILE ',I4.4,' R-SER ',I9)
	RETURN
C
31	CONTINUE
	WRITE (MBUF,931) (SFNAMES(K,DBUF(2)),K=1,5),DBUF(3)
931	FORMAT('COMMONS RESTORED FROM FILE ',5A4,' SERIAL ',I9)
	RETURN
C
32	CONTINUE
	WRITE (MBUF,932) (SFNAMES(K,DBUF(2)),K=1,5)
932	FORMAT('POOLS RESTORED FROM FILE ',5A4)
	RETURN
C
33	CONTINUE
	WRITE (MBUF,933) DBUF(2)
933	FORMAT('COMMUNICATIONS ON PROC. ',I1,' NOT RESPONDING')
	RETURN
C
34	CONTINUE
	WRITE (MBUF,934) DBUF(2),DBUF(3)
934	FORMAT('COMMUNICATIONS ON PROC. ',I1,' NOT ACTIVE')
	RETURN
C
35	CONTINUE
	WRITE (MBUF,935)
935	FORMAT('COMMUNICATIONS ACTIVE, SYSTEM STARTED')
	RETURN
C
36	CONTINUE
	WRITE (MBUF,936) DBUF(2),DBUF(3)
936	FORMAT('RTS ERROR, TYPE> ',I1,' STATUS> ',Z8)
	RETURN
C
37	CONTINUE
	WRITE (MBUF,937) DBUF(2),DBUF(3)
937	FORMAT('DIGITAL COM SWITCH ERROR, TYPE> ',I1,' STATUS> ',Z8)
	RETURN
C
38	CONTINUE
	WRITE (MBUF,938) DBUF(1),DBUF(2),SITES(DBUF(3))
938	FORMAT('DISCREPENCY: ',I3,'/',I9,1X,A20)
	RETURN
C
39	CONTINUE
	WRITE (MBUF,939) IDSYMB(DBUF(2)),IDSYMB(DBUF(1)),SITES(DBUF(3))
939	FORMAT('ACTIVE MASTER CONNECTION FROM ',A1,
     *	       ' WAS FROM ',A1,1X,A20)
	RETURN
C
C INVALID MESSAGE NUMBER
C
99	CONTINUE
	ALARM=.TRUE.
	WRITE (MBUF,999) MNUM
999	FORMAT('INVALID NETWORK MESSAGE NUMBER> ',I4)
	RETURN
	END
