! VIS_SNIFSNP.FOR
!
! V01 01-JAN-2010 FJG ePASSIVE
!
! ePASSIVE LOTTERY SNAPSHOT
!
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
! This item is the property of GTECH Corporation, Providence, Rhode
! Island, and contains confidential and trade secret information. It
! may not be transferred from the custody or control of GTECH except
! as authorized in writing by an officer of GTECH. Neither this item
! nor the information it contains may be used, transferred,
! reproduced, published, or disclosed, in whole or in part, and
! directly or indirectly, except as expressly authorized by an
! officer of GTECH, pursuant to written agreement.
!
! Copyright 2010 GTECH Corporation. All rights reserved.
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
!
C=======OPTIONS /check=nooverflow
	subroutine snifsnp(gind,emis,tnum)
	implicit none
!
        include 'inclib:sysparam.def'
        include 'inclib:sysextrn.def'
!
        include 'inclib:global.def'
        include 'inclib:concom.def'
        include 'inclib:pascom.def'
        include 'inclib:agtinf.def'
        include 'inclib:prmagt.def'
        include 'inclib:viscom.def'
        include 'inclib:pasiosubs.def'
!
        record /stpasfdb/  pasfdb
        record /stpasrec/  pasrec       
!
        integer*4     emis
        integer*4     gind
        integer*4     tnum
!
        integer*4     xemis
        integer*4     xgind
        integer*4     xtnum
!        
        integer*4     mnum
        integer*4     mser
        integer*4     mfra
        integer*4     msta
        integer*4     mter
        integer*4     mtim
        integer*4     msal
        integer*4     moff     
!
        character*16  slot(numpas) /'Lotaria Classica','Lotaria Popular '/
        character*12  stab(pbilmin:pbilmax)/'RetAftWinner',
     *                                      'RetAfterDraw',
     *                                      'ReturnWinner',
     *                                      'SoldOffWinnr',
     *                                      'ReturnedOffl',
     *                                      'SoldOfflines',
     *                                      'AvailableOff',
     *                                      '*NOTDEFINED*',
     *                                      'AvailableOnl',
     *                                      '+SoldOnline+',
     *                                      'CancelledOnl',
     *                                      'SoldOnlinWin',
     *                                      'CanOnlineWin'/
!
        write(xnew(01),9000)
        if(xemis.ne.emis.or.gind.ne.xgind.or.xtnum.ne.tnum) then
          if(emis.eq.0) then
            emis = xemis
            write(xnew(01),9001)
          endif
          if(emis.le.0.or.emis.gt.pagemi) then
            write(xnew(03),9002) 'Invalid emission index value  ',emis
            return
          endif
          if(gind.le.0.or.gind.gt.numpas) then
            write(xnew(03),9002) 'Invalid game index for Passive',gind
            return
          endif
          if(passts(emis,gind).le.GAMNUL) then
            write(xnew(03),9002) 'Invalid draw status           ',passts(emis,gind)
            return
          endif
          mfra = mod(tnum,100)
          if(mfra.le.0.or.mfra.gt.pasnoffra(emis,gind)) then
            write(xnew(03),9002) 'Invalid fraction for emission ',mfra
            return
          endif
          mser = mod((tnum/100),100)
          if(mser.le.0.or.mser.gt.pasnumser(emis,gind)) then
            write(xnew(03),9002) 'Invalid serie for emission    ',mser
            return
          endif
          mnum = tnum/10000
          if(mnum.lt.0.or.mnum.ge.pasnumtck(emis,gind)) then
            write(xnew(03),9002) 'Invalid number for emission   ',mnum
            return
          endif         
!
          call pasio_init(pasfdb,gind,pasemis(emis,gind),pasnumtck(emis,gind)-1,pasnumser(emis,gind),
     *                    pasnoffra(emis,gind),cpastpffil(emis,gind))
          call pasio_openro(pasfdb)
          if(pasfdb.err.ne.ioe_noerr) then
            write(xnew(03),9002) 'Error opening TPF file:       ',pasfdb.err
            return
          else
            call pasio_read(pasfdb,mnum,mser,mfra,pasrec)
            if(pasfdb.err.ne.ioe_noerr) then
              write(xnew(03),9002) 'Error reading TPF file:       ',pasfdb.err
              pasioerrs(ioreano,emis,gind) = pasioerrs(ioreano,emis,gind) + 1              
              return
            else
              pasioerrs(ioreaok,emis,gind) = pasioerrs(ioreaok,emis,gind) + 1
            endif
            call pasio_close(pasfdb)
          endif
!
          xemis = emis
          xgind = gind
          xtnum = tnum
        endif
!
        write(xnew(02),9010) slot(xgind)
        write(xnew(03),9011) mnum
        write(xnew(04),9012) mser
        write(xnew(05),9013) mfra
        write(xnew(06),9014) pasdraw(xemis,xgind)        
        write(xnew(07),9015) pasemis(xemis,xgind)      
        write(xnew(08),9016) xemis
        write(xnew(09),9032) pasio_key(mnum,mser,mfra)        
        moff = passaltab(xemis,xgind)
        if(moff.gt.0) then ! Draw on sale. Access memory
          if(xgind.eq.PSBCLA) then
            msta = pasnumcla(mnum,moff).billet(mser,mfra)
            mter = pasnumcla(mnum,moff).rester
            mtim = pasnumcla(mnum,moff).restim            
            msal = pasnumcla(mnum,moff).forsal            
          else
            msta = pasnumpop(mnum,moff).billet(mser,mfra)            
            mter = pasnumpop(mnum,moff).rester
            mtim = pasnumpop(mnum,moff).restim            
            msal = pasnumpop(mnum,moff).forsal
          endif
          write(xnew(10),9020)
          write(xnew(11),9021) stab(msta)
          write(xnew(12),9022) mter                    
          write(xnew(13),9023) distim(mtim)
          write(xnew(14),9024) msal
        endif
!
        write(xnew(15),9030)
        write(xnew(16),9031) cpastpffil(xemis,xgind)   
        write(xnew(17),9032) pasrec.key
        write(xnew(18),9033) stab(pasrec.stat)
        write(xnew(19),9034) pasrec.cdc        
        write(xnew(20),9035) pasrec.serial
        write(xnew(21),9036) pasrec.agt
        if(pasrec.stat.lt.pbilnot) then
          write(xnew(22),9037) 99999-pasrec.control                        
        endif
        return
!===============================================================================
!       FORMATS
!===============================================================================
9000    format('Tickets SNIFfer snapshot ')
9001    format('Tickets SNIFfer snapshot... refreshing')
9002    format('ERROR: ',A30,I10)
9010    format('Game Name:  ',A16)
9011    format('Ticket Number:         ',I5.5)
9012    format('Serie:                    ',I2.2)
9013    format('Fraction:                 ',I2.2)
9014    format('SCML Draw:            ',I6.6)
9015    format('Internal Draw:          ',I4.4)
9016    format('Emission Index:           ',I2.2)
9020    format('===== BITMAP IN MEMORY =====')
9021    format('Ticket Status:  ',A12)
9022    format('Reserved by:        ',I8)
9023    format('Reserved at:        ',A8)
9024    format('Available fractions:',I8)
9030    format('====== DETAIL IN DISK ======')
9031    format('TPF File: ',A20)
9032    format('IntegrityKey:         ',Z6.6)
9033    format('Ticket Status:  ',A12)
9034    format('CDC:               ',I9)
9035    format('Serial:            ',I9)
9036    format('Terminal:          ',I9)
9037    format('Control:               ',I5.5)
	end
