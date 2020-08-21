! VIS_NACSNP.FOR
!
! V03 30-JAN-2012 FRP Reduce Cancels count and increase eSales amount
! V02 18-AUG-2011 FJG EVO Project bug with PASRETAFTAMT solved
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
	subroutine nacsnp(pag,ind,snp)
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
	include 'inclib:taskid.def'
	include 'inclib:viscom.def'
!
        integer*4   pag
        integer*4   snp
        integer*4   ind
        integer*4   i4snp
        character*4 chsnp
        equivalence (i4snp,chsnp)
!
        if(snp.eq.0) then
          chsnp='MAIN'
        else
          i4snp=snp
        endif
        if(ind.le.0.or.ind.gt.numpas) ind=1
        if(pag.le.0.or.pag.gt.3) pag=1
!
        if(chsnp.eq.'WEEK'.or.chsnp.eq.'week') then
          call nacsnp_week(ind)
        else if(chsnp.eq.'FILE'.or.chsnp.eq.'file') then
          call nacsnp_file(ind,pag,.false.)        
        else if(chsnp.eq.'LOAD'.or.chsnp.eq.'load') then ! RELOAD FILES
          call nacsnp_file(ind,pag,.true.)  
        else if(chsnp.eq.'ERRO'.or.chsnp.eq.'erro') then
          call nacsnp_erro(ind,pag)            
        else if(chsnp.eq.'FINA'.or.chsnp.eq.'fina') then
          call nacsnp_fina(ind,pag)    
        else if(chsnp.eq.'RESV'.or.chsnp.eq.'resv') then
          call nacsnp_resv                     
        else if(chsnp.eq.'RETU'.or.chsnp.eq.'retu') then
          call nacsnp_retu(ind,pag)   
        else
          call nacsnp_main(ind,pag)
        endif	
!
        return
!=======FORMATS=================================================================
	end
!=======SUBS====================================================================
C=======OPTIONS /check=nooverflow
	subroutine nacsnp_main(ind,pag)
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
	include 'inclib:taskid.def'
	include 'inclib:viscom.def'
	include 'inclib:pasnam.def'
!
        integer*4    ind
        integer*4    pag
        integer*4    tmp
        integer*4    pri
        integer*4    fin
        integer*4    lin
        character*14 typ
        character*1  sts(gamerr:gamref) /'*','N','C','I','O','B','W','V','D','F','X','R'/   
!
        if(pag.eq.2) then
          pri=18
          fin=34
        else if(pag.eq.3) then
          pri=35
          fin=50
        else
          pri=1
          fin=17
        endif      
        write(xnew(01),9000) ind,pag,daycdc              
        write(xnew(03),9010)            
        write(xnew(04),9020)                    
        lin=4
        do tmp=pri,fin
          if(passts(tmp,ind).ne.gamnul) then
            lin=lin+1
            typ=namplantyp(pasemt(tmp,ind))
            if(tmp.eq.pascurdrw(ind)) then
              write(xnew(lin),9040) tmp,passaltab(tmp,ind),sts(passts(tmp,ind)),passubsts(tmp,ind),pasemis(tmp,ind),
     *                              pasnumtck(tmp,ind),pasnumser(tmp,ind),pasnoffra(tmp,ind),cmony(pasprc(tmp,ind),5,betunit),
     *                              pasbsd(tmp,ind),pasesd(tmp,ind),pasesd(tmp,ind)+pasmaxdaypay(tmp,ind),pasprgcdc(tmp,ind),
     *                              distim(pastim(tmp,ind)),pasplan(tmp,ind),pasdiv(tmp,ind),typ(1:3),pasdraw(tmp,ind)              
            else
              write(xnew(lin),9030) tmp,passaltab(tmp,ind),sts(passts(tmp,ind)),passubsts(tmp,ind),pasemis(tmp,ind),
     *                              pasnumtck(tmp,ind),pasnumser(tmp,ind),pasnoffra(tmp,ind),cmony(pasprc(tmp,ind),5,betunit),
     *                              pasbsd(tmp,ind),pasesd(tmp,ind),pasesd(tmp,ind)+pasmaxdaypay(tmp,ind),pasprgcdc(tmp,ind),
     *                              distim(pastim(tmp,ind)),pasplan(tmp,ind),pasdiv(tmp,ind),typ(1:3),pasdraw(tmp,ind)
              
            endif
          endif
        enddo
!        
        return
!=======FORMATS=================================================================
9000    FORMAT('ePassive 'I1,' Control pag ',I1,'/3 ',I4)
9010    FORMAT(1X,'Id X Sts Emis NumTck Sr Fr Price SCDC ECDC pCDC PCDC EstCTime Pl Dv Emt  SCML ')
9020    FORMAT(1X,'== = === ==== ====== == == ===== ==== ==== ==== ==== ======== == == === ======')
9030    FORMAT(1X,I2,1X,I1,1X,A1,1X,I1,1X,I4,1X,I6,1X,I2,1X,I2,1X,A5,1X,I4,1X,I4,1X,I4,1X,I4,1X,A8,1X,I2,1X,I2,1X,A3,1X,I6.6)
9040    FORMAT('*',I2,1X,I1,1X,A1,1X,I1,1X,I4,1X,I6,1X,I2,1X,I2,1X,A5,1X,I4,1X,I4,1X,I4,1X,I4,1X,A8,1X,I2,1X,I2,1X,A3,1X,I6.6,'*')
        end
!=======SUBS====================================================================
C=======OPTIONS /check=nooverflow
	subroutine nacsnp_week(ind)
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
	include 'inclib:taskid.def'
	include 'inclib:viscom.def'
!
        integer*4    ind
        integer*4    yea
        integer*4    wek
        character*2  emi(pmaxwek,0:9)
!
	write(xnew(01),9000) ind, daywek, dayyer + 2000
!
        do yea = 0,9
          do wek = 1,pmaxwek
            if(pasextdrw(wek,yea,ind).ne.0) then
              write(emi(wek,yea),'(I2.2)') pasextdrw(wek,yea,ind)
            else
              emi(wek,yea) = '  '
            endif
          enddo
        enddo
!        
!       write(xnew(2),9010 NO SPACE
        do yea = 0,9   
          write(xnew(yea+2),9030) (emi(wek,yea),wek=1,27)                
        enddo
        write(xnew(12),9020)        
        do yea = 0,9   
          write(xnew(yea+13),9040) (emi(wek,yea),wek=28,53),yea                
        enddo
!        
        return
!=======FORMATS=================================================================
9000    format('ePassive 'I1,' Week ',I2,'/',I4,' Control')
9010    format('01|02|03|04|05|06|07|08|09|10|11|12|13|14|15|16|17|18|19|20|21|22|23|24|25|26|27')
9020    format('28|29|30|31|32|33|34|35|36|37|38|39|40|41|42|43|44|45|46|47|48|49|50|51|52|53| y')
9030    format(26(A2,'|'),A2)
9040    format(26(A2,'|'),X,I1)
        end
!=======SUBS====================================================================
C=======OPTIONS /check=nooverflow
	subroutine nacsnp_file(ind,pag,lod)
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
	include 'inclib:taskid.def'
	include 'inclib:viscom.def'
!
        logical      lod
        logical      istpf(pagemi,numpas)
        logical      isvpf(pagemi,numpas)        
        integer*4    ind
        integer*4    pag
        integer*4    tmp
        integer*4    pri
        integer*4    fin
        integer*4    lin
        integer*4    var
        character*1  sts(gamerr:gamref) /'*','N','C','I','O','B','W','V','D','F','X','R'/   
        logical      fst /.true./
!
        if(fst.or.lod) then
          do var=1,numpas
            do tmp=1,pagemi
              if(passts(tmp,ind).ne.gamnul) then   
                istpf(tmp,var) = filexist(cpastpffil(tmp,var))
                isvpf(tmp,var) = filexist(cpasvpffil(tmp,var))
              endif              
            enddo
          enddo
          fst = .false.
        endif
!
        if(pag.eq.2) then
          pri=18
          fin=34
        else if(pag.eq.3) then
          pri=35
          fin=50
        else
          pri=1
          fin=17
        endif   
        write(xnew(01),9000) ind,pag,daycdc              
        write(xnew(03),9010)            
        write(xnew(04),9020)                    
        lin=4
        do tmp=pri,fin
          if(passts(tmp,ind).ne.gamnul) then
            lin=lin+1
            var = pasioerrs(ioreano,tmp,ind) + pasioerrs(iowrino,tmp,ind)
            write(xnew(lin),9030) tmp,sts(passts(tmp,ind)),passubsts(tmp,ind),pasemis(tmp,ind),pasdraw(tmp,ind),
     *                            cpastpffil(tmp,ind),istpf(tmp,ind),cpasvpffil(tmp,ind),isvpf(tmp,ind),var
          endif
        enddo
!        
        return
!=======FORMATS=================================================================
9000    FORMAT('ePassive 'I1,' Files pag ',I1,'/3 ',I4)
9010    FORMAT(1X,'Id Sts Emis  SCML   Tickets File (TCF)  X  Winners File (VPF)  X TotalError')
9020    FORMAT(1X,'== === ==== ====== ==================== = ==================== = ==========')
9030    FORMAT(1X,I2,1X,A1,1X,I1,1X,I4,1X,I6.6,1X,A20,1X,L1,1X,A20,1X,L1,1X,I10)
        end
!=======SUBS====================================================================
C=======OPTIONS /check=nooverflow
	subroutine nacsnp_erro(ind,pag)
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
!
        integer*4    ind
        integer*4    pag
        integer*4    tmp
        integer*4    pri
        integer*4    fin
        integer*4    lin
        real*8       rea
        real*8       wri
!
        if(pag.eq.2) then
          pri=18
          fin=34
        else if(pag.eq.3) then
          pri=35
          fin=50
        else
          pri=1
          fin=17
        endif  
        write(xnew(01),9000) ind,pag,daycdc              
        write(xnew(03),9010)            
        write(xnew(04),9020)                    
        lin=4
        do tmp=pri,fin
          if(passts(tmp,ind).ne.gamnul) then
            lin=lin+1
            if(pasioerrs(ioreaok,tmp,ind).gt.0) then
              rea = dfloat(pasioerrs(ioreano,tmp,ind)*100)/dfloat(pasioerrs(ioreaok,tmp,ind))
            else
              rea = 0
            endif
            if(pasioerrs(iowriok,tmp,ind).gt.0) then
              wri = dfloat(pasioerrs(iowrino,tmp,ind)*100)/dfloat(pasioerrs(iowriok,tmp,ind))            
            else
              wri = 0
            endif
!            
            write(xnew(lin),9030) tmp,cpastpffil(tmp,ind),
     *           pasioerrs(ioreano,tmp,ind),pasioerrs(ioreaok,tmp,ind),rea,
     *           pasioerrs(iowrino,tmp,ind),pasioerrs(iowriok,tmp,ind),wri
          endif
        enddo
!        
        return
!=======FORMATS=================================================================
9000    FORMAT('ePassive 'I1,' I/O errors pag ',I1,'/3 ',I4)
9010    FORMAT(1X,'Id  Tickets File (TCF)  Read Err  Read OKs   %Err  WriteErr  WriteOKs   %Err ')
9020    FORMAT(1X,'== ==================== ======== ========== ====== ======== ========== ======')
9030    FORMAT(1X,I2,1X,A20,1X,I8,1X,I10,1X,F6.2,1X,I8,1X,I10,1X,F6.2)
        end
!=======SUBS====================================================================
C=======OPTIONS /check=nooverflow
	subroutine nacsnp_fina(ind,pag)
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
	include 'inclib:taskid.def'
	include 'inclib:viscom.def'
!
        integer*4    ind
        integer*4    pag
        integer*4    tmp
        integer*4    pri
        integer*4    fin
        integer*4    lin
!
        if(pag.eq.2) then
          pri=18
          fin=34
        else if(pag.eq.3) then
          pri=35
          fin=50
        else
          pri=1
          fin=17
        endif      
        write(xnew(01),9000) ind,pag,daycdc              
        write(xnew(03),9010)            
        write(xnew(04),9020)                    
        lin=4
        do tmp=pri,fin
          if(passts(tmp,ind).ne.gamnul) then
            lin=lin+1
            write(xnew(lin),9030) tmp,pasemis(tmp,ind),
     *            pasfin(tracnt,twag,tmp,ind),cmony(pasfin(dolamt,twag,tmp,ind),10,valunit),
     *            pasfin(tracnt,tcan,tmp,ind),cmony(pasfin(dolamt,tcan,tmp,ind),8,valunit),
     *            pasfin(tracnt,tval,tmp,ind),cmony(pasfin(dolamt,tval,tmp,ind),12,valunit),
     *            pasfin(tracnt,tret,tmp,ind),cmony(pasfin(dolamt,tret,tmp,ind),10,valunit)
          endif
        enddo
!        
        return
!=======FORMATS=================================================================
9000    FORMAT('ePassive 'I1,' Financial pag ',I1,'/3 ',I4)
9010    FORMAT(1X,'Id Emis -----eSales------ ---Cancels---- -----Payments------ -----Returns-----')
9020    FORMAT(1X,'== ==== ======= ========= ====== ======= ======= =========== ======= =========')
9030    FORMAT(1X,I2,1X,I4,I8,     A10,      I7,    A8,    I8,     A12,        I8,     A10)
        end
!=======SUBS====================================================================
C=======OPTIONS /check=nooverflow
	subroutine nacsnp_resv
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
	include 'inclib:taskid.def'
	include 'inclib:viscom.def'
!
        integer*4    emi
        integer*4    ind
        integer*4    num
        integer*4    ent
        integer*4    lin
        integer*4    seg/1/
        integer*4    tab(5,pmaxsal,numpas)
!
        write(xnew(01),9000) seg
        if(seg.le.1) then
          seg = 15
          call fastset(0,tab,5*pmaxsal*numpas)
          do emi=1,pagemi
            do ind=1,numpas
              if(passaltab(emi,ind).gt.0) then
                ent = passaltab(emi,ind)
                tab(1,ent,ind) = pasemis(emi,ind)
                tab(2,ent,ind) = emi                    
                tab(3,ent,ind) = pasdraw(emi,ind)              
                if(ind.eq.PSBCLA) then
                  do num=0,pmaxnumcla
                    if(pasnumcla(num,ent).rester.ne.0) tab(4,ent,ind) = tab(4,ent,ind) + 1
                    tab(5,ent,ind) = tab(5,ent,ind) + pasnumcla(num,ent).forsal
                  enddo
                else
                  do num=0,pmaxnumpop
                    if(pasnumpop(num,ent).rester.ne.0) tab(4,ent,ind) = tab(4,ent,ind) + 1
                    tab(5,ent,ind) = tab(5,ent,ind) + pasnumpop(num,ent).forsal
                  enddo                
                endif
              endif
            enddo
          enddo
        else
          seg = seg - 1
        endif
!        
        write(xnew(03),9020)            
        write(xnew(05),9030)      
        write(xnew(06),9040)              
        lin=7
        do ent=1,pmaxsal
          write(xnew(lin),9050) 
     *         ent,tab(1,ent,PSBCLA),tab(2,ent,PSBCLA),tab(3,ent,PSBCLA),tab(4,ent,PSBCLA),tab(5,ent,PSBCLA),
     *         ent,tab(1,ent,PSBPOP),tab(2,ent,PSBPOP),tab(3,ent,PSBPOP),tab(4,ent,PSBPOP),tab(5,ent,PSBPOP)
          lin=lin+1
        enddo
!        
        return
!=======FORMATS=================================================================
9000    FORMAT('ePassive Reservations snapshot (',I2,')')
9020    FORMAT(3X,'+++++++++++ CLASSICA ++++++++++++     +++++++++++ POPULAR +++++++++++++')
9030    FORMAT(3X,'Sal Emis Id  SCML  nResvd Tickets  |  Sal Emis Id  SCML  nResvd Tickets')
9040    FORMAT(3X,'=== ==== == ====== ====== =======  |  === ==== == ====== ====== =======')
9050    format(3X,I3,1X,I4,1X,I2,1X,I6.6,I7,1X,I7,'  |  ',I3,1X,I4,1X,I2,1X,I6.6,I7,1X,I7)
        end
!=======SUBS====================================================================
C=======OPTIONS /check=nooverflow
	subroutine nacsnp_retu(ind,pag)
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
	include 'inclib:taskid.def'
	include 'inclib:viscom.def'
!
        integer*4    ind
        integer*4    pag
        integer*4    tmp
        integer*4    pri
        integer*4    fin
        integer*4    lin
!
        if(pag.eq.2) then
          pri=18
          fin=34
        else if(pag.eq.3) then
          pri=35
          fin=50
        else
          pri=1
          fin=17
        endif      
        write(xnew(01),9000) ind,pag,daycdc              
        write(xnew(03),9010)            
        write(xnew(04),9020)                    
        lin=4
        do tmp=pri,fin
          if(passts(tmp,ind).ne.gamnul) then
            lin=lin+1
            write(xnew(lin),9030) tmp,pasemis(tmp,ind),
     *            pasretcnt(tmp,ind),pasretaftamt(tmp,ind),
     *            pastodpay(tmp,ind),cmony(pastopayamt(tmp,ind),10,valunit)
          endif
        enddo
!        
        return
!=======FORMATS=================================================================
9000    FORMAT('ePassive 'I1,' Returns pag ',I1,'/3 ',I4)
9010    FORMAT(1X,'Id Emis    RetCnt  RetAftDCnt   PayCnt   PayableAmt')
9020    FORMAT(1X,'== ==== ========== ========== ========== ==========')
9030    FORMAT(1X,I2,1X,I4,1X,I10,1X,I10,1X,I10,1X,A10)
        end
