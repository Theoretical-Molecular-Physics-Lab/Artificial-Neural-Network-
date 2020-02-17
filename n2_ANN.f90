!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!
!!    THIS CODE WAS CREATED BY MARCOS D. ALVES, RAMOM S. DA SILVA, 
!! AND MAIKEL Y. BALLESTER IN 02/12/2020. THIS PROGRAM GENERATES AN
!! OUTPUT CONTAINING A SET OF POINTS LIKE (R,V(R),V'(R)) FROM A TRAINED
!! ARTIFICIAL NEURAL NETWORK. MORE DETAILS CAN BE FOUNDED ON PAPER "Toward 
!! efficient construction of artificial neural networks represent-
!! ation for diatomic systems: A benchmark study". 
!!
!!  
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    Module start
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
module MMODULE
USE fann
implicit none
!
type(C_PTR) :: ann_N2,ann_dv
real,parameter:: dx = 1.0e-5 ! step of the derivative
real,parameter:: dr_inf = 0.392337084  !Parameter inserted for first derivative of  V(R)
integer,parameter :: mrm_a=23
integer :: i,j,points
real(kind=4)::dr,ii,xx
real(kind=4):: aux
real(kind=4),dimension(1) :: rr
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!Define the domain of V(R).
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
real(kind=4)::xi     ! initial interatomic distance
real(kind=4)::xf     ! final interatomic distance
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
 contains


!----------------------------------------------------------------------------------------------------------------
!            
!            Initializes trained networks
!----------------------------------------------------------------------------------------------------------------
    !----------------------------------------------------------------------------------------------------------------
 	subroutine initialize()
 	implicit none
	!
       !
       !                                         trained neural
       !                                         network for V'(R)
       !
	ann_dV = fann_create_from_file(f_c_string('dV_N2.dat'))
	!
	return
	end subroutine initialize   
    !----------------------------------------------------------------------------------------------------------------
    !----------------------------------------------------------------------------------------------------------------
 	subroutine begin()
 	implicit none
	!
       !
       !                                         trained neural
       !                                         network for V(R)
       !
	ann_N2 = fann_create_from_file(f_c_string('NN_N2.dat'))
	!
	return
	end subroutine begin   
    !-------------------------------------------------------------------
!            
!            End of initialization of trained artificial neural networks
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
!            Converter V(R) to a real number   ---------------------------------
!-----------------------------------------------------------------------
    real(kind=4) function potential(x)
    implicit none
    real(kind=4) :: x
       potential = pot(x)
    return
    end function potential
!----------------------------------------------------------------------------------------------------------------
    real(kind=4) function pot(x)
    implicit none
    !
    real(kind=4),intent(in) :: x   
    real(kind=4),dimension(1):: r
    real(kind=4),dimension(1) :: v
    !
    r(1) = x
    v=f_fann_run(ann_N2,r)
    pot=v(1) - 0.358847857!! well depth value for O2 molecule (in a.u.)
    return
    end function pot
!----------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------------------------------------------------
!----------------------------------------------------------------------
! first derivative calculation  
!----------------------------------------------------------------------
    real(kind=4) function der_1(x)
    implicit none
    !
    real(kind=4):: x
    real(kind=4),parameter::dx=0.00001d0        
    !   
    der_1 = 0.5*(pot(x+dx) - pot(x-dx))/dx  + 0.392337084! To avoid a division by zero, a non-zero value must be inserted
    !
    return
    end function der_1
   !-----------------------------------------------------------------------------------
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    Load artificial neural network with the information of the V'(R)
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(kind=4) function der_2(x)
    implicit none
    !
    real(kind=4),intent(in) :: x   
    real(kind=4),dimension(1):: r
    real(kind=4),dimension(1) :: vv
    !
    r(1) = x
    vv=f_fann_run(ann_dv,r)
    der_2=vv(1) - 2.39913034 ! 
    return
    end function der_2
    !----------------------------------------------------------------------------------
! correction term for V'(R) 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
    real(kind=4) function mrm(x)
    implicit none
    !
    real(kind=4),intent(in) :: x
    mrm=(der_2(x)/der_1(x))
    return
    end function mrm
    !
end module MMODULE 
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    End of module
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!    !             MAIN PROGRAM
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
program NN_N2_MRM
use MMODULE
implicit none                                                                        
! Open file to write the obtained results 
open(UNIT=mrm_a+1,FILE='n2_potential.res')       
! 
call initialize()
call begin()
write(*,*)' '                                                                                         
write(*,*)' '                                                                                         
write(*,*)'Diatomic system: N2 (ground state)' 
write(*,*)'Level of theory: MRCI(Q)/AVQZ' 
write(*,*) 'All values are in atomic units'
write(*,*)'This code provides the NN potential energy curve (PEC) and its first order derivative'
write(*,*)'for N2 molecule' 
write(*,*)' '                                                                                         
write(*,*)' '                                                                                         
write(*,*)' '                                                                                         
write(*,*)'Range of distances used in training: 1.5 to 22.0'
write(*,*)'------------------------------------------------------------------------------------- '
write(*,*)'------------------------------------------------------------------------------------- '
write(*,*)'Warning: The artificial neural network has been trained only in this range of distances,'
write(*,*)'For any interatomic distance outside of this interval, the trained NN provides non-physical values for this PEC.'                                                                                        
write(*,*)' '                                                                                         
write(*,*)' '                                                                                         
write(*,*)'Enter the V(R) domain:'
write(*,*)'Initial distance = ';read(*,*)xi             
write(*,*)'Final distance = ' ;read(*,*)xf         
write(*,*)'Enter the step associated to distances = ';read(*,*)dr
write(*,*)' '                                                                                         
write(*,*)' '                                                                                         
write(*,*)' '  
xx = xi
! 
points = int((xf - xi)/ dr)
do i=1,points
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!         
!                   column 1                     column 2                 column 3          
write(mrm_a+1,*)    xx,      pot(xx-dx)+mrm(xx-dx)*der_1(XX-dx)*dx,  mrm(xx)*der_1(XX)-dr_inf  
     xx=xx+dr
end do
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
stop
end program NN_N2_MRM
