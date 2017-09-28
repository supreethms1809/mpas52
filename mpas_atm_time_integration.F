!KGEN-generated Fortran source file 
  
!Generated at : 2017-07-10 21:23:06 
!KGEN version : 0.7.1 
  
! Copyright (c) 2013,  Los Alamos National Security, LLC (LANS)
! and the University Corporation for Atmospheric Research (UCAR).
!
! Unless noted otherwise source code is licensed under the BSD license.
! Additional copyright and license information can be found in the LICENSE file
! distributed with this code, or at http://mpas-dev.github.com/license.html
!

module atm_time_integration

    USE mpas_kind_types 
   ! Added only clause to keep xlf90 from getting confused from the overloaded abs intrinsic in mpas_timekeeping


    USE mpas_atm_iau 

    USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
    USE tprof_mod, ONLY: tstart, tstop, tnull, tprnt 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL 
    USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL 

   ! Used to store physics tendencies for dynamics variables
   real (kind=RKIND), allocatable, dimension(:,:) :: tend_ru_physics, tend_rtheta_physics, tend_rho_physics
   
   ! Used in compute_dyn_tend
   real (kind=RKIND), allocatable, dimension(:,:) :: qtot 
   real (kind=RKIND), allocatable, dimension(:,:) :: delsq_theta, delsq_w, delsq_divergence
   real (kind=RKIND), allocatable, dimension(:,:) :: delsq_u
!   real (kind=RKIND), allocatable, dimension(:,:) :: delsq_circulation    ! no longer used -> removed 
   real (kind=RKIND), allocatable, dimension(:,:) :: delsq_vorticity
   real (kind=RKIND), allocatable, dimension(:,:) :: dpdz

   ! Used in atm_advance_scalars

   ! Used in atm_advance_scalars_mono

   ! Used in compute_solve_diagnostics

      
   PUBLIC kr_externs_in_atm_time_integration 
   PUBLIC kr_externs_out_atm_time_integration 
   PUBLIC atm_compute_dyn_tend 
   REAL(KIND=rkind), allocatable, dimension(:,:) :: kgenref_delsq_theta, kgenref_delsq_w, kgenref_delsq_divergence 
   REAL(KIND=rkind), allocatable, dimension(:,:) :: kgenref_delsq_u 
   REAL(KIND=rkind), allocatable, dimension(:,:) :: kgenref_delsq_vorticity 
   REAL(KIND=rkind), allocatable, dimension(:,:) :: kgenref_dpdz 
   PUBLIC kv_externs_atm_time_integration 
   contains


   SUBROUTINE atm_compute_dyn_tend(kgen_unit, kgen_elapsed_time, kgen_isverified, nvertlevels, rk_step, dt, cellstart, cellend, &
   &vertexstart, vertexend, edgestart, edgeend, cellsolvestart, cellsolveend, vertexsolvestart, vertexsolveend, edgesolvestart, &
   &edgesolveend) 
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
   ! Compute height and normal wind tendencies, as well as diagnostic variables
   !
   ! Input: state - current model state
   !        mesh - grid metadata
   !        diag - some grid diagnostics
   !
   ! Output: tend - tendencies: tend_u, tend_w, tend_theta and tend_rho
   !                these are all coupled-variable tendencies.
   !         various other quantities in diag: Smagorinsky eddy viscosity
   !                
   !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 

       USE kgen_utils_mod, ONLY: kgen_dp, kgen_array_sumcheck 
       USE kgen_utils_mod, ONLY: check_t, kgen_init_check, CHECK_IDENTICAL, CHECK_IN_TOL, CHECK_OUT_TOL 
       USE kgen_utils_mod, ONLY: kgen_perturb_real 
       IMPLICIT NONE 

      !
      ! Dummy arguments
      !
       INTEGER, INTENT(INOUT) :: nvertlevels 
       INTEGER, INTENT(INOUT) :: rk_step 
       REAL(KIND=rkind), INTENT(INOUT) :: dt 
       INTEGER, INTENT(INOUT) :: cellstart, cellend, vertexstart, vertexend, edgestart, edgeend 
       INTEGER, INTENT(INOUT) :: cellsolvestart, cellsolveend, vertexsolvestart, vertexsolveend, edgesolvestart, edgesolveend 

      !
      ! Local variables
      !
       INTEGER, pointer :: ncells, nedges, nvertices, ncellssolve, nedgessolve, vertexdegree, maxedges, maxedges2 
       INTEGER, pointer :: moist_start, moist_end, num_scalars 

       REAL(KIND=rkind), dimension(:), pointer :: fedge, dvedge, dcedge, invdcedge, invdvedge, invareacell, invareatriangle, &
       &meshscalingdel2, meshscalingdel4 
       REAL(KIND=rkind), dimension(:,:), pointer :: weightsonedge, zgrid, rho_edge, rho_zz, ru, u, v, tend_u, divergence, &
       &vorticity, ke, pv_edge, theta_m, rw, tend_rho, rt_diabatic_tend, tend_theta, tend_w, w, cqw, rb, rr, pp, pressure_b, zz, &
       &zxu, cqu, h_divergence, kdiff, edgesoncell_sign, edgesonvertex_sign, rw_save, ru_save 

       REAL(KIND=rkind), dimension(:,:), pointer :: theta_m_save 

       REAL(KIND=rkind), dimension(:,:), pointer :: exner 

       REAL(KIND=rkind), dimension(:,:), pointer :: rr_save 


       REAL(KIND=rkind), dimension(:,:), pointer :: tend_rtheta_adv 
       REAL(KIND=rkind), dimension(:,:), pointer :: rthdynten 

       REAL(KIND=rkind), dimension(:,:,:), pointer :: scalars 

       REAL(KIND=rkind), dimension(:,:), pointer :: tend_u_euler, tend_w_euler, tend_theta_euler 

       REAL(KIND=rkind), dimension(:,:,:), pointer :: deriv_two 
       INTEGER, dimension(:,:), pointer :: cellsonedge, verticesonedge, edgesoncell, edgesonedge, cellsoncell, edgesonvertex 
       INTEGER, dimension(:), pointer :: nedgesoncell, nedgesonedge 
       REAL(KIND=rkind), dimension(:), pointer :: latcell, latedge, angleedge, u_init 

       INTEGER, dimension(:,:), pointer :: advcellsforedge 
       INTEGER, dimension(:), pointer :: nadvcellsforedge 
       REAL(KIND=rkind), dimension(:,:), pointer :: adv_coefs, adv_coefs_3rd 

       REAL(KIND=rkind), dimension(:), pointer :: rdzu, rdzw, fzm, fzp, qv_init 
       REAL(KIND=rkind), dimension(:,:), pointer :: t_init 

       REAL(KIND=rkind), pointer :: cf1, cf2, cf3 

       REAL(KIND=rkind), pointer :: r_earth 
       REAL(KIND=rkind), dimension(:,:), pointer :: ur_cell, vr_cell 

       REAL(KIND=rkind), dimension(:,:), pointer :: defc_a, defc_b 

       REAL(KIND=rkind), dimension(:,:), pointer :: tend_w_pgf, tend_w_buoy 

       REAL(KIND=rkind), pointer :: coef_3rd_order, c_s 
       LOGICAL, pointer :: config_mix_full 
       CHARACTER(LEN=strkind), pointer :: config_horiz_mixing 
       REAL(KIND=rkind), pointer :: config_del4u_div_factor 
       REAL(KIND=rkind), pointer :: config_h_theta_eddy_visc4 
       REAL(KIND=rkind), pointer :: config_h_mom_eddy_visc4 
       REAL(KIND=rkind), pointer :: config_visc4_2dsmag 
       REAL(KIND=rkind), pointer :: config_len_disp 
       REAL(KIND=rkind), pointer :: config_h_mom_eddy_visc2, config_v_mom_eddy_visc2 
       REAL(KIND=rkind), pointer :: config_h_theta_eddy_visc2, config_v_theta_eddy_visc2 


       INTEGER, INTENT(IN) :: kgen_unit 
       REAL(KIND=kgen_dp), INTENT(OUT) :: kgen_elapsed_time 
       LOGICAL, INTENT(OUT) :: kgen_isverified 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
         
       INTEGER :: kgenref_nvertlevels 
       INTEGER, pointer :: kgenref_ncells 
       INTEGER, pointer :: kgenref_nedges 
       INTEGER, pointer :: kgenref_nvertices 
       INTEGER, pointer :: kgenref_ncellssolve 
       INTEGER, pointer :: kgenref_nedgessolve 
       INTEGER, pointer :: kgenref_vertexdegree 
       INTEGER, pointer :: kgenref_maxedges 
       INTEGER, pointer :: kgenref_maxedges2 
       INTEGER, pointer :: kgenref_num_scalars 
       INTEGER, pointer :: kgenref_moist_start 
       INTEGER, pointer :: kgenref_moist_end 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_fedge 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_dvedge 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_dcedge 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_invdcedge 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_invdvedge 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_invareacell 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_invareatriangle 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_meshscalingdel2 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_meshscalingdel4 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_weightsonedge 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_zgrid 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_rho_edge 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_rho_zz 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_ru 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_u 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_v 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_tend_u 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_divergence 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_vorticity 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_ke 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_pv_edge 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_theta_m 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_rw 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_tend_rho 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_rt_diabatic_tend 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_tend_theta 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_tend_w 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_w 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_cqw 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_rb 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_rr 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_pp 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_pressure_b 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_zz 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_zxu 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_cqu 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_h_divergence 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_kdiff 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_edgesoncell_sign 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_edgesonvertex_sign 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_rw_save 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_ru_save 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_theta_m_save 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_exner 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_rr_save 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_tend_rtheta_adv 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_rthdynten 
       REAL(KIND=rkind), pointer, dimension(:,:,:) :: kgenref_scalars 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_tend_u_euler 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_tend_w_euler 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_tend_theta_euler 
       REAL(KIND=rkind), pointer, dimension(:,:,:) :: kgenref_deriv_two 
       INTEGER, pointer, dimension(:,:) :: kgenref_cellsonedge 
       INTEGER, pointer, dimension(:,:) :: kgenref_verticesonedge 
       INTEGER, pointer, dimension(:,:) :: kgenref_edgesoncell 
       INTEGER, pointer, dimension(:,:) :: kgenref_edgesonedge 
       INTEGER, pointer, dimension(:,:) :: kgenref_cellsoncell 
       INTEGER, pointer, dimension(:,:) :: kgenref_edgesonvertex 
       INTEGER, pointer, dimension(:) :: kgenref_nedgesoncell 
       INTEGER, pointer, dimension(:) :: kgenref_nedgesonedge 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_latcell 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_latedge 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_angleedge 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_u_init 
       INTEGER, pointer, dimension(:,:) :: kgenref_advcellsforedge 
       INTEGER, pointer, dimension(:) :: kgenref_nadvcellsforedge 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_adv_coefs 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_adv_coefs_3rd 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_rdzu 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_rdzw 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_fzm 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_fzp 
       REAL(KIND=rkind), pointer, dimension(:) :: kgenref_qv_init 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_t_init 
       REAL(KIND=rkind), pointer :: kgenref_cf1 
       REAL(KIND=rkind), pointer :: kgenref_cf2 
       REAL(KIND=rkind), pointer :: kgenref_cf3 
       REAL(KIND=rkind), pointer :: kgenref_r_earth 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_ur_cell 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_vr_cell 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_defc_a 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_defc_b 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_tend_w_pgf 
       REAL(KIND=rkind), pointer, dimension(:,:) :: kgenref_tend_w_buoy 
       REAL(KIND=rkind), pointer :: kgenref_coef_3rd_order 
       REAL(KIND=rkind), pointer :: kgenref_c_s 
       LOGICAL, pointer :: kgenref_config_mix_full 
       CHARACTER(LEN=strkind), pointer :: kgenref_config_horiz_mixing 
       REAL(KIND=rkind), pointer :: kgenref_config_del4u_div_factor 
       REAL(KIND=rkind), pointer :: kgenref_config_h_theta_eddy_visc4 
       REAL(KIND=rkind), pointer :: kgenref_config_h_mom_eddy_visc4 
       REAL(KIND=rkind), pointer :: kgenref_config_visc4_2dsmag 
       REAL(KIND=rkind), pointer :: kgenref_config_len_disp 
       REAL(KIND=rkind), pointer :: kgenref_config_h_mom_eddy_visc2 
       REAL(KIND=rkind), pointer :: kgenref_config_v_mom_eddy_visc2 
       REAL(KIND=rkind), pointer :: kgenref_config_h_theta_eddy_visc2 
       REAL(KIND=rkind), pointer :: kgenref_config_v_theta_eddy_visc2 
       TYPE(check_t) :: check_status 
       INTEGER*8 :: kgen_intvar, kgen_start_clock, kgen_stop_clock, kgen_rate_clock 
       INTEGER, PARAMETER :: kgen_maxiter = 1 


      !
      ! rthdynten is currently associated with packages, and if those packages
      ! are not active at run-time, we need to produce an rthdynten array for
      ! use in the atm_compute_dyn_tend_work routine
      !


!$KGEN begin_callsite atm_compute_dyn_tend_work
         
       !local input variables 
       CALL kr_atm_compute_dyn_tend_integer___ptr(ncells, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(nedges, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(nvertices, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(ncellssolve, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(nedgessolve, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(vertexdegree, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(maxedges, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(maxedges2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(num_scalars, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(moist_start, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(moist_end, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(fedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(dvedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(dcedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(invdcedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(invdvedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(invareacell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(invareatriangle, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(meshscalingdel2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(meshscalingdel4, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(weightsonedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(zgrid, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(rho_edge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(rho_zz, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(ru, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(u, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(v, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(tend_u, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(divergence, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(vorticity, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(ke, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(pv_edge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(theta_m, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(rw, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(tend_rho, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(rt_diabatic_tend, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(tend_theta, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(tend_w, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(w, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(cqw, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(rb, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(rr, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(pp, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(pressure_b, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(zz, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(zxu, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(cqu, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(h_divergence, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kdiff, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(edgesoncell_sign, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(edgesonvertex_sign, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(rw_save, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(ru_save, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(theta_m_save, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(exner, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(rr_save, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(tend_rtheta_adv, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(rthdynten, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim3_ptr(scalars, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(tend_u_euler, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(tend_w_euler, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(tend_theta_euler, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim3_ptr(deriv_two, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(cellsonedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(verticesonedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(edgesoncell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(edgesonedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(cellsoncell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(edgesonvertex, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim1_ptr(nedgesoncell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim1_ptr(nedgesonedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(latcell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(latedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(angleedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(u_init, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(advcellsforedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim1_ptr(nadvcellsforedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(adv_coefs, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(adv_coefs_3rd, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(rdzu, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(rdzw, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(fzm, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(fzp, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(qv_init, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(t_init, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(cf1, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(cf2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(cf3, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(r_earth, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(ur_cell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(vr_cell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(defc_a, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(defc_b, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(tend_w_pgf, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(tend_w_buoy, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(coef_3rd_order, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(c_s, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_logical___ptr(config_mix_full, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_character_strkind__ptr(config_horiz_mixing, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(config_del4u_div_factor, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(config_h_theta_eddy_visc4, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(config_h_mom_eddy_visc4, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(config_visc4_2dsmag, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(config_len_disp, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(config_h_mom_eddy_visc2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(config_v_mom_eddy_visc2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(config_h_theta_eddy_visc2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(config_v_theta_eddy_visc2, kgen_unit) 
         
       !extern output variables 
       CALL kr_externs_out_atm_time_integration(kgen_unit) 
         
       !local output variables 
       READ (UNIT = kgen_unit) kgenref_nvertlevels 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_ncells, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_nedges, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_nvertices, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_ncellssolve, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_nedgessolve, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_vertexdegree, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_maxedges, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_maxedges2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_num_scalars, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_moist_start, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___ptr(kgenref_moist_end, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_fedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_dvedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_dcedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_invdcedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_invdvedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_invareacell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_invareatriangle, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_meshscalingdel2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_meshscalingdel4, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_weightsonedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_zgrid, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_rho_edge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_rho_zz, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_ru, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_u, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_v, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_tend_u, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_divergence, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_vorticity, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_ke, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_pv_edge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_theta_m, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_rw, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_tend_rho, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_rt_diabatic_tend, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_tend_theta, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_tend_w, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_w, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_cqw, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_rb, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_rr, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_pp, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_pressure_b, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_zz, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_zxu, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_cqu, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_h_divergence, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_kdiff, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_edgesoncell_sign, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_edgesonvertex_sign, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_rw_save, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_ru_save, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_theta_m_save, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_exner, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_rr_save, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_tend_rtheta_adv, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_rthdynten, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim3_ptr(kgenref_scalars, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_tend_u_euler, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_tend_w_euler, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_tend_theta_euler, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim3_ptr(kgenref_deriv_two, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(kgenref_cellsonedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(kgenref_verticesonedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(kgenref_edgesoncell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(kgenref_edgesonedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(kgenref_cellsoncell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(kgenref_edgesonvertex, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim1_ptr(kgenref_nedgesoncell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim1_ptr(kgenref_nedgesonedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_latcell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_latedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_angleedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_u_init, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim2_ptr(kgenref_advcellsforedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_integer___dim1_ptr(kgenref_nadvcellsforedge, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_adv_coefs, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_adv_coefs_3rd, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_rdzu, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_rdzw, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_fzm, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_fzp, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(kgenref_qv_init, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_t_init, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_cf1, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_cf2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_cf3, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_r_earth, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_ur_cell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_vr_cell, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_defc_a, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_defc_b, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_tend_w_pgf, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(kgenref_tend_w_buoy, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_coef_3rd_order, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_c_s, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_logical___ptr(kgenref_config_mix_full, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_character_strkind__ptr(kgenref_config_horiz_mixing, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_config_del4u_div_factor, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_config_h_theta_eddy_visc4, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_config_h_mom_eddy_visc4, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_config_visc4_2dsmag, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_config_len_disp, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_config_h_mom_eddy_visc2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_config_v_mom_eddy_visc2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_config_h_theta_eddy_visc2, kgen_unit) 
       CALL kr_atm_compute_dyn_tend_real__rkind_ptr(kgenref_config_v_theta_eddy_visc2, kgen_unit) 
         
       !Uncomment following call statement to turn on perturbation experiment. 
       !Adjust perturbation value and/or kind parameter if required. 
       !CALL kgen_perturb_real( your_variable, 1.0E-15_8 ) 
         
         
       !call to kgen kernel 
      call atm_compute_dyn_tend_work(nCells, nEdges, nVertices, nVertLevels, &
         nCellsSolve, nEdgesSolve, vertexDegree, maxEdges, maxEdges2, num_scalars, moist_start, moist_end, &
         fEdge, dvEdge, dcEdge, invDcEdge, invDvEdge, invAreaCell, invAreaTriangle, meshScalingDel2, meshScalingDel4, &
         weightsOnEdge, zgrid, rho_edge, rho_zz, ru, u, v, tend_u, &
         divergence, vorticity, ke, pv_edge, theta_m, rw, tend_rho, &
         rt_diabatic_tend, tend_theta, tend_w, w, cqw, rb, rr, pp, pressure_b, zz, zxu, cqu, & 
         h_divergence, kdiff, edgesOnCell_sign, edgesOnVertex_sign, rw_save, ru_save, &
         theta_m_save, exner, rr_save, scalars, tend_u_euler, tend_w_euler, tend_theta_euler, deriv_two, &
         cellsOnEdge, verticesOnEdge, edgesOnCell, edgesOnEdge, cellsOnCell, edgesOnVertex, nEdgesOnCell, nEdgesOnEdge, &
         latCell, latEdge, angleEdge, u_init, advCellsForEdge, nAdvCellsForEdge, adv_coefs, adv_coefs_3rd, &
         rdzu, rdzw, fzm, fzp, qv_init, t_init, cf1, cf2, cf3, r_earth, ur_cell, vr_cell, defc_a, defc_b, &
         tend_w_pgf, tend_w_buoy, coef_3rd_order, c_s, config_mix_full, config_horiz_mixing, config_del4u_div_factor, &
         config_h_mom_eddy_visc2, config_v_mom_eddy_visc2, config_h_theta_eddy_visc2, config_v_theta_eddy_visc2, &
         config_h_theta_eddy_visc4, config_h_mom_eddy_visc4, config_visc4_2dsmag, config_len_disp, rk_step, dt, &
         tend_rtheta_adv, rthdynten, &
         cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
         cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd)
!$KGEN end_callsite atm_compute_dyn_tend_work


        
      !verify init 
      CALL kgen_init_check(check_status, tolerance=1.D-14, verboseLevel=1) 
        
      !extern verify variables 
      CALL kv_externs_atm_time_integration(check_status) 
        
      !local verify variables 
      CALL kv_atm_compute_dyn_tend_integer__("nvertlevels", check_status, nvertlevels, kgenref_nvertlevels) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("nedges", check_status, nedges, kgenref_nedges) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("vertexdegree", check_status, vertexdegree, kgenref_vertexdegree) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("maxedges2", check_status, maxedges2, kgenref_maxedges2) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("nvertices", check_status, nvertices, kgenref_nvertices) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("ncellssolve", check_status, ncellssolve, kgenref_ncellssolve) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("nedgessolve", check_status, nedgessolve, kgenref_nedgessolve) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("maxedges", check_status, maxedges, kgenref_maxedges) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("ncells", check_status, ncells, kgenref_ncells) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("moist_start", check_status, moist_start, kgenref_moist_start) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("moist_end", check_status, moist_end, kgenref_moist_end) 
      CALL kv_atm_compute_dyn_tend_integer___ptr("num_scalars", check_status, num_scalars, kgenref_num_scalars) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("invareacell", check_status, invareacell, kgenref_invareacell) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("invdcedge", check_status, invdcedge, kgenref_invdcedge) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("fedge", check_status, fedge, kgenref_fedge) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("meshscalingdel4", check_status, meshscalingdel4, &
      &kgenref_meshscalingdel4) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("invdvedge", check_status, invdvedge, kgenref_invdvedge) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("meshscalingdel2", check_status, meshscalingdel2, &
      &kgenref_meshscalingdel2) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("dcedge", check_status, dcedge, kgenref_dcedge) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("dvedge", check_status, dvedge, kgenref_dvedge) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("invareatriangle", check_status, invareatriangle, &
      &kgenref_invareatriangle) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("pv_edge", check_status, pv_edge, kgenref_pv_edge) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("edgesonvertex_sign", check_status, edgesonvertex_sign, &
      &kgenref_edgesonvertex_sign) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("vorticity", check_status, vorticity, kgenref_vorticity) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("zz", check_status, zz, kgenref_zz) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("zgrid", check_status, zgrid, kgenref_zgrid) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("ru", check_status, ru, kgenref_ru) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("pp", check_status, pp, kgenref_pp) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("rw", check_status, rw, kgenref_rw) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("rr", check_status, rr, kgenref_rr) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("tend_u", check_status, tend_u, kgenref_tend_u) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("tend_w", check_status, tend_w, kgenref_tend_w) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("rt_diabatic_tend", check_status, rt_diabatic_tend, &
      &kgenref_rt_diabatic_tend) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("edgesoncell_sign", check_status, edgesoncell_sign, &
      &kgenref_edgesoncell_sign) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("zxu", check_status, zxu, kgenref_zxu) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("rb", check_status, rb, kgenref_rb) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("cqw", check_status, cqw, kgenref_cqw) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("cqu", check_status, cqu, kgenref_cqu) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("kdiff", check_status, kdiff, kgenref_kdiff) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("h_divergence", check_status, h_divergence, kgenref_h_divergence) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("divergence", check_status, divergence, kgenref_divergence) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("ru_save", check_status, ru_save, kgenref_ru_save) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("pressure_b", check_status, pressure_b, kgenref_pressure_b) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("tend_rho", check_status, tend_rho, kgenref_tend_rho) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("tend_theta", check_status, tend_theta, kgenref_tend_theta) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("weightsonedge", check_status, weightsonedge, kgenref_weightsonedge) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("rho_edge", check_status, rho_edge, kgenref_rho_edge) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("ke", check_status, ke, kgenref_ke) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("theta_m", check_status, theta_m, kgenref_theta_m) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("rho_zz", check_status, rho_zz, kgenref_rho_zz) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("u", check_status, u, kgenref_u) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("w", check_status, w, kgenref_w) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("v", check_status, v, kgenref_v) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("rw_save", check_status, rw_save, kgenref_rw_save) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("theta_m_save", check_status, theta_m_save, kgenref_theta_m_save) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("exner", check_status, exner, kgenref_exner) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("rr_save", check_status, rr_save, kgenref_rr_save) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("tend_rtheta_adv", check_status, tend_rtheta_adv, &
      &kgenref_tend_rtheta_adv) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("rthdynten", check_status, rthdynten, kgenref_rthdynten) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim3_ptr("scalars", check_status, scalars, kgenref_scalars) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("tend_u_euler", check_status, tend_u_euler, kgenref_tend_u_euler) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("tend_theta_euler", check_status, tend_theta_euler, &
      &kgenref_tend_theta_euler) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("tend_w_euler", check_status, tend_w_euler, kgenref_tend_w_euler) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim3_ptr("deriv_two", check_status, deriv_two, kgenref_deriv_two) 
      CALL kv_atm_compute_dyn_tend_integer___dim2_ptr("edgesonvertex", check_status, edgesonvertex, kgenref_edgesonvertex) 
      CALL kv_atm_compute_dyn_tend_integer___dim2_ptr("edgesoncell", check_status, edgesoncell, kgenref_edgesoncell) 
      CALL kv_atm_compute_dyn_tend_integer___dim2_ptr("cellsonedge", check_status, cellsonedge, kgenref_cellsonedge) 
      CALL kv_atm_compute_dyn_tend_integer___dim2_ptr("cellsoncell", check_status, cellsoncell, kgenref_cellsoncell) 
      CALL kv_atm_compute_dyn_tend_integer___dim2_ptr("edgesonedge", check_status, edgesonedge, kgenref_edgesonedge) 
      CALL kv_atm_compute_dyn_tend_integer___dim2_ptr("verticesonedge", check_status, verticesonedge, kgenref_verticesonedge) 
      CALL kv_atm_compute_dyn_tend_integer___dim1_ptr("nedgesonedge", check_status, nedgesonedge, kgenref_nedgesonedge) 
      CALL kv_atm_compute_dyn_tend_integer___dim1_ptr("nedgesoncell", check_status, nedgesoncell, kgenref_nedgesoncell) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("u_init", check_status, u_init, kgenref_u_init) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("angleedge", check_status, angleedge, kgenref_angleedge) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("latcell", check_status, latcell, kgenref_latcell) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("latedge", check_status, latedge, kgenref_latedge) 
      CALL kv_atm_compute_dyn_tend_integer___dim2_ptr("advcellsforedge", check_status, advcellsforedge, kgenref_advcellsforedge) 
      CALL kv_atm_compute_dyn_tend_integer___dim1_ptr("nadvcellsforedge", check_status, nadvcellsforedge, &
      &kgenref_nadvcellsforedge) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("adv_coefs", check_status, adv_coefs, kgenref_adv_coefs) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("adv_coefs_3rd", check_status, adv_coefs_3rd, kgenref_adv_coefs_3rd) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("qv_init", check_status, qv_init, kgenref_qv_init) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("fzp", check_status, fzp, kgenref_fzp) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("rdzu", check_status, rdzu, kgenref_rdzu) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("fzm", check_status, fzm, kgenref_fzm) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim1_ptr("rdzw", check_status, rdzw, kgenref_rdzw) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("t_init", check_status, t_init, kgenref_t_init) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("cf2", check_status, cf2, kgenref_cf2) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("cf3", check_status, cf3, kgenref_cf3) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("cf1", check_status, cf1, kgenref_cf1) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("r_earth", check_status, r_earth, kgenref_r_earth) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("vr_cell", check_status, vr_cell, kgenref_vr_cell) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("ur_cell", check_status, ur_cell, kgenref_ur_cell) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("defc_b", check_status, defc_b, kgenref_defc_b) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("defc_a", check_status, defc_a, kgenref_defc_a) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("tend_w_buoy", check_status, tend_w_buoy, kgenref_tend_w_buoy) 
      CALL kv_atm_compute_dyn_tend_real__rkind_dim2_ptr("tend_w_pgf", check_status, tend_w_pgf, kgenref_tend_w_pgf) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("c_s", check_status, c_s, kgenref_c_s) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("coef_3rd_order", check_status, coef_3rd_order, kgenref_coef_3rd_order) 
      CALL kv_atm_compute_dyn_tend_logical___ptr("config_mix_full", check_status, config_mix_full, kgenref_config_mix_full) 
      CALL kv_atm_compute_dyn_tend_character_strkind__ptr("config_horiz_mixing", check_status, config_horiz_mixing, &
      &kgenref_config_horiz_mixing) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("config_del4u_div_factor", check_status, config_del4u_div_factor, &
      &kgenref_config_del4u_div_factor) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("config_h_theta_eddy_visc4", check_status, config_h_theta_eddy_visc4, &
      &kgenref_config_h_theta_eddy_visc4) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("config_h_mom_eddy_visc4", check_status, config_h_mom_eddy_visc4, &
      &kgenref_config_h_mom_eddy_visc4) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("config_visc4_2dsmag", check_status, config_visc4_2dsmag, &
      &kgenref_config_visc4_2dsmag) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("config_len_disp", check_status, config_len_disp, kgenref_config_len_disp) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("config_v_mom_eddy_visc2", check_status, config_v_mom_eddy_visc2, &
      &kgenref_config_v_mom_eddy_visc2) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("config_h_mom_eddy_visc2", check_status, config_h_mom_eddy_visc2, &
      &kgenref_config_h_mom_eddy_visc2) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("config_h_theta_eddy_visc2", check_status, config_h_theta_eddy_visc2, &
      &kgenref_config_h_theta_eddy_visc2) 
      CALL kv_atm_compute_dyn_tend_real__rkind_ptr("config_v_theta_eddy_visc2", check_status, config_v_theta_eddy_visc2, &
      &kgenref_config_v_theta_eddy_visc2) 
      WRITE (*, *) "" 
      IF (check_status%verboseLevel > 0) THEN 
          WRITE (*, *) "Number of output-type variables: ", check_status%numTotal 
          WRITE (*, *) "Number of identical variables: ", check_status%numIdentical 
          WRITE (*, *) "Number of non-identical variables within tolerance: ", check_status%numInTol 
          WRITE (*, *) "Number of non-identical variables out of tolerance: ", check_status%numOutTol 
          WRITE (*, *) "Tolerance: ", check_status%tolerance 
      END IF   
      WRITE (*, *) "" 
      IF (check_status%numOutTol > 0) THEN 
          WRITE (*, *) "Verification FAILED" 
          check_status%Passed = .FALSE. 
          kgen_isverified = .FALSE. 
      ELSE 
          WRITE (*, *) "Verification PASSED" 
          check_status%Passed = .TRUE. 
          kgen_isverified = .TRUE. 
      END IF   
      WRITE (*, *) "" 
        
      !Measuring elapsed time. Please increase the value of kgen_maxiter to get improve timing measurment resolution. 
     ! CALL SYSTEM_CLOCK(kgen_start_clock, kgen_rate_clock) 
     ! DO kgen_intvar = 1, kgen_maxiter 
     ! call atm_compute_dyn_tend_work(nCells, nEdges, nVertices, nVertLevels, &
     !    nCellsSolve, nEdgesSolve, vertexDegree, maxEdges, maxEdges2, num_scalars, moist_start, moist_end, &
     !    fEdge, dvEdge, dcEdge, invDcEdge, invDvEdge, invAreaCell, invAreaTriangle, meshScalingDel2, meshScalingDel4, &
     !    weightsOnEdge, zgrid, rho_edge, rho_zz, ru, u, v, tend_u, &
     !    divergence, vorticity, ke, pv_edge, theta_m, rw, tend_rho, &
     !    rt_diabatic_tend, tend_theta, tend_w, w, cqw, rb, rr, pp, pressure_b, zz, zxu, cqu, & 
     !    h_divergence, kdiff, edgesOnCell_sign, edgesOnVertex_sign, rw_save, ru_save, &
     !    theta_m_save, exner, rr_save, scalars, tend_u_euler, tend_w_euler, tend_theta_euler, deriv_two, &
     !    cellsOnEdge, verticesOnEdge, edgesOnCell, edgesOnEdge, cellsOnCell, edgesOnVertex, nEdgesOnCell, nEdgesOnEdge, &
     !    latCell, latEdge, angleEdge, u_init, advCellsForEdge, nAdvCellsForEdge, adv_coefs, adv_coefs_3rd, &
     !    rdzu, rdzw, fzm, fzp, qv_init, t_init, cf1, cf2, cf3, r_earth, ur_cell, vr_cell, defc_a, defc_b, &
     !    tend_w_pgf, tend_w_buoy, coef_3rd_order, c_s, config_mix_full, config_horiz_mixing, config_del4u_div_factor, &
     !    config_h_mom_eddy_visc2, config_v_mom_eddy_visc2, config_h_theta_eddy_visc2, config_v_theta_eddy_visc2, &
     !    config_h_theta_eddy_visc4, config_h_mom_eddy_visc4, config_visc4_2dsmag, config_len_disp, rk_step, dt, &
     !    tend_rtheta_adv, rthdynten, &
     !    cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
     !    cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd)
     ! END DO   
     ! CALL SYSTEM_CLOCK(kgen_stop_clock, kgen_rate_clock) 
     ! kgen_elapsed_time = 1.0e6*(kgen_stop_clock - kgen_start_clock)/REAL(kgen_rate_clock*kgen_maxiter) 
     ! WRITE (*, *) "atm_compute_dyn_tend_work : Time per call (usec): ", kgen_elapsed_time 
        
      CONTAINS 
        
      !read state subroutine for kr_atm_compute_dyn_tend_integer___ptr 
      SUBROUTINE kr_atm_compute_dyn_tend_integer___ptr(var, kgen_unit, printvar) 
          INTEGER, INTENT(INOUT), POINTER :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
            
          READ (UNIT = kgen_unit) kgen_istrue 
          IF (kgen_istrue) THEN 
              IF (ASSOCIATED( var )) THEN 
                  NULLIFY (var) 
              END IF   
              ALLOCATE (var) 
              READ (UNIT = kgen_unit) var 
              IF (PRESENT( printvar )) THEN 
                  WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
              END IF   
          END IF   
      END SUBROUTINE kr_atm_compute_dyn_tend_integer___ptr 
        
      !read state subroutine for kr_atm_compute_dyn_tend_real__rkind_dim1_ptr 
      SUBROUTINE kr_atm_compute_dyn_tend_real__rkind_dim1_ptr(var, kgen_unit, printvar) 
          REAL(KIND=rkind), INTENT(INOUT), POINTER, DIMENSION(:) :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
          INTEGER :: idx1 
          INTEGER, DIMENSION(2,1) :: kgen_bound 
            
          READ (UNIT = kgen_unit) kgen_istrue 
          IF (kgen_istrue) THEN 
              IF (ASSOCIATED( var )) THEN 
                  NULLIFY (var) 
              END IF   
              READ (UNIT = kgen_unit) kgen_array_sum 
              READ (UNIT = kgen_unit) kgen_bound(1, 1) 
              READ (UNIT = kgen_unit) kgen_bound(2, 1) 
              ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
              READ (UNIT = kgen_unit) var 
              IF (PRESENT( printvar )) THEN 
                  CALL kgen_array_sumcheck(printvar, kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
                  WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
              ELSE 
                  CALL kgen_array_sumcheck("UNKNOWN", kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
              END IF   
          END IF   
      END SUBROUTINE kr_atm_compute_dyn_tend_real__rkind_dim1_ptr 
        
      !read state subroutine for kr_atm_compute_dyn_tend_real__rkind_dim2_ptr 
      SUBROUTINE kr_atm_compute_dyn_tend_real__rkind_dim2_ptr(var, kgen_unit, printvar) 
          REAL(KIND=rkind), INTENT(INOUT), POINTER, DIMENSION(:,:) :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
          INTEGER :: idx1, idx2 
          INTEGER, DIMENSION(2,2) :: kgen_bound 
            
          READ (UNIT = kgen_unit) kgen_istrue 
          IF (kgen_istrue) THEN 
              IF (ASSOCIATED( var )) THEN 
                  NULLIFY (var) 
              END IF   
              READ (UNIT = kgen_unit) kgen_array_sum 
              READ (UNIT = kgen_unit) kgen_bound(1, 1) 
              READ (UNIT = kgen_unit) kgen_bound(2, 1) 
              READ (UNIT = kgen_unit) kgen_bound(1, 2) 
              READ (UNIT = kgen_unit) kgen_bound(2, 2) 
              ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
              READ (UNIT = kgen_unit) var 
              IF (PRESENT( printvar )) THEN 
                  CALL kgen_array_sumcheck(printvar, kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
                  WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
              ELSE 
                  CALL kgen_array_sumcheck("UNKNOWN", kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
              END IF   
          END IF   
      END SUBROUTINE kr_atm_compute_dyn_tend_real__rkind_dim2_ptr 
        
      !read state subroutine for kr_atm_compute_dyn_tend_real__rkind_dim3_ptr 
      SUBROUTINE kr_atm_compute_dyn_tend_real__rkind_dim3_ptr(var, kgen_unit, printvar) 
          REAL(KIND=rkind), INTENT(INOUT), POINTER, DIMENSION(:,:,:) :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
          INTEGER :: idx1, idx2, idx3 
          INTEGER, DIMENSION(2,3) :: kgen_bound 
            
          READ (UNIT = kgen_unit) kgen_istrue 
          IF (kgen_istrue) THEN 
              IF (ASSOCIATED( var )) THEN 
                  NULLIFY (var) 
              END IF   
              READ (UNIT = kgen_unit) kgen_array_sum 
              READ (UNIT = kgen_unit) kgen_bound(1, 1) 
              READ (UNIT = kgen_unit) kgen_bound(2, 1) 
              READ (UNIT = kgen_unit) kgen_bound(1, 2) 
              READ (UNIT = kgen_unit) kgen_bound(2, 2) 
              READ (UNIT = kgen_unit) kgen_bound(1, 3) 
              READ (UNIT = kgen_unit) kgen_bound(2, 3) 
              ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2), kgen_bound(1,3):kgen_bound(2,3))) 
              READ (UNIT = kgen_unit) var 
              IF (PRESENT( printvar )) THEN 
                  CALL kgen_array_sumcheck(printvar, kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
                  WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
              ELSE 
                  CALL kgen_array_sumcheck("UNKNOWN", kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
              END IF   
          END IF   
      END SUBROUTINE kr_atm_compute_dyn_tend_real__rkind_dim3_ptr 
        
      !read state subroutine for kr_atm_compute_dyn_tend_integer___dim2_ptr 
      SUBROUTINE kr_atm_compute_dyn_tend_integer___dim2_ptr(var, kgen_unit, printvar) 
          INTEGER, INTENT(INOUT), POINTER, DIMENSION(:,:) :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
          INTEGER :: idx1, idx2 
          INTEGER, DIMENSION(2,2) :: kgen_bound 
            
          READ (UNIT = kgen_unit) kgen_istrue 
          IF (kgen_istrue) THEN 
              IF (ASSOCIATED( var )) THEN 
                  NULLIFY (var) 
              END IF   
              READ (UNIT = kgen_unit) kgen_array_sum 
              READ (UNIT = kgen_unit) kgen_bound(1, 1) 
              READ (UNIT = kgen_unit) kgen_bound(2, 1) 
              READ (UNIT = kgen_unit) kgen_bound(1, 2) 
              READ (UNIT = kgen_unit) kgen_bound(2, 2) 
              ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
              READ (UNIT = kgen_unit) var 
              IF (PRESENT( printvar )) THEN 
                  CALL kgen_array_sumcheck(printvar, kgen_array_sum, REAL(SUM(var), 8), .TRUE.) 
                  WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
              ELSE 
                  CALL kgen_array_sumcheck("UNKNOWN", kgen_array_sum, REAL(SUM(var), 8), .TRUE.) 
              END IF   
          END IF   
      END SUBROUTINE kr_atm_compute_dyn_tend_integer___dim2_ptr 
        
      !read state subroutine for kr_atm_compute_dyn_tend_integer___dim1_ptr 
      SUBROUTINE kr_atm_compute_dyn_tend_integer___dim1_ptr(var, kgen_unit, printvar) 
          INTEGER, INTENT(INOUT), POINTER, DIMENSION(:) :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
          INTEGER :: idx1 
          INTEGER, DIMENSION(2,1) :: kgen_bound 
            
          READ (UNIT = kgen_unit) kgen_istrue 
          IF (kgen_istrue) THEN 
              IF (ASSOCIATED( var )) THEN 
                  NULLIFY (var) 
              END IF   
              READ (UNIT = kgen_unit) kgen_array_sum 
              READ (UNIT = kgen_unit) kgen_bound(1, 1) 
              READ (UNIT = kgen_unit) kgen_bound(2, 1) 
              ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1))) 
              READ (UNIT = kgen_unit) var 
              IF (PRESENT( printvar )) THEN 
                  CALL kgen_array_sumcheck(printvar, kgen_array_sum, REAL(SUM(var), 8), .TRUE.) 
                  WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
              ELSE 
                  CALL kgen_array_sumcheck("UNKNOWN", kgen_array_sum, REAL(SUM(var), 8), .TRUE.) 
              END IF   
          END IF   
      END SUBROUTINE kr_atm_compute_dyn_tend_integer___dim1_ptr 
        
      !read state subroutine for kr_atm_compute_dyn_tend_real__rkind_ptr 
      SUBROUTINE kr_atm_compute_dyn_tend_real__rkind_ptr(var, kgen_unit, printvar) 
          REAL(KIND=rkind), INTENT(INOUT), POINTER :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
            
          READ (UNIT = kgen_unit) kgen_istrue 
          IF (kgen_istrue) THEN 
              IF (ASSOCIATED( var )) THEN 
                  NULLIFY (var) 
              END IF   
              ALLOCATE (var) 
              READ (UNIT = kgen_unit) var 
              IF (PRESENT( printvar )) THEN 
                  WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
              END IF   
          END IF   
      END SUBROUTINE kr_atm_compute_dyn_tend_real__rkind_ptr 
        
      !read state subroutine for kr_atm_compute_dyn_tend_logical___ptr 
      SUBROUTINE kr_atm_compute_dyn_tend_logical___ptr(var, kgen_unit, printvar) 
          LOGICAL, INTENT(INOUT), POINTER :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
            
          READ (UNIT = kgen_unit) kgen_istrue 
          IF (kgen_istrue) THEN 
              IF (ASSOCIATED( var )) THEN 
                  NULLIFY (var) 
              END IF   
              ALLOCATE (var) 
              READ (UNIT = kgen_unit) var 
              IF (PRESENT( printvar )) THEN 
                  WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
              END IF   
          END IF   
      END SUBROUTINE kr_atm_compute_dyn_tend_logical___ptr 
        
      !read state subroutine for kr_atm_compute_dyn_tend_character_strkind__ptr 
      SUBROUTINE kr_atm_compute_dyn_tend_character_strkind__ptr(var, kgen_unit, printvar) 
          CHARACTER(LEN=strkind), INTENT(INOUT), POINTER :: var 
          INTEGER, INTENT(IN) :: kgen_unit 
          CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
          LOGICAL :: kgen_istrue 
          REAL(KIND=8) :: kgen_array_sum 
            
          READ (UNIT = kgen_unit) kgen_istrue 
          IF (kgen_istrue) THEN 
              IF (ASSOCIATED( var )) THEN 
                  NULLIFY (var) 
              END IF   
              ALLOCATE (var) 
              READ (UNIT = kgen_unit) var 
              IF (PRESENT( printvar )) THEN 
                  WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
              END IF   
          END IF   
      END SUBROUTINE kr_atm_compute_dyn_tend_character_strkind__ptr 
        
      !verify state subroutine for kv_atm_compute_dyn_tend_integer__ 
      RECURSIVE SUBROUTINE kv_atm_compute_dyn_tend_integer__(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          INTEGER, INTENT(IN) :: var, kgenref_var 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          integer :: diff 
            
          check_status%numTotal = check_status%numTotal + 1 
            
          IF (var == kgenref_var) THEN 
              check_status%numIdentical = check_status%numIdentical + 1 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
              END IF   
              check_result = CHECK_IDENTICAL 
          ELSE 
              diff = ABS(var - kgenref_var) 
              IF (diff <= check_status%tolerance) THEN 
                  check_status%numInTol = check_status%numInTol + 1 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                  END IF   
                  check_result = CHECK_IN_TOL 
              ELSE 
                  check_status%numOutTol = check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              END IF   
          END IF   
          IF (check_result == CHECK_IDENTICAL) THEN 
              IF (check_status%verboseLevel > 2) THEN 
                  WRITE (*, *) "Difference is ", 0 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_OUT_TOL) THEN 
              IF (check_status%verboseLevel > 0) THEN 
                  WRITE (*, *) "Difference is ", diff 
                  WRITE (*, *) "" 
              END IF   
          ELSE IF (check_result == CHECK_IN_TOL) THEN 
              IF (check_status%verboseLevel > 1) THEN 
                  WRITE (*, *) "Difference is ", diff 
                  WRITE (*, *) "" 
              END IF   
          END IF   
            
      END SUBROUTINE kv_atm_compute_dyn_tend_integer__ 
        
      !verify state subroutine for kv_atm_compute_dyn_tend_integer___ptr 
      RECURSIVE SUBROUTINE kv_atm_compute_dyn_tend_integer___ptr(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          INTEGER, pointer, INTENT(IN) :: var, kgenref_var 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          integer :: diff 
            
          IF (ASSOCIATED(var)) THEN 
              check_status%numTotal = check_status%numTotal + 1 
                
              IF (var == kgenref_var) THEN 
                  check_status%numIdentical = check_status%numIdentical + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
                  check_result = CHECK_IDENTICAL 
              ELSE 
                  diff = ABS(var - kgenref_var) 
                  IF (diff <= check_status%tolerance) THEN 
                      check_status%numInTol = check_status%numInTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                      END IF   
                      check_result = CHECK_IN_TOL 
                  ELSE 
                      check_status%numOutTol = check_status%numOutTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                      check_result = CHECK_OUT_TOL 
                  END IF   
              END IF   
              IF (check_result == CHECK_IDENTICAL) THEN 
                  IF (check_status%verboseLevel > 2) THEN 
                      WRITE (*, *) "Difference is ", 0 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) "Difference is ", diff 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_IN_TOL) THEN 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) "Difference is ", diff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
                
          END IF   
      END SUBROUTINE kv_atm_compute_dyn_tend_integer___ptr 
        
      !verify state subroutine for kv_atm_compute_dyn_tend_real__rkind_dim1_ptr 
      RECURSIVE SUBROUTINE kv_atm_compute_dyn_tend_real__rkind_dim1_ptr(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          REAL(KIND=rkind), pointer, INTENT(IN), DIMENSION(:) :: var, kgenref_var 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          INTEGER :: idx1 
          INTEGER :: n 
          real(KIND=rkind) :: nrmsdiff, rmsdiff 
          real(KIND=rkind), ALLOCATABLE :: buf1(:), buf2(:) 
            
          IF (ASSOCIATED(var)) THEN 
              check_status%numTotal = check_status%numTotal + 1 
                
              IF (ALL(var == kgenref_var)) THEN 
                  check_status%numIdentical = check_status%numIdentical + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
                  check_result = CHECK_IDENTICAL 
              ELSE 
                  ALLOCATE (buf1(SIZE(var,dim=1))) 
                  ALLOCATE (buf2(SIZE(var,dim=1))) 
                  n = COUNT(var /= kgenref_var) 
                  WHERE ( ABS(kgenref_var) > check_status%minvalue ) 
                      buf1 = ((var-kgenref_var)/kgenref_var)**2 
                      buf2 = (var-kgenref_var)**2 
                  ELSEWHERE 
                      buf1 = (var-kgenref_var)**2 
                      buf2 = buf1 
                  END WHERE   
                  nrmsdiff = SQRT(SUM(buf1)/REAL(n)) 
                  rmsdiff = SQRT(SUM(buf2)/REAL(n)) 
                  IF (nrmsdiff > check_status%tolerance) THEN 
                      check_status%numOutTol = check_status%numOutTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                      check_result = CHECK_OUT_TOL 
                  ELSE 
                      check_status%numInTol = check_status%numInTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                      END IF   
                      check_result = CHECK_IN_TOL 
                  END IF   
              END IF   
              IF (check_result == CHECK_IDENTICAL) THEN 
                  IF (check_status%verboseLevel > 2) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", 0 
                      WRITE (*, *) "Normalized RMS of difference is ", 0 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_IN_TOL) THEN 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
                
          END IF   
      END SUBROUTINE kv_atm_compute_dyn_tend_real__rkind_dim1_ptr 
        
      !verify state subroutine for kv_atm_compute_dyn_tend_real__rkind_dim2_ptr 
      RECURSIVE SUBROUTINE kv_atm_compute_dyn_tend_real__rkind_dim2_ptr(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          REAL(KIND=rkind), pointer, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          INTEGER :: idx1, idx2 
          INTEGER :: n 
          real(KIND=rkind) :: nrmsdiff, rmsdiff 
          real(KIND=rkind), ALLOCATABLE :: buf1(:,:), buf2(:,:) 
            
          IF (ASSOCIATED(var)) THEN 
              check_status%numTotal = check_status%numTotal + 1 
                
              IF (ALL(var == kgenref_var)) THEN 
                  check_status%numIdentical = check_status%numIdentical + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
                  check_result = CHECK_IDENTICAL 
              ELSE 
                  ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
                  ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
                  n = COUNT(var /= kgenref_var) 
                  WHERE ( ABS(kgenref_var) > check_status%minvalue ) 
                      buf1 = ((var-kgenref_var)/kgenref_var)**2 
                      buf2 = (var-kgenref_var)**2 
                  ELSEWHERE 
                      buf1 = (var-kgenref_var)**2 
                      buf2 = buf1 
                  END WHERE   
                  nrmsdiff = SQRT(SUM(buf1)/REAL(n)) 
                  rmsdiff = SQRT(SUM(buf2)/REAL(n)) 
                  IF (nrmsdiff > check_status%tolerance) THEN 
                      check_status%numOutTol = check_status%numOutTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                      check_result = CHECK_OUT_TOL 
                  ELSE 
                      check_status%numInTol = check_status%numInTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                      END IF   
                      check_result = CHECK_IN_TOL 
                  END IF   
              END IF   
              IF (check_result == CHECK_IDENTICAL) THEN 
                  IF (check_status%verboseLevel > 2) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", 0 
                      WRITE (*, *) "Normalized RMS of difference is ", 0 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_IN_TOL) THEN 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
                
          END IF   
      END SUBROUTINE kv_atm_compute_dyn_tend_real__rkind_dim2_ptr 
        
      !verify state subroutine for kv_atm_compute_dyn_tend_real__rkind_dim3_ptr 
      RECURSIVE SUBROUTINE kv_atm_compute_dyn_tend_real__rkind_dim3_ptr(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          REAL(KIND=rkind), pointer, INTENT(IN), DIMENSION(:,:,:) :: var, kgenref_var 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          INTEGER :: idx1, idx2, idx3 
          INTEGER :: n 
          real(KIND=rkind) :: nrmsdiff, rmsdiff 
          real(KIND=rkind), ALLOCATABLE :: buf1(:,:,:), buf2(:,:,:) 
            
          IF (ASSOCIATED(var)) THEN 
              check_status%numTotal = check_status%numTotal + 1 
                
              IF (ALL(var == kgenref_var)) THEN 
                  check_status%numIdentical = check_status%numIdentical + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
                  check_result = CHECK_IDENTICAL 
              ELSE 
                  ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3))) 
                  ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2),SIZE(var,dim=3))) 
                  n = COUNT(var /= kgenref_var) 
                  WHERE ( ABS(kgenref_var) > check_status%minvalue ) 
                      buf1 = ((var-kgenref_var)/kgenref_var)**2 
                      buf2 = (var-kgenref_var)**2 
                  ELSEWHERE 
                      buf1 = (var-kgenref_var)**2 
                      buf2 = buf1 
                  END WHERE   
                  nrmsdiff = SQRT(SUM(buf1)/REAL(n)) 
                  rmsdiff = SQRT(SUM(buf2)/REAL(n)) 
                  IF (nrmsdiff > check_status%tolerance) THEN 
                      check_status%numOutTol = check_status%numOutTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                      check_result = CHECK_OUT_TOL 
                  ELSE 
                      check_status%numInTol = check_status%numInTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                      END IF   
                      check_result = CHECK_IN_TOL 
                  END IF   
              END IF   
              IF (check_result == CHECK_IDENTICAL) THEN 
                  IF (check_status%verboseLevel > 2) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", 0 
                      WRITE (*, *) "Normalized RMS of difference is ", 0 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_IN_TOL) THEN 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
                
          END IF   
      END SUBROUTINE kv_atm_compute_dyn_tend_real__rkind_dim3_ptr 
        
      !verify state subroutine for kv_atm_compute_dyn_tend_integer___dim2_ptr 
      RECURSIVE SUBROUTINE kv_atm_compute_dyn_tend_integer___dim2_ptr(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          INTEGER, pointer, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          INTEGER :: idx1, idx2 
          INTEGER :: n 
          integer :: nrmsdiff, rmsdiff 
          integer, ALLOCATABLE :: buf1(:,:), buf2(:,:) 
            
          IF (ASSOCIATED(var)) THEN 
              check_status%numTotal = check_status%numTotal + 1 
                
              IF (ALL(var == kgenref_var)) THEN 
                  check_status%numIdentical = check_status%numIdentical + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
                  check_result = CHECK_IDENTICAL 
              ELSE 
                  ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
                  ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
                  n = COUNT(var /= kgenref_var) 
                  WHERE ( ABS(kgenref_var) > check_status%minvalue ) 
                      buf1 = ((var-kgenref_var)/kgenref_var)**2 
                      buf2 = (var-kgenref_var)**2 
                  ELSEWHERE 
                      buf1 = (var-kgenref_var)**2 
                      buf2 = buf1 
                  END WHERE   
                  nrmsdiff = SQRT(SUM(buf1)/REAL(n)) 
                  rmsdiff = SQRT(SUM(buf2)/REAL(n)) 
                  IF (nrmsdiff > check_status%tolerance) THEN 
                      check_status%numOutTol = check_status%numOutTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                      check_result = CHECK_OUT_TOL 
                  ELSE 
                      check_status%numInTol = check_status%numInTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                      END IF   
                      check_result = CHECK_IN_TOL 
                  END IF   
              END IF   
              IF (check_result == CHECK_IDENTICAL) THEN 
                  IF (check_status%verboseLevel > 2) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", 0 
                      WRITE (*, *) "Normalized RMS of difference is ", 0 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_IN_TOL) THEN 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
                
          END IF   
      END SUBROUTINE kv_atm_compute_dyn_tend_integer___dim2_ptr 
        
      !verify state subroutine for kv_atm_compute_dyn_tend_integer___dim1_ptr 
      RECURSIVE SUBROUTINE kv_atm_compute_dyn_tend_integer___dim1_ptr(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          INTEGER, pointer, INTENT(IN), DIMENSION(:) :: var, kgenref_var 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          INTEGER :: idx1 
          INTEGER :: n 
          integer :: nrmsdiff, rmsdiff 
          integer, ALLOCATABLE :: buf1(:), buf2(:) 
            
          IF (ASSOCIATED(var)) THEN 
              check_status%numTotal = check_status%numTotal + 1 
                
              IF (ALL(var == kgenref_var)) THEN 
                  check_status%numIdentical = check_status%numIdentical + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
                  check_result = CHECK_IDENTICAL 
              ELSE 
                  ALLOCATE (buf1(SIZE(var,dim=1))) 
                  ALLOCATE (buf2(SIZE(var,dim=1))) 
                  n = COUNT(var /= kgenref_var) 
                  WHERE ( ABS(kgenref_var) > check_status%minvalue ) 
                      buf1 = ((var-kgenref_var)/kgenref_var)**2 
                      buf2 = (var-kgenref_var)**2 
                  ELSEWHERE 
                      buf1 = (var-kgenref_var)**2 
                      buf2 = buf1 
                  END WHERE   
                  nrmsdiff = SQRT(SUM(buf1)/REAL(n)) 
                  rmsdiff = SQRT(SUM(buf2)/REAL(n)) 
                  IF (nrmsdiff > check_status%tolerance) THEN 
                      check_status%numOutTol = check_status%numOutTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                      check_result = CHECK_OUT_TOL 
                  ELSE 
                      check_status%numInTol = check_status%numInTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                      END IF   
                      check_result = CHECK_IN_TOL 
                  END IF   
              END IF   
              IF (check_result == CHECK_IDENTICAL) THEN 
                  IF (check_status%verboseLevel > 2) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", 0 
                      WRITE (*, *) "Normalized RMS of difference is ", 0 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_IN_TOL) THEN 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                      WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                      WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                      WRITE (*, *) "RMS of difference is ", rmsdiff 
                      WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
                
          END IF   
      END SUBROUTINE kv_atm_compute_dyn_tend_integer___dim1_ptr 
        
      !verify state subroutine for kv_atm_compute_dyn_tend_real__rkind_ptr 
      RECURSIVE SUBROUTINE kv_atm_compute_dyn_tend_real__rkind_ptr(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          REAL(KIND=rkind), pointer, INTENT(IN) :: var, kgenref_var 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          real(KIND=rkind) :: diff 
            
          IF (ASSOCIATED(var)) THEN 
              check_status%numTotal = check_status%numTotal + 1 
                
              IF (var == kgenref_var) THEN 
                  check_status%numIdentical = check_status%numIdentical + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
                  check_result = CHECK_IDENTICAL 
              ELSE 
                  diff = ABS(var - kgenref_var) 
                  IF (diff <= check_status%tolerance) THEN 
                      check_status%numInTol = check_status%numInTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                      END IF   
                      check_result = CHECK_IN_TOL 
                  ELSE 
                      check_status%numOutTol = check_status%numOutTol + 1 
                      IF (check_status%verboseLevel > 0) THEN 
                          WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                      END IF   
                      check_result = CHECK_OUT_TOL 
                  END IF   
              END IF   
              IF (check_result == CHECK_IDENTICAL) THEN 
                  IF (check_status%verboseLevel > 2) THEN 
                      WRITE (*, *) "Difference is ", 0 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) "Difference is ", diff 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_IN_TOL) THEN 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) "Difference is ", diff 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
                
          END IF   
      END SUBROUTINE kv_atm_compute_dyn_tend_real__rkind_ptr 
        
      !verify state subroutine for kv_atm_compute_dyn_tend_logical___ptr 
      RECURSIVE SUBROUTINE kv_atm_compute_dyn_tend_logical___ptr(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          LOGICAL, pointer, INTENT(IN) :: var, kgenref_var 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          logical :: diff 
            
          IF (ASSOCIATED(var)) THEN 
              check_status%numTotal = check_status%numTotal + 1 
                
              IF (var .EQV. kgenref_var) THEN 
                  check_status%numIdentical = check_status%numIdentical + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
                  check_result = CHECK_IDENTICAL 
              ELSE 
                  check_status%numOutTol = check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              END IF   
              IF (check_result == CHECK_IDENTICAL) THEN 
                  IF (check_status%verboseLevel > 2) THEN 
                      WRITE (*, *) "NOT IMPLEMENTED" 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) "NOT IMPLEMENTED" 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_IN_TOL) THEN 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) "NOT IMPLEMENTED" 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
                
          END IF   
      END SUBROUTINE kv_atm_compute_dyn_tend_logical___ptr 
        
      !verify state subroutine for kv_atm_compute_dyn_tend_character_strkind__ptr 
      RECURSIVE SUBROUTINE kv_atm_compute_dyn_tend_character_strkind__ptr(varname, check_status, var, kgenref_var) 
          CHARACTER(LEN=*), INTENT(IN) :: varname 
          TYPE(check_t), INTENT(INOUT) :: check_status 
          CHARACTER(LEN=strkind), pointer, INTENT(IN) :: var, kgenref_var 
          INTEGER :: check_result 
          LOGICAL :: is_print = .FALSE. 
            
          character(LEN=strkind) :: diff 
            
          IF (ASSOCIATED(var)) THEN 
              check_status%numTotal = check_status%numTotal + 1 
                
              IF (var == kgenref_var) THEN 
                  check_status%numIdentical = check_status%numIdentical + 1 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
                  END IF   
                  check_result = CHECK_IDENTICAL 
              ELSE 
                  check_status%numOutTol = check_status%numOutTol + 1 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL." 
                  END IF   
                  check_result = CHECK_OUT_TOL 
              END IF   
              IF (check_result == CHECK_IDENTICAL) THEN 
                  IF (check_status%verboseLevel > 2) THEN 
                      WRITE (*, *) "NOT IMPLEMENTED" 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_OUT_TOL) THEN 
                  IF (check_status%verboseLevel > 0) THEN 
                      WRITE (*, *) "NOT IMPLEMENTED" 
                      WRITE (*, *) "" 
                  END IF   
              ELSE IF (check_result == CHECK_IN_TOL) THEN 
                  IF (check_status%verboseLevel > 1) THEN 
                      WRITE (*, *) "NOT IMPLEMENTED" 
                      WRITE (*, *) "" 
                  END IF   
              END IF   
                
          END IF   
      END SUBROUTINE kv_atm_compute_dyn_tend_character_strkind__ptr 
        
   END SUBROUTINE atm_compute_dyn_tend 


   subroutine atm_compute_dyn_tend_work(nCells, nEdges, nVertices, nVertLevels_dummy, &
      nCellsSolve, nEdgesSolve, vertexDegree, maxEdges_dummy, maxEdges2_dummy, num_scalars_dummy, moist_start, moist_end, &
      fEdge, dvEdge, dcEdge, invDcEdge, invDvEdge, invAreaCell, invAreaTriangle, meshScalingDel2, meshScalingDel4, &
      weightsOnEdge, zgrid, rho_edge, rho_zz, ru, u, v, tend_u, &
      divergence, vorticity, ke, pv_edge, theta_m, rw, tend_rho, &
      rt_diabatic_tend, tend_theta, tend_w, w, cqw, rb, rr, pp, pressure_b, zz, zxu, cqu, & 
      h_divergence, kdiff, edgesOnCell_sign, edgesOnVertex_sign, rw_save, ru_save, &
      theta_m_save, exner, rr_save, scalars, tend_u_euler, tend_w_euler, tend_theta_euler, deriv_two, &
      cellsOnEdge, verticesOnEdge, edgesOnCell, edgesOnEdge, cellsOnCell, edgesOnVertex, nEdgesOnCell, nEdgesOnEdge, &
      latCell, latEdge, angleEdge, u_init, advCellsForEdge, nAdvCellsForEdge, adv_coefs, adv_coefs_3rd, &
      rdzu, rdzw, fzm, fzp, qv_init, t_init, cf1, cf2, cf3, r_earth, ur_cell, vr_cell, defc_a, defc_b, &
      tend_w_pgf, tend_w_buoy, coef_3rd_order, c_s, config_mix_full, config_horiz_mixing, config_del4u_div_factor, &
      config_h_mom_eddy_visc2, config_v_mom_eddy_visc2, config_h_theta_eddy_visc2, config_v_theta_eddy_visc2, &
      config_h_theta_eddy_visc4, config_h_mom_eddy_visc4, config_visc4_2dsmag, config_len_disp, rk_step, dt, &
      tend_rtheta_adv, rthdynten, &
      cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd, &
      cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd)


       USE mpas_atm_dimensions 


      implicit none


      !
      ! Dummy arguments
      !
      integer :: nCells, nEdges, nVertices, nVertLevels_dummy, nCellsSolve, nEdgesSolve, vertexDegree, &
                 maxEdges_dummy, maxEdges2_dummy, num_scalars_dummy, moist_start, moist_end

      real (kind=RKIND), dimension(nEdges+1) :: fEdge
      real (kind=RKIND), dimension(nEdges+1) :: dvEdge
      real (kind=RKIND), dimension(nEdges+1) :: dcEdge
      real (kind=RKIND), dimension(nEdges+1) :: invDcEdge
      real (kind=RKIND), dimension(nEdges+1) :: invDvEdge
      real (kind=RKIND), dimension(nCells+1) :: invAreaCell
      real (kind=RKIND), dimension(nVertices+1) :: invAreaTriangle
      real (kind=RKIND), dimension(nEdges+1) :: meshScalingDel2
      real (kind=RKIND), dimension(nEdges+1) :: meshScalingDel4
      real (kind=RKIND), dimension(maxEdges2,nEdges+1) :: weightsOnEdge
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: zgrid
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: rho_edge
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rho_zz
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: u
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: v
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: tend_u
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: divergence
      real (kind=RKIND), dimension(nVertLevels,nVertices+1) :: vorticity
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: ke
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: pv_edge
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: theta_m
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: tend_rho
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rt_diabatic_tend
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: tend_theta
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: tend_w
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: w
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: cqw
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rb
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rr
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: pp
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: pressure_b
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: zz
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: zxu
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: cqu
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: h_divergence
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: kdiff
      real (kind=RKIND), dimension(maxEdges,nCells+1) :: edgesOnCell_sign
      real (kind=RKIND), dimension(vertexDegree,nVertices+1) :: edgesOnVertex_sign
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: rw_save
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: ru_save

      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: theta_m_save
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: exner
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rr_save
      real (kind=RKIND), dimension(num_scalars,nVertLevels,nCells+1) :: scalars
      real (kind=RKIND), dimension(nVertLevels,nEdges+1) :: tend_u_euler
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: tend_w_euler
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: tend_theta_euler
      real (kind=RKIND), dimension(15,2,nEdges+1) :: deriv_two
      integer, dimension(2,nEdges+1) :: cellsOnEdge
      integer, dimension(2,nEdges+1) :: verticesOnEdge
      integer, dimension(maxEdges,nCells+1) :: edgesOnCell
      integer, dimension(maxEdges2,nEdges+1) :: edgesOnEdge
      integer, dimension(maxEdges,nCells+1) :: cellsOnCell
      integer, dimension(vertexDegree,nVertices+1) :: edgesOnVertex
      integer, dimension(nCells+1) :: nEdgesOnCell
      integer, dimension(nEdges+1) :: nEdgesOnEdge
      real (kind=RKIND), dimension(nCells+1) :: latCell
      real (kind=RKIND), dimension(nEdges+1) :: latEdge
      real (kind=RKIND), dimension(nEdges+1) :: angleEdge
      real (kind=RKIND), dimension(nVertLevels) :: u_init

      integer, dimension(15,nEdges+1) :: advCellsForEdge
      integer, dimension(nEdges+1) :: nAdvCellsForEdge
      real (kind=RKIND), dimension(15,nEdges+1) :: adv_coefs
      real (kind=RKIND), dimension(15,nEdges+1) :: adv_coefs_3rd

      real (kind=RKIND), dimension(nVertLevels) :: rdzu
      real (kind=RKIND), dimension(nVertLevels) :: rdzw
      real (kind=RKIND), dimension(nVertLevels) :: fzm
      real (kind=RKIND), dimension(nVertLevels) :: fzp
      real (kind=RKIND), dimension(nVertLevels) :: qv_init
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: t_init 

      real (kind=RKIND) :: cf1, cf2, cf3
      real (kind=RKIND) :: prandtl_inv, r_areaCell, rgas_cprcv

      real (kind=RKIND) :: r_earth
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: ur_cell
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: vr_cell

      real (kind=RKIND), dimension(maxEdges,nCells+1) :: defc_a
      real (kind=RKIND), dimension(maxEdges,nCells+1) :: defc_b

      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: tend_w_pgf
      real (kind=RKIND), dimension(nVertLevels+1,nCells+1) :: tend_w_buoy

      real (kind=RKIND) :: coef_3rd_order, c_s
      logical :: config_mix_full
      character (len=StrKIND) :: config_horiz_mixing
      real (kind=RKIND) :: config_del4u_div_factor
      real (kind=RKIND) :: config_h_theta_eddy_visc4
      real (kind=RKIND) :: config_h_mom_eddy_visc4
      real (kind=RKIND) :: config_visc4_2dsmag
      real (kind=RKIND) :: config_len_disp
      real (kind=RKIND) :: config_h_mom_eddy_visc2, config_v_mom_eddy_visc2, config_h_theta_eddy_visc2, config_v_theta_eddy_visc2

      integer, intent(in) :: rk_step
      real (kind=RKIND), intent(in) :: dt

      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: tend_rtheta_adv
      real (kind=RKIND), dimension(nVertLevels,nCells+1) :: rthdynten

      integer, intent(in) :: cellStart, cellEnd, vertexStart, vertexEnd, edgeStart, edgeEnd
      integer, intent(in) :: cellSolveStart, cellSolveEnd, vertexSolveStart, vertexSolveEnd, edgeSolveStart, edgeSolveEnd


      !
      ! Local variables
      !
      integer :: iEdge, iCell, iVertex, k, cell1, cell2, vertex1, vertex2, eoe, i, j, iq, iAdvCell

      !real (kind=RKIND), parameter :: c_s = 0.125
      real (kind=RKIND), dimension( nVertLevels+1 ) :: d_diag, d_off_diag, flux_arr
      real (kind=RKIND), dimension( nVertLevels + 1 ) :: wduz, wdwz, wdtz, dpzx
      real (kind=RKIND), dimension( nVertLevels ) :: ru_edge_w, q, u_mix
      real (kind=RKIND) :: theta_turb_flux, w_turb_flux, r
      real (kind=RKIND) :: scalar_weight
      real (kind=RKIND) :: inv_r_earth

      real (kind=RKIND) :: invDt, flux, workpv
      real (kind=RKIND) :: edge_sign, pr_scale, r_dc, r_dv, u_mix_scale
      real (kind=RKIND) :: h_mom_eddy_visc4, v_mom_eddy_visc2
      real (kind=RKIND) :: h_theta_eddy_visc4, v_theta_eddy_visc2
      real (kind=RKIND) :: u_diffusion

      real (kind=RKIND) :: kdiffu, z1, z2, z3, z4, zm, z0, zp
      real (kind=RKIND) :: temp1, temp2
      

      real (kind=RKIND) :: flux3, flux4
      real (kind=RKIND) :: q_im2, q_im1, q_i, q_ip1, ua, coef3

      flux4(q_im2, q_im1, q_i, q_ip1, ua) =                     &
                ua*( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

      flux3(q_im2, q_im1, q_i, q_ip1, ua, coef3) =              &
                flux4(q_im2, q_im1, q_i, q_ip1, ua) +           &
                coef3*abs(ua)*((q_ip1 - q_im2)-3.*(q_i-q_im1))/12.0


      prandtl_inv = 1.0_RKIND / prandtl
      invDt = 1.0_RKIND / dt
      inv_r_earth = 1.0_RKIND / r_earth

       v_mom_eddy_visc2   = config_v_mom_eddy_visc2
       v_theta_eddy_visc2 = config_v_theta_eddy_visc2

      if (rk_step == 1) then

!         tend_u_euler(1:nVertLevels,edgeStart:edgeEnd) = 0.0

         ! Smagorinsky eddy viscosity, based on horizontal deformation (in this case on model coordinate surfaces).
         ! The integration coefficients were precomputed and stored in defc_a and defc_b

         if(config_horiz_mixing == "2d_smagorinsky") then
           temp1 = (c_s * config_len_disp)**2
           temp2 = (0.01*config_len_disp**2) * invDt
!$acc parallel vector_length(128) 
!$acc loop private(d_diag,d_off_diag)
           do iCell = cellStart,cellEnd
             do k=1,nVertLevels
               d_diag(k) = 0.0
               d_off_diag(k) = 0.0
!$acc loop seq
                  do iEdge=1,nEdgesOnCell(iCell)
                  
                     d_diag(k)     = d_diag(k)     + defc_a(iEdge,iCell)*u(k,EdgesOnCell(iEdge,iCell))  &
                                                   - defc_b(iEdge,iCell)*v(k,EdgesOnCell(iEdge,iCell))
                     d_off_diag(k) = d_off_diag(k) + defc_b(iEdge,iCell)*u(k,EdgesOnCell(iEdge,iCell))  &
                                                   + defc_a(iEdge,iCell)*v(k,EdgesOnCell(iEdge,iCell))
                  end do
!DIR$ IVDEP
                  ! here is the Smagorinsky formulation, 
                  ! followed by imposition of an upper bound on the eddy viscosity
                  !kdiff(k,iCell) = min((c_s * config_len_disp)**2 * sqrt(d_diag(k)**2 + d_off_diag(k)**2),(0.01*config_len_disp**2) * invDt)
                  kdiff(k,iCell) = min(temp1 * sqrt(d_diag(k)**2 + d_off_diag(k)**2),temp2)
               end do
            end do
!$acc end parallel do
            h_mom_eddy_visc4   = config_visc4_2dsmag * config_len_disp**3
            h_theta_eddy_visc4 = h_mom_eddy_visc4

         else if(config_horiz_mixing == "2d_fixed") then

            kdiff(1:nVertLevels,cellStart:cellEnd) = config_h_theta_eddy_visc2
            h_mom_eddy_visc4 = config_h_mom_eddy_visc4
            h_theta_eddy_visc4 = config_h_theta_eddy_visc4

         end if
            
      end if

      ! tendency for density.
      ! accumulate total water here for later use in w tendency calculation.

      ! accumulate horizontal mass-flux

      do iCell=cellStart,cellEnd
         h_divergence(1:nVertLevels,iCell) = 0.0
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)
            edge_sign = edgesOnCell_sign(i,iCell) * dvEdge(iEdge)
!DIR$ IVDEP
            do k=1,nVertLevels
               h_divergence(k,iCell) = h_divergence(k,iCell) + edge_sign * ru(k,iEdge)
            end do
         end do
      end do

      ! compute horiontal mass-flux divergence, add vertical mass flux divergence to complete tend_rho

      do iCell = cellStart,cellEnd
         r = invAreaCell(iCell)
         do k = 1,nVertLevels
            h_divergence(k,iCell) = h_divergence(k,iCell) * r
         end do
      end do    

      !
      ! dp / dz and tend_rho
      !
      ! only needed on first rk_step with pert variables defined a pert from time t
      !
      if(rk_step == 1) then

        rgas_cprcv = rgas*cp/cv
        do iCell = cellStart,cellEnd

!DIR$ IVDEP
          do k = 1,nVertLevels
            tend_rho(k,iCell) = -h_divergence(k,iCell)-rdzw(k)*(rw(k+1,iCell)-rw(k,iCell)) + tend_rho_physics(k,iCell)
            dpdz(k,iCell) = -gravity*(rb(k,iCell)*(qtot(k,iCell)) + rr_save(k,iCell)*(1.+qtot(k,iCell)))
          end do
        end do
      end if

!$OMP BARRIER

      !
      ! Compute u (normal) velocity tendency for each edge (cell face)
      !

      do iEdge=edgeSolveStart,edgeSolveEnd

         cell1 = cellsOnEdge(1,iEdge)
         cell2 = cellsOnEdge(2,iEdge)

         ! horizontal pressure gradient 

         if(rk_step == 1) then
!DIR$ IVDEP
            do k=1,nVertLevels
               tend_u_euler(k,iEdge) =  - cqu(k,iEdge)*( (pp(k,cell2)-pp(k,cell1))*invDcEdge(iEdge)/(.5*(zz(k,cell2)+zz(k,cell1))) &
                                              -0.5*zxu(k,iEdge)*(dpdz(k,cell1)+dpdz(k,cell2)) )
            end do

         end if

         ! vertical transport of u

         wduz(1) = 0.

         k = 2
         wduz(k) =  0.5*( rw(k,cell1)+rw(k,cell2))*(fzm(k)*u(k,iEdge)+fzp(k)*u(k-1,iEdge))
         do k=3,nVertLevels-1
            wduz(k) = flux3( u(k-2,iEdge),u(k-1,iEdge),u(k,iEdge),u(k+1,iEdge),0.5*(rw(k,cell1)+rw(k,cell2)), 1.0_RKIND )
         end do
         k = nVertLevels
         wduz(k) =  0.5*( rw(k,cell1)+rw(k,cell2))*(fzm(k)*u(k,iEdge)+fzp(k)*u(k-1,iEdge))

         wduz(nVertLevels+1) = 0.

!DIR$ IVDEP
         do k=1,nVertLevels
            tend_u(k,iEdge) = - rdzw(k)*(wduz(k+1)-wduz(k)) !  first use of tend_u
         end do

         ! Next, nonlinear Coriolis term (q) following Ringler et al JCP 2009

         q(:) = 0.0
         do j = 1,nEdgesOnEdge(iEdge)
            eoe = edgesOnEdge(j,iEdge)
            do k=1,nVertLevels
               workpv = 0.5 * (pv_edge(k,iEdge) + pv_edge(k,eoe))
!  the original definition of pv_edge had a factor of 1/density.  We have removed that factor
!  given that it was not integral to any conservation property of the system
               q(k) = q(k) + weightsOnEdge(j,iEdge) * u(k,eoe) * workpv
            end do
         end do

!DIR$ IVDEP
         do k=1,nVertLevels

            ! horizontal ke gradient and vorticity terms in the vector invariant formulation
            ! of the horizontal momentum equation
            tend_u(k,iEdge) = tend_u(k,iEdge) + rho_edge(k,iEdge)* (q(k) - (ke(k,cell2) - ke(k,cell1))       &
                                                                 * invDcEdge(iEdge))                            &
                                             - u(k,iEdge)*0.5*(h_divergence(k,cell1)+h_divergence(k,cell2)) 
         end do

      end do


      !
      !  horizontal mixing for u
      !  mixing terms are integrated using forward-Euler, so this tendency is only computed in the
      !  first Runge-Kutta substep and saved for use in later RK substeps 2 and 3.
      !

      if (rk_step == 1) then

!$OMP BARRIER

         ! del^4 horizontal filter.  We compute this as del^2 ( del^2 (u) ).
         ! First, storage to hold the result from the first del^2 computation.

         delsq_u(1:nVertLevels,edgeStart:edgeEnd) = 0.0

         do iEdge=edgeStart,edgeEnd
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
            vertex1 = verticesOnEdge(1,iEdge)
            vertex2 = verticesOnEdge(2,iEdge)
            r_dc = invDcEdge(iEdge)
            r_dv = min(invDvEdge(iEdge), 4*invDcEdge(iEdge))

!DIR$ IVDEP
            do k=1,nVertLevels

               ! Compute diffusion, computed as \nabla divergence - k \times \nabla vorticity
               !                    only valid for h_mom_eddy_visc4 == constant
              u_diffusion =   ( divergence(k,cell2)  - divergence(k,cell1) ) * r_dc  &
                              -( vorticity(k,vertex2) - vorticity(k,vertex1) ) * r_dv

               delsq_u(k,iEdge) = delsq_u(k,iEdge) + u_diffusion

               kdiffu = 0.5*(kdiff(k,cell1)+kdiff(k,cell2))

               ! include 2nd-orer diffusion here 
               tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) &
                                       + rho_edge(k,iEdge)* kdiffu * u_diffusion * meshScalingDel2(iEdge)

            end do
         end do

         if (h_mom_eddy_visc4 > 0.0) then  ! 4th order mixing is active

!$OMP BARRIER

            do iVertex=vertexStart,vertexEnd
               delsq_vorticity(1:nVertLevels,iVertex) = 0.0
               do i=1,vertexDegree
                  iEdge = edgesOnVertex(i,iVertex)
                  edge_sign = invAreaTriangle(iVertex) * dcEdge(iEdge) * edgesOnVertex_sign(i,iVertex)
                  do k=1,nVertLevels
                     delsq_vorticity(k,iVertex) = delsq_vorticity(k,iVertex) + edge_sign * delsq_u(k,iEdge)
                  end do
               end do
            end do

            do iCell=cellStart,cellEnd
               delsq_divergence(1:nVertLevels,iCell) = 0.0
               r = invAreaCell(iCell)
               do i=1,nEdgesOnCell(iCell)
                  iEdge = edgesOnCell(i,iCell)
                  edge_sign = r * dvEdge(iEdge) * edgesOnCell_sign(i,iCell)
                  do k=1,nVertLevels
                     delsq_divergence(k,iCell) = delsq_divergence(k,iCell) + edge_sign * delsq_u(k,iEdge)
                  end do
               end do
            end do

!$OMP BARRIER

            do iEdge=edgeSolveStart,edgeSolveEnd
               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)
               vertex1 = verticesOnEdge(1,iEdge)
               vertex2 = verticesOnEdge(2,iEdge)

               u_mix_scale = meshScalingDel4(iEdge)*h_mom_eddy_visc4
               r_dc = u_mix_scale * config_del4u_div_factor * invDcEdge(iEdge)
               r_dv = u_mix_scale * min(invDvEdge(iEdge), 4*invDcEdge(iEdge))

!DIR$ IVDEP
               do k=1,nVertLevels

                  ! Compute diffusion, computed as \nabla divergence - k \times \nabla vorticity
                  !                    only valid for h_mom_eddy_visc4 == constant
                  !
                  ! Here, we scale the diffusion on the divergence part a factor of config_del4u_div_factor 
                  !    relative to the rotational part.  The stability constraint on the divergence component is much less
                  !    stringent than the rotational part, and this flexibility may be useful.
                  !
                  u_diffusion =  rho_edge(k,iEdge) *  ( ( delsq_divergence(k,cell2)  - delsq_divergence(k,cell1) ) * r_dc  &
                                                       -( delsq_vorticity(k,vertex2) - delsq_vorticity(k,vertex1) ) * r_dv )
                  tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) - u_diffusion
                  
               end do
            end do
         
         end if ! 4th order mixing is active 

      !
      !  vertical mixing for u - 2nd order filter in physical (z) space
      !
         if ( v_mom_eddy_visc2 > 0.0 ) then

            if (config_mix_full) then  ! mix full state

               do iEdge=edgeSolveStart,edgeSolveEnd

                  cell1 = cellsOnEdge(1,iEdge)
                  cell2 = cellsOnEdge(2,iEdge)

                  do k=2,nVertLevels-1

                     z1 = 0.5*(zgrid(k-1,cell1)+zgrid(k-1,cell2))
                     z2 = 0.5*(zgrid(k  ,cell1)+zgrid(k  ,cell2))
                     z3 = 0.5*(zgrid(k+1,cell1)+zgrid(k+1,cell2))
                     z4 = 0.5*(zgrid(k+2,cell1)+zgrid(k+2,cell2))

                     zm = 0.5*(z1+z2)
                     z0 = 0.5*(z2+z3)
                     zp = 0.5*(z3+z4)

                     tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) + rho_edge(k,iEdge) * v_mom_eddy_visc2*(  &
                                        (u(k+1,iEdge)-u(k  ,iEdge))/(zp-z0)                      &
                                       -(u(k  ,iEdge)-u(k-1,iEdge))/(z0-zm) )/(0.5*(zp-zm))
                  end do
               end do

            else  ! idealized cases where we mix on the perturbation from the initial 1-D state

               do iEdge=edgeSolveStart,edgeSolveEnd

                  cell1 = cellsOnEdge(1,iEdge)
                  cell2 = cellsOnEdge(2,iEdge)

                  do k=1,nVertLevels


                     u_mix(k) = u(k,iEdge) - u_init(k) * cos( angleEdge(iEdge) )

                  end do

                  do k=2,nVertLevels-1

                     z1 = 0.5*(zgrid(k-1,cell1)+zgrid(k-1,cell2))
                     z2 = 0.5*(zgrid(k  ,cell1)+zgrid(k  ,cell2))
                     z3 = 0.5*(zgrid(k+1,cell1)+zgrid(k+1,cell2))
                     z4 = 0.5*(zgrid(k+2,cell1)+zgrid(k+2,cell2))

                     zm = 0.5*(z1+z2)
                     z0 = 0.5*(z2+z3)
                     zp = 0.5*(z3+z4)

                     tend_u_euler(k,iEdge) = tend_u_euler(k,iEdge) + rho_edge(k,iEdge) * v_mom_eddy_visc2*(  &
                                        (u_mix(k+1)-u_mix(k  ))/(zp-z0)                      &
                                       -(u_mix(k  )-u_mix(k-1))/(z0-zm) )/(0.5*(zp-zm))
                  end do
               end do

            end if  ! mix perturbation state

         end if  ! vertical mixing of horizontal momentum

      end if ! (rk_step 1 test for computing mixing terms)

!$OMP BARRIER

!  add in mixing for u

      do iEdge=edgeSolveStart,edgeSolveEnd
!DIR$ IVDEP
         do k=1,nVertLevels
!            tend_u(k,iEdge) = tend_u(k,iEdge) + tend_u_euler(k,iEdge)
            tend_u(k,iEdge) = tend_u(k,iEdge) + tend_u_euler(k,iEdge) + tend_ru_physics(k,iEdge)
         end do
      end do


!----------- rhs for w


      !
      !  horizontal advection for w
      !

      do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...
         tend_w(1:nVertLevels+1,iCell) = 0.0
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)
            edge_sign = edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * 0.5

            do k=2,nVertLevels
               ru_edge_w(k) = fzm(k)*ru(k,iEdge) + fzp(k)*ru(k-1,iEdge)
            end do

            flux_arr(1:nVertLevels) = 0.0

            ! flux_arr stores the value of w at the cell edge used in the horizontal transport

            do j=1,nAdvCellsForEdge(iEdge)
               iAdvCell = advCellsForEdge(j,iEdge)
               do k=2,nVertLevels
                  scalar_weight = adv_coefs(j,iEdge) + sign(1.0_RKIND,ru_edge_w(k)) * adv_coefs_3rd(j,iEdge)
                  flux_arr(k) = flux_arr(k) + scalar_weight * w(k,iAdvCell)
               end do
            end do

!DIR$ IVDEP
            do k=2,nVertLevels
               tend_w(k,iCell) = tend_w(k,iCell) - edgesOnCell_sign(i,iCell) * ru_edge_w(k)*flux_arr(k)
            end do

         end do
      end do


      !
      !  horizontal mixing for w - we could combine this with advection directly (i.e. as a turbulent flux),
      !  but here we can also code in hyperdiffusion if we wish (2nd order at present)
      !

      if (rk_step == 1) then

!  !OMP BARRIER  why is this openmp barrier here???

         ! del^4 horizontal filter.  We compute this as del^2 ( del^2 (u) ).
         !
         ! First, storage to hold the result from the first del^2 computation.
         !  we copied code from the theta mixing, hence the theta* names.


         delsq_w(1:nVertLevels,cellStart:cellEnd) = 0.0

         do iCell=cellStart,cellEnd
            tend_w_euler(1:nVertLevels+1,iCell) = 0.0
            r_areaCell = invAreaCell(iCell)
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)

               edge_sign = 0.5 * r_areaCell*edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge)

               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)

!DIR$ IVDEP
              do k=2,nVertLevels

                  w_turb_flux =  edge_sign*(rho_edge(k,iEdge)+rho_edge(k-1,iEdge))*(w(k,cell2) - w(k,cell1))
                  delsq_w(k,iCell) = delsq_w(k,iCell) + w_turb_flux
                  w_turb_flux = w_turb_flux * meshScalingDel2(iEdge) * 0.25 * &
                                  (kdiff(k,cell1)+kdiff(k,cell2)+kdiff(k-1,cell1)+kdiff(k-1,cell2))
                  tend_w_euler(k,iCell) = tend_w_euler(k,iCell) + w_turb_flux
               end do
            end do
         end do

!$OMP BARRIER

         if (h_mom_eddy_visc4 > 0.0) then  ! 4th order mixing is active

            do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...
               r_areaCell = h_mom_eddy_visc4 * invAreaCell(iCell)
               do i=1,nEdgesOnCell(iCell)
                  iEdge = edgesOnCell(i,iCell)
                  cell1 = cellsOnEdge(1,iEdge)
                  cell2 = cellsOnEdge(2,iEdge)

                  edge_sign = meshScalingDel4(iEdge)*r_areaCell*dvEdge(iEdge)*edgesOnCell_sign(i,iCell) * invDcEdge(iEdge)

                  do k=2,nVertLevels
                     tend_w_euler(k,iCell) = tend_w_euler(k,iCell) - edge_sign * (delsq_w(k,cell2) - delsq_w(k,cell1))
                  end do
           
               end do
            end do

         end if ! 4th order mixing is active 

      end if ! horizontal mixing for w computed in first rk_step

! Note for OpenMP parallelization: We could avoid allocating the delsq_w scratch
!   array, and just use the delsq_theta array as was previously done; however,
!   particularly when oversubscribing cores with threads, there is the risk that
!   some threads may reach code further below that re-uses the delsq_theta array, 
!   in which case we would need a barrier somewhere between here and that code 
!   below to ensure correct behavior.

      !
      !  vertical advection, pressure gradient and buoyancy for w
      !

      do iCell=cellSolveStart,cellSolveEnd

         wdwz(1) = 0.0

         k = 2
         wdwz(k) =  0.25*(rw(k,icell)+rw(k-1,iCell))*(w(k,iCell)+w(k-1,iCell))
         do k=3,nVertLevels-1
            wdwz(k) = flux3( w(k-2,iCell),w(k-1,iCell),w(k,iCell),w(k+1,iCell),0.5*(rw(k,iCell)+rw(k-1,iCell)), 1.0_RKIND )
         end do
         k = nVertLevels
         wdwz(k) =  0.25*(rw(k,icell)+rw(k-1,iCell))*(w(k,iCell)+w(k-1,iCell))

         wdwz(nVertLevels+1) = 0.0

      !  Note: next we are also dividing through by the cell area after the horizontal flux divergence

!DIR$ IVDEP
         do k=2,nVertLevels
            tend_w(k,iCell) = tend_w(k,iCell) * invAreaCell(iCell) -rdzu(k)*(wdwz(k+1)-wdwz(k))
         end do

         if(rk_step == 1) then
!DIR$ IVDEP
            do k=2,nVertLevels
              tend_w_euler(k,iCell) = tend_w_euler(k,iCell) - cqw(k,iCell)*(   &
                                           rdzu(k)*(pp(k,iCell)-pp(k-1,iCell)) &
                                         - (fzm(k)*dpdz(k,iCell) + fzp(k)*dpdz(k-1,iCell)) )  ! dpdz is the buoyancy term here.
            end do
          end if

      end do

      if (rk_step == 1) then

         if ( v_mom_eddy_visc2 > 0.0 ) then

            do iCell=cellSolveStart,cellSolveEnd
!DIR$ IVDEP
               do k=2,nVertLevels
                  tend_w_euler(k,iCell) = tend_w_euler(k,iCell) + v_mom_eddy_visc2*0.5*(rho_zz(k,iCell)+rho_zz(k-1,iCell))*(  &
                                           (w(k+1,iCell)-w(k  ,iCell))*rdzw(k)                              &
                                          -(w(k  ,iCell)-w(k-1,iCell))*rdzw(k-1) )*rdzu(k)
               end do
            end do

         end if

      end if ! mixing term computed first rk_step

      ! add in mixing terms for w

      do iCell = cellSolveStart,cellSolveEnd
!DIR$ IVDEP
         do k=2,nVertLevels
            tend_w(k,iCell) = tend_w(k,iCell) + tend_w_euler(k,iCell)
         end do
      end do

!----------- rhs for theta

      !
      !  horizontal advection for theta
      !

      do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...
         tend_theta(1:nVertLevels,iCell) = 0.0
         do i=1,nEdgesOnCell(iCell)
            iEdge = edgesOnCell(i,iCell)

            flux_arr(1:nVertLevels) = 0.0

            do j=1,nAdvCellsForEdge(iEdge)
               iAdvCell = advCellsForEdge(j,iEdge)
               do k=1,nVertLevels
                  scalar_weight = adv_coefs(j,iEdge) + sign(1.0_RKIND,ru(k,iEdge))*adv_coefs_3rd(j,iEdge)
                  flux_arr(k) = flux_arr(k) + scalar_weight* theta_m(k,iAdvCell)
               end do
            end do

!DIR$ IVDEP
            do k=1,nVertLevels
               tend_theta(k,iCell) = tend_theta(k,iCell) - edgesOnCell_sign(i,iCell) * ru(k,iEdge) * flux_arr(k)
            end do

         end do
      end do

!  addition to pick up perturbation flux for rtheta_pp equation

      if(rk_step > 1) then
        do iCell=cellSolveStart,cellSolveEnd
          do i=1,nEdgesOnCell(iCell) 
            iEdge = edgesOnCell(i,iCell)
            cell1 = cellsOnEdge(1,iEdge)
            cell2 = cellsOnEdge(2,iEdge)
!DIR$ IVDEP
            do k=1,nVertLevels
               flux = edgesOnCell_sign(i,iCell)*dvEdge(iEdge)*(ru_save(k,iEdge)-ru(k,iEdge))*0.5*(theta_m_save(k,cell2)+theta_m_save(k,cell1))
               tend_theta(k,iCell) = tend_theta(k,iCell)-flux  ! division by areaCell picked up down below
            end do
          end do
        end do
      end if

      !
      !  horizontal mixing for theta_m - we could combine this with advection directly (i.e. as a turbulent flux),
      !  but here we can also code in hyperdiffusion if we wish (2nd order at present)
      !

      if (rk_step == 1) then

         delsq_theta(1:nVertLevels,cellStart:cellEnd) = 0.0

         do iCell=cellStart,cellEnd
            tend_theta_euler(1:nVertLevels,iCell) = 0.0
            r_areaCell = invAreaCell(iCell)
            do i=1,nEdgesOnCell(iCell)
               iEdge = edgesOnCell(i,iCell)
               edge_sign = r_areaCell*edgesOnCell_sign(i,iCell) * dvEdge(iEdge) * invDcEdge(iEdge)
               pr_scale = prandtl_inv * meshScalingDel2(iEdge)
               cell1 = cellsOnEdge(1,iEdge)
               cell2 = cellsOnEdge(2,iEdge)
!DIR$ IVDEP
               do k=1,nVertLevels

!  we are computing the Smagorinsky filter at more points than needed here so as to pick up the delsq_theta for 4th order filter below

                  theta_turb_flux = edge_sign*(theta_m(k,cell2) - theta_m(k,cell1))*rho_edge(k,iEdge)
                  delsq_theta(k,iCell) = delsq_theta(k,iCell) + theta_turb_flux
                  theta_turb_flux = theta_turb_flux*0.5*(kdiff(k,cell1)+kdiff(k,cell2)) * pr_scale
                  tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) + theta_turb_flux

               end do
            end do
          end do

!$OMP BARRIER
            
         if (h_theta_eddy_visc4 > 0.0) then  ! 4th order mixing is active

            do iCell=cellSolveStart,cellSolveEnd    ! Technically updating fewer cells than before...
               r_areaCell = h_theta_eddy_visc4 * prandtl_inv * invAreaCell(iCell)
               do i=1,nEdgesOnCell(iCell)

                  iEdge = edgesOnCell(i,iCell)
                  edge_sign = meshScalingDel4(iEdge)*r_areaCell*dvEdge(iEdge)*edgesOnCell_sign(i,iCell)*invDcEdge(iEdge)

                  cell1 = cellsOnEdge(1,iEdge)
                  cell2 = cellsOnEdge(2,iEdge)

                  do k=1,nVertLevels
                     tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) - edge_sign*(delsq_theta(k,cell2) - delsq_theta(k,cell1))
                  end do
               end do
            end do

         end if ! 4th order mixing is active 

      end if ! theta mixing calculated first rk_step

      !
      !  vertical advection plus diabatic term
      !  Note: we are also dividing through by the cell area after the horizontal flux divergence
      !
      do iCell = cellSolveStart,cellSolveEnd

         wdtz(1) = 0.0

         k = 2
         wdtz(k) =  rw(k,icell)*(fzm(k)*theta_m(k,iCell)+fzp(k)*theta_m(k-1,iCell))  
         wdtz(k) =  wdtz(k)+(rw_save(k,icell)-rw(k,icell))*(fzm(k)*theta_m_save(k,iCell)+fzp(k)*theta_m_save(k-1,iCell))
         do k=3,nVertLevels-1
            wdtz(k) = flux3( theta_m(k-2,iCell),theta_m(k-1,iCell),theta_m(k,iCell),theta_m(k+1,iCell), rw(k,iCell), coef_3rd_order )
            wdtz(k) =  wdtz(k) + (rw_save(k,icell)-rw(k,iCell))*(fzm(k)*theta_m_save(k,iCell)+fzp(k)*theta_m_save(k-1,iCell))  ! rtheta_pp redefinition
         end do
         k = nVertLevels
         wdtz(k) =  rw_save(k,icell)*(fzm(k)*theta_m(k,iCell)+fzp(k)*theta_m(k-1,iCell))  ! rtheta_pp redefinition

         wdtz(nVertLevels+1) = 0.0

!DIR$ IVDEP
         do k=1,nVertLevels
            tend_theta(k,iCell) = tend_theta(k,iCell)*invAreaCell(iCell) -rdzw(k)*(wdtz(k+1)-wdtz(k))
            tend_rtheta_adv(k,iCell) = tend_theta(k,iCell)   !  this is for the Tiedke scheme
            rthdynten(k,iCell) = tend_theta(k,iCell)/rho_zz(k,iCell)  !  this is for the Grell-Freitas scheme
            tend_theta(k,iCell) = tend_theta(k,iCell) + rho_zz(k,iCell)*rt_diabatic_tend(k,iCell)
         end do
      end do

      !
      !  vertical mixing for theta - 2nd order 
      !

      if (rk_step == 1) then

         if ( v_theta_eddy_visc2 > 0.0 ) then  ! vertical mixing for theta_m

            if (config_mix_full) then

               do iCell = cellSolveStart,cellSolveEnd
                  do k=2,nVertLevels-1
                     z1 = zgrid(k-1,iCell)
                     z2 = zgrid(k  ,iCell)
                     z3 = zgrid(k+1,iCell)
                     z4 = zgrid(k+2,iCell)

                     zm = 0.5*(z1+z2)
                     z0 = 0.5*(z2+z3)
                     zp = 0.5*(z3+z4)

                     tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) + v_theta_eddy_visc2*prandtl_inv*rho_zz(k,iCell)*(&
                                              (theta_m(k+1,iCell)-theta_m(k  ,iCell))/(zp-z0)                 &
                                             -(theta_m(k  ,iCell)-theta_m(k-1,iCell))/(z0-zm) )/(0.5*(zp-zm))
                  end do
               end do

         else  ! idealized cases where we mix on the perturbation from the initial 1-D state

               do iCell = cellSolveStart,cellSolveEnd
                  do k=2,nVertLevels-1
                     z1 = zgrid(k-1,iCell)
                     z2 = zgrid(k  ,iCell)
                     z3 = zgrid(k+1,iCell)
                     z4 = zgrid(k+2,iCell)

                     zm = 0.5*(z1+z2)
                     z0 = 0.5*(z2+z3)
                     zp = 0.5*(z3+z4)

                     tend_theta_euler(k,iCell) = tend_theta_euler(k,iCell) + v_theta_eddy_visc2*prandtl_inv*rho_zz(k,iCell)*(&
                                              ((theta_m(k+1,iCell)-t_init(k+1,iCell))-(theta_m(k  ,iCell)-t_init(k,iCell)))/(zp-z0)      &
                                             -((theta_m(k  ,iCell)-t_init(k,iCell))-(theta_m(k-1,iCell)-t_init(k-1,iCell)))/(z0-zm) )/(0.5*(zp-zm))
                  end do
               end do

            end if

         end if

      end if ! compute vertical theta mixing on first rk_step

      do iCell = cellSolveStart,cellSolveEnd
!DIR$ IVDEP
         do k=1,nVertLevels
!            tend_theta(k,iCell) = tend_theta(k,iCell) + tend_theta_euler(k,iCell)
            tend_theta(k,iCell) = tend_theta(k,iCell) + tend_theta_euler(k,iCell) + tend_rtheta_physics(k,iCell)
         end do
      end do

   end subroutine atm_compute_dyn_tend_work


   !read state subroutine for kr_externs_in_atm_time_integration 
   SUBROUTINE kr_externs_in_atm_time_integration(kgen_unit) 
       INTEGER, INTENT(IN) :: kgen_unit 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
         
       CALL kr_atm_time_integration_real__rkind_dim2(tend_rtheta_physics, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(tend_ru_physics, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(tend_rho_physics, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(qtot, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(delsq_divergence, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(delsq_w, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(delsq_theta, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(delsq_u, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(delsq_vorticity, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(dpdz, kgen_unit) 
   END SUBROUTINE kr_externs_in_atm_time_integration 
     
   !read state subroutine for kr_externs_out_atm_time_integration 
   SUBROUTINE kr_externs_out_atm_time_integration(kgen_unit) 
       INTEGER, INTENT(IN) :: kgen_unit 
         
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
       CALL kr_atm_time_integration_real__rkind_dim2(kgenref_delsq_divergence, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(kgenref_delsq_w, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(kgenref_delsq_theta, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(kgenref_delsq_u, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(kgenref_delsq_vorticity, kgen_unit) 
       CALL kr_atm_time_integration_real__rkind_dim2(kgenref_dpdz, kgen_unit) 
   END SUBROUTINE kr_externs_out_atm_time_integration 
     
   !read state subroutine for kr_atm_time_integration_real__rkind_dim2 
   SUBROUTINE kr_atm_time_integration_real__rkind_dim2(var, kgen_unit, printvar) 
       REAL(KIND=rkind), INTENT(INOUT), ALLOCATABLE, DIMENSION(:,:) :: var 
       INTEGER, INTENT(IN) :: kgen_unit 
       CHARACTER(LEN=*), INTENT(IN), OPTIONAL :: printvar 
       LOGICAL :: kgen_istrue 
       REAL(KIND=8) :: kgen_array_sum 
       INTEGER :: idx1, idx2 
       INTEGER, DIMENSION(2,2) :: kgen_bound 
         
       READ (UNIT = kgen_unit) kgen_istrue 
       IF (kgen_istrue) THEN 
           IF (ALLOCATED( var )) THEN 
               DEALLOCATE (var) 
           END IF   
           READ (UNIT = kgen_unit) kgen_array_sum 
           READ (UNIT = kgen_unit) kgen_bound(1, 1) 
           READ (UNIT = kgen_unit) kgen_bound(2, 1) 
           READ (UNIT = kgen_unit) kgen_bound(1, 2) 
           READ (UNIT = kgen_unit) kgen_bound(2, 2) 
           ALLOCATE (var(kgen_bound(1,1):kgen_bound(2,1), kgen_bound(1,2):kgen_bound(2,2))) 
           READ (UNIT = kgen_unit) var 
           IF (PRESENT( printvar )) THEN 
               CALL kgen_array_sumcheck(printvar, kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
               WRITE (*, *) "** KGEN DEBUG: " // printvar // "**" // NEW_LINE("A"), var 
           ELSE 
               CALL kgen_array_sumcheck("UNKNOWN", kgen_array_sum, REAL(SUM(var, mask=(var .eq. var)), 8), .TRUE.) 
           END IF   
       END IF   
   END SUBROUTINE kr_atm_time_integration_real__rkind_dim2 
     
   !verify state subroutine for kv_externs_atm_time_integration 
   SUBROUTINE kv_externs_atm_time_integration(check_status) 
       TYPE(check_t), INTENT(INOUT) :: check_status 
         
       CALL kv_atm_time_integration_real__rkind_dim2("delsq_divergence", check_status, delsq_divergence, &
       &kgenref_delsq_divergence) 
       CALL kv_atm_time_integration_real__rkind_dim2("delsq_w", check_status, delsq_w, kgenref_delsq_w) 
       CALL kv_atm_time_integration_real__rkind_dim2("delsq_theta", check_status, delsq_theta, kgenref_delsq_theta) 
       CALL kv_atm_time_integration_real__rkind_dim2("delsq_u", check_status, delsq_u, kgenref_delsq_u) 
       CALL kv_atm_time_integration_real__rkind_dim2("delsq_vorticity", check_status, delsq_vorticity, kgenref_delsq_vorticity) 
       CALL kv_atm_time_integration_real__rkind_dim2("dpdz", check_status, dpdz, kgenref_dpdz) 
   END SUBROUTINE kv_externs_atm_time_integration 
     
   !verify state subroutine for kv_atm_time_integration_real__rkind_dim2 
   RECURSIVE SUBROUTINE kv_atm_time_integration_real__rkind_dim2(varname, check_status, var, kgenref_var) 
       CHARACTER(LEN=*), INTENT(IN) :: varname 
       TYPE(check_t), INTENT(INOUT) :: check_status 
       REAL(KIND=rkind), allocatable, INTENT(IN), DIMENSION(:,:) :: var, kgenref_var 
       INTEGER :: check_result 
       LOGICAL :: is_print = .FALSE. 
         
       INTEGER :: idx1, idx2 
       INTEGER :: n 
       real(KIND=rkind) :: nrmsdiff, rmsdiff 
       real(KIND=rkind), ALLOCATABLE :: buf1(:,:), buf2(:,:) 
         
       IF (ALLOCATED(var)) THEN 
           check_status%numTotal = check_status%numTotal + 1 
             
           IF (ALL(var == kgenref_var)) THEN 
               check_status%numIdentical = check_status%numIdentical + 1 
               IF (check_status%verboseLevel > 1) THEN 
                   WRITE (*, *) trim(adjustl(varname)), " is IDENTICAL." 
               END IF   
               check_result = CHECK_IDENTICAL 
           ELSE 
               ALLOCATE (buf1(SIZE(var,dim=1),SIZE(var,dim=2))) 
               ALLOCATE (buf2(SIZE(var,dim=1),SIZE(var,dim=2))) 
               n = COUNT(var /= kgenref_var) 
               WHERE ( ABS(kgenref_var) > check_status%minvalue ) 
                   buf1 = ((var-kgenref_var)/kgenref_var)**2 
                   buf2 = (var-kgenref_var)**2 
               ELSEWHERE 
                   buf1 = (var-kgenref_var)**2 
                   buf2 = buf1 
               END WHERE   
               nrmsdiff = SQRT(SUM(buf1)/REAL(n)) 
               rmsdiff = SQRT(SUM(buf2)/REAL(n)) 
               IF (nrmsdiff > check_status%tolerance) THEN 
                   check_status%numOutTol = check_status%numOutTol + 1 
                   IF (check_status%verboseLevel > 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(out of tolerance)." 
                   END IF   
                   check_result = CHECK_OUT_TOL 
               ELSE 
                   check_status%numInTol = check_status%numInTol + 1 
                   IF (check_status%verboseLevel > 0) THEN 
                       WRITE (*, *) trim(adjustl(varname)), " is NOT IDENTICAL(within tolerance)." 
                   END IF   
                   check_result = CHECK_IN_TOL 
               END IF   
           END IF   
           IF (check_result == CHECK_IDENTICAL) THEN 
               IF (check_status%verboseLevel > 2) THEN 
                   WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                   WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                   WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                   WRITE (*, *) "RMS of difference is ", 0 
                   WRITE (*, *) "Normalized RMS of difference is ", 0 
                   WRITE (*, *) "" 
               END IF   
           ELSE IF (check_result == CHECK_OUT_TOL) THEN 
               IF (check_status%verboseLevel > 0) THEN 
                   WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                   WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                   WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                   WRITE (*, *) "RMS of difference is ", rmsdiff 
                   WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                   WRITE (*, *) "" 
               END IF   
           ELSE IF (check_result == CHECK_IN_TOL) THEN 
               IF (check_status%verboseLevel > 1) THEN 
                   WRITE (*, *) count( var /= kgenref_var), " of ", size( var ), " elements are different." 
                   WRITE (*, *) "Average - kernel ", sum(var)/real(size(var)) 
                   WRITE (*, *) "Average - reference ", sum(kgenref_var)/real(size(kgenref_var)) 
                   WRITE (*, *) "RMS of difference is ", rmsdiff 
                   WRITE (*, *) "Normalized RMS of difference is ", nrmsdiff 
                   WRITE (*, *) "" 
               END IF   
           END IF   
             
       END IF   
   END SUBROUTINE kv_atm_time_integration_real__rkind_dim2 
     
end module atm_time_integration
