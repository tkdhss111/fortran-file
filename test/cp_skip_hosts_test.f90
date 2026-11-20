! Test of cp_skip_failed_hosts. Needs network (DNS case, about 4 minutes) and a local HTTP server
! on 127.0.0.1:18765 serving a file ok.csv, e.g.:
!   mkdir -p www && echo a,b > www/ok.csv && python3 -m http.server 18765 --bind 127.0.0.1 --directory www &
!   gfortran -ffree-line-length-none ../src/file_mo.f90 cp_skip_hosts_test.f90 -o cp_skip_hosts_test && ./cp_skip_hosts_test
program cp_skip_hosts_test
  use file_mo
  implicit none
  integer :: st, fails
  real :: t0, t1
  fails = 0
  ! 1. default (off): unresolvable host keeps the old behaviour: outer retry runs (3 attempts)
  call get( 'https://nonexistent-host.invalid/a.csv', 3, st, t0 )
  call check( '1 off: DNS failure returns 6', st == 6 )
  ! 2. on: same failure, no outer retry, host marked down
  call cp_skip_failed_hosts( .true. )
  call get( 'https://nonexistent-host.invalid/b.csv', 3, st, t1 )
  call check( '2 on: DNS failure returns 6', st == 6 )
  call check( '2 on: no outer retry (faster than off)', t1 < t0 - 1.5 )
  ! 3. later file from the same host: skipped without curl
  call get( 'https://nonexistent-host.invalid/c.csv', 3, st, t1 )
  call check( '3 on: skipped host returns 6', st == 6 )
  call check( '3 on: skipped at once (<0.5 s)', t1 < 0.5 )
  ! 4. connection refused (7) on one port marks only that host:port
  call get( 'http://127.0.0.1:1/x.csv', 3, st, t1 )
  call check( '4 on: refused returns 7', st == 7 )
  call get( 'http://127.0.0.1:1/y.csv', 3, st, t1 )
  call check( '4 on: refused port skipped', st == 7 .and. t1 < 0.5 )
  ! 5. a working server on the same machine, other port: unaffected; a 404 does not mark it down
  call get( 'http://127.0.0.1:18765/missing.csv', 3, st, t1 )
  call check( '5 on: 404 returns 22', st == 22 )
  call get( 'http://127.0.0.1:18765/ok.csv', 3, st, t1 )
  call check( '5 on: same host after 404 still downloads', st == 0 )
  ! 6. turning it off forgets down hosts
  call cp_skip_failed_hosts( .false. )
  call get( 'http://127.0.0.1:1/z.csv', 1, st, t1 )
  call check( '6 off again: refused tried again by curl (7)', st == 7 )
  print '(a,i0)', 'FAILURES: ', fails
  if ( fails > 0 ) error stop 1
contains
  subroutine get( url, n, st, secs )
    character(*), intent(in) :: url
    integer, intent(in) :: n
    integer, intent(out) :: st
    real, intent(out) :: secs
    type(file_ty) :: a, b
    integer(8) :: c0, c1, r
    call a%init( path = url ); a%exist = .true.; a%local = .false.
    call b%init( path = './cp_out/'//trim(a%name) )
    call system_clock( c0, r )
    call cp( a, b, stat = st, max_retries = n, wait_sec = 1 )
    call system_clock( c1 )
    secs = real(c1 - c0) / real(r)
    print '(a,i0,a,f6.2,a)', '  -> stat ', st, ' in ', secs, ' s'
  end subroutine
  subroutine check( what, ok )
    character(*), intent(in) :: what
    logical, intent(in) :: ok
    print '(a)', merge('PASS ', 'FAIL ', ok)//what
    if ( .not. ok ) fails = fails + 1
  end subroutine
end program cp_skip_hosts_test
