program lock_test
  use file_mo
  implicit none
  character(64) :: mode, arg; integer :: fd, st, i; real :: w
  call get_command_argument(1, mode); call get_command_argument(2, arg)
  select case (trim(mode))
  case ('hold')        ! hold the lock for arg seconds
    call lock_path('t.lock', fd, stat=st); print '(a,i0)', 'hold: stat=', st
    call execute_command_line('sleep '//trim(arg)); call unlock_path(fd); print '(a,i0)', 'hold: released fd=', fd
  case ('try')         ! wait at most arg seconds
    read(arg,*) w; call lock_path('t.lock', fd, stat=st, wait_s=w); print '(a,i0)', 'try: stat=', st; call unlock_path(fd)
  case ('block')
    call lock_path('t.lock', fd); print '(a)', 'block: acquired'; call unlock_path(fd)
  case ('nested')
    call lock_path('t.lock', fd, stat=st); call lock_path('t.lock', i, stat=st, wait_s=0.2); print '(a,i0)', 'nested second: stat=', st
  end select
end program lock_test
