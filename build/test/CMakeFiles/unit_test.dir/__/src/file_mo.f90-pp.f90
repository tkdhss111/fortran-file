# 1 "/home/hss/0_tkd/1_hss/2_tools/fortran-file/src/file_mo.f90"
# 1 "<built-in>"
# 1 "<command-line>"
# 1 "/home/hss/0_tkd/1_hss/2_tools/fortran-file/src/file_mo.f90"
module file_mo

  implicit none

  private
  public :: file_ty
  public :: find_files
  public :: dirname, filename, basename, extname
  public :: touch, rm, cp, mv
  public :: mkdir, rmdir, cldir

  type file_ty

    character(255) :: path     = ''
    character(255) :: dir      = ''
    character(255) :: content  = ''
    character(50)  :: name     = ''
    character(50)  :: basename = ''
    character(10)  :: ext      = ''
    integer(8)     :: size     = 0
    logical        :: exist    = .false.

  contains

    procedure :: init  => init_file
    procedure :: print => print_file
    procedure :: touch
    procedure :: rm
    procedure :: cp
    procedure :: mv
    procedure :: mkdir
    procedure :: rmdir
    procedure :: cldir

  end type

contains

  subroutine touch ( this )
    class(file_ty), intent(inout) :: this
    call exec ( 'touch '//trim(this%path) )
    this%exist = .true.
  end subroutine

  subroutine rm ( this )
    class(file_ty), intent(inout) :: this
    call exec ( 'rm '//trim(this%path) )
    this%exist = .false.
  end subroutine

  subroutine cp ( from, to )
    class(file_ty), intent(inout) :: from
    type(file_ty),  intent(inout) :: to
    call exec ( 'cp '//trim(from%path)//' '//trim(to%path) )
    to%exist = .true.
  end subroutine

  subroutine mv ( from, to )
    class(file_ty), intent(inout) :: from
    type(file_ty),  intent(inout) :: to
    call exec ( 'mv '//trim(from%path)//' '//trim(to%path) )
    from%exist = .false.
    to%exist   = .true.
  end subroutine

  subroutine mkdir ( this )
    class(file_ty), intent(inout) :: this
    call exec ( 'mkdir -p '//trim(this%dir) )
  end subroutine

  subroutine rmdir ( this )
    class(file_ty), intent(inout) :: this
    call exec ( 'rmdir '//trim(this%dir) )
  end subroutine

  subroutine cldir ( this )
    class(file_ty), intent(inout) :: this
    type(file_ty), allocatable    :: files(:)
    integer i
    files = find_files ( dir =  this%dir )
    do i = 1, size(files)
      call rm ( files(i) ) 
    end do
  end subroutine

  subroutine exec ( command )
    character(*)   :: command
    integer        :: cmdstat, exitstat
    character(255) :: cmdmsg
    call execute_command_line ( &
      command  = command,       &
      exitstat = exitstat,      &
      cmdstat  = cmdstat,       &
      cmdmsg   = cmdmsg )
    print *, 'Command: '//trim(command)
    if ( cmdstat > 0 ) then
      print *, 'Command execution failed with error: '//trim(cmdmsg)
      stop cmdstat
    else if ( cmdstat < 0 ) then
      print *, 'Command execution not supported.'
      stop cmdstat
    else ! cmdstat == 0
      if ( exitstat /= 0 ) then 
        print *, 'Command completed with status ', exitstat
        stop exitstat
      end if
    end if
  end subroutine

  subroutine init_file ( this, path, content )
    class(file_ty),         intent(inout) :: this
    character(*),           intent(in)    :: path
    character(*), optional, intent(in)    :: content
    if ( present( content ) ) then
      this%content = trim(content)
    else
      this%content = 'NA'
    end if
    this%path     = trim(path)
    this%dir      = dirname  ( path )
    this%basename = basename ( path )
    this%name     = filename ( path )
    this%ext      = extname  ( path )
    inquire ( file = this%path, exist = this%exist, size = this%size )
  end subroutine

  subroutine print_file ( this )
    class(file_ty), intent(in) :: this
    print '(a, l$)', 'File/Directory exists: ', this%exist
    if ( this%size < 2**10 ) then
      print '(a, i0, a$)', ', Size: ', this%size, 'B'
    else
      print '(a, g0.2, a$)', ', Size: ', real(this%size) / real(2**10), 'KiB'
    end if
    print '(a)', ', Path: '//trim(this%path)
  end subroutine

  pure function dirname ( path )
    character(*), intent(in)  :: path
    integer                   :: p_sep
    character(:), allocatable :: dirname
    p_sep = index( path, '/', back = .true. )
    if (p_sep > 1) then
      dirname = path(1:p_sep)
    else
      dirname = './'
    end if
  end function

  pure function filename ( path )
    character(*), intent(in)  :: path
    integer                   :: p_sep
    character(:), allocatable :: filename
    p_sep = index( path, '/', back = .true. )
    filename = path(p_sep+1:len_trim(path))
  end function

  pure function basename ( path )
    character(*), intent(in)  :: path
    integer                   :: p_sep, p_comma
    character(:), allocatable :: basename
    p_sep   = index( path, '/', back = .true. )
    p_comma = index( path, '.', back = .true. )
    basename = path(p_sep + 1:p_comma - 1)
  end function

  pure function extname ( path )
    character(*), intent(in)  :: path
    integer                   :: p_comma
    character(:), allocatable :: extname
    p_comma = index( path, '.', back = .true. )
    extname = path(p_comma:len_trim(path))
  end function

  function find_files ( dir, pattern, ignore, maxdepth, fullpath, type, image ) result ( files )
    ! Note. Use "image" option for thread safe search (e.g., image = this_image())
    type(file_ty), allocatable         :: files(:)
    character(*), intent(in), optional :: dir
    character(*), intent(in), optional :: pattern, ignore, type 
    integer,      intent(in), optional :: maxdepth 
    integer,      intent(in), optional :: image
    logical,      intent(in), optional :: fullpath 
    logical                            :: fullpath_
    character(1000)                    :: dir_, pattern_, ignore_, maxdepth_
    character(1)                       :: type_
    character(1000)                    :: list_fnms = 'filelist', fno
    character(1000)                    :: command, cmdmsg, filelist
    character(1000)                    :: pwd 
    character(30), allocatable         :: patterns(:), ignores(:)
    integer                            :: cmdstat, exitstat 
    integer                            :: image_
    integer                            :: i, u, n, nrows, nors
    logical                            :: exist

    print *, repeat( '-', 79 )

    if ( present( dir ) ) then
      dir_ = trim(dir)
      n = len_trim(dir_)
      if ( dir_(n:n) /= '/' ) dir_ = trim(dir_)//'/' 
    else
      dir_ = './'
    end if

    if ( present( fullpath ) ) then
      fullpath_ = fullpath
    else
      fullpath_ = .true.
    end if

    print '(a)', 'Search Directory: '//trim(dir_)
    call get_environment_variable ( 'PWD', pwd )
    print '(a)', 'Current Directory: '//trim(pwd)

    if ( present( pattern ) ) then
      pattern_ = trim(pattern)
    else
      pattern_ = '*'
    end if

    if ( present( ignore ) ) then
      ignore_ = trim(ignore)
    else
      ignore_ = ''
    end if

    if ( present( maxdepth ) ) then
      write ( maxdepth_, '(a, i0)' ) ' -maxdepth ', maxdepth
    else
      maxdepth_ = '' ! find files recursively
    end if

    if ( present( type ) ) then
      type_ = trim(type)
    else
      type_ = 'f'
    end if

    if ( present( image ) ) then
      image_ = image
    else
      image_ = 1 
    end if

    !
    ! Search patterns
    !
    nors = count_ors ( pattern_ )

    if ( nors > 0 ) then
      print *, 'Number of patterns', nors
      allocate ( patterns(nors + 1) )
      do i = 1, len_trim(pattern_)
        if (pattern_(i:i) == '|' ) pattern_(i:i) = ','
      end do
      read ( pattern_, * ) patterns
      pattern_ = '-iname "'//trim(patterns(1))//'"'
      do i = 2, nors + 1 
        pattern_ = trim(pattern_)//' -or -iname "'//trim(patterns(i))//'"'
      end do
    else
      pattern_ = ' -iname "'//trim(pattern_)//'"'
    end if

    !
    ! Exclude search patterns
    !
    nors = count_ors ( ignore_ )

    if ( nors > 0 ) then
      print *, 'Number of ignores', nors
      allocate ( ignores(nors + 1) )
      do concurrent ( i = 1:len_trim(ignore_) )
        if (ignore_(i:i) == '|' ) ignore_(i:i) = ','
      end do
      read ( ignore_, * ) ignores
      ignore_ = ' -and ! -iname "'//trim(ignores(1))//'"'
      do i = 2, nors + 1 
        ignore_ = trim(ignore_)//' -and ! -iname "'//trim(ignores(i))//'"'
      end do
    else
      ignore_ = ' -and ! -iname "'//trim(ignore_)//'"'
    end if

    do
      write ( fno, '(i0)' ) image_
      !filelist = trim(dir_)//trim(list_fnms)//trim(fno)
      filelist = '/tmp/'//trim(list_fnms)//trim(fno)
      inquire ( file = filelist, exist = exist )
      if ( .not. exist ) exit
      image_ = image_ + 1
    end do
    
    if ( fullpath_ ) then
      command = 'find '//trim(dir_)//trim(maxdepth_)//&
                ' -type '//trim(type_)//&
                ' \( '//trim(pattern_)//trim(ignore_)//' \)'//&
                '| sort -n | tee '//trim(filelist)
    else
      command = 'find '//trim(dir_)//trim(maxdepth_)//&
                ' -type '//trim(type_)//&
                ' \( '//trim(pattern_)//trim(ignore_)//' \)'//&
                '| sort -n | xargs -n 1 basename | tee '//trim(filelist)
    end if

    if ( index( command, '~' ) > 0 ) then
      stop '*** Error: Do not include ~ as home directory in path.'
    end if

    print *, 'Command: ', trim(command)

    call execute_command_line ( command = command, &
      exitstat = exitstat, cmdstat = cmdstat, cmdmsg = cmdmsg )

    if ( cmdstat /= 0 ) then
      print *, trim(cmdmsg)
      return
    end if

    if ( exitstat /= 0 ) then
      print *, '*** Warning: No file found in the search directory: '//trim(dir_)
      allocate ( files(1) )
      files(1)%path = 'NA'
      return
    end if

    open ( newunit = u, file = filelist, status = 'old' )
    nrows = count_rows ( u )
    print *, 'Number of files/dirs found: ', nrows

    allocate ( files(nrows) )
    do i = 1, nrows
      read ( u, '(a)' ) files(i)%path
      if ( type == 'f' ) then
        print '(a, i0, a, a)', '    File #', i, ': ', trim(files(i)%path)
      end if
      if ( type == 'd' ) then
        n = len_trim(files(i)%path)
        if ( files(i)%path(n:n) /= '/' ) files(i)%path = trim(files(i)%path)//'/' 
        print '(a, i0, a, a)', '    Dir #', i, ': ', trim(files(i)%path)
      end if
    end do
    close ( u )

    do i = 1, size(files)
      call files(i)%init ( files(i)%path ) 
    end do

    call execute_command_line ( command = 'rm '//trim(filelist), &
                                exitstat = exitstat, cmdstat = cmdstat, cmdmsg = cmdmsg )

    if ( cmdstat /= 0 ) then
      print *, trim(cmdmsg)
      return
    end if

    if ( exitstat /= 0 ) then
      stop '*** Error: Function "find_files" is not thread safe. Consider using "image" option.'
    end if

    print *, repeat( '-', 79 )

  end function

  pure elemental function count_ors ( line ) result ( n )
    character(*), intent(in) :: line
    integer i, n
    n = 0
    do i = 1, len_trim(line)   
      if ( line(i:i) /= '|' ) cycle
      n = n + 1
    end do
  end function

  function count_rows ( u ) result ( nr )
    integer, intent(in) :: u
    integer             :: nr
    character(255)      :: iomsg
    integer             :: iostat
    nr = 0
    rewind(u)
    do
      read ( u, '()', end = 10, iostat = iostat, iomsg = iomsg )
      if ( iostat /= 0 ) then
        nr = 0
        return
      end if
      nr = nr + 1
    end do
    10 rewind (u)
  end function

end module
