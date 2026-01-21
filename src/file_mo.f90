module file_mo

  use, intrinsic :: iso_c_binding
  implicit none

  private
  public :: file_ty
  public :: hostname, dirname, filename, basename, extname, schemename
  public :: find, touch, rm, cp, mv
  public :: mkdir, rmdir, cldir
  public :: KiB, MiB, GiB

  type t_ty
    character(len=19) :: accessed = '' ! YYYY-MM-DD HH:MM:SS
    character(len=19) :: modified = '' ! YYYY-MM-DD HH:MM:SS
    character(len=19) :: changed  = '' ! YYYY-MM-DD HH:MM:SS
  end type

  type, extends(t_ty) :: file_ty
    type(t_ty) :: t
    character(:), allocatable :: path     ! Full/relative path to a file
    character(:), allocatable :: dir      ! Directory name with tailing /
    character(:), allocatable :: name     ! File name with extention
    character(:), allocatable :: hostname ! Hostname
    character(:), allocatable :: basename ! File name without extention
    character(:), allocatable :: ext      ! Extension of a file
    character(:), allocatable :: content  ! File content
    character(:), allocatable :: scheme   ! URI scheme {https, http, file}
    character(:), allocatable :: encoding ! File encoding
    character(1) :: type  = ''
    integer(8)   :: size  = 0
    logical      :: exist = .false.
    logical      :: local = .true.

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

  integer, parameter :: KiB = 2 ** 10
  integer, parameter :: MiB = 2 ** 20
  integer, parameter :: GiB = 2 ** 30

  !-----------------------------------------------------------------------------
  ! C struct stat for Linux x86_64
  !-----------------------------------------------------------------------------
  type, bind(C) :: c_stat_t
    integer(c_long)  :: st_dev
    integer(c_long)  :: st_ino
    integer(c_long)  :: st_nlink
    integer(c_int)   :: st_mode
    integer(c_int)   :: st_uid
    integer(c_int)   :: st_gid
    integer(c_int)   :: pad0
    integer(c_long)  :: st_rdev
    integer(c_long)  :: st_size
    integer(c_long)  :: st_blksize
    integer(c_long)  :: st_blocks
    integer(c_long)  :: st_atime
    integer(c_long)  :: st_atime_nsec
    integer(c_long)  :: st_mtime
    integer(c_long)  :: st_mtime_nsec
    integer(c_long)  :: st_ctime
    integer(c_long)  :: st_ctime_nsec
    integer(c_long)  :: reserved(3)
  end type

  !-----------------------------------------------------------------------------
  ! C function interfaces
  !-----------------------------------------------------------------------------
  interface
    integer(c_int) function c_stat( path, buf ) bind(C, name="stat")
      import :: c_char, c_int, c_stat_t
      character(kind=c_char), intent(in) :: path(*)
      type(c_stat_t), intent(out)        :: buf
    end function

    integer(c_int) function c_gethostname( name, namelen ) bind(C, name="gethostname")
      import :: c_char, c_int, c_size_t
      character(kind=c_char), intent(out) :: name(*)
      integer(c_size_t), value            :: namelen
    end function
  end interface

contains

  subroutine touch ( this )
    class(file_ty), intent(inout) :: this
    call exec( 'touch '//trim(this%path) )
    this%exist = .true.
  end subroutine

  subroutine rm ( this )
    class(file_ty), intent(inout) :: this
    if ( this%local ) then
      call exec( 'rm -f '//trim(this%path) )
      this%exist = .false.
    else
      error stop '*** Error: Can not remove file in remote'
    end if
  end subroutine

  subroutine cp ( from, to )

    class(file_ty), intent(inout) :: from
    type(file_ty),  intent(inout) :: to
    character(:), allocatable     :: cmd
    logical                       :: need_iconv

    need_iconv = from%encoding /= '' .and. to%encoding /= '' .and. from%encoding /= to%encoding

    if ( from%local ) then
      if ( need_iconv ) then
        cmd = 'iconv -f '//trim(from%encoding)// &
                   ' -t '//trim(to%encoding)// &
                   ' '//trim(from%path)//' > '//trim(to%path)
      else
        cmd = 'cp '//trim(from%path)//' '//trim(to%path)
      end if
    else
      if ( from%exist ) then
        if ( need_iconv ) then
          cmd = 'curl --location --show-error --silent --fail-with-body --create-dirs --insecure '// &
                trim(from%path)//' | iconv -f '//trim(from%encoding)//' -t '//trim(to%encoding)//' > '//trim(to%path)
        else
          cmd = 'curl --location --show-error --silent --fail-with-body --create-dirs --insecure '// &
                trim(from%path)//' > '//trim(to%path)
        end if
      else
        error stop '*** Error: Remote file does not exist'
      end if
    end if
    write( *, '(a)' ) 'Command: '//trim(cmd)
    call exec( cmd )
    to%exist = .true.
  end subroutine

  subroutine mv ( from, to )
    class(file_ty), intent(inout) :: from
    type(file_ty),  intent(inout) :: to
    if ( from%local ) then
      call exec( 'mv '//trim(from%path)//' '//trim(to%path) )
      from%exist = .false.
      to%exist   = .true.
    else
      error stop '*** Error: Can not move file in remote'
    end if
  end subroutine

  subroutine mkdir ( this )
    class(file_ty), intent(inout) :: this
    if ( this%local ) then
      call exec( 'mkdir -p '//trim(this%dir) )
    else
      error stop '*** Error: Can not make directory in remote'
    end if
  end subroutine

  subroutine rmdir ( this )
    class(file_ty), intent(inout) :: this
    if ( this%local ) then
      call exec( 'rmdir '//trim(this%dir) )
    else
      error stop '*** Error: Can not remove directory in remote'
    end if
  end subroutine

  subroutine cldir ( this )
    class(file_ty), intent(inout) :: this
    type(file_ty), allocatable    :: files(:)
    integer i
    if ( .not. this%local ) then
      error stop '*** Error: Can not clear files in remote'
    end if
    files = find( dir = this%dir )
    do i = 1, size(files)
      call rm( files(i) )
    end do
  end subroutine

  subroutine exec ( command )
    character(*), intent(in) :: command
    integer        :: cmdstat, exitstat
    character(255) :: cmdmsg
    call execute_command_line( &
      command  = command,      &
      exitstat = exitstat,     &
      cmdstat  = cmdstat,      &
      cmdmsg   = cmdmsg )
    if ( cmdstat > 0 ) then
      write( *, '(a)' ) 'Command execution failed with error: '//trim(cmdmsg)
      error stop cmdstat
    else if ( cmdstat < 0 ) then
      write( *, '(a)' ) 'Command execution not supported.'
      error stop cmdstat
    else
      if ( exitstat /= 0 ) then
        write( *, '(a,i0)' ) 'Command completed with status ', exitstat
        error stop exitstat
      end if
    end if
  end subroutine

  subroutine init_file ( this, path, content, encoding )
    class(file_ty),         intent(inout) :: this
    character(*), optional, intent(in)    :: path
    character(*), optional, intent(in)    :: content
    character(*), optional, intent(in)    :: encoding
    type(t_ty) :: t

    if ( present(content) ) then
      this%content = trim(content)
    else
      this%content = 'NA'
    end if
    if ( present(encoding) ) then
      this%encoding = trim(encoding)
    else
      this%encoding = ''
    end if
    if ( present(path) ) then
      this%path = trim(path)
    end if

    this%dir      = dirname( this%path )
    this%basename = basename( this%path )
    this%hostname = hostname()
    this%name     = filename( this%path )
    this%ext      = extname( this%path )
    this%scheme   = schemename( this%path )

    if ( this%scheme == 'NA' ) then
      inquire( file = this%path, exist = this%exist, size = this%size )
      this%local = .true.
    else
      call check_uri_file( this )
      if ( this%scheme == 'file' ) then
        this%local = .true.
        this%path  = uri2path( this%path )
      else
        this%local = .false.
      end if
    end if

    if ( this%exist .and. this%local ) then
      call get_file_times( this%path, t )
      this%accessed = t%accessed
      this%modified = t%modified
      this%changed  = t%changed
    end if
  end subroutine

  subroutine print_file ( this )
    class(file_ty), intent(in) :: this
    print '(a,l$)', 'File/Directory exists: ', this%exist
    if ( this%size < 2**10 ) then
      print '(a,i0,a$)', ', Size: ', this%size, 'B'
    else if ( this%size < 2**20 ) then
      print '(a,g0.2,a$)', ', Size: ', real(this%size) / KiB, 'KiB'
    else if ( this%size < 2**30 ) then
      print '(a,g0.2,a$)', ', Size: ', real(this%size) / MiB, 'MiB'
    else
      print '(a,g0.2,a$)', ', Size: ', real(this%size) / GiB, 'GiB'
    end if
    print '(a$)', ', Path: '//trim(this%path)
    print '(a$)', ', Accessed: '//trim(this%accessed)
    print '(a$)', ', Modified: '//trim(this%modified)
    print '(a)', ', Changed:  '//trim(this%changed)
  end subroutine

  pure function dirname ( path )
    character(*), intent(in)  :: path
    character(:), allocatable :: dirname
    integer :: p_sep
    p_sep = index( path, '/', back = .true. )
    if ( p_sep > 1 ) then
      dirname = path(1:p_sep)
    else
      dirname = './'
    end if
  end function

  pure function filename ( path )
    character(*), intent(in)  :: path
    character(:), allocatable :: filename
    integer :: p_sep
    p_sep = index( path, '/', back = .true. )
    filename = path(p_sep+1:len_trim(path))
  end function

  pure function basename ( path )
    character(*), intent(in)  :: path
    character(:), allocatable :: basename
    integer :: p_sep, p_dot
    p_sep = index( path, '/', back = .true. )
    p_dot = index( path, '.', back = .true. )
    if ( p_dot > p_sep ) then
      basename = path(p_sep+1:p_dot-1)
    else
      basename = path(p_sep+1:len_trim(path))
    end if
  end function

  pure function extname ( path )
    character(*), intent(in)  :: path
    character(:), allocatable :: extname
    integer :: p_dot
    p_dot = index( path, '.', back = .true. )
    if ( p_dot > 0 ) then
      extname = path(p_dot:len_trim(path))
    else
      extname = ''
    end if
  end function

  pure function schemename ( path )
    character(*), intent(in)  :: path
    character(:), allocatable :: schemename
    integer :: p_sep
    p_sep = index( path, '://' )
    if ( p_sep > 1 ) then
      schemename = path(1:p_sep-1)
    else
      schemename = 'NA'
    end if
  end function

  function find ( dir, pattern, ignore, maxdepth, fullpath, type, image ) result ( files )
    type(file_ty), allocatable         :: files(:)
    character(*), intent(in), optional :: dir
    character(*), intent(in), optional :: pattern, ignore
    character(1), intent(in), optional :: type
    integer,      intent(in), optional :: maxdepth
    integer,      intent(in), optional :: image
    logical,      intent(in), optional :: fullpath

    logical         :: fullpath_
    character(1000) :: dir_, pattern_, ignore_, maxdepth_
    character(1)    :: type_
    character(1000) :: list_fnms = 'filelist', fno
    character(1000) :: command, cmdmsg, filelist
    character(1000) :: path, pwd
    character(30), allocatable :: patterns(:), ignores(:)
    integer :: cmdstat, exitstat
    integer :: image_
    integer :: i, u, n, nrows, nors
    logical :: exist

    write( *, * ) repeat( '-', 79 )

    if ( present(dir) ) then
      dir_ = trim(dir)
      n = len_trim(dir_)
      if ( dir_(n:n) /= '/' ) dir_ = trim(dir_)//'/'
    else
      dir_ = './'
    end if

    fullpath_ = .true.
    if ( present(fullpath) ) fullpath_ = fullpath

    print '(a)', 'Search Directory: '//trim(dir_)
    call get_environment_variable( 'PWD', pwd )
    print '(a)', 'Current Directory: '//trim(pwd)

    pattern_ = '*'
    if ( present(pattern) ) pattern_ = trim(pattern)

    ignore_ = ''
    if ( present(ignore) ) ignore_ = trim(ignore)

    maxdepth_ = ''
    if ( present(maxdepth) ) write( maxdepth_, '(a,i0)' ) ' -maxdepth ', maxdepth

    type_ = 'f'
    if ( present(type) ) type_ = type

    image_ = 1
    if ( present(image) ) image_ = image

    ! Search patterns
    nors = count_ors( pattern_ )

    if ( nors > 0 ) then
      write( *, * ) 'Number of patterns', nors
      allocate( patterns(nors+1) )
      do i = 1, len_trim(pattern_)
        if ( pattern_(i:i) == '|' ) pattern_(i:i) = ','
      end do
      read( pattern_, * ) patterns
      pattern_ = '-iname "'//trim(patterns(1))//'"'
      do i = 2, nors + 1
        pattern_ = trim(pattern_)//' -or -iname "'//trim(patterns(i))//'"'
      end do
    else
      pattern_ = ' -iname "'//trim(pattern_)//'"'
    end if

    ! Exclude search patterns
    nors = count_ors( ignore_ )

    if ( nors > 0 ) then
      write( *, * ) 'Number of ignores', nors
      allocate( ignores(nors+1) )
      do concurrent ( i = 1:len_trim(ignore_) )
        if ( ignore_(i:i) == '|' ) ignore_(i:i) = ','
      end do
      read( ignore_, * ) ignores
      ignore_ = ' -and ! -iname "'//trim(ignores(1))//'"'
      do i = 2, nors + 1
        ignore_ = trim(ignore_)//' -and ! -iname "'//trim(ignores(i))//'"'
      end do
    else
      ignore_ = ' -and ! -iname "'//trim(ignore_)//'"'
    end if

    do
      write( fno, '(i0)' ) image_
      filelist = trim(list_fnms)//trim(fno)
      inquire( file = filelist, exist = exist )
      if ( .not. exist ) exit
      image_ = image_ + 1
    end do

    if ( fullpath_ ) then
      command = 'find "'//trim(dir_)//'"'//trim(maxdepth_)// &
                ' -type '//trim(type_)// &
                ' \( '//trim(pattern_)//trim(ignore_)//' \)'// &
                '| sort -n > '//trim(filelist)
    else
      command = 'find "'//trim(dir_)//'"'//trim(maxdepth_)// &
                ' -type '//trim(type_)// &
                ' \( '//trim(pattern_)//trim(ignore_)//' \)'// &
                '| sort -n | xargs -n 1 basename > '//trim(filelist)
    end if

    if ( index( command, '~' ) > 0 ) then
      error stop '*** Error: Do not include ~ as home directory in path.'
    end if

    write( *, * ) 'Command: ', trim(command)

    call execute_command_line( command = command, &
      exitstat = exitstat, cmdstat = cmdstat, cmdmsg = cmdmsg )

    if ( cmdstat /= 0 ) then
      write( *, * ) trim(cmdmsg)
      return
    end if

    if ( exitstat /= 0 ) then
      write( *, * ) '*** Warning: No file found in the search directory: '//trim(dir_)
      allocate( files(1) )
      files(1)%path = 'NA'
      return
    end if

    open( newunit = u, file = filelist, status = 'old' )
    nrows = count_rows( u )
    write( *, * ) 'Number of files/dirs found: ', nrows

    allocate( files(nrows) )
    do i = 1, nrows
      read( u, '(a)' ) path
      files(i)%path = trim(path)
      if ( type_ == 'f' ) then
        print '(a,i0,a,a)', '    File #', i, ': ', trim(files(i)%path)
      end if
      if ( type_ == 'd' ) then
        n = len_trim(files(i)%path)
        if ( files(i)%path(n:n) /= '/' ) files(i)%path = trim(files(i)%path)//'/'
        print '(a,i0,a,a)', '    Dir #', i, ': ', trim(files(i)%path)
      end if
    end do
    close( u )

    do i = 1, size(files)
      call files(i)%init( files(i)%path )
    end do

    call execute_command_line( command = 'rm '//trim(filelist), &
      exitstat = exitstat, cmdstat = cmdstat, cmdmsg = cmdmsg )

    if ( cmdstat /= 0 ) then
      write( *, * ) trim(cmdmsg)
      return
    end if

    if ( exitstat /= 0 ) then
      error stop '*** Error: Function "find" is not thread safe. Consider using "image" option.'
    end if

    write( *, * ) repeat( '-', 79 )

  end function

  pure elemental function count_ors ( line ) result ( n )
    character(*), intent(in) :: line
    integer :: i, n
    n = 0
    do i = 1, len_trim(line)
      if ( line(i:i) /= '|' ) cycle
      n = n + 1
    end do
  end function

  function count_rows ( u ) result ( nr )
    integer, intent(in) :: u
    integer             :: nr, iostat
    character(255)      :: iomsg
    nr = 0
    rewind( u )
    do
      read( u, '()', end = 10, iostat = iostat, iomsg = iomsg )
      if ( iostat /= 0 ) then
        nr = 0
        return
      end if
      nr = nr + 1
    end do
    10 rewind( u )
  end function

  subroutine check_uri_file ( this, image )
    class(file_ty),    intent(inout) :: this
    integer, optional, intent(in)    :: image
    integer        :: image_
    character(512) :: cmd
    character(256) :: line
    character(256) :: tmpfile
    integer        :: j, u, iostat

    image_ = 1
    if ( present(image) ) image_ = image

    write( tmpfile, '(a,i0)' ) '/tmp/fortran-file-tmp', image_
    write( cmd, '(a)' ) 'curl -sI '//trim(this%path)//' > '//trim(tmpfile)

    call exec( cmd )

    open( newunit = u, file = tmpfile, status = 'old' )

    this%exist = .false.
    do
      read( u, '(a)', iostat = iostat ) line
      if ( iostat /= 0 ) exit
      if ( index( line, 'HTTP/' ) > 0 .and. index( line, '200' ) > 0 ) then
        this%exist = .true.
        this%local = .false.
      end if
      j = index( line, 'Content-Length:' )
      if ( j > 0 ) then
        read( line(16:len_trim(line)), * ) this%size
        exit
      end if
    end do

    close( u )

  end subroutine

  function uri2path ( uri ) result ( path )
    character(*), intent(in) :: uri
    character(len(uri))      :: path
    integer :: i, j
    path = ''
    j = 1
    if ( index(uri, 'file://') == 1 ) then
      do i = 8, len_trim(uri)
        path(j:j) = uri(i:i)
        j = j + 1
      end do
    else
      path = uri
    end if
  end function

  !-----------------------------------------------------------------------------
  ! Get hostname using C gethostname (works with gfortran and ifx)
  !-----------------------------------------------------------------------------
  function hostname () result ( name )
    character(:), allocatable :: name
    character(256) :: buf
    integer(c_int) :: status
    integer :: i

    buf = ''
    status = c_gethostname( buf, 256_c_size_t )

    ! Find null terminator and trim
    do i = 1, 256
      if ( buf(i:i) == c_null_char ) exit
    end do
    name = trim(buf(1:i-1))

    if ( len_trim(name) == 0 ) name = 'localhost'
  end function

  !-----------------------------------------------------------------------------
  ! Convert Unix timestamp to human readable string (YYYY-MM-DD HH:MM:SS)
  !-----------------------------------------------------------------------------
  pure function unix_to_datetime ( unix_time ) result ( str )
    integer(c_long), intent(in) :: unix_time
    character(19) :: str
    integer :: y, m, d, hh, mm, ss
    integer :: days, leap, t
    integer :: month_days(12)

    t = int(unix_time)

    ss   = mod(t, 60);  t = t / 60
    mm   = mod(t, 60);  t = t / 60
    hh   = mod(t, 24);  days = t / 24

    y = 1970
    do
      if ( mod(y,4) == 0 .and. (mod(y,100) /= 0 .or. mod(y,400) == 0) ) then
        leap = 366
      else
        leap = 365
      end if
      if ( days < leap ) exit
      days = days - leap
      y = y + 1
    end do

    if ( mod(y,4) == 0 .and. (mod(y,100) /= 0 .or. mod(y,400) == 0) ) then
      month_days = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    else
      month_days = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    end if

    m = 1
    do while ( days >= month_days(m) )
      days = days - month_days(m)
      m = m + 1
    end do
    d = days + 1

    write( str, '(I4,"-",I2.2,"-",I2.2," ",I2.2,":",I2.2,":",I2.2)' ) y, m, d, hh, mm, ss
  end function

  !-----------------------------------------------------------------------------
  ! Get file times using C stat (works with gfortran and ifx)
  !-----------------------------------------------------------------------------
  subroutine get_file_times ( filepath, t )
    character(*), intent(in)  :: filepath
    type(t_ty),   intent(out) :: t

    type(c_stat_t) :: stat_buf
    integer(c_int) :: status
    character(:), allocatable :: c_path

    c_path = trim(filepath) // c_null_char
    status = c_stat( c_path, stat_buf )

    if ( status /= 0 ) then
      t%accessed = ''
      t%modified = ''
      t%changed  = ''
      return
    end if

    t%accessed = unix_to_datetime( stat_buf%st_atime )
    t%modified = unix_to_datetime( stat_buf%st_mtime )
    t%changed  = unix_to_datetime( stat_buf%st_ctime )

  end subroutine

end module
