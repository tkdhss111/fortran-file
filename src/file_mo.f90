module file_mo

  implicit none

  private
  public :: file_ty
  public :: dirname, filename, basename, extname, schemename
  public :: find, touch, rm, cp, mv
  public :: mkdir, rmdir, cldir

  type file_ty

    character(:), allocatable :: path     ! Full/relative path to a file
    character(:), allocatable :: dir      ! Directory name with tailing /
    character(:), allocatable :: name     ! File name with extention
    character(:), allocatable :: basename ! File name without extention
    character(:), allocatable :: ext      ! Extension of a file
    character(:), allocatable :: content  ! File content
    character(:), allocatable :: scheme   ! URI scheme {https, http, file}
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

  real, parameter :: KiB = 2.0 ** 20
  real, parameter :: MiB = 2.0 ** 30
  real, parameter :: GiB = 2.0 ** 40

contains

  subroutine touch ( this )
    class(file_ty), intent(inout) :: this
    call exec ( 'touch '//trim(this%path) )
    this%exist = .true.
  end subroutine

  subroutine rm ( this )
    class(file_ty), intent(inout) :: this
    if ( this%local ) then
      call exec ( 'rm '//trim(this%path) )
      this%exist = .false.
    else
      stop '*** Error: Can not remove file in remote'
    end if
  end subroutine

  subroutine cp ( from, to )
    class(file_ty), intent(inout) :: from
    type(file_ty),  intent(inout) :: to
    if ( from%local ) then
      call exec ( 'cp '//trim(from%path)//' '//trim(to%path) )
      to%exist = .true.
    else
      if ( from%exist ) then
        call exec ( 'curl --silent --fail-with-body --create-dirs --insecure '//trim(from%path)//' --output '//trim(to%path) )
      else
        stop '*** Error: Remote file does not exist'
      end if
    end if
  end subroutine

  subroutine mv ( from, to )
    class(file_ty), intent(inout) :: from
    type(file_ty),  intent(inout) :: to
    if ( from%local ) then
      call exec ( 'mv '//trim(from%path)//' '//trim(to%path) )
      from%exist = .false.
      to%exist   = .true.
    else
      stop '*** Error: Can not move file in remote'
    end if
  end subroutine

  subroutine mkdir ( this )
    class(file_ty), intent(inout) :: this
    if ( this%local ) then
      call exec ( 'mkdir -p '//trim(this%dir) )
    else
      stop '*** Error: Can not make directory in remote'
    end if
  end subroutine

  subroutine rmdir ( this )
    class(file_ty), intent(inout) :: this
    if ( this%local ) then
      call exec ( 'rmdir '//trim(this%dir) )
    else
      stop '*** Error: Can not remove directory in remote'
    end if
  end subroutine

  subroutine cldir ( this )
    class(file_ty), intent(inout) :: this
    type(file_ty), allocatable    :: files(:)
    integer i
    if ( .not. this%local ) then
      stop '*** Error: Can not clear files in remote'
    end if
    files = find ( dir =  this%dir )
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
    this%dir      = dirname    ( path )
    this%basename = basename   ( path )
    this%name     = filename   ( path )
    this%ext      = extname    ( path )
    this%scheme   = schemename ( path )
    if ( this%scheme == 'NA' ) then
      inquire ( file = this%path, exist = this%exist, size = this%size )
      this%local = .true.
    else
      call check_uri_file ( this )
      if ( this%scheme == 'file' ) then
        this%local = .true.
        this%path = uri2path ( this%path )
      else
        this%local = .false.
      end if
    end if
  end subroutine

  subroutine print_file ( this )
    class(file_ty), intent(in) :: this
    print '(a, l$)', 'File/Directory exists: ', this%exist
    if ( this%size < 2**10 ) then
      print '(a, i0, a$)', ', Size: ', this%size, 'B'
    else if ( this%size < 2**20 ) then
      print '(a, g0.2, a$)', ', Size: ', real(this%size) / KiB, 'KiB'
    else if ( this%size < 2**30 ) then
      print '(a, g0.2, a$)', ', Size: ', real(this%size) / MiB, 'MiB'
    else
      print '(a, g0.2, a$)', ', Size: ', real(this%size) / GiB, 'GiB'
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
    if ( p_comma > 0 ) then
      extname = path(p_comma:len_trim(path))
    else
      extname = ''
    end if
  end function

  pure function schemename ( path )
    character(*), intent(in)  :: path
    integer                   :: p_sep
    character(:), allocatable :: schemename
    p_sep = index( path, '://' )
    if (p_sep > 1) then
      schemename = path(1:p_sep-1)
    else
      schemename = 'NA'
    end if
  end function

  function find ( dir, pattern, ignore, maxdepth, fullpath, type, image ) result ( files )
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
    character(1000)                    :: path, pwd 
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
      filelist = trim(list_fnms)//trim(fno)
      inquire ( file = filelist, exist = exist )
      if ( .not. exist ) exit
      image_ = image_ + 1
    end do

    if ( fullpath_ ) then
      command = 'find "'//trim(dir_)//'"'//trim(maxdepth_)//&
                ' -type '//trim(type_)//&
                ' \( '//trim(pattern_)//trim(ignore_)//' \)'//&
                '| sort -n > '//trim(filelist)
    else
      command = 'find "'//trim(dir_)//'"'//trim(maxdepth_)//&
                ' -type '//trim(type_)//&
                ' \( '//trim(pattern_)//trim(ignore_)//' \)'//&
                '| sort -n | xargs -n 1 basename > '//trim(filelist)
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
      read ( u, '(a)' ) path
      files(i)%path = trim(path)
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
      stop '*** Error: Function "find" is not thread safe. Consider using "image" option.'
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

  subroutine check_uri_file ( this, image )
    class(file_ty),    intent(inout) :: this
    integer, optional, intent(in)    :: image
    integer :: image_
    character(512) :: cmd
    character(256) :: line
    character(256) :: tmpfile
    integer j, u, iostat
    if ( present( image ) ) then
      image_ = image
    else
      image_ = 1
    end if
    write( tmpfile, '(a, i0)' ) './tmp', image_
    write ( cmd, '(a, i0)' ) 'curl -sI '//trim(this%path)//' > '//trim(tmpfile)
    call exec ( cmd )
    open ( newunit = u, file = tmpfile, status = 'old' )
    do
      read ( u, '(a)', iostat = iostat ) line
      if ( iostat /=0 ) exit
      if ( index ( line, 'HTTP/' ) > 0 .and. index ( line, '200' ) > 0 ) then
        this%exist = .true.
        this%local = .false.
      end if
      j = index ( line, 'Content-Length:' )
      if ( j > 0 ) then
        read ( line(16:len_trim(line)), * ) this%size
        exit
      end if
    end do
    close ( u )
  end subroutine check_uri_file

  function uri2path ( uri ) result ( path )
    character(*), intent(in) :: uri
    character(len(uri))      :: path
    integer :: i, j
    path = ''
    j = 1
    ! Remove 'file://' prefix
    if (index(uri, 'file://') == 1) then
      do i = 8, len_trim(uri)
        ! On Windows, change '/' to '\'
#ifdef _WIN32
        if (uri(i:i) == '/') then
          path(j:j) = '\'
        else
          path(j:j) = uri(i:i)
        end if
#else
        path(j:j) = uri(i:i)
#endif
        j = j + 1
      end do
    else
      ! If no prefix, just copy
      path = uri
    end if
  end function uri2path

end module
