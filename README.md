# fortran-file

Fortran Functions for File Manipulation

- Wrapper functions of system operation commands

- Manipulate file or directory with the commands:\
touch, cp, mv, rm, mkdir, rmdir, cldir, print and find

## file_ty

```
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
```

## Initialization

Initialization process stores file\_ty object with meta data of a file such as:\
directory name, file name, base name, extension name, file size, and content information.

The derived type member (logical) **exist** is used for the status of actual file existence.

1. Single File Initialization

```
type(file_ty) :: file1, file2

call file1%init ( path = './dir_a/file_a1.txt' )

! With content option
call file2%init ( path = './dir_a/file_a2.txt', content = 'Weather Data' )
```

2. Multiple Files Initialization via File Search

```
type(file_ty), allocatable :: files(:)

files = find ( dir = './dir_a' )
```

- **find** is a wrapper function of the system operation command "find".
- Available options are: pattern, ignore, maxdepth, fullpath, type, and image.
- Use notation "|" for "OR" operator in pattern/ignore option (e.g.,  pattern = '*.txt | *.csv').
- Use image option for this_image() in coarray multi-threaded environment to avoid filelist file crash.

### Print File

```
call file1%print
```
The above command prints file existence, file size and file path in the following way:

    File/Directory exists: T, Size: 0B, Path: ./dir_test/dir_a/file_a1.txt

### Make Empty File

An empty file on **file_a4%path** is made by the following command:

```
call touch ( file_a4 )
```
or
```
call file_a4%touch
```

**file_a4%exist**(default: .false.) becomes **.true.** after making the file.

    File/Directory exists: T, Size: 0B, Path: ./dir_test/dir_a/file_a1.txt 

### Remove File

A file on **file_a4%path** is removed by the following command:

```
call rm ( file_a4 )
```
or
```
call file_a4%rm
```

**file_a4%exist** becomes **.false.** after the removal of the file.

### Copy File

A file on **file_a1%path** is copied to the place of **file_c1%path** by the following command:

```
call cp ( file_a1, file_c1 ) 
```
or
```
call file_a1%cp ( file_c1 ) 
```

### Move File

A file on **file_a2%path** is moved to the place of **file_c2%path** by the following command:

```
call mv ( file_a2, file_c2 ) 
```
or
```
call file_a2%mv ( file_c2 ) 
```

**file_a2%exist** becomes **.false.** after the file move.

### Make Directory

A directory **dir_f** is created recursively (mkdir -p) by the following command:

```
type(file_ty) :: dir_f
call dir_f%init ( path = './dir_f/' ) ! The tailing "/" is required to recognize directory
```

```
call mkdir ( dir_f )
```
or
```
call dir_f%mkdir
```

### Remove Directory

A directory **dir_f** is removed if it is empty by the following command:

```
call rmdir ( dir_f )
```
or
```
call dir_f%rmdir
```

### Clear Directory

A directory **dir_f** becomes empty by the following command:

```
call cldir ( dir_f )
```
or
```
call dir_f%cldir
```

Only files within the directory are recursively removed.

### Obtain names

Use the following functions:

- **dirname** for directory name
- **filename** for file name with extension
- **basename** for file name without extension
- **extname** for extension name

```
dirname  ( './dir_test/dir_a/file_a1.txt' ) ! ./dir_test/dir_a/
filename ( './dir_test/dir_a/file_a1.txt' ) ! file_a1.txt 
basename ( './dir_test/dir_a/file_a1.txt' ) ! file_a1
extname  ( './dir_test/dir_a/file_a1.txt' ) ! .txt
```

## Test Directory and Files

dir_test\
├── dir_a\
│   ├── file_a1.txt\
│   ├── file_a2.csv\
│   └── file_a3.html\
└── dir_b (empty; not on github)
