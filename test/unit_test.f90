program unit_test_file_mo

  use file_mo

  implicit none

  type(file_ty) :: file_new, file1, file2, file3, file_remote, file_local, file_local2
  type(file_ty) :: dir_b, dir_c, dir_d
  type(file_ty) :: file1_cp, file2_cp, file1_mv, file2_mv
  type(file_ty), allocatable :: files(:), dirs(:)
  integer i

  print *, '=========================================='
  print *, 'Test 1: Utility Functions'
  print *, '=========================================='

  print *, 'dirname    : ', dirname    ( './dir_test/dir_a/file_a1.txt' )
  print *, 'filename   : ', filename   ( './dir_test/dir_a/file_a1.txt' )
  print *, 'basename   : ', basename   ( './dir_test/dir_a/file_a1.txt' )
  print *, 'hostname   : ---'//hostname()//'---'
  print *, 'extname    : ', extname    ( './dir_test/dir_a/file_a1.txt' )
  print *, 'schemename : ', schemename ( 'https://github.com/tkdhss111/fortran-file/blob/main/test/dir_test/dir_a/file_a1.txt' )

  print *, '=========================================='
  print *, 'Test 2: Initialization'
  print *, '=========================================='

  print *, '------------------------------------------'
  print *, 'Test 2-1: Basic Initialization'
  print *, '------------------------------------------'

  call file1%init ( path = './dir_test/dir_a/file_a1.txt'  )
  call file2%init ( path = './dir_test/dir_a/file_a2.csv'  )
  call file3%init ( path = './dir_test/dir_a/file_a3.html' )

  call file1%print
  call file2%print
  call file3%print

  print *, '------------------------------------------'
  print *, 'Test 2-2: Initialization via File Search'
  print *, '------------------------------------------'

  files = find ( dir = './dir_test' )

  do i = 1, size(files)
    call files(i)%print
  end do

  ! With path that contains "=" in the path (need to be escaped by "/")
  files = find ( dir = './dir_test/dir=c' )

  do i = 1, size(files)
    call files(i)%print
  end do

  ! With pattern option (extract text file or HTML file)
  files = find ( dir = './dir_test', pattern = '*.txt|*.html' )

  do i = 1, size(files)
    call files(i)%print
  end do

  ! With ignore option (ignore text file or HTML file)
  files = find ( dir = './dir_test', ignore = '*.txt|*.html' )

  do i = 1, size(files)
    call files(i)%print
  end do

  ! With type option (find directory only)
  dirs = find ( dir = './dir_test/', type = 'd' )

  do i = 1, size(dirs)
    call dirs(i)%print
  end do

  print *, '=========================================='
  print *, 'Test 3: System Operation Functions'
  print *, '=========================================='

  print *, '------------------------------------------'
  print *, 'Test 3-1: touch'
  print *, '------------------------------------------'

  call touch ( file1 )
  call file1%print

  print *, '------------------------------------------'
  print *, 'Test 3-2: touch (type-bound procedure)'
  print *, '------------------------------------------'

  call file2%touch
  call file2%print

  call file3%touch
  call file3%print

  print *, '------------------------------------------'
  print *, 'Test 3-3: rm'
  print *, '------------------------------------------'

  call file_new%init ( path = './dir_test/dir_b/file_new.txt' )
  call file_new%touch
  call file_new%print

  call rm ( file_new )

  call file_new%print

  print *, '------------------------------------------'
  print *, 'Test 3-4: rm (type-bound procedure)'
  print *, '------------------------------------------'

  call touch ( file_new )

  call file_new%rm

  call file_new%print

  print *, '------------------------------------------'
  print *, 'Test 3-5: cp'
  print *, '------------------------------------------'

  call file1_cp%init ( path = './dir_test/dir_b/file_a1_copy.txt'  )

  call cp ( file1, file1_cp )

  call file1%print
  call file1_cp%print

  print *, '------------------------------------------'
  print *, 'Test 3-6: cp (type-bound procedure)'
  print *, '------------------------------------------'
  call file2_cp%init ( path = './dir_test/dir_b/file_a2_copy.txt'  )

  call file2%cp ( file2_cp )

  call file2%print
  call file2_cp%print

  print *, '------------------------------------------'
  print *, 'Test 3-7: mv'
  print *, '------------------------------------------'
  call file1_mv%init ( path = './dir_test/dir_b/file_a3_move.html' )

  call mv ( file1, file1_mv )

  call file1%print
  call file1_mv%print

  print *, '------------------------------------------'
  print *, 'Test 3-8: mv (type-bound procedure)'
  print *, '------------------------------------------'
  call touch ( file1 )
  call file2_mv%init ( path = './dir_test/dir_b/file_a3_move2.html' )

  call file1%mv ( file2_mv )

  call file1_mv%print
  call file2_mv%print

  print *, '------------------------------------------'
  print *, 'Test 3-9: mkdir'
  print *, '------------------------------------------'

  call dir_c%init ( path = './dir_test/dir_c/' )

  call mkdir ( dir_c )

  call dir_c%print

  print *, '------------------------------------------'
  print *, 'Test 3-10: mkdir (type-bound procedure)'
  print *, '------------------------------------------'

  call dir_d%init ( path = './dir_test/dir_d/' )

  call dir_d%mkdir

  call dir_d%print

  print *, '------------------------------------------'
  print *, 'Test 3-11: rmdir'
  print *, '------------------------------------------'

  call rmdir ( dir_c )

  print *, '------------------------------------------'
  print *, 'Test 3-12: rmdir (type-bound procedure)'
  print *, '------------------------------------------'

  call dir_d%rmdir

  print *, '------------------------------------------'
  print *, 'Test 3-13: cldir'
  print *, '------------------------------------------'

  call dir_b%init ( path = './dir_test/dir_b/' )
  call cldir ( dir_b )

  print *, '------------------------------------------'
  print *, 'Test 3-13: cldir (type-bound procedure)'
  print *, '------------------------------------------'

  call touch ( file1_mv )
  call dir_b%cldir

  print *, '------------------------------------------'
  print *, 'Test 4-1: Copy remote file via curl'
  print *, '------------------------------------------'

  call file_remote%init ( path = 'https://stats.dip.jp/css/tkd_crest_sakura.png' )
  call file_local%init ( path = './family_crest.png' )
  call file_remote%print
  call cp ( from = file_remote, to = file_local )
  !call mv ( from = file_remote, to = file_local ) ! Error
  !call rm ( file_remote ) ! Error

  print *, '-----------------------------------------------------------------'
  print *, 'Test 4-2: Copy remote file via curl with different file encodings'
  print *, '-----------------------------------------------------------------'

  call file_remote%init ( path = 'https://www.tepco.co.jp/forecast/html/images/juyo-d1-j.csv', encoding = 'SHIFT-JIS' )
  call file_local%init ( path = './utf8.csv', encoding = 'UTF-8' )
  call file_remote%print
  call cp ( from = file_remote, to = file_local )

  print *, '-----------------------------------------------------------------'
  print *, 'Test 4-3: Copy local file with different file encodings'
  print *, '-----------------------------------------------------------------'
  call file_remote%init ( path = 'https://www.tepco.co.jp/forecast/html/images/juyo-d1-j.csv', encoding = 'SHIFT-JIS' )
  call file_local%init ( path = './encoding=sjis.csv', encoding = 'SHIFT-JIS' )
  call cp ( from = file_remote, to = file_local )
  call file_local2%init ( path = './encoding=utf8.csv', encoding = 'UTF-8' )
  call file_local2%print
  call cp ( from = file_local, to = file_local2 )

  print *, '-----------------------------------------------------------------'
  print *, 'Test 4-4: Non-existent remote file'
  print *, '-----------------------------------------------------------------'
  call file_remote%init ( path = 'https://denkiyoho.hepco.co.jp/area/data/zip/202507-07_hokkaido_denkiyohou.zip' )
  call file_remote%print

  print *, '-----------------------------------------------------------------'
  print *, 'Test 4-5: Existent remote file'
  print *, '-----------------------------------------------------------------'
  call file_remote%init ( path = 'https://denkiyoho.hepco.co.jp/area/data/zip/202004-06_hokkaido_denkiyohou.zip' )
  call file_remote%print

  print *, '------------------------------------------'
  call touch ( file1 ) ! for recovery to the initial setup
end program
