program binary_tree
use, intrinsic :: iso_fortran_env
use rbtree
use mod_rowcol
implicit none
type(thetree) :: mytree
integer(int64) :: i
integer(int64), allocatable :: A(:)
!real, allocatable :: A(:)
!character(len=8), allocatable :: A(:)
!type(rowcol), allocatable :: A(:)
real, allocatable :: B(:)
real(real64) :: t0, t1
!A = [5,6,21,24,25,65,75,81,85,95]
!A = [5.5,6.6,21.21,24.24,25.25,65.65,75.75,81.81,85.85,95.95]
!A = ["Aongding", "Biujuan1", "Cordon66", "Diao o12", "EinXiao2", &
!    "Fuguang3", "Giaolei4", "HaoQin88", "HbiYan77", "Hiaoming"]
allocate(A(10))
do i=1, 10
   A(i) = i
end do

B = [1.11, 2.22, 3.33, 4.44, 5.55, 6.66, 7.77, 8.88, 9.99, 10.10]

!!print *, "The vector before sorting "
!write(*, '(*(i0, 1x))') A
!write(*, '(*(f8.3, 1x))') A
!write(*, '(*(a, 1x))') A
!!write(*, '(*(g0, 1x))') A

! constructor
!mytree = thetree()

call cpu_time(t0)
do i=1, size(A)
    !mytree = A(i)
    call mytree%add(A(i), B(i))
    !print*, mytree%has_key(A(i))
end do
call cpu_time(t1)
print*, "Time of setting up the tree ", t1 - t0

call cpu_time(t0)
print*, mytree%has_key(A(6))
call cpu_time(t1)
print*, "Time of check the key ", t1 - t0


call cpu_time(t0)
call  mytree%get(A(6))
call cpu_time(t1)
print*, "Time of get the value ", t1 - t0

print*, "Show the tree "
print *
call mytree%preorder()
print *

call mytree%inorder()
print *

print*, "The height of the tree is ", mytree%tree_height()
print*, "The total number of nodes is ", mytree%tree_size()

call mytree%deleteValue(A(6))

!print *
!call mytree%preorder()
!print *

!call mytree%inorder()
!print *

print*, "The height of the tree is ", mytree%tree_height()
print*, "The number of nodes now is ", mytree%tree_size()

end program binary_tree
