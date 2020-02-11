program binary_tree
use rbtree
implicit none
type(thetree) :: mytree
integer(kind=8) :: i
!integer, allocatable :: A(:)
!real, allocatable :: A(:)
character(len=8), allocatable :: A(:)
real, allocatable :: B(:)
real(kind=8) :: t0, t1
!A = [5,6,21,24,25,65,75,81,85,95]
!A = [5.5,6.6,21.21,24.24,25.25,65.65,75.75,81.81,85.85,95.95]
A = ["Aongding", "Biujuan1", "Cordon66", "Diao o12", "EinXiao2", &
    "Fuguang3", "Giaolei4", "HaoQin88", "HbiYan77", "Hiaoming"]
B = [1.11, 2.22, 3.33, 4.44, 5.55, 6.66, 7.77, 8.88, 9.99, 10.10]

print *, "The vector before sorting "
!write(*, '(*(i0, 1x))') A
!write(*, '(*(f8.3, 1x))') A
write(*, '(*(a, 1x))') A


! constructor
!mytree = thetree()

do i=1, size(A)
    !mytree = A(i)
    call mytree%add(A(i), B(i))
end do

print*, "Show the tree "
print *
call mytree%preorder()
print *

call mytree%inorder()
print *

call mytree%deleteValue(A(6))

print *
call mytree%preorder()
print *

call mytree%inorder()
print *


end program binary_tree
