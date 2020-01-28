program binary_tree
use rbtree
implicit none
type(thetree) :: mytree
integer(kind=8) :: i
integer(kind=8), allocatable :: A(:)
real(kind=8) :: t0, t1
A = [5,6,21,24,25,65,75,81,85,95]
!A = [7, 3, 11, 1, 5, 9, 13, 0, 2, 4, 6, 8, 10, 12, 14]
!A = [64, 90, 85, 91]

print *, "The vector before sorting "
write(*, '(*(i0, 1x))') A


!call init_tree(mytree)

do i=1, size(A)
    mytree = A(i)
end do

print*, "Show the tree "
print *
call mytree%preorder()
print *

call mytree%inorder()


end program binary_tree
