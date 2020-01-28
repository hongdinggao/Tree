program binary_tree
use rbtree
implicit none
type(thetree) :: mytree
integer(kind=8) :: i
integer(kind=8), allocatable :: A(:)
integer(kind=8), allocatable :: B(:)
real(kind=8) :: t0, t1
A = [5,6,21,24,25,65,75,81,85,95]
!A = [7, 3, 11, 1, 5, 9, 13, 0, 2, 4, 6, 8, 10, 12, 14]
!A = [64, 90, 85, 91]

print *, "The vector before sorting "
write(*, '(*(i0, 1x))') A


print *, "The vector after sorting "
do i=1, size(A)
    mytree = A(i)
    !call mytree%add(A(i))
end do
!call mytree%preorder()
print *
!call mytree%inorder()


end program binary_tree
