module rbtree
  implicit none
  private
  integer, parameter :: RED = 0
  integer, parameter :: BLACK = 1
  integer, parameter :: DOUBLE_BLACK = 2

  type, public :: node
     integer(kind=8) :: va
     !integer :: color = RED 
     type(node), pointer :: left => null()
     type(node), pointer :: right => null()
     !type(node), pointer :: parent => null()
  end type node

  type, public :: thetree
     private
     type(node), pointer :: root => null()
     contains
     generic, public :: assignment(=) => add
     procedure :: add => addtree
     !procedure :: preorder
     !procedure :: inorder
     !procedure :: deleteValue
  end type thetree

  interface
     ! constructor of node
     recursive module subroutine  addnode(root, i)
        type(node), pointer, intent(inout) :: root
        integer(kind=8), intent(in) :: i
     end subroutine addnode

     module subroutine addtree(this, i)
        class(thetree), intent(inout) :: this
        integer(kind=8), intent(in) :: i
     end subroutine addtree

  end interface
end module rbtree
































